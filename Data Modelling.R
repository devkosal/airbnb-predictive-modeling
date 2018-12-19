####Setting up the datasets and normalizing####

#loading the dataset with all new variables pre added 
data = read.csv("airbnb/final_airbnb_data.csv")

#Following are the variables I chose for my final model. These were based on an extensive feature selction process which included correlation matrices, hybrid step-wise, lasso and summary functions of boosting or rf functions. At times, I calculated the effect on test RMSE of removing (or adding) a variable. Most of these mehtods are not coded here in the interest of this file's length. Please let me know if that is needed and I will send that section of my work as well. Also, you will note that quite a many engineered variables did not make the cut here (sadly!)

used_vars = c("price","id","group","income","neighbourhood_group_cleansed","accommodates","latitude","room_type","bathrooms","bedrooms", "availability_30", "Elevator", "property_type","longitude","since_last_review","review_scores_rating", "Indoor.fireplace","Cable.TV","Gym","review_scores_value", "minimum_nights", "reviews_per_month", "calculated_host_listings_count", "availability_365", "since_first_review", "guests_included", "Free.street.parking","Washer", "review_scores_cleanliness", "cancellation_policy", "Suitable.for.events", "Doorman", "Family.kid.friendly", "review_scores_checkin", "len_space", "is_business_travel_ready", "host_length", "len_description", "len_host_about", "num_amenities", "number_of_reviews", "extra_people", "weekly_price","security_deposit","cleaning_fee", "square_feet", "Fire.extinguisher", "Paid.parking.off.premises", "Private.entrance", "Microwave", "name_upper" , "Shampoo", "Self.check.in")
#53 variables (2 are not predictors: price and id)

#subsetting data with only the used varaibles
data2 = subset(data,select = used_vars) 

#creating dummy variables 
library(caret)
dmy = dummyVars(" ~ .", data = data2, drop2nd = T) 

#creating new columns for dummy variables 
data3 = data.frame(predict(dmy, newdata = data2)) 

#Fixing the skew of skewed numeric variables
library(psych)
#identifying numeric names (please disregard those which are not present in used_vars)
numericVarNames = c("accommodates","host_id","group2",  "extra_people",  "minimum_nights", "maximum_nights", "availability_30", "availability_365", "number_of_reviews", "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", "review_scores_location",   "review_scores_value", "calculated_host_listings_count","reviews_per_month",  "len_name","len_summary","len_space","len_notes", "len_description", "len_neighborhood_overview","len_transit", "len_access","len_house_rules",  "len_host_about","len_words",  "num_amenities","host_length", "since_first_review","since_last_review","cleaning_fee2","income","rank","population","security_deposit2","price","security_deposit","cleaning_fee","bathrooms",   "bedrooms", "group" ,  "guests_included")


DFnumeric = data3[, names(data3) %in% numericVarNames[numericVarNames %in% used_vars]]
DFnumeric = DFnumeric[, names(DFnumeric) != 'price']
DFnonNumeric = data3[, (!names(data3) %in% numericVarNames)] #also includes price and id since they are not predictors 
cat('There are', length(DFnumeric), 'numeric predictors, and', length(DFnonNumeric), 'non-numeric variables')

#fixes skew of those numeric columns 
for(i in 1:length(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>.8){
    cat(colnames(DFnumeric)[i]," -   ",skew(DFnumeric[,i]),"  ")
    DFnumeric[,i] = log(DFnumeric[,i] +1)
  }
}
PreNum = preProcess(DFnumeric, method=c("center", "scale"))
DFnorm = predict(PreNum, DFnumeric)

#combining the numeric and non numeric varaibles 
data4 = cbind(DFnorm, DFnonNumeric) 
data4["price"] = data3$price 

a = data4[!is.na(data4$price),] # analysis data
s = data4[is.na(data4$price),] # scoring data


#3. Splitting the anlaysis data into test and train####
library(caret)
set.seed(100)
split = createDataPartition(y = a$price,p = 0.85,list = F,groups=1450) # groups are ~5% of total number of rows
train = a[split,]
test = a[-split,]

#4. Train and test modelling with H2o GBM (if you wish to skip directly to training on all of the data, skip to the next section)####
library(h2o)
h2o.init() #needs Java JDK 64 bit installed
#https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
#h2o.shutdown(prompt = TRUE) #in case the local host server(?) becomes slow or crashes

#need to convert normal dfs into h2o compatible data frames
train.hex <- as.h2o(train, destination_frame = "train.hex")
test.hex <- as.h2o(test, destination_frame = "test.hex")
drops <- c("price","id")

#setting up the model with tuned paramters
hbm = h2o.gbm(x = colnames(train[ , !(names(train) %in% drops)]),
              y = "price",
              training_frame = train.hex,
              validation_frame = test.hex,
              ntrees = 3500, max_depth = 11,
              seed = 101, 
              learn_rate = 0.008, 
              learn_rate_annealing = 1,
              sample_rate = .65, sample_rate_per_class = NULL, col_sample_rate = .5,
              col_sample_rate_change_per_level = 1,
              col_sample_rate_per_tree = .5,
              min_split_improvement = 1e-06)


#view train and test rmse along with other information
summary(hbm)
#train RMSE is 17.60582 and test is 48.10652

#View variable importance
View(h2o.varimp(hbm))


#5. Building the model using all of the analysis data####
library(h2o)
h2o.init()

a.hex <- as.h2o(a, destination_frame = "a.hex")
s.hex <- as.h2o(s, destination_frame = "s.hex")
drops <- c("price","id")

hbms = h2o.gbm(x = colnames(a[ , !(names(a) %in% drops)])
               ,y = "price",
               training_frame = a.hex,
               ntrees = 3500, max_depth = 11,
               seed = 100, 
               learn_rate = 0.008, 
               learn_rate_annealing = 1,
               sample_rate = .65, sample_rate_per_class = NULL, col_sample_rate = .5,
               col_sample_rate_change_per_level = 1,
               col_sample_rate_per_tree = .5,
               min_split_improvement = 1e-06)
summary(hbms)
#train rmse is 18.2618

#View feature importance once again
View(h2o.varimp(hbms))

#Submission file creation#####
hpreds = h2o.predict(hbms,s.hex)
submissionFile = data.frame(id = s$id, price = as.vector(hpreds))
write.csv(submissionFile, 'Sharma_submission.csv',row.names = F)


#I have chosen not to include any other models since this is the one I used for my lowest RMSE score (as per the emailed instructions).

#Thank you for reading

