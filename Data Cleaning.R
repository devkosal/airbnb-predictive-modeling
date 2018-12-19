####################################################################################
####################  PREDICTING NYC AIRBNB LISTING'S PRICES ###########################
####################################################################################

#Setting the working directory
setwd("/Users/devsharma/Dropbox/Education/Data Science/R Learning")

#Loading the data (set stringAsFactors as F to prevent NA values when merging all frames later on)
an = read.csv('airbnb/raw_analysisData.csv',stringsAsFactors = F)
sc = read.csv('airbnb/raw_scoringData.csv',stringsAsFactors = F)

#1. Preparing the data####

#Preliminary changes before combing the all frames into one
#visaulaing how price is distributed to get a better understanding of what is being predicted 
library(ggplot2)
ggplot(data=an, aes(x=price)) + 
  geom_histogram(fill="blue", binwidth = 10)
table(an$price)
an = an[!an$price==0,] #Removing all observations where price is 0 since this is erroneous data
sc$price = NA #creating a price column with null values in scoring all so it can be merged with analysis all

#Combing the analysis and scoring all into a single preliminary all frame so I can later clean the all in both at once
all_prelim = rbind(an, sc)

#Dropping irrellevant variables based on initial manual selection
all = subset(all_prelim, select = -c(listing_url,thumbnail_url,medium_url,xl_picture_url,listing_url,last_scraped,picture_url,host_url,host_thumbnail_url,host_picture_url,street,city,country,state,smart_location,country_code,market,jurisdiction_names,calendar_last_scraped))

#Using a corrplot to check for correlations between numeric variabels and price
library(corrplot)
#first I isolate the numeric variables 
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
#then construct a correlation all frame
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sorting on decreasing correlations with price
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
print(cor_sorted)
#selecting only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
#These results will be impact my feature selction process moving forward

#Further evaluating the effect of certiain variables (mostly catagorical) on price by visualizing them with ggplot
ggplot(data=all, aes(x=factor(accommodates), y=price))+
  geom_boxplot(col='blue') + labs(x='Accommodates')

ggplot(data=all, aes(x=cleaning_fee, y=price))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))

box = geom_boxplot(varwidth=T, fill="steelblue") #box plot vairable to be used for catagorical variables
scatter = geom_point(color="steelblue") #scatter plot vairable to be used for continuous variables

#creating a temporary df to visualize analysis data
all_temp = all[!is.na(all$price),]
#the following either showed a visible impact on price or were worth further exploring
ggplot(all_temp, aes(x=is_business_travel_ready,y=price)) + box
ggplot(all_temp, aes(x=property_type,y=price)) + box
ggplot(all_temp, aes(x=room_type,y=price)) + box
ggplot(all_temp, aes(x=bathrooms,y=price)) + scatter
ggplot(all_temp, aes(x=bedrooms,y=price)) + scatter
ggplot(all_temp, aes(x=guests_included,y=price)) + scatter
ggplot(all_temp, aes(x=extra_people,y=price)) + scatter
ggplot(all_temp, aes(x=minimum_nights,y=price)) + scatter
ggplot(all_temp, aes(x=maximum_nights,y=price)) + scatter
ggplot(all_temp, aes(x=number_of_reviews,y=price)) + scatter
ggplot(all_temp, aes(x=bed_type,y=price)) + box
ggplot(all_temp, aes(x=latitude,y=price)) + scatter
ggplot(all_temp, aes(x=longitude,y=price)) + scatter
ggplot(all_temp, aes(x=cancellation_policy,y=price)) + box
ggplot(all_temp, aes(x=instant_bookable,y=price)) + box


#the following showed hardly any affect on price or possesses no variation. I will drop most of these in the next steps
ggplot(all_temp, aes(x=host_is_superhost,y=price)) + box
ggplot(all_temp, aes(x=has_availability,y=price)) + box
ggplot(all_temp, aes(x=requires_license,y=price)) + box
ggplot(all_temp, aes(x=require_guest_profile_picture,y=price)) + box
ggplot(all_temp, aes(x=require_guest_phone_verification,y=price)) + box
ggplot(all_temp, aes(x=host_response_rate,y=price)) + scatter
ggplot(all_temp, aes(x=host_has_profile_pic,y=price)) + box
ggplot(all_temp, aes(x=is_location_exact,y=price)) + box

#dropping more visibly insignifacnt variables based on manual feature selection
all = subset(all,select = -c(has_availability,requires_license,require_guest_profile_picture, require_guest_phone_verification,license,host_response_rate,license))

#Grouping lesser used levels of certain variables to make modelling easier later on 
table(all$property_type)
table(all$bedrooms)


#creating a function to perform this action
removeLevels = function(vect,threshold,alternate){
  #'vect: a vector (or a all frame's column)
  #'threshold: num, values under this value will be grouped into a single value
  #'alternate: str or num, the value which will reaplce levels under the threshold
  l = levels(as.factor(vect))
  t = table(vect)
  for(i in l){
    if (t[i] < threshold){
      vect[vect == i] <- alternate
    }
  }
  return(vect)
}

#performing the function on various hand-picked variables and converting some to factor later use. Thresholds and alternate values were chosen based on the results the table() fucntion
all$property_type = as.factor(removeLevels(all$property_type,100,"Other"))
all$bedrooms = removeLevels(all$bedrooms,60,5)
all$bathrooms = removeLevels(all$bathrooms,40,5)
all$cancellation_policy = removeLevels(all$cancellation_policy,20,"strict")
all$guests_included = removeLevels(all$guests_included,50,8)
all$calculated_host_listings_count = removeLevels(all$calculated_host_listings_count,200,9)

#creating a specific for loop to transform bed_type into  2-level "t" or "f" varaible indicating whether a real_bed is offered
#bed_type
lbe = levels(as.factor(all$bed_type))
tbe = table(all$bed_type)
for(i in lbe){
  if (tbe[i] < 30000){
    all$bed_type[all$bed_type == i] = "f"
  } else{
    all$bed_type[all$bed_type == i] = "t"
  }
}
table(all$bed_type)
colnames(all)[which(names(all) == "bed_type")] <- "real_bed"
all$real_bed = as.factor(all$real_bed)


#Checking for missing values (takes some time to run)
library(Amelia)
missmap(all[-1], col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8) #to get a braod picture of what I am dealing with

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

#Manually imputing values for reviews_per_month, beds, some weekly price columns which contain monthly_prices
all[is.na(all$reviews_per_month),]$reviews_per_month = 0
all[is.na(all$beds),]$beds = all[is.na(all$beds),]$accommodates*mean(all$beds/all$accommodates,na.rm=T) #imputing this based on accommodates since it seems fairly logical
all[!is.na(all$monthly_price) & is.na(all$weekly_price),]$weekly_price = all[!is.na(all$monthly_price) & is.na(all$weekly_price),]$monthly_price/mean(all$monthly_price/all$weekly_price,na.rm=T)


#Will reutrn to using advanced methods of imputing missing values after some more cleaning up feature engineering

#Cleaning up and prepping all for modelling

#Since I wish to use neighbourhood as a feature, I check to see if some neighbourhoods in scoring all don't exist in analysis.

for(i in levels(as.factor(sc$neighbourhood))){
  if(i %in% setdiff(an$neighbourhood, sc$neighbourhood)){
    print(i)
  }
}
#looks like we are all good.

#Feature Engineering

#Creating features based on length of strings 
all$len_name = nchar(all$name)
all$len_summary = nchar(all$summary)
all$len_space = nchar(all$space)
all$len_notes = nchar(all$notes)
all$len_description = nchar(all$description)
all$len_experiences_offered = nchar(all$experiences_offered)
all$len_neighborhood_overview = nchar(all$neighborhood_overview)
all$len_transit = nchar(all$transit)
all$len_access = nchar(all$access)
all$len_house_rules = nchar(all$house_rules)
all$len_host_about = nchar(all$host_about)
all$len_words = nchar(all$summary) + nchar(all$space) + nchar(all$notes) + nchar(all$description) + nchar(all$experiences_offered) + nchar(all$neighborhood_overview) + nchar(all$transit) + nchar(all$access) + nchar(all$house_rules) + nchar(all$host_about) + nchar(all$amenities)

#number of amenities
x = strsplit(all$amenities, ",")
y = c()
for (i in 1:length(x)){
  y[i] = length(x[[i]])
}
all$num_amenities = y

#host_since_length
all$host_since = as.Date(all$host_since)
all$host_length = as.integer(as.Date("2018-11-29") - all$host_since)

#since_first_review
all$first_review = as.Date(all$first_review)
all$since_first_review = as.integer(as.Date("2018-11-29") - all$first_review)

#since_last_review
all$last_review = as.Date(all$last_review)
all$since_last_review = as.integer(as.Date("2018-11-29") - all$last_review)

#whether a host has multiple listings
greaterThanOne <- function(n){
  if(n>1){
    return(1)
  }else{
    return(0)
  }
}
all$multiple_host_listings = unlist(lapply(all$calculated_host_listings_count,greaterThanOne),use.names=FALSE)

#whether name contains an exclamation mark
containsExclaim = function(s){
  if(grepl("!", s)){
    return(1)
  }else
    return(0)
}
all$exclaim = unlist(lapply(all$name,containsExclaim), use.names = F)

#number of uppercase letters in names
library(plyr)
library(stringr)
all$name_upper = ldply(str_match_all(all$name,"[A-Z]"),length) # counting how many upper case letters are in name


#number of uppercase letters used in textual fields
all_strings = paste(all$space,all$description,all$summary,all$notes,all$experiences_offered,all$neighborhood_overview,all$transit,all$access,all$house_rules,all$host_about,all$name)

all$uppers = ldply(str_match_all(all_strings,"[A-Z]"),length)


#creating varaibles for each separate amenity (with >300 occurences)
#function to clean up the amenities string and then convert into a vector
amenitiesToVector = function(string){ 
  return(strsplit(gsub("([a-z])([A-Z])", "\\1 \\2", gsub("\\W","",gsub("TV","Tv",gsub("translation missing: en.hosting_amenity_50","",gsub("translation missing: en.hosting_amenity_49","",string))))), " ")[[1]])
}
all$amenities2 = lapply(all$amenities,amenitiesToVector) #takes time to run

#creating a fucntion to count the frequency of each amenity
counter = function(vector_list){
  #' vector is a list of vectors 
  #' returns hte 
  every_amen = c()
  for(i in 1:length(vector_list)){
    every_amen = c(every_amen, vector_list[[i]])
  }
  return(table(every_amen))
}
counter(all$amenities2) = amenities_count #(takes a lot of time to run)
#this fucntion retruns a table similar to this (the following is converted to list)

amentieis_freq = list(35619, 34335, 33902, 32538, 31039, 30300, 25082, 23874, 23863, 23496, 22253, 21598, 21033, 17181, 15272, 13934, 13796, 12732, 12371, 11892, 11684, 10702, 9933, 9680, 9451, 8952, 7352, 6791, 6603, 6517, 6447, 6363, 6328, 6006, 5931, 5427, 4995, 4739, 4256, 3938, 3749, 3685, 3674, 3653, 3637, 3489, 3411, 3336, 3167, 2985, 2761, 2546, 2445, 2401, 2065, 2043, 1746, 1652, 1631, 1593, 1448, 1426, 1422, 1415, 1333, 1255, 1226, 1167, 1088, 1052, 1033, 936, 909, 872, 860, 847, 841, 815, 798, 752, 750, 728, 532, 512, 509, 491, 476, 468, 396, 371, 352, 344, 340, 315, 312)
names(amentieis_freq) = c('Wifi','Heating','Kitchen','Essentials','Airconditioning','Smokedetector','Hangers','TV','Shampoo','Carbonmonoxidedetector','Hairdryer','Laptopfriendlyworkspace','Iron','Internet','Familykidfriendly','Washer','Dryer','Fireextinguisher','Firstaidkit','Buzzerwirelessintercom','translationmissing:en.hostingamenity50','Lockonbedroomdoor','CableTV','Elevator','translationmissing:en.hostingamenity49','Hotwater','Refrigerator','Dishesandsilverware','Bedlinens','Stove','Cookingbasics','Selfcheckin','Oven','Microwave','24hourcheckin','Coffeemaker','Extrapillowsandblankets','Stepfreeaccess','Safetycard','Petsallowed','Privateentrance','Longtermstaysallowed','Petsliveonthisproperty','Hostgreetsyou','Lockbox','Breakfast','Luggagedropoffallowed','Freeparkingonpremises','Freestreetparking','Dishwasher','Bathtub','Privatelivingroom','Widedoorway','Gym','Wheelchairaccessible','Doorman','Smokingallowed','Welllitpathtoentrance','Cats','Dogs','Keypad','Patioorbalcony','Hottub','Indoorfireplace','Gardenorbackyard','Roomdarkeningshades','Ethernetconnection','Paidparkingoffpremises','Suitableforevents','Widehallwayclearance','Other','Wideclearancetobed','Singlelevelhome','Frontdeskdoorperson','Windowguards','Children’sbooksandtoys','Accessibleheightbed','Flatpathtofrontdoor','Wideentryway','PacknPlaytravelcrib','BBQgrill','Accessibleheighttoilet','Babysitterrecommendations','toilet','Cleaningbeforecheckout','Pocketwifi','Smartlock','Highchair','Childrensdinnerware','Crib','Wideclearancetoshower','Gameconsole','Handheldshowerhead','Paidparkingonpremises','Pool') 

checkAmenities = function(vector,n){
      if(n %in% vector){
        return(1)
      }else{
        return(0)
      }
    }
#function to create new columns (takes time to run)
for(n in names(amentieis_freq)){
  all[n] = lapply(all$amenities2,checkAmenities,n=n) 
}

#For some reason, this code (e.g. all$group = lapply(all$amenities2,checkAmenities,n="Wifi") does't work in R. Thus, as per Dr. Lala's instructions, here is the code I used to create the columns in Python (you will find the CSV file with the columns pre loaded as part of this submission)

# for c in range(len(amentieis_vals)):
#  all[amentieis_vals[c][0]] = all["amenities2"].apply(lambda x: 1 if cnt[c][0] in x else 0)


#binning neighbourhoods by price
library(dplyr)
all$neighbourhood_cleansed[all$neighbourhood_cleansed =="Hollis Hills"] = "Bayside"
all$neighbourhood_cleansed[all$neighbourhood_cleansed =="Westerleigh"] = "Castleton Corners"
freq = all %>% 
  group_by(neighbourhood_cleansed) %>% 
  count(neighbourhood_cleansed) %>%
  arrange(desc(neighbourhood_cleansed))
Mean = aggregate(price~neighbourhood_cleansed,all,mean)
cluster = data.frame(neighbourhood_cleansed = Mean$neighbourhood_cleansed, Mean = Mean$price, freq = freq$n)
summary(cluster)


#creating quantiles to help me form clusters in the next step
library(stats)
quantile(cluster$mean, .16667*c(1,2,3,4,5)) 
quantile(a$price,.16667*c(1,2,3,4,5))

price1a = subset(cluster, Mean<74)
price2a = subset(cluster, Mean>=74 & Mean < 91.793)
price3a = subset(cluster, Mean>=91.793 & Mean < 110)
price4a = subset(cluster, Mean>=110 & Mean < 135)
price5a = subset(cluster, Mean>=135 & Mean < 170)
price6a = subset(cluster, Mean>=170)

getGroups = function(x){
  if(x %in% price1a$neighbourhood_cleansed){
    return(1)
  }else if(x %in% price2a$neighbourhood_cleansed){
    return(2)
  }else if(x %in% price3a$neighbourhood_cleansed){
    return(3)
  }else if(x %in% price4a$neighbourhood_cleansed){
    return(4)
  }else if(x %in% price5a$neighbourhood_cleansed){
    return(5)
  }else if(x %in% price6a$neighbourhood_cleansed){
    return(6)
  }
}
#manual clustering
all$group = unlist(lapply(all$neighbourhood_cleansed,getGroups),use.names=FALSE)

#Converting certain variables to factors (zipcodes to strings to edit later)
all$zipcode = toString(all$zipcode)
all$instant_bookable = as.factor(all$instant_bookable)
all$is_business_travel_ready = as.factor(all$is_business_travel_ready)
all$host_has_profile_pic = as.factor(all$host_has_profile_pic)
all$host_identity_verified = as.factor(all$host_identity_verified)
all$host_is_superhost = as.factor(all$host_is_superhost)
all$is_location_exact = as.factor(all$is_location_exact)
all$room_type = as.factor(all$room_type)


#fixing zip codes (changing some to adajcent zipcodes for modelling putposes)
table(all$zipcode) # notice some inconsistent values
all$zipcode[all$zipcode=="11103-3233"] = 11103
all$zipcode[all$zipcode=="11249\n11249"] = 11249
all$zipcode[all$zipcode=="10003-8623"] = 10003
all$zipcode[all$zipcode=="11413-3220"] = 11413
all$zipcode[all$zipcode=="10065"] = 10021
all$zipcode[all$zipcode==11249] = 11211
all$zipcode[all$zipcode=="11249"] = 11211
all$zipcode = as.factor(all$zipcode) #convert to factor


#mapping zipcode to household income (Since Tableau did not work, I manually imported median household income, national rank and population from http://zipatlas.com/us/ny/zip-code-comparison/median-household-income.htm)
#This data is already implemented in the csv I provided with my submission. If you need this separately, please let me know and I can send it over.
library(readxl)
zips = read_xlsx("zipdata.xlsx")
#I was unable to find a dictionary structure in R similar to Python to perform this task. In addition, this task required me to create for loops which filled missing zipcode values based on the most popular neighbourhood zipcode of a listing (which has a missing zip). Here is the Python code (once again, this is also already done in the CSV also submitted)
# def checkZips():
#   """
#   returns if there are any zipcodes which are missing in either the AirBnb all or Zipcode/Income   all
#   """
# zfix = []
# for i in all["zipcode"].value_counts().index.astype(str):
#   if i not in list(zip["zipcode"].astype(str)):
#   zfix.append(i)
# return zfix
# checkZips()
# 
# #manual corrections based on google searching the nearest zipcodes
# all.loc[(all["neighbourhood_cleansed"]=="Westerleigh") & (pd.isnull(all["zipcode"])),"zipcode"] = 10314
# 
# for z in all[pd.isnull(all["zipcode"])]["neighbourhood_cleansed"].value_counts().index:
#   zx = all[(pd.notnull(all["zipcode"])) & (all["neighbourhood_cleansed"]==z)]["zipcode"]
#   if len(zx) > 0:
#     all.loc[(all["neighbourhood_cleansed"]==z) & (pd.isnull(all["zipcode"])),"zipcode"] = zx.value_counts().index[0]
#   else:
#     print("we got a problemo")
# 
# for z in checkZips():
#   n = all[all["zipcode"]==z]["neighbourhood_cleansed"].value_counts().index[0]
#   zx2 = all[(pd.notnull(all["zipcode"])) & (all["neighbourhood_cleansed"]==n)]["zipcode"]
# if len(zx2) > 0:
#   all.loc[(all["neighbourhood_cleansed"]==n) & (all["zipcode"]==z),"zipcode"] = zx2.value_counts().index[0] 
# else:
#   print("we got a problemo")
# 
# zfix2 = []
# for z in all[all["zipcode"]==""]["id"]:
#   zfix2.append(z)
# 
# for i in zfix2:
#   n = list(all[all["id"]==i]["neighbourhood_cleansed"])[0]
#   zx3 = all[all["neighbourhood_cleansed"]==n]["zipcode"]
# if len(zx3) > 0:
#   all.loc[all["id"]==i,"zipcode"] = zx3.value_counts().index[0]
# else:
#   print("we got a problemo")
# 
# all["zipcode"] = all["zipcode"].astype(str)
# zip["zipcode"] = zip["zipcode"].astype(str)
# zip["rank"] = zip["rank"].str.replace(",","")
# zip["rank"] = zip["rank"].astype(int)
# 
# #creating the zip to income and population dictionaries
# income = pd.Series(zi.income.values,index=zi.zipcode).to_dict()
# population = pd.Series(zi.population.values,index=zi.zipcode).to_dict()
# rank = pd.Series(zip["rank"].values,index=zi.zipcode).to_dict()
# 
# #mapping the values to new columns
# all["zipcode2"] = all["zipcode"]
# all["income"] = all["zipcode2"].map(income)
# all["population"] = all["zipcode2"].map(population)
# all["rank"] = all["zipcode2"].map(rank)


#Using mice's cart method to impute missing values for cleaning_fee and security_deposit. note: I subset my all_frame to not include price and any other variables with other missing values. 

#look at what's missing again first (takes a long time to run so hashed out)
missmap(all[-1], col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)
sort(sapply(all, function(x) { sum(is.na(x)) }), decreasing=TRUE)

library(mice)
all_mice = subset(all, select = c(host_total_listings_count, host_has_profile_pic,
                                   longitude, is_location_exact,             
                                   property_type,room_type,                     
                                   accommodates,bathrooms,                     
                                   bedrooms,real_bed,
                                   guests_included, extra_people, cleaning_fee,security_deposit,
                                   minimum_nights,maximum_nights,                
                                   availability_30,availability_365,number_of_reviews, 
                                   review_scores_rating,review_scores_accuracy,
                                   review_scores_cleanliness,review_scores_checkin,         
                                   review_scores_communication,review_scores_location,        
                                   review_scores_value,instant_bookable,              
                                   is_business_travel_ready,cancellation_policy,           
                                   calculated_host_listings_count,reviews_per_month,             
                                   len_name,len_summary,len_space,len_notes,                     
                                   len_description,len_experiences_offered,       
                                   len_neighborhood_overview,len_transit,                   
                                   len_access,len_house_rules,              
                                   len_host_about,len_words,                     
                                   num_amenities,host_length,                   
                                   since_first_review,since_last_review
))
imputed_all <- mice(all_mice, m=4,method="cart",printFlag=FALSE)

#create new columns with complete set of values in 
all$cleaning_fee2 = mice::complete(imputed_all)$cleaning_fee
all$security_deposit2 = mice::complete(imputed_all)$security_deposit

#checking corr again including new engineered variables
all_numVar =- all[, numericVars]
cor_numVar = cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with price
cor_sorted = as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
cor_sorted
#select only high corelations
CorHigh = names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar = cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
#Looks like none of our new variables made the cut but the imputed column of cleaning fee still has a high correlation with price

#This concludes the all cleaning process, so I will export the all to a CSV file, preventing the need to reimpute values again
write.csv(all, "clean_airbnb_data.csv",row.names = F)