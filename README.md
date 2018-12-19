# airbnb-predictive-modeling
Predicting Airbnb NYC Rental Prices in New York City

“Which factors determine an AirBnb listing’s price?” was the question that kick started my journey into this competition. Tasked with creating the best predictive model, I had at my disposal 95 possible independent variables across 36,403 observations. 

Exploratory Data Analysis
I began by filtering out variables which were clearly not relevant such as those only containing URLs or locations which were the same across all the listings (since all listings were in New York, country, state, city etc. were not needed). 

Now that I had removed all irrelevant variables, it was time for me to start building a thorough understanding of the data. I began by looking at the distribution of the response variable, price. After removing outliers in price which had values of zero, it was time to start visually analyzing relationships between price and predictors.

For numeric variables, I created a correlation matrix which showed that weekly_price, cleaning_fee, accommodates, beds, bedrooms had a correlation of .4 or higher. However, I also noticed that beds had a high multicollinearity with accommodates. With beds no longer in consideration, I strongly felt that the four remaining variables would be used in my final model. 

For categorical variables, I utilized box and scatter plots to visually interpret a trend. Thanks to this process, I was able to find several strong predictors of price such as room_type, property_type, and neighbourhood_group_cleansed. I retained those with a weak level of correlation for further analysis. Lastly, I was able to further drop variables which either had little to no variation or showed no correlation visually. 

Feature Engineering
Before I went any further towards feature selection, I wished to make suitable changes to existing variables as well as engineer new ones. Undoubtedly, this section contained the most amount of missteps but also produced some of my strongest variables. I began by creating a function which reassigned all values in certain variables beneath a certain threshold to a single value. For instance, I reassigned any property with more than six bedrooms to simply have six bedrooms. Therefore, even if a property had 11 bedrooms, it was counted as six. My reasoning for this followed that after a certain number of bedrooms, there are diminishing returns on the impact on price. Similarly, I implemented this logic on five other variables. Although I cannot say for certain, I believe this made an impactful enough difference on my predictions. More importantly, this was the first function and for loop I created in R. It gave me confidence moving forward as this skill was going to be invaluable in the hurdles I was going to soon come across.

Using my newfound skills, I progressed to transform the bed_type column into a Boolean variable which distinguished whether a property had a real_bed or not. Unfortunately, this variable did not have enough variance to be significant. Hence, it was not used it in the models to follow. 

Next, it was time to engineer some features. At the forefront of understanding the data, I first looked at ways in which I could place myself in the owner’s shoes. I hoped to create features which could predict how an owner would price their property. I pondered whether the amount an owner writes in text fields makes such an impact. In fact, I counted the length of all textual fields from description to host_about to house_rules. Out of eleven of these new length variables, three made it past my feature selection methods into my final models. 

Similarly, I counted the number of uppercase letters used in the listing’s name and throughout all of a host’s writing. I also created two variables which looked for an exclamation mark or the word, “cozy” in the name respectively. Unfortunately, neither of these showed any significant correlation with price. However, I was rewarded with more experience with text analytics, a skill which evolved rapidly for me during the course of this competition. 

As my last foray into understanding the host’s perspective, I subtracted the host_since from a constant date to calculate how long an owner has been a host in addition to creating a Boolean column to identify whether a host has multiple listings.  The insight gained from this exercise supported my assumption that experienced hosts with multiple listings will price listings differently. 

My subsequent batch of features involved specific property features with a heavy focus on amenities and location. I first created a column to count the number of amenities offered by a listing. The process of dividing amenities into each of their own columns often took me down the annals of stackverflow.com. I used my ability to create functions and for loops to transform the original amenities column into a vector listing each different amenity for every observation. 

One major boon was the grouping or manual clustering of neighborhoods by price into six different groups. This process required me to manually impute missing values as well as create robust functions which filled missing values in the following process:
1.	Separate all rows with missing or incorrect zip codes (e.g. “1m”)
2.	Then for each row, use the neighbourhood_cleansed value to find the most common zip code for that neighbourhood.
3.	Assign that zip code value to that specific row

I found that using the zip codes column in my models did not yield positive results. Although latitude and longitude helped models have an idea of an observation’s location, I was not satisfied. I wished to somehow use zip codes as an ordinal factor. As that was not possible, I research methods to assign household incomes to zip codes. Since I could not extract values from Tableau, I manually created a dataset for zip code and incomes data. In the process, I also found population and national rank data associated with each zip code. Mapping zip code values to incomes, population and national rank was a very intensive process. I first had to match zip code levels from each dataset to each other. For those which did not, I repeated the same process as above, using neighbourhood_cleansed values to generate an alternate nearby zip code. Ultimately, this process was rewarding. Income had a relatively strong correlation with price. 

Missing Values
I realized early on that my accounting for missing values played a key role in the success of my predictions. First, I visualized the missing values with a “missmap” plot which displayed a heatmap of all variables’ missing values for each observation. I started from the bottom by manually imputing values for the sole reviews_per_month missing value and nineteen missing values in beds by utilizing the ratio of beds to accommodates in other observations. Similarly, I used ratios to impute values for weekly_price and monthly_price for listings which had either one.  

I then used the MICE package’s cart function to impute missing values. Since analysis data would have an unfair advantage over scoring data, I intentionally did not provide MICE a data frame with the price column. This ensured that imputations would be consistent in both sets of data. I created new columns in my main database for imputed values, security_deposit2 and cleaning_fee2. Although this process took a long time, it was not one ultimately worthwhile. In using boosting models later on, I found performance to be better when using columns with missing values instead of imputed ones. However, these still helped me run models such as linear regression, decision trees and random forest since they cannot work with missing values. 

Feature Selection
Progressing past the correlation matrices used earlier, I performed a hybrid step wise analysis for almost all features. I used the yielded features as my base to create a feature set. As an additional step, I also ran a lasso model to reinforce these findings. Overall, features selected in both methods were very similar. 

However, my main two feature selection tools turned out to be the summary function for various models (i.e. decision trees, random forest, boosting) as well as how the test RMSE was impacted by the addition of a single variable. This was an evolving process. I created multiple vectors with different features (i.e. used_vars1, used_vars2 and so on) to test different combinations of variables. 

Dimension Reduction
I conducted a Principal Component Analysis to merge all of my features into three components. Unfortunately, my approach was deeply flawed. Hence, the results were underwhelming. In retrospect, I should have selected only certain variables for PCA instead of all of them.

Data Set Up
Before I ran any models, I one-hot encoded all categorical variables using caret package’s dummyVars function. Next, I created the following automated process to normalize all numeric variables:
1.	Separated all possible numeric predictors in a single vector from my whole dataset
2.	Ran a for loop to check the skew of each variable (function from psych package)
3.	For variables with a skew higher than 0.8, I used the log function to normalize them
4.	I updated my dataset to only contain normalized values

While this process had a positive impact on RMSE, normalizing price in the same fashion yielded much worse RMSE results for unknown reasons. Hence, I left it untouched. Furthermore, I separated the dataset back into analysis and scoring data. I then used the caret package to further divide the analysis data into train and test datasets with an 85:15 split. I used 1450 group since Dr. Lala recommended to use groups equal to 5% of the total observations in the dataset.

Modelling
In the early days of the competition, I mainly used linear regression albeit unsuccessfully. My best RMSE from this type of model was ~60. Next, I used decision trees which yielded marginally better results. However, the order of nodes in combination with tuning the complexity parameter helped me better select features. 

Time turned out to be a precious resource in this competition. Although random forests took a significant amount of time to run, they did not yield the kind of results I expected (~54.8 best public RMSE). On the contrary, gradient boosting models performed much better in a fraction of the time. My next step was to perform cross validation with GBM, but my plans came to a quick end once my computer crashed after twelve hours of processing. From that point, I manually adjusted the model’s parameters such as the number of trees, learning rate, sample rate and depth to evaluate how the test RMSE was affected. Usually, improvements in the test RMSE translated in improvements of my Kaggle public score. Strangely, continuously increasing interaction depth led to constant improvements in the test RMSE. While I received good results, GBM also started running slower as I increased the number of trees and depth. I aimed to find an alternative.

My attempts to use my machine’s dedicated GPU to run models did not pan out since they required coding skills beyond my current scope. However, I found some middle ground with the h2o package which utilizes multiple CPU cores instead of a single one as in most other R packages. With this newfound ability to run complex and large models faster, I was able to relatively quickly test different parameters. As mentioned previously, this process entailed manually testing each and every variable and its effect on the test RMSE. I ended up using 51 variables out of 197 possible variables (not including dummy variables). These along with a manually tuned h2o GBM model yielded my best public Kaggle score. 

Unfortunately, although I realized I most likely overfit my model to the test and public RMSEs, I could not produce a less overfitted model which yielded similar results for the test and public RMSEs. For my private submission, instead of selecting the best public score, I selected the model which I thought contained the best balance between the train, test and public RMSEs. However, to no surprise, it was still not good enough as my private RMSE dropped by two points. Although the competition is over, my next objective is to create a model which matches or outperforms the efficacy of my final model without overfitting. 

Takeaway
In the process of exploring data, selecting the best features and creating predictive models, I was introduced to a vast amount of new knowledge. Extracting information from class sessions as well as outside resources, I had a wide variety of possible approaches in front of me. Due to time constraints, I had no choice but to be selective in deciding which approaches were worth attempting. The journey to my final model took many twists and turns. While many of my approaches ended up as failures, each made me more proficient at R and thus better at implementing my next idea.
