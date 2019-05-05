## Problem Statement:

# The data scientists at BigMart have collected 2013 sales data for 1559 products across 10 stores in different cities. Also, certain attributes of each product and store have been defined. The aim is to build a predictive model and find out the sales of each product at a particular store.
# 
# Using this model, BigMart will try to understand the properties of products and stores which play a key role in increasing sales.
# 
# 
# Hypothesis Generation :
#   
# Store Level Hypotheses
# 
# 
# City type: Stores located in urban or Tier 1 cities should have higher sales because of the higher income levels of people there.
# 
# Population Density: Stores located in densely populated areas should have higher sales because of more demand. Store Capacity: Stores which are very big in size should have higher sales as they act like one-stop-shops and people would prefer getting everything from one place
# 
# Competitors: Stores having similar establishments nearby should have less sales because of more competition.
# 
# Marketing: Stores which have a good marketing division should have higher sales as it will be able to attract customers through the right offers and advertising.
# 
# Location: Stores located within popular marketplaces should have higher sales because of better access to customers.
# 
# Ambiance: Stores which are well-maintained and managed by polite and humble people are expected to have higher footfall and thus higher sales.
# 
# Product Level Hypotheses
# 
# 
# Brand: Branded products should have higher sales because of higher trust in the customer.
# 
# Packaging: Products with good packaging can attract customers and sell more.
# 
# Utility: Daily use products should have a higher tendency to sell as compared to the specific use products.
# 
# Display Area: Products which are given bigger shelves in the store are likely to catch attention first and sell more. Visibility in Store: The location of product in a store will impact sales. Ones which are right at entrance will catch the eye of customer first rather than the ones in back.
# 
# Advertising: Better advertising of products in the store will should higher sales in most cases. Promotional Offers: Products accompanied with attractive offers and discounts will sell more.
# 
# Customer Level Hypotheses
# 
# 
# Customer Behavior: Stores keeping the right set of products to meet the local needs of customers will have higher sales.
# 
# Job Profile: Customer working at executive levels would have higher chances of purchasing high amount products as compared to customers working at entry or mid senior level.
# 
# Family Size: More the number of family members, more amount will be spent by a customer to buy products
# 
# Annual Income: Higher the annual income of a customer, customer is more likely to buy high cost products. Past Purchase History: Availablity of this information can help us to determine the frequency of a product being purchased by a user.
# 
# Macro Level Hypotheses
# 
# 
# Environment: If the environment is declared safe by government, customer would be more likely to purchase products without worrying if it's environment friendly or not.
# 
# Economic Growth: If the current economy shows a consistent growth, per capita income will rise, therefore buying power of customers will increase.



rm(list=ls(all=TRUE))

#set working directory
setwd("C:/Users/Hai/Desktop/avmlhack/avmlhack/BigMartSales")

#Loading Packages



library(data.table) # used for reading and manipulation of data 
library(dplyr)      # used for data manipulation and joining 
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling 
library(corrplot)   # used for making correlation plot 
library(xgboost)    # used for building XGBoost model 
library(cowplot)    # used for combining multiple plots 



#Reading Data



train = fread("Train_UWu5bXk.csv") 
test = fread("Test_u94Q5KV.csv") 
submission = fread("SampleSubmission_TmnO39y.csv")



# Understanding the data


#Dimensions of data

dim(train)  # 8523   12

dim(test)  # 5681   11



#Features of data

names(train)
# [1] "Item_Identifier"           "Item_Weight"               "Item_Fat_Content"          "Item_Visibility"          
# [5] "Item_Type"                 "Item_MRP"                  "Outlet_Identifier"         "Outlet_Establishment_Year"
# [9] "Outlet_Size"               "Outlet_Location_Type"      "Outlet_Type"               "Item_Outlet_Sales"        

names(test)

# "Item_Identifier"           "Item_Weight"               "Item_Fat_Content"          "Item_Visibility"          
# [5] "Item_Type"                 "Item_MRP"                  "Outlet_Identifier"         "Outlet_Establishment_Year"
# [9] "Outlet_Size"               "Outlet_Location_Type"      "Outlet_Type"     



#Target variable :Item_Outlet_Sales 


#Check structure

str(train)

str(test)

# 4 numeric and 7 categorical variables.


# $ Item_Identifier          : chr  
# $ Item_Weight              : num  
# $ Item_Fat_Content         : chr  
# $ Item_Visibility          : num  
# $ Item_Type                : chr  
# $ Item_MRP                 : num  
# $ Outlet_Identifier        : chr  
# $ Outlet_Establishment_Year: int  
# $ Outlet_Size              : chr  
# $ Outlet_Location_Type     : chr  
# $ Outlet_Type              : chr  
# $ Item_Outlet_Sales        : num  


 # Combine Train and Test


test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test datasets
dim(combi)  #14204    12



# Univariate Analysis

# Target Variable
# Since our target variable is continuous, we can visualise it by plotting its histogram.

ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") +  xlab("Item_Outlet_Sales")

#saved as Rplot1


# it is a right skewd variable and would need some data transformation to treat its skewness.



# Independent Variables (numeric variables)
# let's check the numeric independent variables. Use the histograms for visualizations because that will help us in visualizing the distribution of the variables.


p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package

#Rplot2

# Observations:
# 
# There seems to be no clear-cut pattern in Item_Weight.
# Item_Visibility is right-skewed and should be transformed to curb its skewness.
# We can clearly see 4 different distributions for Item_MRP.



#Independent Variables (categorical variables)


ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

#Rplot3

#'LF', 'low fat', and 'Low Fat' are the same category and can be combined into one. Similarly we can be done for 'reg' and 'Regular' into one. After making these corrections we'll plot the same figure again.

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

#Rplot4

#Now let's check the other categorical variables.

# plot for Item_Type 
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  xlab("") +  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ggtitle("Item_Type")
# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
second_row = plot_grid(p5, p6, nrow = 1) 
plot_grid(p4, second_row, ncol = 1)



#Rplot5


#In Outlet_Size's plot, for 4016 observations, Outlet_Size is blank or missing.We will check for this in the bivariate analysis to substitute the missing values in the Outlet_Size.





#We'll also check the remaining categorical variables.

# plot for Outlet_Establishment_Year 
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  xlab("Outlet_Establishment_Year") +  theme(axis.text.x = element_text(size = 8.5))
# plot for Outlet_Type 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  theme(axis.text.x = element_text(size = 8.5))
# ploting both plots together 
plot_grid(p7, p8, ncol = 2)

#Rplot6

# Observations

# Lesser number of observations in the data for the outlets established in the year 1998 as compared to the other years.
# Supermarket Type 1 seems to be the most popular category of Outlet_Type.


 # Bivariate Analysis


 #Explore the independent variables with respect to the target variable. The objective is to discover hidden relationships between the independent variable and the target variable and use those findings in missing data imputation and feature engineering in the next module.


train = combi[1:nrow(train)] # extracting train data from the combined data


#Target Variable vs Independent Numerical Variables


#Let's explore the numerical variables first.

# Item_Weight vs Item_Outlet_Sales 
p9 = ggplot(train) +      geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     theme(axis.title = element_text(size = 8.5))
# Item_Visibility vs Item_Outlet_Sales 
p10 = ggplot(train) +       geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
# Item_MRP vs Item_Outlet_Sales 
p11 = ggplot(train) +       geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)



# Observations
# 
# Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern.
# In Item_Visibility vs Item_Outlet_Sales, there is a string of points at Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero. We will take note of this issue and deal with it in the later stages.
# In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 4 segments of prices that can be used in feature engineering to create a new variable.


# Target Variable vs Independent Categorical Variables
# Now we'll visualise the categorical variables with respect to Item_Outlet_Sales. We will try to check the distribution of the target variable across all the categories of each of the categorical variable.
# 
# We could have used boxplots here, but instead we'll use the violin plots as they show the full distribution of the data. The width of a violin plot at a particular level indicates the concentration or density of data at that level. The height of a violin tells us about the range of the target variable values.

# Item_Type vs Item_Outlet_Sales 
p12 = ggplot(train) +       geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1),            axis.text = element_text(size = 6),            axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales 
p13 = ggplot(train) +       geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1),            axis.text = element_text(size = 8),            axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales 
p14 = ggplot(train) +       geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      theme(axis.text.x = element_text(angle = 45, hjust = 1),            axis.text = element_text(size = 8),            axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2) 
plot_grid(p12, second_row_3, ncol = 1)

# Observations
# 
# Distribution of Item_Outlet_Sales across the categories of Item_Type is not very distinct and same is the case with Item_Fat_Content.
# The distribution for OUT010 and OUT019 categories of Outlet_Identifier are quite similar and very much different from the rest of the categories of Outlet_Identifier.


# In the univariate analysis, we came to know about the empty values in Outlet_Size variable. Let's check the distribution of the target variable across Outlet_Size.



ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")


# The distribution of 'Small' Outlet_Size is almost identical to the distribution of the blank category (first vioin) of Outlet_Size. So, we can substitute the blanks in Outlet_Size with 'Small'.
# 
# 
# Let's examine the remaining variables.


p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") 
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") 
plot_grid(p15, p16, ncol = 1)

#Rplot10
# Observations
# 
# Tier 1 and Tier 3 locations of Outlet_Location_Type look similar.
# In the Outlet_Type plot, Grocery Store has most of its data points around the lower sales values as compared to the other categories.




#Missing Value Treatment


sum(is.na(combi$Item_Weight)) #2439


#Imputing Missing Value
#We have missing values in Item_Weight and Item_Outlet_Sales.
#Missing data in Item_Outlet_Sales can be ignored since they belong to the test dataset. We'll now impute Item_Weight with mean weight based on the Item_Identifier variable.

missing_index = which(is.na(combi$Item_Weight)) 
for(i in missing_index){    
  item = combi$Item_Identifier[i]  
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T) 
  }




sum(is.na(combi$Item_Weight))



# Replacing 0's in Item_Visibility variable
# Similarly, zeroes in Item_Visibility variable can be replaced with Item_Identifier wise mean values of Item_Visibility. 

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)



#Let's replace the zeroes.

zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index)
{    item = combi$Item_Identifier[i]  
     combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  
}



# we can see that the issue of zero item visibility has been resolved.

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)






#Create new features:
  
# Item_Type_new: Broader categories for the variable Item_Type.
# Item_category: Categorical variable derived from Item_Identifier.
# Outlet_Years: Years of operation for outlets.
# price_per_unit_wt: Item_MRP/Item_Weight
# Item_MRP_clusters: Binned feature for Item_MRP.
# We can have a look at the Item_Type variable and classify the categories into perishable 
# and non_perishable as per our understanding and make it into a new feature.


perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
# create a new feature 'Item_Type_new' 
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]


#Let's compare Item_Type with the first 2 characters of Item_Identifier, i.e., 'DR', 'FD', and 'NC'. These identifiers most probably stand for drinks, food, and non-consumable.

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))


# Item_category
combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]


#We will also change the values of Item_Fat_Content wherever Item_category is 'NC' because non-consumable items cannot have any fat content. We will also create a couple of more features - Outlet_Years (years of operation) and price_per_unit_wt (price per unit weight).

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible" 
# years of operation for outlets 
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year] 
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year) 
# Price per unit weight 
combi[,price_per_unit_wt := Item_MRP/Item_Weight]



# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]



 #Encoding Categorical variables

# Label encoding for the categorical variables
# We will label encode Outlet_Size and Outlet_Location_Type as these are ordinal variables.

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,ifelse(Outlet_Size == "Medium", 1, 2))] 

combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 

# removing categorical variables after label encoding 

combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]
#One hot encoding for the categorical variable
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T) 
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")])) 
combi = cbind(combi[,"Item_Identifier"], ohe_df)

#PreProcessing Data


#Removing Skewness


combi[,Item_Visibility := log(Item_Visibility + 1)] # log + 1 to avoid division by zero 
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]



#Scaling numeric predictors
#Let's scale and center the numeric variables to make them have a mean of zero, standard deviation of one and scale of 0 to 1. Scaling and centering is required for linear regression models.

num_vars = which(sapply(combi, is.numeric)) # index of numeric features 
num_vars_names = names(num_vars) combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F] 
prep_num = preProcess(combi_numeric, method=c("center", "scale")) 
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables combi = cbind(combi, combi_numeric_norm)
#Splitting the combined data combi back to train and test set.

train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)] 
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset



# Correlated Variables
# Let's examine the correlated features of train dataset. Correlation varies from -1 to 1.
# 
# negative correlation: < 0 and >= -1
# positive correlation: > 0 and <= 1
# no correlation: 0
# It is not desirable to have correlated features if we are using linear regressions.

cor_train = cor(train[,-c("Item_Identifier")]) 
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)




# Variables price_per_unit_wt and Item_Weight are highly correlated as the former one was created from the latter. Similarly price_per_unit_wt and Item_MRP are highly correlated for the same reason.





#Model Building

#1. Linear Regression
#Building Model
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
#Making Predictions on test Data
# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)

#1202.544131139692


# k-fold Cross Validation


#Regularized Linear Regression



#Lasso Regression
set.seed(1235) 
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0002)) 
lasso_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,method='glmnet', trControl= my_control, tuneGrid = Grid)
lasso_linear_reg_mod


#Making Predictions on test Data
# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(lasso_linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "lasso_linear_reg_mod_submit.csv", row.names = F)


#Leaderboard score: 1202.544131139692.

#Ridge Regression
set.seed(1236)
my_control = trainControl(method="cv", number=5) 
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0002)) 
ridge_linear_reg_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], y = train$Item_Outlet_Sales,method='glmnet', trControl= my_control, tuneGrid = Grid)

#Making Predictions on test Data
# preparing dataframe for submission and writing it in a csv file 
submission$Item_Outlet_Sales = predict(ridge_linear_reg_mod, test[,-c("Item_Identifier")]) 
write.csv(submission, "ridge_linear_reg_mod_submit.csv", row.names = F)

#Leaderboard score:.1205



# Random Forest


set.seed(1237) 
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")




#Best Model Parameters
plot(rf_mod)


# As per the plot shown above, the best score is achieved at mtry = 5 and min.node.size = 20.
# 
# Variable Importance
# Let's plot feature importance based on the RandomForest model

plot(varImp(rf_mod))



#Xgboost
param_list = list(objective = "reg:linear",        eta=0.01,        gamma = 1,        max_depth=6,        subsample=0.8,        colsample_bytree=0.5        )
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales) 
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))
#Cross Validation
#We are going to use the xgb.cv() function for cross validation. 
#Here we are using cross validation for finding the optimal value of nrounds.

set.seed(112) 
xgbcv = xgb.cv(params = param_list,data = dtrain, nrounds = 1000, nfold = 5,print_every_n = 10, early_stopping_rounds = 30, maximize = F)


#Model Training
#As per the verbose above, we got the best validation/test score at the 430th iteration. Hence, we will use nrounds = 430 for building the XGBoost model.

xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)
#Leaderboard score: 1154.70

#This model has even outperformed the RandomForest model.

#Variable Importance
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), model = xgb_model) 
xgb.plot.importance(var_imp)


#After trying and testing 5 different algorithms, the best score on the public leaderboard has been achieved by XGBoost (1154.70), followed by RandomForest (1157.25).
