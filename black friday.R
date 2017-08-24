library(dummies)
library(plyr)


## loading data
train <- read.csv(file.choose(), sep = ",", stringsAsFactors = F, header = T, fileEncoding = "utf8")
test<- read.csv(file.choose(),sep =",", stringsAsFactors = F,header = T,fileEncoding = "utf8")

## function for importing Rscript from github
#source_https <- function(url)
#{
#  library(RCurl)
#  eval(parse(text=getURL(url,followlocation=T,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))),envir=.GlobalEnv)
#}

## cleaning data

z<- subset(train, Product_Category_1 %in% c(19))
v<- subset(train, Product_Category_1 %in% c(20))
b<- subset(train, Product_Category_1 %in% c(1))
# removing categories 19 and 20
#X_train <- subset(train, !Product_Category_1 %in% c(19,20))
X_train<- train
X_test <- test

help("dummy.data.frame")
# onehot-encoding city variable
X_train <- dummy.data.frame(X_train, names=c("City_Category"), sep="_")
X_test <- dummy.data.frame(X_test, names=c("City_Category"), sep="_")

print(X_train$Age[0:10])
# converting age variable to numeric
X_train$Age[X_train$Age == "0-17"] <- "15"
X_train$Age[X_train$Age == "18-25"] <- "21"
X_train$Age[X_train$Age == "26-35"] <- "30"
X_train$Age[X_train$Age == "36-45"] <- "40"
X_train$Age[X_train$Age == "46-50"] <- "48"
X_train$Age[X_train$Age == "51-55"] <- "53"
X_train$Age[X_train$Age == "55+"] <- "60"

X_test$Age[X_test$Age == "0-17"] <- "15"
X_test$Age[X_test$Age == "18-25"] <- "21"
X_test$Age[X_test$Age == "26-35"] <- "30"
X_test$Age[X_test$Age == "36-45"] <- "40"
X_test$Age[X_test$Age == "46-50"] <- "48"
X_test$Age[X_test$Age == "51-55"] <- "53"
X_test$Age[X_test$Age == "55+"] <- "60"

help("as.integer")
X_train$Age <- as.integer(X_train$Age)
X_test$Age <- as.integer(X_test$Age)

#print(X_train$Stay_In_Current_City_Years) 0 1 2 3 4
# converting stay in current city to numeric
X_train$Stay_In_Current_City_Years[X_train$Stay_In_Current_City_Years == "4+"] <- "4"
X_test$Stay_In_Current_City_Years[X_test$Stay_In_Current_City_Years == "4+"] <- "4"

X_train$Stay_In_Current_City_Years <- as.integer(X_train$Stay_In_Current_City_Years)
X_test$Stay_In_Current_City_Years <- as.integer(X_test$Stay_In_Current_City_Years)

# converting gender to binary
X_train$Gender <- ifelse(X_train$Gender == "F", 1, 0)
X_test$Gender <- ifelse(X_test$Gender == "F", 1, 0)

#help("nrow") the number of row
#help("ddply") split dataframe, apply function, return results 
# feature representing the count of each user
#count the 消費次數 for every use
user_count <- ddply(X_train, .(User_ID), nrow)
names(user_count)[2] <- "User_Count"
X_train <- merge(X_train, user_count, by="User_ID")
X_test <- merge(X_test, user_count, all.x=T, by="User_ID")


# feature representing the count of each product
product_count <- ddply(X_train, .(Product_ID), nrow)
names(product_count)[2] <- "Product_Count"
X_train <- merge(X_train, product_count, by="Product_ID")
X_test <- merge(X_test, product_count, all.x=T, by="Product_ID")
X_test$Product_Count[is.na(X_test$Product_Count)] <- 0

# feature representing the average Purchase of each product
product_mean <- ddply(X_train, .(Product_ID), summarize, Product_Mean=mean(Purchase))
X_train <- merge(X_train, product_mean, by="Product_ID")
X_test <- merge(X_test, product_mean, all.x=T, by="Product_ID")
X_test$Product_Mean[is.na(X_test$Product_Mean)] <- mean(X_train$Purchase)

#print(count(X_test$Product_Mean[is.na(X_test$Product_Mean)])) 61

# feature representing the proportion of times the user purchases the product more than the product's average
X_train$flag_high <- ifelse(X_train$Purchase > X_train$Product_Mean,1,0)
user_high <- ddply(X_train, .(User_ID), summarize, User_High=mean(flag_high))
X_train <- merge(X_train, user_high, by="User_ID")
X_test <- merge(X_test, user_high, by="User_ID")

# subsetting columns for submission
submit <- X_test[,c("User_ID","Product_ID")]

# target variable
y <- X_train$Purchase

# removing irrelevant columns
X_train <- subset(X_train, select=-c(Purchase,Product_ID,flag_high))
X_test <- subset(X_test, select=c(colnames(X_train)))


## xgboost with cross validation
model_xgb_1 <- xgboost(data= data.matrix(X_train),label=y,objective="reg:linear",nrounds=100,max.depth=15,eta=1,metric="rmse")
y_pred <- predict(model_xgb_1, data.matrix(X_test))

## submission file
test_xgb_1 <- model_xgb_1[[2]]

# adding predictions
submit$Purchase <-y_pred

# tweaking final predictions (You know, to get those extra decimals :-) )
submit$Purchase[submit["Purchase"] < 185] <- 185
submit$Purchase[submit$Purchase > 23961] <- 23961

write.csv(submit, "./SampleSubmission.csv", row.names=F)
