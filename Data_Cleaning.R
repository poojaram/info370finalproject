library(Boruta)
library(mlbench)
library(lattice)
library(ggplot2)
library(caret)

# Data Prep
data <- read.csv("Data/listings.csv", stringsAsFactors = FALSE)

# Removing the unnecessary features like the website url, etc.
data <- data[,-c(1:4)]

#names <- names(data)

# Removing the non-available data or the ones that have only NA values
# Removing the names of the user because they can be identified with user_id
data <- data[, -c(1, 5, 12, 13, 14, 15, 17, 24, 26, 27, 30, 66, 67, 68, 69)]

names <- names(data)

# Host Information
host_info <- data[, 10:22]
data <- data[, -c(10:22)]

listing_qualitative_info <- data[, 1:9]

data <- data[, -c(1:9)]

# sum(data$is_location_exact == "t")/nrow(data) * 100 # 86.20404% exact true location

listing_quantitative_info <- data[,c(1:21, 24:34)]

data <- data[, -c(1:6,8:14)]
data <- data[,-c(9:10)]
data <- data[,-c(10:11,18:21,29:30,38:49)]

data <- data[,-c(2, 3, 8, 20)]

# Boruta

data[data == ""] <- NA
data <- na.omit(data)

#df$payment_2 = as.numeric(gsub("\\$", "", df$payment))

data$price = as.numeric(gsub("\\$", "", data$price))
data$security_deposit = as.numeric(gsub("\\$", "", data$security_deposit))
data$cleaning_fee = as.numeric(gsub("\\$", "", data$cleaning_fee))
data$extra_people = as.numeric(gsub("\\$", "", data$extra_people))
smp_size <- floor(0.75 * nrow(data))
## set the seed to make your partition reproducible

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


train <- train[complete.cases(train),]
price.train <- train$price

price.test <- test$price

# ensure results are repeatable
set.seed(7)
# load the library


# We removed NA values 


# prepare training scheme
#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# train the model
#results <- rfe(train[,-6], train[,6], sizes=c(1:25), rfeControl=control)# estimate variable importance
#importance <- varImp(model, scale=FALSE)
# summarize importance
#print(importance)
# plot importance
#plot(importance)

## Logistic regression
train <- train[,-6]
test <- test[,-6]

test <- cbind(test,price.test)
x <- cbind(train, price.train)
fit.p <- glm(formula = price.train ~ ., family = "poisson", data = x)
predict.p <- predict(fit.p, test)
plot(predict.p,test$price.test, type = "h",  xlab = "Actual Values",
     ylab = "Predicted Values", main = "Predicted vs Actual for Gaussian Poisson")




test <- cbind(test,price.test)
x <- cbind(train, price.train)
fit.g <- glm(formula = price.train ~ ., family = "gaussian", data = x)
predict.g <- predict(fit.g, test)
plot(predict.g,test$price.test, type = "h", xlab = "Actual Values",
     ylab = "Predicted Values", main = "Predicted vs Actual for Gaussian Prediction")


boruta.train <- Boruta(price~., data = data, doTrace=3)

#library(cluster)
#fit <- kmeans(train, 5)


