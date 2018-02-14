# Libraries ----
rm(list = ls())
library(ggplot2)
library(glmnet)


# Data ----
# https://archive.ics.uci.edu/ml/datasets/Auto+MPG
# https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/
#
#
# Attribute Information:
#     
# 1. mpg:           continuous
# 2. cylinders:     multi-valued discrete
# 3. displacement:  continuous
# 4. horsepower:    continuous
# 5. weight:        continuous
# 6. acceleration:  continuous
# 7. model year:    multi-valued discrete
# 8. origin:        multi-valued discrete
# 9. car name:      string (unique for each instance)
#

dtf <- read.table("data/auto-mpg.data", stringsAsFactors = F)
colnames(dtf) <- c("mpg",
                   "cylinders",
                   "displacement",
                   "horsepower",
                   "weight",
                   "acceleration",
                   "model_year",
                   "origin",
                   "car_name")

#We'll have to fix horsepower column, some rows have ?
dtf$horsepower <- ifelse(dtf$horsepower == "?", NA, dtf$horsepower)
dtf$horsepower <- as.numeric(dtf$horsepower)

# Sort out NA's 
dtf <- dtf[complete.cases(dtf), ]


# Initial look at data ----
# Take less data
set.seed(123)
train_index <- sample(1:nrow(dtf), 15)
test_index  <- sample(1:nrow(dtf), 15)

dtf_train <- dtf[train_index, ]

# we'll look at how horsepower explains mpg
qplot(y=dtf_train$mpg, x=dtf_train$horsepower, 
      xlab = "Horsepower", ylab = "MPG", main = "Horsepower vs MPG") + 
    stat_smooth(aes(y=dtf$mpg, x=dtf$horsepower),
                formula = y ~ x, method = "lm", se =F)


# see how I higher order polynomial would fit
qplot(y=dtf_train$mpg, x=dtf_train$horsepower, 
      xlab = "Horsepower", ylab = "MPG", main = "Horsepower vs MPG") + 
    stat_smooth(aes(y=dtf$mpg, x=dtf$horsepower),
                formula = y ~ x + I(x^2) + I(x^3)  + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8), method = "lm", se =F)


# add those polynomials to data
for(i in 2:8) {
    dtf[, paste0("horsepower", i)] <- dtf$horsepower^i
}


# Ridge ----
dtf_train <- dtf[train_index, ]
dtf_test  <- dtf[test_index, ]

y <- as.matrix(dtf_train[, "mpg"])
x <- as.matrix(dtf_train[, colnames(dtf_train)[which(substr(colnames(dtf_train), 1, 10) == "horsepower")]])

y_test <- as.matrix(dtf_test[, "mpg"])
x_test <- as.matrix(dtf_test[, colnames(dtf_test)[which(substr(colnames(dtf_test), 1, 10) == "horsepower")]])

fits <- list()
lambdas <- 5^seq(0,1,0.05)-1
rss  <- c()

for(i in 1:length(lambdas)) {
    lambda <- lambdas[i]
    
    fits[[paste0("r", i)]] <- glmnet(x, y, alpha = 0, lambda = lambda)
    
    y_hat_test <- predict.glmnet(fits[[paste0("r", i)]], newx = x_test)

    
    rss <- c(rss, sum((y_test-y_hat_test)^2))
    
}

qplot(x=lambdas, y=rss, main = "RSS depending on lambda")

