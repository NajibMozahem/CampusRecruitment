## First we prepare the environment

packages <- c("tidyverse", "caret")

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }  
})

## Next we read the data
the_data <- read_csv("dataset/Placement_Data_Full_Class.csv")

## Check for na values
colSums(is.na(the_data))
## missing values for salary. These missing values are for
## records that were not placed.

## Now take a look at the structure of the data set
str(the_data)

## look at the values of the character variables
lapply(the_data, function(x){
  if (is.character(x)) {
    unique(x)
  }
})

## No strange values. Convert these to factor variables:
the_data <- the_data %>% mutate(across(where(is.character), as.factor))


## Modeling

## First remove the salary column
the_data <- the_data[, names(the_data) != "salary"]

## need to pick the right variables to include
the_data <- the_data[, names(the_data) %in% c("status", "workex","ssc_p","degree_p","hsc_p","specialisation","etest_p","mba_p")]

test_index <- createDataPartition(the_data$status, p = 0.5, 
                                  list = FALSE)
train_set <- the_data[-test_index, ]
test_set <- the_data[test_index, ]
trControl <- trainControl(method = "repeatedcv", number = 10,
                          repeats = 3)
##Logistic
model_glm <- train(status ~ ., data = train_set,
                  method = "glm", trControl = trControl)
predict_glm <- predict(model_glm, test_set, type = "raw")
confusionMatrix(predict_glm, test_set$status)

model_knn <- train(status ~ ., data = train_set,
                   method = "knn", trControl = trControl,
                   tuneGrid = data.frame(k = seq(1, 20, 1)))
predict_knn <- predict(model_knn, test_set, type = "raw")
confusionMatrix(predict_knn, test_set$status)

model_rf <- train(status ~ ., data = train_set,
                  method = "rf", trControl = trControl,
                  tuneGrid = data.frame(.mtry=seq(1, 10, 1)))
ggplot(model_rf, highlight = TRUE)
predict_rf <- predict(model_rf, test_set, type = "raw")
confusionMatrix(predict_rf, test_set$status)
