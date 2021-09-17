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

set.seed(1982, sample.kind = "Rounding")
test_index <- createDataPartition(the_data$status, p = 0.5, 
                                  list = FALSE)
train_set <- the_data[-test_index, ]
test_set <- the_data[test_index, ]

cutoff <- seq(0.05, 0.3, 0.01)
cutoff_simulate <- lapply(cutoff, function(x){
  chi <- lapply(train_set[, sapply(train_set, is.factor) & colnames(train_set) != "status"], 
                function(x){
                  chisq.test(train_set[, names(train_set) == "status"], x)
                })
  chi <- do.call(rbind, chi)[, c(1, 3)]
  chi <- as.data.frame(chi)
  chi$p.value <- as.numeric(chi$p.value)
  chi %>% arrange(p.value)
  chi <- chi %>% filter(p.value < x)
  chosen_variables_categorical <- row.names(chi)
  ttest <- lapply(train_set[, sapply(train_set, is.numeric) & colnames(train_set) != "salary"],
                  function(x){
                    t.test(x ~ train_set$status)
                  })
  ttest <- do.call(rbind, ttest)[, c(1, 3)]
  ttest <- as.data.frame(ttest)
  ttest$p.value <- as.numeric(ttest$p.value)
  ttest %>% arrange(p.value)
  ttest <- ttest %>% filter(p.value < x)
  chosen_variables_numeric <- row.names(ttest)
  
  ## need to pick the right variables to include
  train_set <- train_set[, names(train_set) %in% chosen_variables_categorical |
                         names(train_set) %in% chosen_variables_numeric |
                         names(train_set) == "status"]
  test_set <- test_set[, names(test_set) %in% chosen_variables_categorical |
                           names(test_set) %in% chosen_variables_numeric |
                           names(test_set) == "status"]
  trControl <- trainControl(method = "repeatedcv", number = 10,
                            repeats = 3)
  ##Logistic
  model_glm <- train(status ~ ., data = train_set,
                     method = "glm", trControl = trControl)
  predict_glm <- predict(model_glm, test_set, type = "raw")
  accuracy_glm <- confusionMatrix(predict_glm, test_set$status)$overall["Accuracy"]
  
  model_knn <- train(status ~ ., data = train_set,
                     method = "knn", trControl = trControl,
                     tuneGrid = data.frame(k = seq(1, 20, 1)))
  predict_knn <- predict(model_knn, test_set, type = "raw")
  accuracy_knn <- confusionMatrix(predict_knn, test_set$status)$overall["Accuracy"]
  
  model_rf <- train(status ~ ., data = train_set,
                    method = "rf", trControl = trControl,
                    tuneGrid = data.frame(.mtry=seq(1, 10, 1)))
  ggplot(model_rf, highlight = TRUE)
  predict_rf <- predict(model_rf, test_set, type = "raw")
  accuracy_rf <- confusionMatrix(predict_rf, test_set$status)$overall["Accuracy"]
  
  ## ensemble
  ensemble <- data.frame(GLM = predict_glm, KNN = predict_knn, RF = predict_rf)
  ensemble$vote <- apply(ensemble, 1, function(x){
    names(which.max(table(x)))
  })
  ensemble$vote <- factor(ensemble$vote, levels = levels(the_data$status))
  accuracy_ensemble <- confusionMatrix(ensemble$vote, test_set$status)$overall["Accuracy"]
  tibble(glm = accuracy_glm, knn = accuracy_knn, 
         rf = accuracy_rf, ensemble = accuracy_ensemble)
})

cutoff_simulate <- do.call(rbind, cutoff_simulate)
cutoff_simulate <- cbind(cutoff_simulate, cutoff)

cutoff_simulate_long <- cutoff_simulate %>% 
  pivot_longer(cols = glm:ensemble, names_to = "model", values_to = "accuracy")

ggplot(cutoff_simulate_long) + geom_line(aes(cutoff, accuracy, color = model))

## Get the model with the highest accuracy
cutoff_simulate_long[which.max(cutoff_simulate_long$accuracy), ]

## List the chosen variables in the best model
cutoff_best <- cutoff_simulate_long[which.max(cutoff_simulate_long$accuracy), ]$cutoff
chi <- lapply(train_set[, sapply(train_set, is.factor) & colnames(train_set) != "status"], 
              function(x){
                chisq.test(train_set[, names(train_set) == "status"], x)
              })
chi <- do.call(rbind, chi)[, c(1, 3)]
chi <- as.data.frame(chi)
chi$p.value <- as.numeric(chi$p.value)
chi <- chi %>% filter(p.value < cutoff_best)
chosen_variables_categorical <- row.names(chi)
ttest <- lapply(train_set[, sapply(train_set, is.numeric) & colnames(train_set) != "salary"],
                function(x){
                  t.test(x ~ train_set$status)
                })
ttest <- do.call(rbind, ttest)[, c(1, 3)]
ttest <- as.data.frame(ttest)
ttest$p.value <- as.numeric(ttest$p.value)
ttest <- ttest %>% filter(p.value < cutoff_best)
chosen_variables_numeric <- row.names(ttest)
chosen_variables_categorical
chosen_variables_numeric


