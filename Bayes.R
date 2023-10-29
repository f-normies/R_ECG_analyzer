library(readr)
library(caret)
library(e1071)
library(pROC)

# Misc functions
split_data <- function(data) {
  trainIndex <- createDataPartition(data$class, p = 0.7, list = FALSE)
  training_data <- data[trainIndex,]
  temp_data <- data[-trainIndex,]
  valIndex <- createDataPartition(temp_data$class, p = 0.5, list = FALSE)
  val_data <- temp_data[valIndex,]
  test_data <- temp_data[-valIndex,]
  list(train = training_data, val = val_data, test = test_data)
}

# Load and preprocess data
data <- read_delim("data/Common dataset.txt", delim = "\t")
data[, 2:217] <- scale(data[, 2:217])
datasets <- list()

unique_classes <- unique(data$class)
for (class in unique_classes) {
  if (class != "N") {
    binary_data <- data
    binary_data$class <- ifelse(binary_data$class == class | binary_data$class == "N", binary_data$class, NA)
    binary_data <- na.omit(binary_data)
    binary_data$class <- ifelse(binary_data$class == "N", 1, 0)
    datasets[[class]] <- binary_data
  }
}

splits <- lapply(datasets, split_data)

#  Training Naive Bayes
models <- list()

for (class in names(splits)) {
  model <- naiveBayes(class ~ ., data = splits[[class]]$train)
  models[[class]] <- model
}

#  Testing Naive Bayes
roc_objects <- list()
colors <- rainbow(length(names(models)))

for (i in 1:length(names(models))) {
  class <- names(models)[i]
  predictions <- predict(models[[class]], splits[[class]]$test, type = "raw")
  roc_obj <- roc(splits[[class]]$test$class, predictions[,2])
  roc_objects[[class]] <- roc_obj
  
  if (i == 1) {
    plot(roc_obj, main = "ROC Curves", col = colors[i], lwd = 2)
  } else {
    plot(roc_obj, add = TRUE, col = colors[i], lwd = 2)
  }
}

legend("bottomright", legend = names(models), col = colors, lty = 1, lwd = 2)

accuracy <- sapply(names(models), function(class) {
  predictions <- predict(models[[class]], splits[[class]]$test)
  sum(predictions == splits[[class]]$test$class) / nrow(splits[[class]]$test)
})

print(accuracy*100, "%"")
