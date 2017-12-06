###### Start of the code #######

fifa <- read.csv("fifa_processed.csv")

# Setting the seed for reproduciablity
set.seed(1)

# Performing data spliting
cv_index <- createDataPartition(fifa$Rating, p = 0.8, list = FALSE)
fifaTrain <- fifa[cv_index, ]
fifaTest <- fifa[-cv_index, ]
X_train <- fifaTrain[, !colnames(fifaTrain) %in% c("Rating")]
X_test <- fifaTest[, !colnames(fifaTest) %in% c("Rating")]
y_train <- as.numeric(fifaTrain[, "Rating"])
y_test <- as.numeric(fifaTest[, "Rating"])

# Setting up the control parameter
ctrl <- trainControl(method = "cv", number = 3)

# Create the tune grid
tune_grid <- expand.grid(.decay = c(0, 0.01, .1), .size = c(1, 3, 5, 7, 9), .bag = FALSE)

# Setting the seed
set.seed(1)

# Check the file exists and load to variables
# else bulid and store the model
if(file.exists("models/avg_nnet_model.rds")) {
avg_nnet_model <- readRDS("models/avg_nnet_model.rds")
} else {
avg_nnet_model <- train(x = X_train, y = y_train, tuneGrid = tune_grid, method = "avNNet", preProc = c("center", "scale"),
linout = TRUE, trace = FALSE, MaxNWts = 9 * (ncol(X_train) + 1) + 9 + 1, maxit = 500)
saveRDS(avg_nnet_model, "models/avg_nnet_model.rds")
}

# Print the model
avg_nnet_model

# Plot the model
plot(avg_nnet_model)

# Make the prediction 
avg_nnet_pred <- predict(avg_nnet_model, newdata = X_test)

# Get the performance scores
postResample(pred = avg_nnet_pred, obs = y_test)

##### End of the code ######