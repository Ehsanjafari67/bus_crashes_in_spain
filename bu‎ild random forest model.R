#-------------------------------random forest model-----------------------------
#| cache: TRUE

# Set a random seed for reproducibility
set.seed(1212)

# Split the data into training and testing sets
bus_crash_split <- initial_split(InitialDataForAnalysis, strata = INJURY_LEVEL)
bus_crash_train <- training(bus_crash_split)
bus_crash_test <- testing(bus_crash_split)

# Create a recipe with median imputation for all numeric columns
bus_crash_rec <- recipe(INJURY_LEVEL ~ ., data = bus_crash_train) %>%
  step_impute_median(
    all_numeric_predictors(),
    id = "impute_median"
  )

# Define the random forest model specification
rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("randomForest", importance = TRUE)

# Create a workflow
bus_crash_wf <- workflow() %>%
  add_recipe(bus_crash_rec) %>%
  add_model(rf_spec)

# Train the model
rf_fit <- fit(bus_crash_wf, data = bus_crash_train)

# Make predictions on the test set
rf_pred <- predict(rf_fit, new_data = bus_crash_test)

# Combine the predicted values with the actual values from the test dataset
combined_data <- bind_cols(
  rf_pred,
  truth = as.factor(bus_crash_test$INJURY_LEVEL)  # Convert to factor
)

# Calculate and print evaluation metrics
rf_metrics <- combined_data %>%
  metrics(truth = truth, estimate = .pred_class) %>%
  filter(.metric %in% c("accuracy", "precision", "recall", "f1"))

# Print the evaluation metrics
print(rf_metrics)

#--------------------Visualize a variable importance & ROC curve----------------
vip(rf_fit$fit$fit, num_features = 20)

# ROC curves
# Ensure truth and .pred_class are factors
combined_data$truth <- as.factor(combined_data$truth)
combined_data$.pred_class <- as.factor(combined_data$.pred_class)

# Initialize an empty list to store ROC curves
roc_curves <- list()

# Get unique class levels
class_levels <- levels(combined_data$truth)

# Create ROC curves for each class
for (class in class_levels) {
  # Create binary indicators for the current class
  binary_truth <- ifelse(combined_data$truth == class, 1, 0)
  binary_pred <- ifelse(combined_data$.pred_class == class, 1, 0)
  
  # Calculate ROC curve for the current class
  roc_curves[[class]] <- roc(binary_truth, binary_pred)
}

# Plot the ROC curves for each class
par(mar = c(5.1, 4.1, 0.1, 0.1))  # Adjust plot margins
plot(roc_curves[[1]], col = "red", main = "Multi-Class ROC Curves")
for (i in 2:length(roc_curves)) {
  lines(roc_curves[[i]], col = rainbow(length(roc_curves))[i])
}
legend("bottomright", legend = class_levels, col = rainbow(length(roc_curves)))

