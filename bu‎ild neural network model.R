#------------------------------neural network model-----------------------------
#| cache: TRUE

set.seed(1212) 
bus_crash_split <- initial_split(InitialDataForAnalysis, strata = INJURY_LEVEL) 
bus_crash_train <- training(bus_crash_split) 
bus_crash_test <- testing(bus_crash_split)  

set.seed(123) 
bus_crash_folds <- vfold_cv(bus_crash_train, strata = INJURY_LEVEL) 
bus_crash_folds
#-------------------------------------------------------------------------------
#| cache: TRUE

names(InitialDataForAnalysis)

# Create a recipe for data preprocessing
bus_crash_rec <- recipe(INJURY_LEVEL ~ ., data = bus_crash_train) %>%
  step_dummy(all_nominal(), -all_outcomes())  # One-hot encode the target variable

# Prepare the data
bus_crash_prep <- prep(bus_crash_rec, training = bus_crash_train)

# Apply the preprocessing to the data
bus_crash_train_encoded <- bake(bus_crash_prep, new_data = NULL)

# Check the result
head(bus_crash_train_encoded)

# Define the neural network model
neural_net <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = ncol(bus_crash_train_encoded)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax') # 3 units for multi-class classification

# Compile the model
neural_net %>%
  compile(
    loss = 'categorical_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
  )

# Convert the target variable to one-hot encoding
bus_crash_train_labels <- to_categorical(bus_crash_train_encoded$INJURY_LEVEL)

# Train the model
history <- neural_net %>%
  fit(
    as.matrix(bus_crash_train_encoded[, -1]), # Exclude the target variable
    bus_crash_train_labels,
    epochs = 10, # Specify the number of training epochs
    batch_size = 32, # Specify the batch size
    validation_split = 0.2 # Specify the validation split
  )

# Evaluate the model
evaluation <- neural_net %>%
  evaluate(
    as.matrix(bus_crash_train_encoded[, -1]), # Exclude the target variable
    bus_crash_train_labels
  )

# Print the evaluation results
print(evaluation)