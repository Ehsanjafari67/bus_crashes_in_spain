#----------------------multinomial logistic regression model--------------------
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

# Define a recipe
bus_crash_rec <- recipe(INJURY_LEVEL ~ ., data = bus_crash_train)

# Specify a multinomial logistic regression model
logistic_spec <- multinom_reg() |>
  set_engine("nnet", maxit = 1000) |>
  set_mode("classification")

# Create a workflow
logistic_wf <- workflow() |>
  add_recipe(bus_crash_rec) |>
  add_model(logistic_spec)

# Fit the multinomial logistic regression model
logistic_fit <- fit(logistic_wf, data = bus_crash_train)

# Get the coefficients from the model
coefficients <- as.data.frame(coef(logistic_fit$fit))

# Check the coefficients
print(coefficients)

# Evaluate the model using resamples (cross-validation)
bus_crash_folds <- vfold_cv(bus_crash_train, strata = "INJURY_LEVEL")
logistic_res <- fit_resamples(logistic_wf, bus_crash_folds)

# Collect metrics
logistic_metrics <- logistic_res  |> 
  collect_metrics()

# View the metrics
logistic_metrics