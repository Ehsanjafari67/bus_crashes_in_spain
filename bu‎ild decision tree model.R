#-------------------------------decision tree model-----------------------------
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

bus_crash_rec <- recipe(INJURY_LEVEL ~ ., data = bus_crash_train) |> 
  step_downsample(INJURY_LEVEL)  

bag_spec <- bag_tree(min_n = 10) |>    
  set_engine("rpart", times = 25) |>    
  set_mode("classification")  
bus_crash_wf <- workflow() |>    
  add_recipe(bus_crash_rec) |>    
  add_model(bag_spec)  

bus_crash_wf
#-------------------------------------------------------------------------------
#| cache: TRUE

doParallel::registerDoParallel() 
bus_crash_res <- fit_resamples(bus_crash_wf,   
                           bus_crash_folds,   
                           control = control_resamples(save_pred = TRUE))
#-------------------------------------------------------------------------------
#| cache: TRUE

collect_metrics(bus_crash_res)
#-------------------------------------------------------------------------------
#| cache: TRUE

bus_crash_fit <- last_fit(bus_crash_wf, bus_crash_split) 
collect_metrics(bus_crash_fit)
#------------------------------importance plot----------------------------------
#| cache: TRUE
#| label: fig-VarImp
#| fig-cap: "The importance of predictor variables to describe the severity of accidents"

bus_crash_imp <- bus_crash_fit$.workflow[[1]]  |>    
  extract_fit_parsnip()  
bus_crash_imp$fit$imp  |>    
  slice_max(value, n = 20)  |>    
  ggplot(aes(value, fct_reorder(term, value))) +   
  geom_col(alpha = 0.8, fill = "midnightblue") +   
  labs(x = "Variable importance score", y = NULL)
#------------------------------------ROC curve----------------------------------
#| cache: TRUE
#| label: fig-ROC
#| fig-cap: "ROC curve"

# Extract predictions from the first split
predictions <- bus_crash_fit$.predictions[[1]]

# Check column names
colnames(predictions)

# Create ROC curves for each level of INJURY_LEVEL
roc_fatal <- roc(ifelse(predictions$INJURY_LEVEL == "FATAL", 1, 0), predictions$.pred_FATAL)
roc_severe <- roc(ifelse(predictions$INJURY_LEVEL == "SEVERE", 1, 0), predictions$.pred_SEVERE)
roc_minor_pdo <- roc(ifelse(predictions$INJURY_LEVEL == "MINOR/PDO", 1, 0), predictions$`.pred_MINOR/PDO`)

# Plot the ROC curves
plot(roc_fatal, col = "red", main = "Multi-Class ROC Curve")
lines(roc_severe, col = "blue")
lines(roc_minor_pdo, col = "green")
legend("bottomright", legend = c("FATAL", "SEVERE", "MINOR/PDO"), col = c("red", "blue", "green"))