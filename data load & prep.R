#----------------------------------library--------------------------------------
library(vtable)
library(caret)
library(janitor)
library(tidyverse)
library(scales)
library(lubridate)
library(tidymodels)
library(themis)
library(baguette)
library(pROC)
library(openxlsx)
library(keras)
library(tensorflow)
library(recipes)
library(reticulate)
library(writexl)
library(vip)
#---------------------------------load and filter data--------------------------
#| cache: TRUE
load("RawData.RData")

bus_crash <- MGE_drv_acc_veh |> 
  filter(VEHICLE_TYPE == 15 | VEHICLE_TYPE == 16 | VEHICLE_TYPE == 17)
#
save(bus_crash,
     file = "BUS_CRASH.RData")

load("BUS_CRASH.RData")

class(bus_crash)
bus_crash_raw <- as_tibble(bus_crash)  
class(bus_crash_raw)
names(bus_crash_raw)
#--------------------------------Initial Variables------------------------------
#| cache: TRUE
crash <- bus_crash_raw |>  
  arrange(desc(ACCIDENT_DATE))  |> 
  transmute(
    INJURY_LEVEL = case_when(
      TOTAL_DEATH_30D > 0 ~ "FATAL",
      TOTAL_INJ_MORE24H_30D > 0 ~ "SEVERE",
      TRUE ~ "MINOR/PDO"
    ),
    AGE,
    SEX,
    CERTIF_YEAR,
    INEXPERIENCE,
    CERTIF_CHARACTER,
    BELT_USE,
    PLANNED_TRAVEL_DISTANCE,
    VIOLATION,
    VIOL_SPEEDING,
    ACC_RESPONSIBLE,
    FACTOR_AFFECT_ATTENTION,
    weekdays,
    ROAD_TYPE,
    TOTAL_VEHICLES,
    TOTAL_PEDESTRIANS,
    ACC_TYPE_COLLISION,
    SURF_CONDITION,
    LIGHTNING_CONDITION,
    WEATHER_CONDITION,
    FOG_CONDITION,
    VISIBILITY,
    ROAD_FUNCTION,
    SPEED_LIMIT,     
    LANE_WIDTH,
    SHOULDER,
    SEPARATE_LONGIT_LINE,
    SEPARATE_MARKING,
    SEPARATE_MEDIAN,
    SEPARATE_GUARDRAIL,
    SEPARATE_NONE,
    SECTION_BRIDGE,
    SECTION_TUNNEL,
    NARROW_SECTION,
    SECTION_PARKING,
    SECTION_NONE,
    MARGIN_TREES,
    MARGIN_BUILDS,
    MARGIN_POSTS,
    MARGIN_UNKNOWN,
    SPECIAL_CON_CONST,
    ROAD_CONSTRUCT,
    SPECIAL_CON_HOLE,
    SPECIAL_CON_PAVE,
    ROAD_DELIM_CURB,
    ROAD_DELIM_MARK,
    ROAD_DELIM_BARRIER,
    ROAD_DELIM_NONE,
    FATIGUE,
    RESPECT_DAILY_REST,
    EXCEED_CONTIN_DRIV_HOURS,
    EXCEED_PERIODIC_DRIV_HOURS,
    DISTRACTED_DRIVE,
    CARELESS_DRIVE,
    IMPROPER_SPEED,
    NO_RESPECT_PRIORITY,
    NO_SAFE_INTERVAL,
    IMPROPER_OVERTAKE,
    AGGRESSIVE_DRIVE,
    ALCOHOL,
    RESULT_ALCOHOL_TEST,
    RESULT_DRUG_TEST,
    VEHICLE_AGE,
    `TECH_TEST (ITV)`,
    PREV_PROBLEM_BRAKES,
    PREV_PROBLEM_REVENTON,
    PREV_PROBLEM_STEERING,
    PREV_PROBLEM_TIRES,
    PREV_PROBLEM_OTHERS,
    AIRBAG_FRONT_DRIVER,
    AIRBAG_FRONT_PASS,
    AIRBAG_LEFT_KNEE,
    AIRBAG_RIGHT_KNEE,
    AIRBAG_LEFT_FRONT,
    AIRBAG_RIGHT_FRONT,
    AIRBAG_REAR_LEFT,
    AIRBAG_REAR_RIGHT,
    AIRBAG_OTHERS,
    CONTIN_DRIV_HOURS,
    MOST_DAMAGE
  )

str(crash)
sumtable(crash)
#--------------------------------Variables evaluation---------------------------
#| cache: TRUE
df <- crash
df[df == 998 | df == 999 | df == 9999 | df == 77 | df == ""] <- NA

for (col in names(df)) {
  uniq_val <- unique(df[[col]])
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(df[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(df[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}

crash <- df
sumtable(crash)
#-----------------------Classification & create new variables-------------------
#| cache: TRUE

# Creating a new "Driver Age" column based on AGE group categories
crash <- mutate(crash, DRIVER_AGE = 
                  case_when(
                    AGE > 30 & AGE < 65 ~ 2,  # middle-aged driver
                    AGE >= 65 ~ 3,            # old driver
                    TRUE ~ 1                  # young driver
                  ))

# Creating a new "experience" column based on the "CERTIF_YEAR" column
crash <- mutate(crash, EXPERIENCE = 
                  case_when(
                    CERTIF_YEAR >= 2017 | INEXPERIENCE == 1 ~ 1,  # CERTIF_YEAR after or equal to 2017
                    TRUE ~ 2                                     # All other cases
                  ))

# Creating a new "Vehicle age" column based on VEHICLE_AGE group categories
crash <- mutate(crash, VEH_AGE = 
                 case_when(
                   VEHICLE_AGE >= 15 ~ 3, # old Vehicle
                   VEHICLE_AGE <= 5 ~ 1,  # new Vehicle
                    TRUE ~ 2              # middle-aged Vehicle
                  ))

# Creating a new "light condition" column based on LIGHTNING_CONDITION group categories
crash <- mutate(crash, LIGHT_CONDITION = 
                 case_when(
                   LIGHTNING_CONDITION == 1 ~ 1,  #day light
                    TRUE ~ 2                      #dark light
                  ))

# Creating a new "Separation" column
crash <- mutate(crash, SEPARATION = 
                 case_when(
                   SEPARATE_LONGIT_LINE == 1 | SEPARATE_MARKING == 1 | SEPARATE_NONE == 1 ~ 2,  #not Separated road
                    TRUE ~ 1                                                                    #Separated road
                  ))

# Creating a new "Delimitation" column
crash <- mutate(crash, DELIMITATION = 
                  case_when(
                    ROAD_DELIM_CURB == 1 | ROAD_DELIM_BARRIER == 1 ~ 1,  #delimited road
                    TRUE ~ 2                                             #not delimited road
                  ))

# Creating a new "Weekend" column based on weekdays group categories
crash <- mutate(crash, WEEKEND = 
                  case_when(
                    weekdays == "Saturday" | weekdays == "Sunday" ~ 2,  #Weekend
                    TRUE ~ 1                                            #not Weekend
                  ))

# Creating a new "multi/single Vehicle" column based on TOTAL_VEHICLES group categories
crash <- mutate(crash, MULTI_SINGLE_VEHICLE = 
                 case_when(
                    TOTAL_VEHICLES == 1 ~ 1,  #single crash
                    TOTAL_VEHICLES > 1 ~ 2    #multi vehicle crash
                  ))

# Creating a new "expired technical test" column based on `TECH_TEST (ITV)` group categories
crash <- mutate(crash, EXPIRED_TEST = 
                  case_when(
                    `TECH_TEST (ITV)` == 2 ~ 2,  #expired technical test
                    TRUE ~ 1                     #Other
                  ))

# Creating a new "pedestrian" column based on TOTAL_PEDESTRIANS group categories
crash <- mutate(crash, PEDESTRIAN = 
                  case_when(
                    TOTAL_PEDESTRIANS > 0 ~ 2,  #Pedestrian presence
                    TRUE ~ 1                     #Other
                  ))

# Creating a new "weather" column
crash <- mutate(crash, WEATHER = 
                  case_when(
                    WEATHER_CONDITION == 3 | WEATHER_CONDITION == 4 ~ 2,  #rain
                    WEATHER_CONDITION == 5 | WEATHER_CONDITION == 6 ~ 3,  #snow
                    FOG_CONDITION == 1 | FOG_CONDITION == 2 ~ 4,          #fog
                    TRUE ~ 1                                              #sunny
                  ))

# Creating a new "work zone" column
crash <- mutate(crash, WORK_ZONE = 
                  case_when(
                    SPECIAL_CON_CONST == 1 | ROAD_CONSTRUCT == 1 ~ 2,  #work zone
                    TRUE ~ 1                                            #Other
                  ))

# Creating a new "poor pavement" column
crash <- mutate(crash, PAVEMENT = 
                  case_when(
                    SPECIAL_CON_HOLE == 1 | SPECIAL_CON_PAVE == 1 ~ 2,  #poor pavement
                    TRUE ~ 1                                            #Other
                  ))

# Creating a new "fatigue" column
crash <- mutate(crash, FATIGUE = 
                  case_when(
                    FATIGUE == 1 | FACTOR_AFFECT_ATTENTION == 11 | RESPECT_DAILY_REST == 0 | EXCEED_CONTIN_DRIV_HOURS == 1 | EXCEED_PERIODIC_DRIV_HOURS == 1 ~ 2,  #fatigue
                    TRUE ~ 1                                                                                                                                       #normal
                  ))

# Creating a new "careless" column
crash <- mutate(crash, CARELESS_DRV = 
                  case_when(
                    DISTRACTED_DRIVE == 1 | CARELESS_DRIVE == 1 | FACTOR_AFFECT_ATTENTION == 1 | FACTOR_AFFECT_ATTENTION == 5 | FACTOR_AFFECT_ATTENTION == 6 | FACTOR_AFFECT_ATTENTION ==7 ~ 2,  #careless
                    TRUE ~ 1                                                                                                                                                                     #normal
                  ))

# Creating a new "aggressive" column
crash <- mutate(crash, AGGRESSIVE_DRV = 
                  case_when(
                    IMPROPER_SPEED == 1 | NO_RESPECT_PRIORITY == 1 | NO_SAFE_INTERVAL == 1 | IMPROPER_OVERTAKE == 1 | AGGRESSIVE_DRIVE == 1 ~ 2,  #careless
                    TRUE ~ 1                                                                                                                      #normal
                  ))

# Creating a new "alcohol & drug" column
crash <- mutate(crash, ALCOHOL_DRUG = 
                  case_when(
                    ALCOHOL == 1 | RESULT_ALCOHOL_TEST == 1 | RESULT_DRUG_TEST == 1 ~ 2,  #alcohol & drug
                    TRUE ~ 1                                                              #normal
                  ))

# Creating a new "invalid certification" column
crash <- mutate(crash, INVALID_CERTIF = 
                  case_when(
                    CERTIF_CHARACTER == 2 | CERTIF_CHARACTER == 3 | CERTIF_CHARACTER == 5 | CERTIF_CHARACTER == 7 | CERTIF_CHARACTER == 9 ~ 2,  #invalid certification
                    TRUE ~ 1                                                                                                                    #Other
                  ))

# Creating a new "speed violation" column
crash <- mutate(crash, SPEED_VIOLATE = 
                  case_when(
                    VIOL_SPEEDING == 3 ~ 2,  #speed violate
                    TRUE ~ 1                 #Other
                  ))

# Creating a new "Vehicle Technical Defect" column
crash <- mutate(crash, VEH_TECH_DEFECT = 
                  case_when(
                    PREV_PROBLEM_TIRES == 1 | PREV_PROBLEM_REVENTON == 1 | PREV_PROBLEM_STEERING == 1 | PREV_PROBLEM_BRAKES == 1 | PREV_PROBLEM_OTHERS == 1 ~ 2,  #Vehicle Technical Defec
                    TRUE ~ 1                                                                                                                                      #Other
                  ))

# Creating a new "airbag use" column
crash <- mutate(crash, AIRBAG = 
                  case_when(
                    AIRBAG_FRONT_DRIVER == 1 | AIRBAG_FRONT_PASS == 1 | AIRBAG_LEFT_KNEE == 1 | AIRBAG_RIGHT_KNEE == 1 | AIRBAG_LEFT_FRONT == 1 | AIRBAG_RIGHT_FRONT == 1 | AIRBAG_REAR_LEFT == 1 | AIRBAG_REAR_RIGHT == 1 | AIRBAG_OTHERS == 1 ~ 2,  #airbag use
                    TRUE ~ 1                                                                                                                                                                                                                          #Other
                  ))

# Creating a new "at fault driver" column
crash <- mutate(crash, AT_FAULT_DRIVER = 
                  case_when(
                    ACC_RESPONSIBLE == 1  ~ 2,  # at fault driver
                    TRUE ~ 1                    # not at fault driver
                  ))
#-----------------------------select final variables----------------------------
#| cache: TRUE

names(crash)

crash <- subset(crash, select = -c(AGE,ACC_RESPONSIBLE,CERTIF_YEAR,INEXPERIENCE,CERTIF_CHARACTER,
                                   PLANNED_TRAVEL_DISTANCE,VIOL_SPEEDING,FACTOR_AFFECT_ATTENTION,
                                   weekdays,TOTAL_VEHICLES,TOTAL_PEDESTRIANS,LIGHTNING_CONDITION,
                                   WEATHER_CONDITION,FOG_CONDITION,SPEED_LIMIT,VEHICLE_AGE,
                                   LANE_WIDTH,SEPARATE_LONGIT_LINE,SEPARATE_MARKING,
                                   SEPARATE_MEDIAN,SEPARATE_GUARDRAIL,SEPARATE_NONE,
                                   MARGIN_TREES,MARGIN_BUILDS,MARGIN_POSTS,MARGIN_UNKNOWN,
                                   SPECIAL_CON_CONST,ROAD_CONSTRUCT,SPECIAL_CON_HOLE,SPECIAL_CON_PAVE,
                                   ROAD_DELIM_CURB,ROAD_DELIM_MARK,ROAD_DELIM_BARRIER,
                                   ROAD_DELIM_NONE,RESPECT_DAILY_REST,EXCEED_CONTIN_DRIV_HOURS,
                                   EXCEED_PERIODIC_DRIV_HOURS,DISTRACTED_DRIVE,CARELESS_DRIVE,
                                   IMPROPER_SPEED,NO_RESPECT_PRIORITY,NO_SAFE_INTERVAL,IMPROPER_OVERTAKE,
                                   AGGRESSIVE_DRIVE,ALCOHOL,RESULT_ALCOHOL_TEST,RESULT_DRUG_TEST,
                                   `TECH_TEST (ITV)`,CONTIN_DRIV_HOURS,PREV_PROBLEM_TIRES,PREV_PROBLEM_REVENTON,
                                   PREV_PROBLEM_STEERING,PREV_PROBLEM_BRAKES,PREV_PROBLEM_OTHERS,AIRBAG_FRONT_DRIVER,
                                   AIRBAG_FRONT_PASS,AIRBAG_LEFT_KNEE,AIRBAG_LEFT_KNEE,AIRBAG_RIGHT_KNEE,
                                   AIRBAG_LEFT_FRONT,AIRBAG_RIGHT_FRONT,AIRBAG_REAR_LEFT,AIRBAG_REAR_RIGHT,AIRBAG_OTHERS
                                   ))

str(crash)
sumtable(crash)
#------------------------create Descriptive Statistics tables-------------------
#| cache: TRUE

names(crash)

# List of categorical variables in the "crash" data frame
categorical_variables <- c("SEX", "BELT_USE", "VIOLATION", "ROAD_TYPE", "ACC_TYPE_COLLISION",
                           "SURF_CONDITION", "VISIBILITY", "ROAD_FUNCTION", "SHOULDER", "SECTION_BRIDGE",
                           "SECTION_TUNNEL", "NARROW_SECTION", "SECTION_PARKING", "SECTION_NONE",
                           "MOST_DAMAGE","DRIVER_AGE","EXPERIENCE","VEH_AGE", "LIGHT_CONDITION",
                           "SEPARATION", "DELIMITATION", "WEEKEND", "MULTI_SINGLE_VEHICLE",
                           "EXPIRED_TEST", "PEDESTRIAN", "WEATHER", "WORK_ZONE", "PAVEMENT",
                           "FATIGUE", "CARELESS_DRV", "AGGRESSIVE_DRV", "ALCOHOL_DRUG",
                           "INVALID_CERTIF", "SPEED_VIOLATE", "VEH_TECH_DEFECT", "AIRBAG",
                           "AT_FAULT_DRIVER")

# Create a data frame to store the results
summary_table <- data.frame()

# Loop through each categorical variable and calculate statistics
for (var in categorical_variables) {
  # Calculate the frequency table
  freq_table <- table(crash[[var]])
  
  # Calculate the total number of observations (excluding NAs)
  total_obs <- sum(freq_table)
  
  # Calculate the percentage for each category
  percentages <- (freq_table / total_obs) * 100
  
  # Calculate the proportion (decimal) for each category
  proportions <- freq_table / total_obs
  
  # Create a summary table for the current variable with variable name
  var_summary <- data.frame(Variable = var, Category = names(freq_table), Frequency = as.vector(freq_table), Percentage = percentages, Proportion = proportions)
  
  # Add the variable summary to the overall summary table
  summary_table <- rbind(summary_table, var_summary)
}

# Remove unwanted columns "Percentage.Var1" and "Proportion.Var1"
summary_table <- subset(summary_table, select = -c(Percentage.Var1, Proportion.Var1))

# Print the combined summary table
print("Summary Table for Categorical Variables:")
print(summary_table)

# Specify the file path where you want to save the Excel file
excel_file_path <- "summary_table.xlsx"

# Save the table to an Excel file
write_xlsx(summary_table, path = excel_file_path)
#--------------------------------Tables based injury level----------------------
# List of variables to analyze
variables_to_analyze <- c("SEX", "BELT_USE", "VIOLATION", "ROAD_TYPE", "ACC_TYPE_COLLISION",
                          "SURF_CONDITION", "VISIBILITY", "ROAD_FUNCTION", "SHOULDER", "SECTION_BRIDGE",
                          "SECTION_TUNNEL", "NARROW_SECTION", "SECTION_PARKING", "SECTION_NONE",
                          "MOST_DAMAGE","DRIVER_AGE","EXPERIENCE","VEH_AGE", "LIGHT_CONDITION",
                          "SEPARATION", "DELIMITATION", "WEEKEND", "MULTI_SINGLE_VEHICLE",
                          "EXPIRED_TEST", "PEDESTRIAN", "WEATHER", "WORK_ZONE", "PAVEMENT",
                          "FATIGUE", "CARELESS_DRV", "AGGRESSIVE_DRV", "ALCOHOL_DRUG",
                          "INVALID_CERTIF", "SPEED_VIOLATE", "VEH_TECH_DEFECT", "AIRBAG",
                          "AT_FAULT_DRIVER")

# Create a data frame to store the final results
final_summary_table <- data.frame()

# Loop through each variable and calculate the summary table
for (var in variables_to_analyze) {
  # Calculate the cross-tabulation of the variable and injury level
  variable_injury_table <- table(crash[[var]], crash$INJURY_LEVEL)
  
  # Calculate the row sums for each category within the variable
  row_sums_variable <- rowSums(variable_injury_table)
  
  # Calculate the percentage for each combination of the variable and injury level
  variable_injury_percentages <- (variable_injury_table / row_sums_variable) * 100
  
  # Create a summary data frame for the current variable
  var_summary <- data.frame(Variable = var, Category = rownames(variable_injury_table),
                            Fatal = variable_injury_table[, "FATAL"],
                            Severe = variable_injury_table[, "SEVERE"],
                            Minor_PDO = variable_injury_table[, "MINOR/PDO"],
                            Percentage_Fatal = variable_injury_percentages[, "FATAL"],
                            Percentage_Severe = variable_injury_percentages[, "SEVERE"],
                            Percentage_Minor_PDO = variable_injury_percentages[, "MINOR/PDO"])
  
  # Add the variable summary to the final summary table
  final_summary_table <- rbind(final_summary_table, var_summary)
}

# Print the final summary table
print("Final Summary Table for Selected Variables:")
print(final_summary_table)

# Specify the file path where you want to save the Excel file
excel_file_path <- "final_summary_table.xlsx"

# Save the table to an Excel file
write_xlsx(final_summary_table, path = excel_file_path)
#-------------------------------save final data frame---------------------------
cat("\014")
#
for (col in names(crash)) {
  uniq_val <- unique(crash[[col]]) 
  n_uniq <- length(uniq_val)
  n_miss <- sum(is.na(crash[[col]]))
  if (n_uniq < 100) {
    print(paste("Column:", col, "- Number of unique values:", n_uniq))
    print(paste("Column:", col, "- Number of missing values:", n_miss))
    tbl <- table(crash[[col]])
    print(paste("Column:", col, "- Ordered Frequency Table:"))
    print(tbl[order(tbl, decreasing = TRUE)])
  } 
}

InitialDataForAnalysis <- crash
save(InitialDataForAnalysis,
     file = "InitialDataForAnalysis.RData")

write.xlsx(InitialDataForAnalysis, "InitialDataForAnalysis.xlsx")