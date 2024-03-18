library(readr) 

# Read the dataset
dataset <- read_csv("road_accident_data.csv")


# CLEAN MISSING VALUES

# 1)
# Calculate the mean of non-missing values for Speed_Limit 
mean_speed_limit <- floor(mean(dataset$Speed_Limit, na.rm = TRUE))

# Replace missing Speed_Limit values with the calculated mean
dataset$Speed_Limit <- ifelse(is.na(dataset$Speed_Limit), mean_speed_limit, dataset$Speed_Limit)

# 2)
# Calculate the mean of non-missing values for Vehicle_Speed
mean_vehicle_speed <- floor(mean(dataset$Vehicle_Speed, na.rm = TRUE))

# Replace missing Vehicle_Speed values with the calculated mean
dataset$Vehicle_Speed <- ifelse(is.na(dataset$Vehicle_Speed), mean_vehicle_speed, dataset$Vehicle_Speed)

# 3)
# Calculate the mean of non-missing values for Emergency response time
mean_response_time <- mean(dataset$Time_Taken_for_Emergency_Response, na.rm = TRUE)

# Replace missing Emergency response time values with the calculated mean
dataset$Time_Taken_for_Emergency_Response <- ifelse(is.na(dataset$Time_Taken_for_Emergency_Response), mean_response_time, dataset$Time_Taken_for_Emergency_Response)


# CLEANING AND CREATING DUMMY VARIABLES FOR CATEGORICAL DATA 

# Replace missing Weather_Conditions based on Road_Conditions
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions == "Muddy"] <- "Rainy"
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions == "Wet"] <- "Stormy"
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions == "Dry"] <- "Sunny"
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions %in% c("Snowy", "Icy")] <- "Snowy"

# Create dummy variable for Weather_Conditions (bad weather are encoded as 1)
dataset$New_Weather_Condition <- as.factor(ifelse(dataset$Weather_Conditions %in% c("Rainy", "Snowy", "Foggy", "Stormy"), 1, 0))

# Create dummy variable for Road_Conditions (bad road condition are encoded as 1)
dataset$New_Road_Condition <- as.factor(ifelse(dataset$Road_Conditions %in% c("Icy", "Muddy", "Snowy", "Wet"), 1, 0))

# Create dummy variable for Driver_age_group (impact of younger people on accident severity - encode young people as 1, and older drivers as 0)
dataset$New_Driver_Age_Group <- as.factor(ifelse(dataset$Driver_Age_Group %in% c("Teenager", "Young Adult"), 1, 0))

# Create dummy variable for Light_Conditions (dark lighting = 1, bright lighting = 0)
dataset$Light_Conditions <- as.factor(ifelse(dataset$Light_Conditions %in% c("Dusk", "Night"), 1, 0))

# Save new dataset
write.csv(dataset, "clean_road_accident_data.csv", row.names = FALSE)