library(readr)  
library(ggplot2)
library(viridis)

#read the dataset
accident_data <- read_csv("road_accident_data.csv")

#Number of missing values by column
colSums(is.na(accident_data))

#Distribution of road speed limit & vehicle speed during accident
hist(accident_data$Speed_Limit, breaks = 10, col = "lightblue", 
     main = "Histogram of Speed Limit", xlab = "Speed Limit", ylab ="Number of observations")

hist(accident_data$Vehicle_Speed, breaks = 10, col = "lightblue", 
     main = "Histogram of Vehicle Speed", xlab = "Vehicle Speed", ylab ="Number of observations")

#Distribution of emergency response time
hist(accident_data$Time_Taken_for_Emergency_Response, breaks = 20, col = "lightblue", 
     main = "Histogram of Time Taken for Emergency Response", xlab = "Time Taken", ylab ="Number of observations")


#contingency table of weather_conditions and road_conditions
contingency_table <- table(accident_data$Weather_Conditions, accident_data$Road_Conditions)

 
#plot a clustered bar plot to see the relationship between weather_condition and road_condition
ggplot(data = as.data.frame(contingency_table), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Road Conditions", y = "Frequency", fill = "Weather Conditions") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), size = 3) + 
  scale_fill_brewer(palette = "Set3")



# Boxplot for Distance_to_Nearest_Hospital
boxplot(accident_data$Distance_to_Nearest_Hospital,
        main = "Distribution of Distance to Nearest Hospital",
        ylab = "Distance (in meters)")

# Boxplot for Distance_to_Nearest_Hospital
boxplot(accident_data$Distance_to_Nearest_Hospital,
        main = "Distribution of Distance to Nearest Hospital",
        ylab = "Distance (in meters)")

# Boxplot for Distance_to_Nearest_Police_Station
boxplot(accident_data$Distance_to_Nearest_Police_Station,
        main = "Distribution of Distance to Nearest Police Station",
        ylab = "Distance (in meters)")

# Boxplot for Time_Taken_for_Emergency_Response
boxplot(accident_data$Time_Taken_for_Emergency_Response,
        main = "Distribution of Time taken for emergency response",
        ylab = "Distance (in meters)")





  











 


 


 




