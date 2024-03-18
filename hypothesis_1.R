library(readr) 

#read the dataset
dataset <- read_csv("road_accident_data.csv")

#replace missing Weather_Conditions based on Road_Conditions
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions == "Muddy"] <- "Stormy"
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions == "Wet"] <- "Rainy"
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions == "Dry"] <- "Sunny"
dataset$Weather_Conditions[is.na(dataset$Weather_Conditions) & dataset$Road_Conditions %in% c("Snowy", "Icy")] <- "Snowy"

#create dummy variable for Weather_Conditions 
dataset$New_Weather_Condition <- as.factor(ifelse(dataset$Weather_Conditions %in% c("Rainy", "Snowy", "Foggy", "Stormy"), 1, 0))

dataset <- subset(dataset, select = -c(Weather_Conditions))

#save new dataset
write.csv(dataset, "clean_road_accident_data.csv", row.names = FALSE)
