library(readr)
library(corrplot)
library(ggplot2) 
library(DescTools)
library(tidyr)
library(dplyr)


# Read the dataset
airbnb <- read_csv("airbnb.csv")
  

################## IMPUTE MISSING VALUES ################### 

#host_response_time has missing values, replaced with the mode
airbnb$host_response_time <- ifelse(is.na(airbnb$host_response_time),
                                    Mode(airbnb$host_response_time, na.rm = TRUE),
                                    airbnb$host_response_time)


#host_response_time: impute with the median  
# convert the data type into numeric first before imputing with median
airbnb$host_response_rate <- as.numeric(gsub("%", "", airbnb$host_response_rate))
 
median_response_rate <- median(airbnb$host_response_rate, na.rm = TRUE)
airbnb$host_response_rate[is.na(airbnb$host_response_rate)] <- median_response_rate




#host_acceptance_rate: impute with the median  
# convert the data type into numeric first before imputing with median
airbnb$host_acceptance_rate <- as.numeric(gsub("%", "", airbnb$host_acceptance_rate))

median_acceptance_rate <- median(airbnb$host_acceptance_rate, na.rm = TRUE)
airbnb$host_acceptance_rate[is.na(airbnb$host_acceptance_rate)] <- median_acceptance_rate




#property_type has missing values, replaced with the mode
airbnb$property_type <- ifelse(is.na(airbnb$property_type),
                                    Mode(airbnb$property_type, na.rm = TRUE),
                                    airbnb$property_type)

#city has missing values, replaced with the mode
airbnb$city <- ifelse(is.na(airbnb$city),
                               Mode(airbnb$city, na.rm = TRUE),
                               airbnb$city)



#beds: impute with the median  
median_beds <- median(airbnb$beds, na.rm = TRUE)
airbnb$beds[is.na(airbnb$beds)] <- median_beds


#review score location: impute with the median  
median_location_score <- median(airbnb$review_scores_location, na.rm = TRUE)
airbnb$review_scores_location[is.na(airbnb$review_scores_location)] <- median_location_score






################## LOG TRANSFORMATION ###################

# host_listings_count
small_const <- 0.001  #data contains 2 rows of "0" value in host_listings_count 
airbnb$host_listings_count <- log(airbnb$host_listings_count + small_const)
 
 

# price 
airbnb$price <- log(airbnb$price)



# minimum_night 
airbnb$minimum_nights <- log(airbnb$minimum_nights)


 



################## CREATING DUMMY VARIABLES ###################

# host_response_time --> within an hour = fast response time = 1, else 0
airbnb$host_response_time <- ifelse(airbnb$host_response_time %in% c("within an hour"), 1, 0)


# host_response_rate > 90% = quick service = 1, else 0 (90% is based on Airbnb's standard for superhost)
airbnb$host_response_rate <- ifelse(airbnb$host_response_rate > 90, 1, 0)

# host_acceptance_rate > 90% = 1, else 0 (90% is based on Airbnb's standard for superhost)
airbnb$host_acceptance_rate <- ifelse(airbnb$host_acceptance_rate > 90, 1, 0)

# cancellation_policy = flexible/moderate = 1, else 0 (strict cancellation policy)
airbnb$cancellation_policy <- ifelse(airbnb$cancellation_policy %in% c("flexible", "moderate"), 1, 0)


 


# change these fields below to factor data type so that it can be used as fixed effects if needed
airbnb$property_type <- factor(airbnb$property_type)
airbnb$room_type <- factor(airbnb$room_type)
airbnb$bed_type <- factor(airbnb$bed_type)
airbnb$neighbourhood_cleansed <- factor(airbnb$neighbourhood_cleansed)

 


# Save new dataset
#write.csv(airbnb, "cleandataset.csv", row.names = FALSE)

 