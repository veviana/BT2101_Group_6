library(readr)
library(corrplot)
library(ggplot2)
library(treemap)
library(car)
 
# Read the dataset
airbnb <- read_csv("airbnb.csv")

# Number of missing values by field
colSums(is.na(airbnb))

# Check data type 
str(airbnb)



############## CHECK DISTRIBUTIONS (sHAPE OF THE DISTRIBUTION) ############## 
review_scores_rating_data <- airbnb$review_scores_rating
ggplot(airbnb, aes(x = review_scores_rating_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for Review Score Rating",
       x = "Review Score Rating",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of review_scores_rating is still acceptable



host_days_data <- airbnb$host_days
ggplot(airbnb, aes(x = host_days_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for Host Days",
       x = "Host Days",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Conclusion: skewness of host_days is still acceptable



host_listings_count_data <- airbnb$host_listings_count
ggplot(airbnb, aes(x = host_listings_count_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for host listings count",
       x = "Host Listings Count",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: host_listings_count is skewed to the left

 
  
accommodates_data <- airbnb$accommodates
ggplot(airbnb, aes(x = accommodates_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for accommodates",
       x = "Accommodates",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of accommodates is still acceptable


bathrooms_data <- airbnb$bathrooms
ggplot(airbnb, aes(x = bathrooms_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for bathrooms",
       x = "Bathrooms",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of bathrooms is still acceptable


bedrooms_data <- airbnb$bedrooms
ggplot(airbnb, aes(x = bedrooms_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for bedrooms",
       x = "Bedrooms",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of bedrooms is still acceptabl



beds_data <- airbnb$beds
ggplot(airbnb, aes(x = beds_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for beds",
       x = "Beds",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of beds is still acceptable




price_data <- airbnb$price
ggplot(airbnb, aes(x = price_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for price",
       x = "Price",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: price is skewed to the left




guests_included_data <- airbnb$guests_included
ggplot(airbnb, aes(x = guests_included_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for guests_included",
       x = "guests_included",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of guests_included is still acceptable




extra_people_data <- airbnb$extra_people
ggplot(airbnb, aes(x = extra_people_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for extra_people",
       x = "extra_people",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of extra_people is still acceptable


minimum_nights_data <- airbnb$minimum_nights
ggplot(airbnb, aes(x = minimum_nights_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for minimum_nights",
       x = "minimum_nights",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: minimum_nights is heavily skewed to the left




reviews_per_month_data <- airbnb$reviews_per_month
ggplot(airbnb, aes(x = reviews_per_month_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for reviews_per_month",
       x = "reviews_per_month",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of reviews_per_month is still acceptable

 

 

review_scores_communication_data <- airbnb$review_scores_communication
ggplot(airbnb, aes(x = review_scores_communication_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for review_scores_communication",
       x = "review_scores_communication",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Conclusion: skewness of review_scores_communication is still acceptable






############## CHECK DISTRIBUTION OF CATEGORICAL VARIABLES ############## 

#Distribution of host_response_time
ggplot(airbnb, aes(x = host_response_time, fill = host_response_time)) +
  geom_bar(color = "black") +  # Change outline color
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "Response time of the host",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE) 




#Distribution of room_type
airbnb$room_type <- factor(airbnb$room_type, levels = names(sort(table(airbnb$room_type))))

ggplot(airbnb, aes(x = room_type, fill = room_type)) +
  geom_bar(color = "black") +   
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "Distribution of room type",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE) 



#Distribution of cancellation_policy
airbnb$cancellation_policy <- factor(airbnb$cancellation_policy, 
                                     levels = names(sort(table(airbnb$cancellation_policy))))
 
ggplot(airbnb, aes(x = cancellation_policy, fill = cancellation_policy)) +
  geom_bar(color = "black") +  
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "Distribution of cancellation policy",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)  


############## CHECK FOR CORRELATION ##############  
 
numeric_variables <- airbnb[sapply(airbnb, is.numeric)]
 
correlation_matrix <- cor(numeric_variables, use = "pairwise.complete.obs")
corrplot(correlation_matrix, method = "number", type = "lower", tl.col = "black", tl.srt = 45, tl.cex = 0.7, addCoef.col = "black")
 




############## CHECK FOR DUPLICATES ##############  
message("Number of duplicates:", sum(duplicated(airbnb)))












