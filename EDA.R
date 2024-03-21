library(readr)
library(corrplot)
library(ggplot2)
 
# Read the dataset
airbnb <- read_csv("airbnb.csv")

# Number of missing values by field
colSums(is.na(airbnb))

# Check data type 
str(airbnb)



############## CHECK DISTRIBUTIONS (sHAPE OF THE DISTRIBUTION) ############## 
host_days_data <- airbnb$host_days
ggplot(airbnb, aes(x = host_days_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for Host Days",
       x = "Host Days",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of host_days is still acceptable



host_listings_count_data <- airbnb$host_listings_count
ggplot(airbnb, aes(x = host_listings_count_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for host listings count",
       x = "Host Listings Count",
       y = "Density") +
  theme_minimal()
#Conclusion: host_listings_count is skewed to the left

 
  
accommodates_data <- airbnb$accommodates
ggplot(airbnb, aes(x = accommodates_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for accommodates",
       x = "Accommodates",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of accommodates is still acceptable


bathrooms_data <- airbnb$bathrooms
ggplot(airbnb, aes(x = bathrooms_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for bathrooms",
       x = "Bathrooms",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of bathrooms is still acceptable


bedrooms_data <- airbnb$bedrooms
ggplot(airbnb, aes(x = bedrooms_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for bedrooms",
       x = "Bedrooms",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of bedrooms is still acceptabl



beds_data <- airbnb$beds
ggplot(airbnb, aes(x = beds_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for beds",
       x = "Beds",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of beds is still acceptable




price_data <- airbnb$price
ggplot(airbnb, aes(x = price_data)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for price",
       x = "Price",
       y = "Density") +
  theme_minimal()
#Conclusion: price is skewed to the left




guests_included_data <- airbnb$guests_included
ggplot(airbnb, aes(x = guests_included)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for guests_included",
       x = "guests_included",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of guests_included is still acceptable




extra_people_data <- airbnb$extra_people
ggplot(airbnb, aes(x = extra_people)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for extra_people",
       x = "extra_people",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of extra_people is still acceptable


minimum_nights_data <- airbnb$minimum_nights
ggplot(airbnb, aes(x = minimum_nights)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for minimum_nights",
       x = "minimum_nights",
       y = "Density") +
  theme_minimal()
#Conclusion: minimum_nights is heavily skewed to the left




reviews_per_month_data <- airbnb$reviews_per_month
ggplot(airbnb, aes(x = reviews_per_month)) +
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Plot for reviews_per_month",
       x = "reviews_per_month",
       y = "Density") +
  theme_minimal()
#Conclusion: skewness of reviews_per_month is still acceptable















