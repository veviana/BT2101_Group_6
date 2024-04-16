library(car)
library(lmtest)

# Read the dataset
airbnb <- read_csv("cleandataset.csv")


# Create a simple linear regression model with cancellation policy as the independent variable
OLSModel1 <- lm(review_scores_rating ~ cancellation_policy, data = airbnb)

# Print model summary
summary(OLSModel1)


######################### TESTING GM ASSUMPTIONS #####################

# Checks for linearity
boxplot(review_scores_rating ~ cancellation_policy, data = airbnb)

# Scatterplot with Lowess smoothing
plot(review_scores_rating ~ cancellation_policy, data = airbnb)
lines(lowess(airbnb$review_scores_rating, airbnb$cancellation_policy), col = "red")



# Checks for Homoscedasticity

# 1) Breusch-Pagan test
bptest(model1)

# 2) Check the residual variance 
residuals <- residuals(model1)
ggplot(airbnb, aes(cancellation_policy, sqrt(abs(residuals)))) + 
  geom_point() +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5) + 
  labs(x = "Cancellation Policy", y = "Square Root of Residuals", 
       title = "Residual Variance vs Cancellation Policy") +
  theme(plot.title = element_text(hjust = 0.5))



# CALCULATE VIF TO CHECK FOR MULTICOLLINEARITY (REVIEW SCORE LOCATION )
data <- airbnb[c("review_scores_accuracy", "review_scores_cleanliness", "review_scores_value", 
                 "review_scores_communication", "review_scores_checkin", "review_scores_location", 
                 "review_scores_rating")]


# Calculate VIF for each variable against review score rating
vif_values <- vif(lm(review_scores_rating ~ ., data = data))
print(vif_values)






######################### Building multiple linear regression by adding controls #####################
MLRModel1 <- lm(review_scores_rating ~ cancellation_policy + instant_bookable + 
                       host_is_superhost + minimum_nights + review_scores_location, data = airbnb)

# Print model summary
summary(MLRModel1)




 


