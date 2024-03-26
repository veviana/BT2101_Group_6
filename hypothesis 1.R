library(lmtest)

# Create a simple linear regression model with cancellation policy as the independent variable
model1 <- lm(review_scores_rating ~ cancellation_policy, data = airbnb)

# Print model summary
summary(model1)


######################### TESTING GM ASSUMPTIONS #####################

# Checks for linearity
boxplot(review_scores_rating ~ cancellation_policy, data = airbnb)

# Scatterplot with Lowess smoothing
plot(review_scores_rating ~ cancellation_policy, data = airbnb)
lines(lowess(airbnb$review_scores_rating, airbnb$cancellation_policy), col = "red")



#Checks for Homoscedasticity

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




#Improved model by adding controls  
improved_model <- lm(review_scores_rating ~ cancellation_policy + instant_bookable + host_is_superhost + minimum_nights + room_type, data = airbnb)

# Print model summary

summary(improved_model)


