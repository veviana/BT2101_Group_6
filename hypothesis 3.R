library(lmtest)

# Create a linear regression model with host_response_time as the independent variable
model3 <- lm(review_scores_rating ~ host_response_time, data = cleandataset)

# Print model summary
summary(model3)

# Check for homoscedasticity with the Breusch-Pagan test

bptest(model3)

boxplot(review_scores_rating ~ host_response_time, data = cleandataset)

# Plotting the residuals for visual inspection
plot(review_scores_rating ~ host_response_time, data = airbnb)
lines(lowess(airbnb$review_scores_rating, airbnb$host_response_time), col = "blue")

residuals <- residuals(model3)
ggplot(airbnb, aes(host_response_time, sqrt(abs(residuals)))) + 
  geom_point() +
  stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5) + 
  labs(x = "host_response_time", y = "Square Root of Residuals", 
       title = "Residual Variance vs host_response_time") +
  theme(plot.title = element_text(hjust = 0.5))
