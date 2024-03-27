library(lmtest)
library(ggplot2)

# Create a linear regression model with host_listings_count as the independent variable
model3 <- lm(review_scores_rating ~ host_listings_count, data = airbnb)

# Print model summary
summary(model3)

# Check for homoscedasticity with the Breusch-Pagan test
bptest(model3)

model3log <- lm(log(review_scores_rating) ~ host_listings_count, data = airbnb)
summary(model3log)

# Homoscedasticity
test_result <- bptest(model3log)
print(test_result)

# Check for linearity 
#plot(airbnb$host_listings_count, airbnb$review_scores_rating, main = "Listing count vs Review Scores", xlab = "Host Listing Count", ylab = "Review Scores Rating")
#abline(model3, col = "red")

# Homoscedasticity
#plot(model3log$fitted.values, resid(model3log), main = "Homoscedasticity Check: Fitted vs Residuals", xlab = "Fitted values", ylab = "Residuals")
#abline(h = 0, col = "red") 

# after adding controls
model3_new<- lm(log(review_scores_rating) ~ host_listings_count + host_response_rate+ host_response_time+ host_acceptance_rate+ host_is_superhost, data = airbnb)
summary(model3_new)
