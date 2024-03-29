library(lmtest)
library(ggplot2)
library("sandwich") # for vcovHC

# Create a linear regression model with host_listings_count as the independent variable
model3 <- lm(review_scores_rating ~ host_listings_count, data = airbnb)

# Print model summary
summary(model3)

# Check for homoscedasticity with the Breusch-Pagan test
bptest(model3)

# Check for linearity 
plot(airbnb$host_listings_count, airbnb$review_scores_rating, main = "Listing count vs Review Scores", xlab = "Host Listing Count", ylab = "Review Scores Rating")
abline(model3, col = "red")

# after adding controls
model3_new<- lm(review_scores_rating ~ host_listings_count + host_response_rate+ host_response_time+ host_acceptance_rate+ host_is_superhost, data = airbnb)
summary(model3_new)
bptest(model3_new)
# estimating heteroscedasticity-robust standard errors
robust_se <- coeftest(model3_new, vcov = vcovHC(model3_new))

# Displaying the result with robust standard errors
print(robust_se)


