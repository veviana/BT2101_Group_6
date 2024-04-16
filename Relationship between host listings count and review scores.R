library(lmtest)
library(ggplot2)
library(sandwich)

# Create a linear regression model with host_listings_count as the independent variable
OLSModel3 <- lm(review_scores_rating ~ host_listings_count, data = airbnb)

# Print model summary
summary(OLSModel3)


######################### TESTING GM ASSUMPTIONS #####################
# Check for homoscedasticity with the Breusch-Pagan test
bptest(OLSModel3)

# Check for linearity 
plot(airbnb$host_listings_count, airbnb$review_scores_rating, main = "Listing count vs Review Scores", xlab = "Host Listing Count", ylab = "Review Scores Rating")
abline(model3, col = "red")




######################### Building multiple linear regression by adding controls #####################
MLRModel3<- lm(review_scores_rating ~ host_listings_count + host_response_rate + host_response_time +
                 host_acceptance_rate + host_is_superhost, data = airbnb)
summary(MLRModel3)


# estimating using robust standard errors
robust_se <- coeftest(MLRModel3, vcov = vcovHC(MLRModel3, type = 'HC0'))
# Displaying the result with robust standard errors
print(robust_se)


# Calculate R-squared
fitted_values <- predict(MLRModel3)
y_mean <- mean(MLRModel3$model$review_scores_rating)
# Calculate the total sum of squares
SST <- sum((MLRModel3$model$review_scores_rating - y_mean)^2)
# Calculate the residual sum of squares  
SSR <- sum((MLRModel3$residuals)^2)
# Calculate R-squared
R_squared <- 1 - (SSR / SST)
# Print R-squared
R_squared



# Calculate adjusted R-squared
n <- nrow(airbnb)
p <- length(coefficients(MLRModel3))
adjusted_R_squared <- 1 - ((n - 1) / (n - p)) * (1 - R_squared)
adjusted_R_squared