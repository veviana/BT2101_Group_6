#airbnb <- read.csv("/Users/xuehan/Desktop/BT2101_Group_6/cleandataset.csv")
library(lmtest)
library(sandwich)


# Create a simple linear regression model with price as the independent variable
OLSModel2 <- lm(review_scores_rating ~ price, data = airbnb)

# Print model summary
summary(OLSModel2)



######################### TESTING GM ASSUMPTIONS #####################
# Check for linearity 
plot(airbnb$price, airbnb$review_scores_rating, main = "Price vs Review Scores", xlab = "Price", ylab = "Review Scores Rating")
abline(OLSModel2, col = "red")

# Check for Homoscedasticity using residual plot
plot(OLSModel2$fitted.values, resid(OLSModel2), main = "Homoscedasticity Check: Fitted vs Residuals", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red") 

# Check for Homoscedasticity using BP test
bptest_result <- bptest(OLSModel2)
print(bptest_result)

#Normality Residual 
qqnorm(resid(model1))
qqline(resid(model1), col = "red")




######################### Building multiple linear regression by adding controls #####################
airbnb$neighbourhood_cleansed <- as.factor(airbnb$neighbourhood_cleansed)
MLRModel2 <- lm(review_scores_rating ~ price + neighbourhood_cleansed + host_is_superhost + minimum_nights, data = airbnb)

# Print the summary of the model
summary(MLRModel2)

# Check for Homoscedasticity using BP test
bptest_result2 <- bptest(MLRModel2)
print(bptest_result2)


# Calculate robust standard errors for model coefficients
coeftest(MLRModel2, vcov = vcovHC(MLRModel2, type = 'HC0'))

# Calculate R-squared
fitted_values <- predict(MLRModel2)
y_mean <- mean(MLRModel2$model$review_scores_rating)
# Calculate the total sum of squares
SST <- sum((MLRModel2$model$review_scores_rating - y_mean)^2)
# Calculate the residual sum of squares  
SSR <- sum((MLRModel2$residuals)^2)
# Calculate R-squared
R_squared <- 1 - (SSR / SST)
# Print R-squared
R_squared



# Calculate adjusted R-squared
n <- nrow(airbnb)
p <- length(coefficients(MLRModel2))
adjusted_R_squared <- 1 - ((n - 1) / (n - p)) * (1 - R_squared)
adjusted_R_squared











