airbnb <- read.csv("/Users/xuehan/Desktop/BT2101_Group_6/cleandataset.csv")
library(lmtest)

# Create a simple linear regression model with price as the independent variable
model1 <- lm(review_scores_rating ~ price, data = airbnb)

# Print model summary
summary(model1)

# Check for linearity 
plot(airbnb$price, airbnb$review_scores_rating, main = "Price vs Review Scores", xlab = "Price", ylab = "Review Scores Rating")
abline(model1, col = "red")

# Homoscedasticity
plot(model1$fitted.values, resid(model1), main = "Homoscedasticity Check: Fitted vs Residuals", xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red") 

test_result <- bptest(model1)
print(test_result)

#Normality Residual 
qqnorm(resid(model1))
qqline(resid(model1), col = "red")

# Goldfeld Quandt Test
gqtest_result <- gqtest(model1, alternative = "two.sided")
print(gqtest_result)

# Adding controls 
airbnb$neighbourhood_cleansed <- as.factor(airbnb$neighbourhood_cleansed)
model2 <- lm(review_scores_rating ~ price + neighbourhood_cleansed + host_is_superhost + minimum_nights, data = airbnb)

# Print the summary of the model
summary(model2)



