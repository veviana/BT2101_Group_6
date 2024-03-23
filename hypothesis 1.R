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













#try_model <- lm(review_scores_rating ~ cancellation_policy + host_acceptance_rate + host_is_superhost + host_identity_verified, data = airbnb)

# Print model summary
#summary(try_model)