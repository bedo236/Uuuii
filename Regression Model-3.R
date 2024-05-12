# Import needed Libraries
library(ggplot2)
library(dplyr)

# Data Selection
# read my data
my_data = read.csv("D:\\Student_Performance.csv")
head(my_data)
my_data <- subset(my_data, !is.na(Performance.Index))
my_data <- subset(my_data, !is.na(Hours.Studied ))
my_data <- subset(my_data, !is.na(Previous.Scores))
my_data <- subset(my_data, !is.na(Extracurricular.Activities))
my_data <- subset(my_data, !is.na(Sample.Question.Papers.Practiced))
my_data <- subset(my_data, !is.na(Sleep.Hours))

# Data Cleaning
# remove duplicated data
cat("Number of duplicated values = ", sum(duplicated(my_data)),"\n")
# head(duplicated(my_data))

my_data = distinct(my_data)
cat("Number of duplicated values after removed them = ", sum(duplicated(my_data)),"\n")

# clean NUll or NA values in data
cat("Number of null values = ", sum(is.na(my_data)),"\n")
# that show we didn't have any null values so we don't need use this code
# head(is.na(my_data))
# my_data <- na.omit(my_data)

# boxplot to detect outliers in data
boxplot(my_data$Hours.Studied, my_data$Previous.Scores, my_data$Sleep.Hours, my_data$Sample.Question.Papers.Practiced)
# that show our data doesn't have any outliers

# Exploratory Data Analysis
#info about data 
str(my_data)
summary(my_data)

# Scatterplot of first 2 Predictor against the response variable
plot(my_data$Hours.Studied, my_data$Performance.Index,
     xlab = "Hours Studied", ylab = "Performance Index",
     main = "Scatterplot of Hours Studied vs. Performance Index")
plot(my_data$Previous.Scores, my_data$Performance.Index,
     xlab = "Previous Scores", ylab = "Performance Index",
     main = "Scatterplot of Previous Scores vs. Performance Index")

#Scatterplot of Performance.Index vs. Extracurricular.Activities
ggplot(my_data, aes(x = Extracurricular.Activities, y = Performance.Index)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Performance.Index vs. Extracurricular.Activities",
       x = "Extracurricular Activities",
       y = "Performance Index") +
  theme_minimal()

# Scatterplot of Performance.Index vs. Sleep.Hours
ggplot(my_data, aes(x = Sleep.Hours, y = Performance.Index)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatterplot of Performance.Index vs. Sleep.Hours",
       x = "Sleep Hours",
       y = "Performance Index") +
  theme_minimal()


# Scatterplot of Performance.Index vs. Previous.Scores
ggplot(my_data, aes(x = Previous.Scores, y = Performance.Index)) +
  geom_point(color = "blue", alpha = 0.7) +  # Using blue color and alpha value of 0.7
  labs(title = "Scatterplot of Performance.Index vs. Previous Scores",
       x = "Previous Scores",
       y = "Performance Index") +
  theme_minimal()


# Regression Model:
# Compute the multiple linear regression model
reg_full = lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = my_data)

# Model summary
summary(reg_full)

# Fitted values
fitted_values = fitted(reg_full) # or predict(reg_full)
head(fitted_values)
# compare fitted values with original values
head(my_data['Performance.Index'])

# Residuals
errors = resid(reg_full)
head(errors)
hist(errors)

# Predict fitted values for new data
new_data = data.frame(
  Hours.Studied = c(5, 6, 7), 
  Previous.Scores = c(80, 85, 90),
  Extracurricular.Activities = c("No", "Yes", "No"),
  Sleep.Hours = c(7, 8, 6),
  Sample.Question.Papers.Practiced = c(2, 3, 4)
)

# Predict with our model for the new data
predictions = predict(reg_full, newdata = new_data)
predictions

# make Anova Table
anova(reg_full)

# Mean Squares Error
MSE = (summary(reg_full)$sigma)^2
cat("MSE =", MSE,"\n")

# R-squared
R_squared = summary(reg_full)$r.square
cat("R squared =", R_squared,"\n")

# Convert R-squared to percentage to calculate Accuracy
accuracy <- R_squared * 100
cat("Accuracy:", accuracy, "%\n")

# F-Test
F_test = summary(reg_full)$fstatistic[1]
cat("F-Test =", F_test,"\n")

# P-Value From Summary
# Since the p-value ("< 2.2e-16") means that the p-value is very close to zero
# a very small p-value indicates that at least one of the predictors has a significant effect on the response variable.
# It suggests strong evidence against the null hypothesis
# In other words, the regression model as a whole is considered statistically significant

# Adjusted R-squared
adj_R_squared = summary(reg_full)$adj.r.square
cat("Adjusted R squared =", adj_R_squared,"\n")

# Confidence interval for coefficients
confint(reg_full, conf.level = 0.95)

# Confidence interval for fitted values
fitted_ci = predict(reg_full, interval = 'confidence', level = 0.95)
head(fitted_ci)

# Confidence interval for new observation values
new_observ_ci = predict(reg_full, int = 'p', level = 0.95)
head(new_observ_ci)

# Plotting the fitted values against the observed values

# # Load the car package for make multiple regression plots
# library(car)
# crPlots(reg_full)

# Create a data frame for visualization
model_data <- data.frame(
  Observed = my_data$Performance.Index,
  Fitted = fitted_values
)

# Scatter plot of observed vs. fitted values
ggplot(model_data, aes(x = Observed, y = Fitted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_abline(intercept = 0, slope = 1, color = "green", lwd = 1) +
  labs(title = "Observed vs. Fitted Values",
       x = "Observed Performance Index",
       y = "Fitted Performance Index") +
  theme_minimal()

# Based on the plot:
# since the majority of points are close to y = x line
# Additionally, regression line is close to the y = x line
# we can conclude that the regression model provides a good overall fit to the observed data.

# Partial Test
# Compute reduced regression model by ignore one predictor For example -> 'Sample.Question.Papers.Practiced'
# we need to know if this predictor is contribute in improving our fitting model or not
reg_reduced = lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours, data = my_data)

# Reduced Model
summary(reg_reduced)
anova(reg_reduced)

# Print adjusted R sqyare for the Reduced model
adj_R_squared_reduced = summary(reg_reduced)$adj.r.squared
cat("Adjusted R-squared (Reduced Model):", adj_R_squared_reduced,"\n")
# Compare
cat("Adjusted R-squared (Full Model):", adj_R_squared,"\n")

# Create a data frame for visualization
model_comparison = data.frame(
  Model = c("Full Model", "Reduced Model"),
  Adjusted_R_squared = c(adj_R_squared, adj_R_squared_reduced)
)

# Plotting
ggplot(model_comparison, aes(x = Model, y = Adjusted_R_squared, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Adjusted R-squared Values",
       y = "Adjusted R-squared",
       x = "Model") +
  theme_minimal()


# Import Libraries
library(glmnet)

# Prepare data
x <- as.matrix(my_data[, -1]) # Remove the response variable 'BodyFat'
y <- my_data$BodyFat

# Fit Lasso model
lasso_model <- cv.glmnet(x, y, alpha = 1) # alpha = 1 for Lasso

# Plot coefficients path
plot(lasso_model)

# Q-Q plot for residuals
qqnorm(errors)
qqline(errors)

# Normality test
shapiro.test(errors)


# Reduced Lasso model
# Reduced Lasso model
lasso_reduced <- glmnet(x, y, alpha = 1, lambda = lasso_model$lambda.min)

# Fit multiple linear regression model
lm_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = my_data)

# Compare models
summary(lasso_reduced)
summary(lm_model) 







# Conclusion: 
# Based on the comparison of adjusted R-squared values between the full model (including all predictors) 
# and the reduced model (ignore the predictor 'Sample.Question.Papers.Practiced')
# adj_R_squared_reduced < adj_R_squared So it indicates that this predictor contributes positively
# it is evident that including 'Sample.Question.Papers.Practiced' in the model 'improves the model fit'.
# Try changing the predictor. I will definitely not repeat the code 5 times !!