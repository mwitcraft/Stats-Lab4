# Task 1
getwd()

# Task 2
spruce.df = read.csv("SPRUCE.csv")
tail(spruce.df)

# Task 3
# Trendscatter Plot
x11()
library(s20x)
trendscatter(Height~BHDiameter, f = 0.5, data = spruce.df)

# Linear Model
spruce.lm = lm(Height~BHDiameter, data = spruce.df)
summary(spruce.lm)

# Residuals of spruce.lm
height.res = residuals(spruce.lm)

# Fitted values from spruce.lm
height.fit = fitted(spruce.lm)

# Plot residuals vs fitted values
plot(height.fit, height.res)

# Plot (trendscatter) residuals vs fitted values
trendscatter(height.fit, height.res)

# What shape is seen in the plot?
# A rough downward parabola is what is present in the plot

# Make the residual plot
plot(spruce.lm, which = 1)

# Check normality using normcheck()
normcheck(spruce.lm, shapiro.wilk = TRUE)

# What is the pvalue for the shapiro-wilk test/What is the NULL hypothesis
# 0.29/NULL Hypothesis is not rejected and the data is normally distributed

# Outline concerns about validity of applying a straight line to this data set
# I am concerned using a straight line to model this set is an error because the relationships
# are obviously not linear

# Task 4
# Fit a quadratic to the points
quad.lm = lm(Height~BHDiameter + I(BHDiameter^2), data = spruce.df)
summary(quad.lm)
# Create scatter plot
plot(Height~BHDiameter, bg = "Blue", pch = 21, cex = 1.2, ylim = c(0, 1.2*max(Height)),
     xlim = c(0, 1.2*max(BHDiameter)), data = spruce.df)
# Function to add take coefficients from quad.lm
myplot = function(x){
  quad.lm$coef[1] + quad.lm$coef[2]*x + quad.lm$coef[3]*x^2
}
# Add the quad.lm curve to the plot
curve(myplot, lwd = 2, col = "steelblue", add = TRUE)

# Create quad.fit
quad.fit = fitted(quad.lm)
# Create quad.res
quad.res = residuals(quad.lm)

# Plot residuals vs fitted values
plot(quad.fit, quad.res)

# Create Shapiro-Wilks Plot
normcheck(quad.lm, shapiro.wilk = TRUE)

# What is the p-value of Shapiro-Wilks Test/What do you conclude
# 0.684/The quadratic model is a much better fit

# Task 5
# Summarize quad.lm
summary(quad.lm)

# Value of B0
# 0.860896

# Value of B1
# 1.469592

# Value of B2
# -0.027457

# Make interval estimates for B0, B1, B2
# TODO: Don't know how to do this

# Write down the equation of the fitted line
# Height = 0.860896 + 1.469592(BHDiameter) - 0.027457(BHDiameter^2)

# Predict Height when BHDiameter is 15, 18, 20cm
predict(quad.lm, data.frame(BHDiameter = c(15, 18, 20)))

# Compare with previous predictions (Lab 3)
# @15 - 0.35795 Taller
# @18 - 0.60402 Taller
# @19 - 0.49352 Taller

# Value of multiple R^2/Compare with previous model
# 0.7741/Previous 0.6569

# Value of the quad.lm adjusted R^2 is significantly greater than the previous one
# Therefore the quadratic model is a better model for this data

# What does multiple R^2 mean?
# TODO: 

# Which model explains the most variability in Height?
# TODO:

# Use anova() and compare the 2 models. 
anova(spruce.lm, quad.lm)
anova(spruce.lm)
anova(quad.lm)
# Give conclusion based on 2 anova outputs above
# TODO:

# Find height.qfit
height.qfit = fitted(quad.lm)
# Find TSS
TSS = with(spruce.df, sum((Height - mean(Height))^2))
TSS
# Find MSS
MSS = with(spruce.df, sum((height.qfit - mean(Height))^2))
MSS
# Find RSS
RSS = with(spruce.df, sum((Height - height.qfit)^2))
RSS
# What is MSS/TSS
MSS/TSS

# Task 6
cooks20x(quad.lm)

# Explain Cook's distance
#   Cook's distance is used to find which data points may be potential
#   outliers and may merit closer examination.
#   It does this by showing the points that have an abnormally high
#   influence on the accuracy of the regression.

# What does cooks distance for the quadratic model say?
#   The data point at location 24 has an abnormally high impact
#   on the model.

# Create quad2.lm without the 24th data point
quad2.lm = lm(Height~BHDiameter + I(BHDiameter^2), data = spruce.df[-24,])
# Summarize quad2.lm
summary(quad2.lm)
# Compare with quad.lm
summary(quad.lm)
# Conclusion
#   Removal of the outlier found at point 24 causes the R^2 value of 
#   the model to increase dramtatically, by about 4 points.
#   This makes the model fit the data much better
