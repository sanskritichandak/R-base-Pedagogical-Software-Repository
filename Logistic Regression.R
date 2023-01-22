#Logistic Regression
#Logistic Regression is used to predict the categorical dependent variable using a given set of independent variables
#Logistic Regression is used for modeling, estimating the probability, and predicting 
#The goal is to link the independent variales together to estimate logit 
#Bernouli's distribution:
#logit (p) = ln (p/(1-p)) = Bo +B1x1
#Estimated Regression Equation 
#p-hat = (e^(Bo+B1x1))/(1+e^(Bo+B1x1))
# Odds ratio for continuous predictors: represents the change in odds for a one unit increase in the independent variable 
#FITS distribution: probability distribution fitting to a series of data concerning the repeated measurements of a variable phenomenon 
#FITS distribution takes into account the parameters of the distribution 
#FITS uses the data points in the set itself to find a prediction value of the likelihood of the event occuring 
#The FITS curve vaguely represents the shape of the letter S
#The FITS curve is theoretically asymptotic to the probability of 0 and 100 
#50% marker: 50% chance probability at which the event occurs 

# Predict a binary dependent variable, like presence absence of a disease
# We will used the following data set:
# Jones (1975) conducted a study of chronic bronchitis in Cardiff. The variables are
# - cigarettes: the number of cigarettes smoked per day
# - pollen: the air pollution level in the locality of residence
# - bronchitis: the presence/absence of bronchitis (indicated by 1/0)

bronchitis <- read.csv(file.choose())

# Attaching a data set makes the names of the variables directly available
# There is no need to use $
attach(bronchitis)

# Plot cigarettes against poll using the bronchitis values as labels:
plot(poll ~ cigs, xlab = "No. cigarettes/day",
     ylab = "Pollution level", type = "n")
text(cigs, poll, labels = bron)
legend(20, 65, legend = c("presence", "absence"),
       title = "Bronchitis",
       pch = c("1", "0"))

# The pattern of bronchitis cases is not very clear
# Consider the relationship of cigs and poll separately
# Scatter plots of a binary response are not very useful
#Improve them by adding jitter

plot(bron ~ poll)
plot(jitter(bron, 0.1) ~ poll)
plot(bron ~ cigs)
plot(jitter(bron, 0.1) ~ cigs)

# This is almost uninformative regarding how Y depends on X, due to the binary nature of Y, 
# but these plots suggest that risk of bronchitis increases with both poll and cigarettes 
# with cigarettes seeming to have the bigger effect.

# An alternative to scatter plots is to use box plots
boxplot(poll ~ bron,
        xlab = "Bronchitis presence/absence (1/0)",
        ylab = "Pollution level")
boxplot(cigs ~ bron,
        xlab = "Bronchitis presence/absence (1/0)",
        ylab = "No. cigarettes/day")

# Fit a logistic regression model:
model1 <- glm(bron ~ cigs + poll, family = binomial)
summary(model1)

# Note the meaning of the slopes is to increase the logit of the disease:
# One extra cigarette per day increases the log of the odds of bronchitis by 0.21169
# Convert the exponent and use multiplicative effects 
exp(coef(model1))

# The odds of bronchitis multiply by 1.23576 for every additional cigarette per day
# This implies that the odds of bronchitis increase by 23.58% with each extra cigarette per day

# Comparing nested models:

model2 = glm(bron ~ cigs*poll, family = binomial)
anova(model1, model2, test = "Chisq")
# There was no need for interaction effect since p-value = 0.6073

# You can also change the link function to probit or log-log:
model3 = glm(bron ~ cigs + poll, family = binomial(link="probit"))
summary(model3)

# Rough measure of goodness of fit:
# Residual deviance should much greater than its degrees of freedom
# Both link functions fit well

# Diagnostics:
plot(model1)
# Diagnostic plots not very helpful for logistic regression

# Better examine residuals:
r <- residuals(model1)
r[abs(r) > 2]
bronchitis[abs(r) > 2, ]
sum(bron[cigs == 0])

# It can be deduced that the model has issues fitting when cigarettes are 0
# 5 out of 7 such cases result in high residuals

# Group data
cutCigs <- cut(cigs, c(-1, 0, 1, 3, 5, 8, 50))
cutPoll <- cut(poll, c(-1, 55, 57.5, 60, 62.5, 65, 100))
xtabs(~cutCigs)
xtabs(~cutPoll)

total <- xtabs( ~ cutCigs + cutPoll)
presence <- xtabs(bron ~ cutCigs + cutPoll)
absence <- c(total) - c(presence)
binData <- as.data.frame.table(presence, responseName = "presence")
binData <- cbind(binData, absence)

model4 <- glm(cbind(presence, absence) ~ cutCigs + cutPoll, family = binomial,
              data = binData)
summary(model4)

