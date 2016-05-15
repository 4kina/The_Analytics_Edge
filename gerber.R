# Read the data
data = read.csv("gerber.csv")
str(data)

# Problem 1 - Exploration and Logistic Regression
# What proportion of people in this dataset voted in this election?
table(data$voting)
108696/(235388 + 108696)

# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(data$voting, data$civicduty, mean)
tapply(data$voting, data$hawthorne, mean)
tapply(data$voting, data$self, mean)
tapply(data$voting, data$neighbors, mean) # answer!

# Which of the following coefficients are significant in the logistic regression model? 
modelLR = glm(voting ~ civicduty + hawthorne + self + neighbors, data = data, family = "binomial")
summary(modelLR)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predictLR = predict(modelLR, type = "response")
table(data$voting, predictLR > 0.3)
(134513 + 51966)/(134513 + 51966 + 100875 + 56730)

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
predictLR = predict(modelLR, type = "response")
table(data$voting, predictLR > 0.5)
235388/( 108696 + 235388)

#Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) and compute the AUC of the model. What is happening here?
1 - 108696/(235388 + 108696) # baseline accuracy
library(ROCR)
ROCRpred = prediction(predictLR, data$voting)
as.numeric(performance(ROCRpred,"auc")@y.values)
0.5308461

# Problem 2 - Trees
library(rpart)
library(rpart.plot)
model = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=data)
prp(model)
# none of the variables make a big enough effect to be split on

model2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=data, cp=0.0)
prp(model2)

# In the control group, which gender is more likely to vote?
model3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=data, cp=0.0)
prp(model3)

# Problem 3 - Interaction Terms
# what is the absolute value of the difference in the predicted probability 
# of voting between being in the control group versus being in a different group? 
model4 = rpart(voting ~ control, data=data, cp=0.0)
prp(model4, digits = 6)
0.34 - 0.296638
model5 = rpart(voting ~ control + sex, data=data, cp=0.0)
prp(model5, digits = 6)

#Going back to logistic regression now, create a model using "sex" and "control". Interpret the coefficient for "sex"
modelLR2 = glm(voting ~ control + sex, data = data, family = "binomial")
summary(modelLR2)
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(modelLR2, newdata=Possibilities, type="response")
0.2908065 - 0.290456

# How do you interpret the coefficient for the new variable in isolation? That is, how does it relate to the dependent variable?
modelLR3 = glm(voting ~ control + sex + sex:control, data = data, family = "binomial")
summary(modelLR3)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(modelLR3, newdata=Possibilities, type="response")
0.2904558  - 0.290456
# Should we always include all possible interaction terms of the independent variables when building a logistic regression model?
# No!