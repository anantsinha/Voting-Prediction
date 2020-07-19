setwd("~/Developer/Analytics Edge/Week 4 - Trees/Voting")
voting <- read.csv("gerber.csv")
str(voting)
summary(voting)
head(voting)

table(voting$voting)

print("Civic Duty")
table(subset(voting, civicduty==1)$voting)

print("Hawthorne")
table(subset(voting, hawthorne==1)$voting)

print("Self")
table(subset(voting, self==1)$voting)

print("Neighbors")
table(subset(voting, neighbors==1)$voting)

gerber <- voting
tapply(gerber$voting, gerber$civicduty, mean)


# logistic regression model
logReg <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = voting, family = "binomial")
summary(logReg)
logPred <- predict(logReg, type = "response")
# threshold 0.3
table(voting$voting, logPred > 0.3)

# threshold 0.5
table(voting$voting, logPred > 0.5)
# 0.684
# baseline
table(voting$voting)
# 0.684
library(ROCR)
rocrPred <- prediction(logPred, voting$voting)
as.numeric(performance(rocrPred, "auc") @y.values)

# generate a regression tree
library(rpart)
tree <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = voting, cp = 0.0)
library(rpart.plot)
summary(tree)
prp(tree)

# Control tree
controlTree <- rpart(voting ~ control, data = voting, cp = 0.0)
prp(controlTree, digits = 6)

# Control tree with sex
controlTreeSex <- rpart(voting ~ control + sex, data = voting, cp = 0.0)
prp(controlTreeSex, digits = 6)

# logistic regression with sex
controlTreeSexGlm <- glm(voting ~ control + sex, data = voting, family = "binomial")
summary(controlTreeSexGlm)

# sex and control
LogModel2 <- glm(voting ~ sex + control + sex:control, data =voting, family = "binomial")
summary(LogModel2)


Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModel2, newdata=Possibilities, type="response")
