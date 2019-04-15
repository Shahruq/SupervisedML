library(titanic)
library(tidyverse)
install.packages("party")
library(party)

str(Titanic)
View(Titanic)

data <- titanic_gender_class_model
data1 <- select(data,PassengerId:Survived)
str(data1)

data2 <- data1 %>% mutate(survived1 = factor(Survived)) %>% group_by(survived1) %>% summarize(count1 = n())

####basic practicing until now let us try decision tree

#importing cardio dataset 

#Read data file
mydata <- Cardiotocographic
mydata$NSPF <- as.factor(mydata$NSP)

#Decision tree with party
library(party)
mytree <- ctree(NSPF~LB+AC+FM, mydata, controls=ctree_control(mincriterion=0.9, minsplit=50)) 
#mincriterion is for deciding 1-p value to be exceeded for the split
#minsplit is the minimum number of weights in the node to be considered for the split
print(mytree)
plot(mytree,type="simple")

#Misclassification error
tab<-table(predict(mytree), mydata$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

#if you want to split the data into train and test before doing CART (Classification and Regression Tree) then do the following
set.seed(1000)
data_partition <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8,0.2))
train <- mydata[data_partition == 1,]
test <- mydata[data_partition == 2,]  #this will simply split the data into train and test datasets


