#Random forest can be used for classification and regression datasets
#In this case it is classification dataset as the variable NSP is a factor independent varaiable
#video link is https://www.youtube.com/watch?v=dJclNIN-TPo&list=PL34t5iLfZddsQ0NzMFszGduj3jE8UFm4O&index=10&t=0s

# Read Data
data <- read.csv("~/Desktop/CTG.csv", header = TRUE)
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest
library(randomForest)
set.seed(222)
rf <- randomForest(NSP~., data=train,
                   ntree = 300,
                   mtry = 8, #this value is decided after tune mtry
                   importance = TRUE,
                   proximity = TRUE) #the dot after NSP indicates its relationship with all the variables in the dataset
print(rf)
attributes(rf)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$NSP)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$NSP)

# Error rate of Random Forest
plot(rf)

# Tune mtry #tuning trees to improve accuracy
t <- tuneRF(train[,-22], train[,22],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300, #300 because after 300 trees the error was constant so lets fine tune that
            trace = TRUE,
            improve = 0.05) 

# No. of nodes for the trees 
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance #demonstrates which variables are important for the analysis
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf)
varUsed(rf) #gives the number times the variables have been used in the analysis

# Partial Dependence Plot
partialPlot(rf, train, ASTV, "2")

# Extract Single Tree
getTree(rf, 1, labelVar = TRUE) #terminal node -1 gives prediction

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$NSP)
