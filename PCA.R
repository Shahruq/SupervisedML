# Data
data("iris")
str(iris)
summary(iris)

# Partition Data
set.seed(111)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,]

# Scatter Plot & Correlations #this graph produces an elegant view of correlated variables
library(psych)
pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)

#  

# Principal Component Analysis
pc <- prcomp(training[,-5], #prcomp performs PCA - no 5th vatiable is included
             center = TRUE, #variables are converted in such a way that average becomes zero
             scale. = TRUE) #all the 4 variables are normalized

attributes(pc) 
pc$center
pc$scale
print(pc) 
summary(pc) #checking out the results from above codes

# Orthogonality of PCs #once pca is performed the multi collinearity problem is now gone
pairs.panels(pc$x, 
             gap=0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)

# Bi-Plot #just the graphical representation of stuff
library(devtools)
# install_github("vqv/ggbiplot")
# alternatively, this can be used instead https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g) # gives bi-plot

# Prediction with Principal Components
trg <- predict(pc, training)
trg <- data.frame(trg, training[5]) # adding 5th column to the dataframe
tst <- predict(pc, testing)
tst <- data.frame(tst, testing[5])

# Multinomial Logistic regression with First Two PCs
library(nnet)
trg$Species <- relevel(trg$Species, ref = "setosa") #reference value
mymodel <- multinom(Species~PC1+PC2, data = trg) #PC1 and PC2 have caught good amount of variance
summary(mymodel)

# Confusion Matrix & Misclassification Error - training
p <- predict(mymodel, trg)
tab <- table(p, trg$Species)
tab


library(caret)
confusionMatrix(p, trg$Species)

# Confusion Matrix & Misclassification Error - testing
p1 <- predict(mymodel, tst)
tab1 <- table(p1, tst$Species)
tab1
1 - sum(diag(tab1))/sum(tab1)
