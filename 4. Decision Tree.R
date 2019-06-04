#1 Build a decision tree to predict whether or not a credit card user will default on his/her credit card payment next
#month using the dataset found here https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients#.
#You need to complete the following steps:

#1a Import the data into R. Convert all categorical variables to \factor" type variables.
library(readxl)
default_of_credit_card_clients <- read_excel("C:/Users/nhing/Downloads/default 
                                             of credit card clients.xls", skip = 1)
View(default_of_credit_card_clients)

creditcard<-default_of_credit_card_clients

names(creditcard)[25]<-'payment'
names(creditcard)
 
creditcard$payment[creditcard$payment==0] <- "NO"
creditcard$payment[creditcard$payment==1] <- "YES"
creditcard$payment<-factor(creditcard$payment)

#1b Start by setting a random seed so that your results will be reproducible
set.seed (100)

#1c Randomly separate the data into training and test datasets of equal size.
set.seed (100)
rand.num<-sample(1:30000)
credit.train<-creditcard[rand.num<15001,]
credit.train<- credit.train[,-1]

credit.test<-creditcard[rand.num>15000,]
credit.test<-credit.test[,-1]

#1d Build a single, unpruned tree on the training data and use it to predict for the test data. Be sure to remove
#the ID variable when specifying the model in the tree() function.
library(tree)
credit.tree<-tree(payment ~.,data=credit.train)
#i.Plot the resulting tree with labels on each node.
plot(credit.tree)
text(credit.tree)
summary(credit.tree)
#ii. 0.1817 is proportion of misclassification of traning tree
#iv. There are 4 nodes of unpruned tree

set.seed(100)
credit.pred<-predict(credit.tree, newdata = credit.test, type = "class")
table(credit.pred, credit.test$payment)
(456+2230)/nrow(credit.test) 
#ii. 0.1790667 is proportion of misclassification of prediction
#iii. misclassification rate for subgroups of people who will default next month
#2230/(1074+2230). Misclassifition of Yes over total of Yes column of prediction table
#misclassification rate for subgroups of people who won't default next month
#456/(11240+456). Misclassification of No over total of No column od prediction table

#credit.pred    NO   YES
#NO  11240  2230
#YES   456  1074

#1e Use the built-in cross-validation function for trees to determine the optimal pruning of your rst tree. What is
#the optimal number of terminal nodes?
credit.cv<-cv.tree(credit.tree, FUN = prune.misclass, K=10)
credit.cv
plot(credit.cv)
default.forest=randomForest(creditcard~., data = creditcard)
#optimal number of terminal nodes are of size 2 and 4 because it has the same smallest
#deviance 2726

#1f Prune the tree. Use the pruned tree to make predictions for your test data and determine the misclassication
#rate.
credit.prune<-prune.misclass(credit.tree, best = 2)
#choose best = 2 or 4 is the same 
credit.prune
summary(credit.prune)
plot(credit.prune)
text(credit.prune)
credit.prune.pred<-predict(credit.prune, newdata=credit.test, type = "class")
table(credit.prune.pred, truth=credit.test$payment)
#misclassification rate is the same as the unpruned tree 
#truth
#credit.prune.pred    NO   YES
#NO  11240  2230
#YES   456  1074

#1g Run a random forest using your training data (you may use all the data, but it will take a while) using the
#default value for mtry. What is your estimate of the misclassification rate? (It is one of the outputs of the
#random forest function). Compare the misclassification rate of your pruned tree and the random forest.                                                                            
creditcard<-creditcard[,-1]
library(randomForest)
set.seed(100)
credit.Forest<- randomForest(payment~., data=creditcard)
credit.Forest
#Call:
#randomForest(formula = payment ~ ., data = creditcard) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 4

#OOB estimate of  error rate: 18.23%. <-This is estimated  misclassification rate
#compare to the pruned which is about 17.9%. Forest method isn't better than pruned 
#tree method
#Confusion matrix:
#  NO  YES class.error
#NO  22126 1238   0.0529875
#YES  4230 2406   0.6374322

#1h rerun the random forest where each split is chosen from a 
#random sample of 4 of the predictors

error.by.mtry<-numeric(16)
oob.error<-numeric(16)
for (i in 1:16) 
{credit.Forest2<-randomForest(payment~.,data = credit.train, mtry= i, ntree=300)
oob.error[i]<-credit.Forest2$err.rate[300]

pred.mtry<-predict(credit.Forest2, data= credit.test, type="class")
num.misclassified<- sum(credit.test$payment!=pred.mtry)
error.by.mtry[i]<-num.misclassified
print(i)

}

error.by.mtry/15000
#[1] 0.2681333 0.2850000 0.2893333 0.2904667 0.2915333 0.2919333 0.2912667 0.2914000
#[9] 0.2925333 0.2932000 0.2920667 0.2916000 0.2932000 0.2944000 0.2940000 0.2924667
oob.error
#[1] 0.1900000 0.1844667 0.1841333 0.1838000 0.1859333 0.1839333 0.1844667 0.1866000
#[9] 0.1865333 0.1861333 0.1863333 0.1868000 0.1874667 0.1874667 0.1881333 0.1862000
#The misclassification rate is a little bit higher by the decimal percent 
#than the default

#2 Using the built-in dataset called iris, a decision tree was constructed to predict Species based on the 4 remaining
#variables using the code below:
head(iris)
library(tree)
iris.tree<-tree(Species~., data=iris)
summary(iris.tree)
iris.tree
levels(iris$Species)

#2a. How many observations fell into the 6th node?
#54 observations fell into 6th node

#2b. Of the observations in node 6, what percent are in each species categories?
#0% setosa, 
#90.741% versicolor
#9.259% virginica

#2c. What is the predicted category for observations in this node?
#Petal.Width is predicted category for observations in 6th node

#More specificcally: Versicolor

#2d. According to what rule will all observations in node 6 be split in the next level of the tree?
#All observations in the node 6 will be split into homogeneous type of flower
#which is based on Petal.length < 4.95 to the right, and otherwise to the left.
#The homogeneity is splitted by the smallest of sum of deviance of vesicolor
#and virginica. The sum of deviance is calculated in 2e

#correct
#Petal.Width < 1.75

#2e. deviance= -2*(n1*ln(0.90741)+n2*ln(0.09259))
n1<-54*0.90741
n2<-54-n1
deviance<- -2*(n1*log(0.90741)+n2*log(0.09259))
#deviance=33.31686, is rounded to 33.32

#2f.Think of a set of predictor variables which would cause an observation to be sent to the 
#right at each possible split in the tree.

#The Petal Length, Petal.Width, and Sepal.Length is less than a specific data that 
#send the observation to the right.And the group will be split by the least deviance 
#sum

## other answers are possible  
#Petal.Length = 5
#Petal.Width=2