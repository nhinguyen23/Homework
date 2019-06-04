#1a Import dataframe
library(readxl)
deptOfEducationFundingbyState <- read_excel("C:/Users/nhing/Downloads/deptOfEducationFundingbyState.xlsx")
View(deptOfEducationFundingbyState)

#1b Join 2 data frames
View(state.x77)
rownames(state.x77)

joint<-merge(deptOfEducationFundingbyState,state.x77,by.x = "State or Other Area",by.y = 0)
View(joint)


#1c Create correlation of the join dataframe
cor(joint[,-1])
#The population and frost are the most correlated with federal funding.The population
#correlates about 95.8% with all the years of 2016 to 2018 funding 
#and it's strong correlation. The area correaltes about -42.8% with all the years 
#funding and it's pretty strong correlation

#1d Plot correlation
install.packages("corrplot")
x<-joint[,-1]
corrplot(cor(x), method = "square")

#2a Go to https://www.kaggle.com/uciml/pima-indians-diabetes-database and import this dataset into RStu-
#dio.
library(readr)
diabetes <- read_csv("C:/Users/nhing/Downloads/diabetes.csv")
View(diabetes)

#2b There are clearly some values that don't make sense in columns 2-6. Try the summary() function to see this.
#Remove the rows from the dataframe for values that are clearly nonsense in column 2. In column 6, replace
#the nonsensical values with NA's.
col2.fix<-subset(diabetes,diabetes$Glucose>0)
col6.fix<- diabetes$BMI[is.na(diabetes$BMI)]<-0 

#***Corect 
# remove rows where plasma glucose (column 2) is 0 (nonsensical)
pima.data.cleaned <- pima.data[which(pima.data$Glucose != 0),]
# for rows where BMI (column 6) is 0, replace with NA
pima.data.cleaned[which(pima.data.cleaned$BMI == 0), "BMI"] <- NA
# view the cleaned dataset
View(pima.data.cleaned)
#*****

#2c Run a linear regression model to try to predict BMI based on Triceps skinfold thickness. The command
#lm(formula,data) will help. The formula argument will be BMI ~ Triceps. Construct a scatterplot and
#overlay the best fit line.
mymodel<-lm(BMI~SkinThickness, data=col2.fix)
plot(col2.fix$SkinThickness,col2.fix$BMI)
abline(mymodel)
 
#2d Just looking at the scatterplots, which two variables appear to have the strongest linear
#correlation?
library(lattice)
splom(col2.fix[c(1:5)])
#glucose and insulin correlate the strongest

#2e Add on a column which gives the age in days of each subject.
col2.fix$Age<- 365*col2.fix$Age

#3a Which variables are factors?
??ChickWeight
#the variables 'diet' and 'chick'  are factors 

#3b For each diet, obtain the mean weight at 21 days for all chicks on that diet.
Chick.Weight<- subset(ChickWeight, ChickWeight$Time==21)
by(Chick.Weight, Chick.Weight$Diet, summary)

#3c Write a single command to split the dataframe into subgroups by diet.
split(ChickWeight, ChickWeight$Diet)

#4 Construct a dataframe where the rst column contains the following gender data of 10 students: F,F,M,F,M,M,M,M,F,M
#and the second column contains the following data on whether the student lives on-campus(coded as 0) or o-campus
#(coded as 1): 1,1,0,0,1,0,1,0,0,0. Obtain a 2x2 contingency table for these data using the table() command
g<- c("F","F","M","F","M","M","M","M","F","M")
student<- c(1,1,0,0,1,0,1,0,0,0) #student lives on-campus coded as 0, lives off-campus
#coded as 1
mydf <- data.frame(list(gender=g,student=student))
table(mydf)
