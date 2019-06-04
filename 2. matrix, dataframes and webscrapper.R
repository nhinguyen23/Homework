#1a Create a vector
x<- c(-7,10,43,NA,7.6,100,NA,13)

#1b Extract all the elements smaller than 14. Save the result as el.lt.14.
el.lt.14<- x[which(x<14)]

#1c Determine the location of values smaller than 14. Save the result as ind.lt.14.
ind.lt.14<- which(x<14)

#1d Find the standard deviation of the values in x. Save the result as sd.x.
sd.x<- sd(x,na.rm = TRUE)

#1e Write one line of code whose output will give the number of missing values in x and will work
#generally on any vector with missing values. This function will help: is.na(x).
length(which(is.na(x)))

#1f Remove the missing values without specifying their indices, so don't use x[-c(4,7)]. 
#Save the result as x.not.na. Use a function to identify missing values.
x.not.na<- subset(x,!is.na(x))
# ! means not. the fucntion is.na is used to take subset of NA from vector x, so
#the !is.na is to take elements that is not NA from x

#2a 
x<- NULL
x[1]<-1
x[2]<-(x[1]^2+4)/(2*x[1])
x[3]<-(x[2]^2+4)/(2*x[2])
x.3<- x[3]

#2b Create sequence from the first 3 values in 2a
for (i in 2:500)
{
  x[i]<- (x[i-1]^2+4)/(2*x[i-1])
  
}
seq.1<-x #The sequence converge to 2

#2c
seq.2<- NULL
seq.2[1]<-300
seq.2[2]<-(seq.2[1]^2+4)/(2*x[1])
seq.2[3]<-(seq.2[2]^2+4)/(2*x[2])

for (j in 2:600) 
{
  seq.2[j]<-(seq.2[j-1]+4)/(2*seq.2[j-1])
}
seq.2 #The sequence converges to 1.6861407

#2d. the sequence does denpend on x1 

#2e Plot the values of the terms of seq.2 against the index, i, on the x-axis.
x.coord<- 1:600
y.coord<- seq.2
plot(x.coord,y.coord)

#3a 
dim(mtcars) #this statment shows mtcars has 32 rows, and 11 columns

#3b Check a type of mtcars
class(mtcars) #it is a data frame

#3c Use the which() function to find the row numbers of rows where the number of cylinders was 8.
#Store the result in a vector named rows.8.cyl.
row.8.cyl<-which(mtcars$cyl==8)

#3d Extract the rows corresponding to cars with 8 
#cylinder engines into a new dataframe called eightcylinder.
eightcylinder<- subset(mtcars,cyl==8)

#3e Find the mean mpg (miles per gallon) for the cars with manual
#transmissions.
mean.manual.mpg<-mean(eightcylinder$mpg)

#4a Use the plot() function to plot the function y = x3=(x + 1) from x = ????4 to x = 3. It will be easier if you
#create a fine grid of points from -4 to 3 using the seq() function.
x.grid<- seq(from=-4, to=3)
y<- ((x.grid)^3)/(x.grid+1)
plot(x.grid,y,type ="l")


#4b Assuming adult male heights are normally distributed with mean 69.1 inches and standard deviation 2.9
#inches, generate 450 random male heights and store them in a vector named rand.male.heights. Use the
#command rnorm(n=450,mean=69.1,sd=2.9). 
rand.male.heights<-rnorm(n=450,mean=69.1,sd=2.9)

#4c Obtain a histogram of the heights.
hist(rand.male.heights,freq = FALSE)

#4d Give the same result as 4c
curve(dnorm(x,mean=69.1,sd=2.9),add=TRUE)

#5a Write a for-loop which saves the cube of each element of x in a new vector named w.
x<- c(4,18,72,27,43,65)
w<- NULL
for (k in 1:length(x))
{
  w[k]<- x[k]^3
}

#5b Redo part (a) without a loop, by using operations on a vector. Store your result as w.vectorized.
w.vectorized<- x^3

#5c Write a for loop which finds the product of each element of x with the element before it, starting with the
#second element, and stores the results in a vector called myproducts.
myproduct<- NULL
for (t in 2:length(x))
{
  myproduct[t]<- x[t]*x[t-1]
}

#6a Using a \for" loop and \if-then" control structure, add the odd Fibonacci numbers in your vector of the
#first 50 Fibonaccis. Store your result in a scalar named sum.odd.fibs.
x<-NULL
x[1]<-1
x[2]<-1
x[3]<-2
for (h in 3:50)
{
  x[h]<- x[h-1]+x[h-2]
}

sum.odd.fibs<- 0
for (i in 1:length(x)) 
{
  if(x[i]%%2==1){sum.odd.fibs<-sum.odd.fibs+x[i]}
}

#6b Repeat part (a), now storing your result as sum.odd.fibs.2, but don't use any loops.
sum.odd.fibs.2<-sum(subset(x,x%%2==1))

#7 Scrape the data on time and date for 100 meter freestyle world records for men on Wikipedia at this url https:
#//en.wikipedia.org/wiki/World_record_progression_100_metres_freestyle. Plot the time against the
#year of each world record (so you will need to extract the year from the string representing date, the command
#strsplt() can help here).
library(rvest)
mywebpage<-read_html("https://en.wikipedia.org/wiki/World_record_progression_100_metres_freestyle")
timenodes<- html_nodes(mywebpage,css="td:nth-child(2)")
timetext<-html_text(timenodes)

timetext<-strsplit(timetext,split= "\n")

timetext[[144]]<-NULL
timetext[[145]]<-NULL
timetext[[146]]<-NULL
timetext[[143]]<-NULL
timetext[[142]]<-NULL
timetext[[141]]<-NULL
timetext[[141]]<-NULL
gsub(":" , ".", timetext)
#Don't know how to change to date time to number
timetext<- as.numeric(timetext)


yearnodes<- html_nodes(mywebpage, css = "td:nth-child(6) span")
yeartext<- html_text(yearnodes)
