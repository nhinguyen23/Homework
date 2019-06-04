#1 Suppose you have a dataset with the values 45, 77, 5, 8 and 110

#1a Input the data values into a vector named w.
w<- c(45,77,5,8,110)

#1b Find the mean, variance and standard deviation of the dataset. Save them as objects
#named x, y, and z, respectively.
x<- mean(w)
y<- var(w)
z<- sd(w)

#1c Create a vector named w.sq containing the squares of the values in this dataset.
w.sq<- w^2

#1d Use R to calculate the sum of the squares of the values in the dataset. Save the value in
#a scalar named sum.w.sq.
sum.w.sq<- sum(w.sq)

#1e Use the computational formula for the variance shown below to compute the variance of
#the dataset.
mean.w.sq<- x^2
w.var.comp<-(1/(length (w)-1))*(sum.w.sq-length (w)*mean.w.sq )

#1f Compare the means of 5*w and w
multiply.data<- 5*w
mean.multiply.data<- mean(multiply.data)
sd.multiply.data<- sd(multiply.data)
#The new mean and sd are 5 times as in part (b)

#1g Compare the means of 5+w and w
add.5<- 5+w
mean.add.5<- mean(add.5)
sd.add.5<- sd(add.5)
#The new mean is bigger than 5 as mean in (b), and the sd is the same as in (b)

#2 Make a matrix
r1<- c(1,8,3)
r2<- c(4,7,9)
r3<- c(3,2,9)
matrix1<- rbind (r1,r2,r3)
matrix1

#2a Extrac column 3 of the matix
colm3<- matrix1[,3]

#2b Extract element in row 3 and column 2 of the matrix
myelt<- matrix1[3,2]

#2c Test a code
2:3 #It returns 2,3
matrix2<- matrix (2:5, nrow=2, ncol=2) #create new matrix
matrix2

#2d Test a code
t(matrix2) #t() tranpose matrix2

#3 Create a list
mylist<- list (mymatrix=matrix (5:8,nrow=2, ncol=2), 
               mynames= c("Ruby", "Miguel", "Tiffany", "Tyler"))

#3a Extract a matrix from the list
mylist$mymatrix
mydet<-det (mylist$mymatrix)

#3b Extract third name in a name vector from the list
mylist$myname[3]

#4 See dataframe swiss
?swiss

#4a Extract Agriculture column of a dataframe 'swiss'
ag.col<-swiss$Agriculture

#4b find the percentage means of male in agriculture
mean.male.ag.<- mean(ag.col)

#4c  Extract row 40 from the dataframe 'swiss'
row40<- swiss[40,]

#4d See summary of the dataframe 'swiss'
summary(swiss)

#5 Write a function that takes a single number as input and returns the sum of the number and its
#square root. Run your function with 14 as an input and store the result as function1output.
function1<- function (n)
{sqr<- sqrt(n)
return (n+sqr(n))}
function1output<- function1 (14)

#6 Write a function to calculate the area of a triangle given its base and height. Apply your
#function to a triangle with base 3.9 and height 4.6 and store the result as function2out.
function2<- function (b,h)
{area<- (h*b)/2
return (area)}
function2output<- function2 (3.9, 4.6)

