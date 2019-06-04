#1An system of 3 equations in 3 unknowns is shown below:
  #2x + 3y + 10z = 5
  #5x ???? y + 8= 1.2
  #10x ???? 3y + 50z = 11

#1a Using Cramer's Rule to solve the system of equations using R.
matrix.D<- matrix(c(2,5,10,3,-1,-3,10,8,50), nrow=3, ncol=3)
D.x<- matrix(c(5, 1.2, 11, 3,-1,-3,10,8,50), nrow=3, ncol=3)
D.y<- matrix(c(2,5,10,5, 1.2, 11,10,8,50),nrow=3, ncol=3)
D.z<- matrix(c(2,5,10,3,-1,-3,5, 1.2, 11),nrow=3, ncol=3)
x<- det(D.x)/ det(matrix.D)
y<- det(D.y)/det(matrix.D)
z<- det(D.z)/det(matrix.D)

#1b Another way to solve this system is to use a linear algebra result
#which states that the solution is D????1 matrix multiplied by the vector b.
b<- matrix(c(5, 1.2, 11), ncol=1)
D.inverse<- solve(matrix.D)
D.inverse %*% b
#This method gives the same roots as part a

#1c Verify that your solution works using arithmetic in R, i.e. just plug
#your solutions back into the 3 original equations.
2*x+3*y+10*z #=5
5*x-y+8*z    #=1.2
10*x-3*y+50*z#=11
#the roots return the correct result.

#1d.This part is just practice with some matrix arithmetic and commands.
#i Find the standard deviation of each column of D.
apply(matrix.D, MARGIN=2, FUN=sd)

#1d.ii Construct another matrix, named E with 3 rows and 3 columns
#filled columnwise by the values 2,4,6,8,...18. Multiply D and E
#using elementwise multiplication.
matrix.E<- matrix(2*1:9, nrow=3)
matrix.D*matrix.E

#1d.iii Put the matrices D and E together by column binding them.
cbind(matrix.D, matrix.E)

