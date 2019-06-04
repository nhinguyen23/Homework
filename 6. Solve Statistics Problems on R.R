#1 Suppose that 10% of Americans are left-handed. Let the random variable X = the number of lefties among 30 randomly
#selected Americans.

#1a Use R to calculate the probability of exactly 5 lefties among the 30 randomly selected Americans.
a<- dbinom(5,30,.1)

#1b Use R to calculate the probability distribution of X.
b<- dbinom(0:30,30,.1)

#1c Obtain a graph of your probabilities from part (b).
plot(0:30,b, type = "h")

#1d Suppose a class which holds 30 students has 3 left-handed desks. Use one command in R to calculate the probability
#there will be enough left-handed desks for a random group of 30 students.
c<- pbinom(3,30,0.1)

#2 Suppose that heights of women are normally distributed with mean 64 inches and standard deviation 2.5 inches.

#2a What percent of women are over 6 feet tall?
d<- pnorm(72,mean = 64, sd=2.5,lower.tail = F) #6ft equal 72 inches

#2b What percent are between 60 and 70 inches tall?
e <- pnorm(60, mean = 64, sd= 2.5, lower.tail = F)#0.9452007
f <- pnorm(70, mean = 64, sd= 2.5, lower.tail = F)#0.008197536
e-f = 0.9452007-0.008197536 #93.70032% of women in between 60 and 70 inches

#2c Find the 90th percentile of womens heights.
qnorm(.9, mean=64, sd=2.5) #67.20388 inches is 90th percentile

#2d Randomly generate the heights of 10 women. Calculate the mean and standard deviation of the 10 sampled heights
#and compare them to the population mean and standard deviation. Comment on how they compare.
sample<- rnorm(10, mean = 64, sd= 2.5)
mean(sample) #64.70673
sd(sample)#2.574915
#This sample has mean and variance little increase 
#from the original mean= 64 and variance=2.5

#2e Repeat part (d) with a new sample.
sample1<- rnorm(10,mean = 64, sd=2.5)
mean(sample1) #65.95296
sd(sample1)#2.692864
#This sample has mean and variance a little 
#increase compare to the original

#It can be proven that that square of a standard normal random variable (mean=0, standard deviation = 1) has a
#chi-square distribution.

#3a Randomly generate 10,000 values from a standard normal distribution, then square them.
sample2<- rnorm(10000, mean = 0, sd=1)
sample3<- c(sample2 ^2)

#3b Obtain a histogram of your values from part (a).
hist(sample3)

#3c Overlay a chi-square distribution with 1 degree of freedom.
dchisq(x,df=1)

#***Correct
curve(dchisq(x, df = 1), add = TRUE)
# Since the chi-square distribution we just graphed is the true square of
#   the standard normal distribution, we can approximate it with our histogram
#   approximation to the squared standard normal (from part (a)).
# sum(normal_sample_squared < 2) adds up the number of elements in normal_sample_squared
#   that are less than 2, since the command
#       normal_sample_squared < 2
#   returns TRUE for any such element.
num_random_variates_less_than_2 <- sum(normal_sample_squared < 2)
prob_less_than_2 <- num_random_variates_less_than_2 / length(normal_sample_squared)
#****

#3d Use the values from part (a) to estimate the probability the square of a standard normal random variable is less
#then 2.
#number of simulated values <2/10000
x<- sum(sample3<2) 
est.prob = x/10000


#3e Find the exact probability a chi-square random variable with 1 degree of freedom is less than 2 using pchisq().
#Compare to part (d).
pchisq(2,df=1)
#3d got 83.64%, and 3e got 84.27%. They are close to equal

#4 An infinite series for the irrational number e is 
#1/0! + 1/1! + 1/2! + 1/3!+....

#4a Write R code to generate a vector whose nth element is 1/n! for n = 0; 1; 2; 3...; 100.
myfactorials<- 1/factorial (0:100)
head(myfactorials)

#4b Write R code to produce another vector of length 101 where the ith element in the vector is the cumulative sum
#of the elements 0,1,2,3..,i from the vector from part (a).
series<-(cumsum(myfactorials))

#4c Plot the elements in the vector in part (b) against the values 0,1,2,3,...,100 on the x-axis. Add a horizontal line at
#y = e.
plot(x= 0:100, y=series)
abline(h=exp(1),col="red")
#the horizontal line y=e, is the same as the line of series converge to


