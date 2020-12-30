#  PSTAT126 Homework 1
#  Problem 1A  
#  The predictor is height and the response is weight

# Problem 1B
x <-Htwt$ht  # [1] 169.6 166.8 157.1 181.1 158.4 165.6 166.7 156.5 168.1 165.3
x
y <-Htwt$wt  # [1] 71.2 58.2 56.0 64.5 53.0 52.4 56.8 49.2 55.6 77.8
y

plot(x,y, xlab= "Height", ylab= "Weight", main = "Test")

# Continuation of PartB. The simple linear regression model does make sense. Because it shows how scattered the points are. 

# Problem 1C
mean(Htwt$ht)  # [1] 165.52
mean(Htwt$wt)  # [1] 59.47
n<- 10
n
sxx <- sum(x^2)- sum(x)^2/n  # [1] 472.076
sxx
syy <- sum(y^2)-sum(y)^2/n  # [1] 731.961
syy
sxy <-sum(x*y)-(sum(x)*sum(y))/n  # [1] 274.786
sxy
Slope <- sxy/sxx  # [1] 0.58208
Slope

B0 <- mean(y)-Slope*mean(x)  # [1] -36.87588
B0

abline(B0,Slope)

# Problem 2 This problem uses the UBSprices data set in the alr4 package.

# Part A

X <-UBSprices$bigmac2003
X
Y <-UBSprices$bigmac2009
Y

plot(X,Y, xlab= "Price of a Big Mac Hamburger in 2003", ylab= "Price of a Big Mac hamburger in 2009", main = "the price of a Big Mac hamburger in 2009 and 2003")
plot

# Continuation of Part A,The reason the simple linear regression line is not fitting is because there is too many variables in a cluttered section wether as being spread out linearly.

# Part B
X1 <- log(UBSprices$bigmac2003)
X1
Y1 <- log(UBSprices$bigmac2009)
Y1
plot(X1,Y1, xlab = "logarithm of Big Mac Hamburger in 2003", ylab= "lograthim of a Big Mac hamburger in 2009")
plot

# Continuation of Part B, This plot is more sensibly summarized because the plot is more scattered out showing there is a betterlinear relationship.

# Part C

mean(X1)  
mean(Y1)  
n1<-54 
n1
sxx1 <- sum(X1^2)- sum(X1)^2/n1
sxx1
syy1 <- sum(Y1^2)-sum(Y1)^2/n1
syy1
sxy1 <-sum(X1*Y1)-(sum(X1)*sum(Y1))/n1
sxy1
Slope1 <- sxy1/sxx1
Slope1

b0 <- mean(Y1)-Slope1*mean(X1)
b0

abline(b0,Slope1, col= "blue")

# Problem 3

# Part A
library(faraway)
a <- prostate$lpsa
a
b <- prostate$lcavol
b

plot(a,b, xlab= "lpsa", ylab= "lcavol", main = "lpsa on lcavol")
abline(lm(a~b))
abline(lm(b~a))

# Part B
# Both of the regression lines cross at the mean of predictor variable (xbar) and the mean of the response variable (ybar) because it will always intersect at the predictor of the regressions.

# Problem 4
# Part A

c <- Heights$mheight
c
d <- Heights$dheight
d

plot(c,d, xlab= "mheight", ylab= "dheight", main = "dheight on mheight")

abline(lm(d~c))
# Part B

pred=predict(lm(d~c),data.frame(mheight=c(70)),interval = "predict",)
print(paste("the 95% confidence interval is (",pred[2],",",pred[3],")"))

# Part C
mean(c)  
mean(d)  
n2<-1375
n2
sxx2 <- sum(c^2)- sum(c)^2/n2
sxx2
syy2 <- sum(d^2)-sum(d)^2/n2
syy2
sxy2 <-sum(c*d)-(sum(c)*sum(d))/n2
sxy2
Slope2 <- sxy2/sxx2
Slope2

sxy2 / sqrt(sxx2*syy2)

# Problem 5
