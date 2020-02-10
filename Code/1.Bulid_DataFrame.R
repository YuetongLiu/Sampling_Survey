#1.Create
#1.1Vector
x <- c(2,3,7,1)
y <- c(3,8,6)
(z <- c(x,10,y))

(x <- rep(0, 5)) #creates a zero vector of length 5.
(x <- rep(1:3, 5)) #repeats vector “1:3” 5 times.
(x <- rep(1:3, each = 5)) #repeats each element of “1:3” 5 times.

1:5
7:3
seq(1, 5) 
seq(1, 7, 2) 

#1.2Matrices
(MyMatrix <- matrix(1:12, ncol=4))
matrix(c(2,3,7,1,5,6), nrow=3,ncol=2)
matrix(1:12, ncol=4) 
matrix(1:12,ncol=4,byrow=TRUE)
matrix(0,2,3)

x <- 1:3
y <- 3:1
cbind(x, y)
rbind(x, y)

diag(4)
diag(1:4)
diag(MyMatrix) # MyMatrix is defined in section 2.4.2 1 matrix

MyMatrix
MyMatrix[2, 2]
MyMatrix[2,] 
MyMatrix[,2]
MyMatrix[, -(1:2)]

#1.3 Data Frames
x <- 1:5
y <- letters[19:23]
MyFrame <- data.frame(x, y) 
MyFrame$x
MyFrame$y
names(MyFrame)
NewMatrix <- as.matrix(MyFrame) 

#2 Random
set.seed(344) # make your code reproducible
runif(5, 0, 1) # random number from uniform(0, 1)
rbinom(5, 10, 0.3) # random number from bin(10, 0.3)
rnorm(5) # random number from standard normal distribution
rnorm(5, mean = 1, sd = 2) # random number from N(1, 4) 

set.seed(2018)
myseq <- 1:10
sample(myseq, 5) 
sample.int(10, 5) 
set.seed(2018) # You get different result if you put set.seed() in different command line, but the whole code is still reproducible
sample.int(10, 5)
sample.int(10, 5)

