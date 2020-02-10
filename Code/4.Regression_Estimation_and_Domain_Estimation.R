#1. Regression Estimation
agpop <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/agpop.csv", header = T)
xbar.pop <- mean(agpop$farms87)
plot(agpop$farms87, agpop$farms92)
cor(agpop$farms87, agpop$farms92) # population correlation
# Take a sample with size 200 N <- nrow(agpop)
n <- 200 # Sample size set.seed(1214)
SRS.index <- sample.int(N, n, replace = FALSE) agsample <- agpop[SRS.index, ]
attach(agsample)
plot(farms87, farms92)
cor(farms87, farms92) # sample correlation
model <- lm(farms92~farms87) # fit a linear model coef(model)
(ybar.reg <- coef(model)[1] + coef(model)[2] * xbar.pop )


#2. Domain Estimation
# Use the same sample "agsample" (we haven't detached it yet)
# How many counties we sampled are in the West? 
region
sum(region == "W")
# Create a new vector. If a region is "W", set the value to "acres92", otherwise set the value to # zero.
u <- ifelse(region == "W", acres92, 0)
# Estimate total number of acres devoted to farms in the West in 1992 
(totalW <- N * mean(u))
(se.totalW <- N * sqrt(1 - n/N) * sd(u) / sqrt(n))
# detach the data frame “agsample” 
detach(agsample)

#3. Compare regression estimation with vanilla estimation and ratio estimation
ybar.pop = mean(faithful$eruptions)
xbar.pop = mean(faithful$waiting) # population mean for waiting time, assume to be known
N <- nrow(faithful)
n <- 50 # Sample size
set.seed(0)
SRS.index <- sample.int(N, n, replace = FALSE)
faithful.sample <-faithful[SRS.index, ] # construct a sample of size 50
attach(faithful.sample) 
vanilla.estimator <- mean(eruptions)
cor(eruptions, waiting)
ratio.estimator <- (mean(eruptions) /mean(waiting)) *xbar.pop
model <- lm(eruptions~waiting) # fit a linear model
coef(model)
regression.estimator <- as.numeric(coef(model)[1] + coef(model)[2] * xbar.pop)
# estimated values using three estimators print(c(ybar.pop,vanilla.estimator, ratio.estimator, regression.estimator))
se.function = function(sample.value, estimated.value) {
  res = sample.value - estimated.value # residual 
  temp = sum(res^2)/(n-1)
  se = sqrt((1-n/N) *(temp/n))
  return (se)
}
vanilla.se <- se.function(eruptions, vanilla.estimator)
ratio.se <- se.function(eruptions, (mean(eruptions) /mean(waiting)) *waiting)
regression.se <- se.function(eruptions, as.numeric(coef(model)[1] + coef(model)[2] *waiting))
print(c(vanilla.se,ratio.se,regression.se)) 
detach (faithful.sample)
