set.seed(180) # set a seed for reproducible results

#1. Generate the population
# generate 1000 random numbers from a Uniform(10, 500) distribution as the Y value of the population
pop <- runif(1000, min = 10, max = 500)
mean(pop)
var(pop)

#2. Simulation 1: width of 95% CI for population mean vs. sample size
# repeat the SRS 50 times, each time with a different sample size; # record the width of 95% CI in each run and plot the results
S <- 50
wci <-rep(NA,S) #save the width of estimated CI
N <- length(pop) #population size
sample.size <- seq(from = 100, length.out = S)
for (i in 1:S) {
  set.seed(0) # we fixed the seed since different sample size is taken 
  sim1_sample <- sample(pop, size = sample.size[i],replace=F) 
  sim1_n <- sample.size[i]
  se <- sqrt( (1-sim1_n/N)/sim1_n ) * sd(sim1_sample)
  wci[i] <- 2 * 1.96 * se }
plot(sample.size, wci, xlab="Sample size", ylab="Width of 95% CI") 
title("Width of 95% CI vs. Sample size")

#3. Simulation 2: compare formula for estimating CI
sim2_n <- 300 # sample size 
pop_mean <- mean(pop)
N <- length(pop)
cov_rate1 <- NULL 
cov_rate2 <- NULL
for (i in 1:1000) {
  set.seed(i)
  sim2_sample <- sample(pop, size = sim2_n, replace = F)
  CI1 <- mean(sim2_sample) + 1.96 * sqrt((1 - sim2_n/N)/sim2_n)*sd(sim2_sample) * c(-1, 1) 
  CI2 <- mean(sim2_sample) + 1.96 * sqrt(var(sim2_sample)/sim2_n) * c(-1, 1)
  cov_rate1 <- c(cov_rate1, (CI1[1] <= pop_mean & pop_mean <= CI1[2]))
  cov_rate2 <- c(cov_rate2, (CI2[1] <= pop_mean & pop_mean <= CI2[2]))
}
(cov_rate1 <- mean(cov_rate1))
(cov_rate2 <- mean(cov_rate2))

#4. Ratio Estimation:
agpop <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/agpop.csv", header = T)
xbar.pop <- mean(agpop$farms87) # population mean
N <- nrow(agpop) # population size
n <- 200 # sample size
set.seed(1)
SRS.index <- sample.int(N, n, replace = FALSE)
agsample <- agpop[SRS.index, ] # get the corresponding rows from the population
ybar.sample <- mean(agsample$farms92) 
xbar.sample <- mean(agsample$farms87)
plot(agsample$farms87, agsample$farms92)
cor(agsample$farms87, agsample$farms92) # check sample correlation 
B.hat <- ybar.sample / xbar.sample
(ybar.ratio <- B.hat * xbar.pop) # ratio estimation






