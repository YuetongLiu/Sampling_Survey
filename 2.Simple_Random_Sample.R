library(RCurl)
cedar <- read.csv(text = 
          getURL("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/cedar.csv"), 
          header=T) # refer to the help document for more details

#1.read
dim(cedar) 
names(cedar) 
str(cedar) 
head(cedar) 
summary(cedar)
# Frequency tables of the counts table(cedar$city) 
table(cedar$idu) 
table(cedar$age) 
table(cedar$rs)
# note: city and idu are discrete, age and rs are continuous # histograms
hist(cedar$age)
hist(cedar$rs)
hist(cedar$rs, xlab="resilient score", main="Histogram of Cedar Resilient Score")
# pie charts
hist(cedar$city)
pie(table(cedar$city))
pie(table(cedar$city), labels=c("PG","Van"))
pie(table(cedar$idu), labels=c("Non-IDU","IDU"))

#2.SRS
mean(cedar$age) # note this is the population mean of age 
mean(cedar$rs) # note this is the population mean of rs
IDS <- cedar$id 
N <- length(IDS) 
n <- 100
mysample <- sample.int(N, n,replace=F) 
mysample.IDS <- IDS[mysample]
cedar2 <- subset(cedar, id %in% mysample.IDS)# sample
dim(cedar2)
names(cedar2)
str(cedar2)
head(cedar2)
summary(cedar2)
hist(cedar2$rs, xlab="resilient score", main="Histogram of subsample of Cedar Resilient Score", las=1)

#3.calculate
# sample mean
(sample.mean <- mean(cedar2$rs))
# sample variance
(sample.variance <- sum((cedar2$rs - mean(cedar2$rs))^2)/(length(cedar2$rs) - 1))
# or use function var()
var(cedar2$rs)
# sample standard deviation 
sqrt(sample.variance)
# or use function sd() 
sd(cedar2$rs)
# calculate 95% CI for population mean
n <- 100
N <- nrow(cedar)
(se <- sqrt( (1-n/N)/n ) * sd(cedar2$rs))
(CI <- c(sample.mean - 1.96 * se, sample.mean + 1.96 * se))

#4.C.L.T
S <- 1000
my.means <- numeric(S) 
for(i in 1:S) {
  set.seed(i)
  this.sample <- sample.int(N, 100,replace=F) 
  this.IDS <- IDS[this.sample]
  cedar.tmp <- subset(cedar, id %in% this.IDS) 
  my.means[i] <- mean(cedar.tmp$rs) }
hist(my.means)
hist(my.means, prob=T, nclass=30) #expect to see a bell curve! lines(density(my.means))

