setwd("/Users/yuetongliu/Desktop")
#2. Fair Sampling
data <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/cars.csv", header = T)
sum(data$cars)  # true value we want to estimate
N <- length(unique(data$county))   # number of clusters
m <- tapply(data$cars,data$county,length)  # cluster size
# first stage
n.fair <- 20
set.seed(30)
county.sample  <- sample(unique(data$county), n.fair, replace=T)  

# second stage
t.hat <- rep(NA, n.fair)
for(i in 1: length(county.sample)) {
  places <- which(data$county == county.sample[i])
  r<- round(2/3 * length(places))
  sample.index <- sample(1:length(places), size=r, replace=F)
  t.hat[i] <- (length(places)) * mean(data$cars[places[sample.index]])
}
total <- t.hat / (1/N)
c(mean(total), sqrt(var(total)/n.fair))

#3. PPS Sampling
# 1st stage
n.pps <- 20
set.seed(30)
county.smp <- sample(unique(data$county), size = n.pps, prob = m/sum(m), replace = T)
county.smp

# 2nd stage
t.hat <- rep(NA, n.pps)
for (i in 1:n.pps) {
  places <- which(data$county == county.smp[i])
  r <- round(length(places) * 2/3)
  place.smp <- sample(1: length(places), size=r, replace=F)    ## size = r
  t.hat[i] <-  (length(places)) * mean(data$cars[places[place.smp]])
}
ests.sep <- t.hat/ (m[as.character(county.smp)] / sum(m))
c(mean(ests.sep), sqrt(var(ests.sep)/n.pps))

#4. Nonresponse 
data <- read.csv("https://github.com/YuetongLiu/Sampling_Survey/blob/master/Data/nonresponse.csv", sep = "", header = T)
sum(is.na(data$cars)) ## number of observations with missing car values
N <- length(unique(data$county)) # number of clusters
m <- tapply(data$cars,data$county,length)  # cluster size

# 1st stage
n.pps <- 20
set.seed(30)
(county.smp <- sample(unique(data$county), size = n.pps, prob = m/sum(m), replace = T))

# 2nd stage
t.hat <- rep(NA, n.pps)
for (i in 1:n.pps) {
  places <- which(data$county == county.smp[i])
  r <- round(length(places) * 2/3)
  place.smp <- sample(1: length(places), size=r, replace=F)    
  t.hat[i] <-  (length(places))  * mean(data$cars[places[place.smp]], na.rm=TRUE)
}
ests.sep <- t.hat/ (m[as.character(county.smp)] / sum(m))
c(mean(ests.sep), sqrt(var(ests.sep)/n.pps))

