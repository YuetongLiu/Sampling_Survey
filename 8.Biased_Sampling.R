#2. Fair Sampling VS PPS Sampling (One Stage) 
trees <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/tree1.csv",sep = "", header = TRUE)
N.cluster  <- length(unique(trees$region))   # number of clusters in the population
N.pop <- nrow(trees)
m <- tapply(trees$number.trees,trees$region,length)  # cluster size
tot.cluster <- tapply(trees$number.trees,trees$region,sum)  # number of trees in each cluster

#Fair Sampling
n.cluster  <- 10
(1-(1-1/N.cluster)^n.cluster)*N.pop  # expected number of SSUs visited
set.seed(0)
region.sample  <- sample(unique(trees$region), n.cluster, replace=T) 
# replace = T denotes sample with replacement
onestage.sample <- trees[trees$region %in% region.sample,]
nrow(onestage.sample)    # actual  number of SSUs visited
onestage.sample$region <- droplevels(onestage.sample$region)  #relevel 
t <- tot.cluster[region.sample]
estimated.total <- mean(t)*N.cluster
estimated.se  <- sqrt(var(t)/n.cluster)*N.cluster

#  PPS Sampling (One Stage) 
n.pps <- 10
sum((1- (1-m/N.pop)^n.pps ) * m )  # expected number of SSUs visited
set.seed(0)
pps.sample <- sample(1:N.cluster, size=n.pps, prob=m/sum(m), replace=T)
sum(m[unique(pps.sample)])  # actual number SSUs visited

estimate.sep <- tot.cluster[pps.sample]/(m[pps.sample]/N.pop)
estimated.total.pps <- mean(estimate.sep)
estimated.se.pps  <-sqrt(var(estimate.sep)/n.pps)

#3. PPS Sampling (Two Stage)
# 1st stage (PPS)
n.pps <- 10
set.seed(1)
region_sample <- sample(unique(trees$region), size = n.pps, prob = m/sum(m), replace = T)
region_sample <- droplevels(region_sample)

# 2nd stage (SRS)
regions <- levels(trees$region)
t.hat <- rep(NA, length(regions))
q <- list() 
for (i in 1:N.cluster) {
  
  q[[i]] <- rep(0,m[i]) 
}
set.seed(5)
for (i in region_sample)  {
  
  row_i <- which(trees$region == i) 
  num <- which(regions == i)  
  r <- round(m[num] * 2/3)   
  
  row_i_sample <- sample(1: length(row_i), r, replace=F)
  smp <- trees[row_i[row_i_sample], ]      
  t.hat[num] <- (m[num]/r) * sum(trees$number.trees[row_i[row_i_sample]])
  
  # track visits to SSUs
  ssu <- row_i_sample
  q[[num]][ssu] <- q[[num]][ssu] + 1
}
# number of SSUs visited
sum(unlist(q)>0)
ests.sep <- NULL
for(i in region_sample) {
  
  num <- which(regions == i)
  ests.sep <- c(ests.sep, t.hat[num]/(m[num]/sum(m)))
}
c(mean(ests.sep), sqrt(var(ests.sep)/n.pps))

