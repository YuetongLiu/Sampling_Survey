#2. One stage cluster sampling 
trees <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/trees.csv",sep = "", header = TRUE)
N  <- length(unique(trees$region))
n <- 5
set.seed(0)
region.sample  <- sample(unique(trees$region), 5)
onestage.sample <- trees[trees$region %in% region.sample,]
onestage.sample$region <- as.character(onestage.sample$region)
onestage.sample$region <- as.factor(onestage.sample$region)
#### calculate t ####
t <- tapply(onestage.sample$number.trees,onestage.sample$region,sum)
#### (A). average surviving trees per region ####
(average.a <- mean(t))
(sd.a <-sqrt((1 - (n/N))*var(t)/n))
#### (B). average surviving trees per stand ####
m <- tapply(onestage.sample$number.trees,onestage.sample$region,length)
(average.b <- mean(t)/mean(m))
res <- t - (mean(t)/mean(m))*m
se2 <- sum(res^2)/(n-1)
(se.b <- sqrt((1-n/N)*se2/n)/mean(m))
####  (C). total trees surviving ####
(total.c <- mean(t)*N)
(sd.c <- N*sd.a)

#3. Two Stage Cluster Sampling
# 1st stage: SRS on the clusters (regions)
n <- 5
set.seed(1)
region_sample <- sample(unique(trees$region), n, replace = F)
# 2nd stage: SRS on stands for each sampled region from the 1st stage
twostage_sample <- NULL
r <- 400
ind <- 1
for (i in region_sample){
  row_i <- which(trees$region == i)
  set.seed(ind)
  row_i_sample <- sample(row_i, r, replace=F)
  twostage_sample <- rbind(twostage_sample, trees[row_i_sample,])
  ind <- ind + 1
}
twostage_sample$region <- as.character(twostage_sample$region)
twostage_sample$region <- as.factor(twostage_sample$region)
# calculate the number of stands of the sampled regions first:
m <- NULL
for (i in region_sample){
  m <- c(m, nrow(trees[trees$region == i,]))}
# sample mean for each region
(avg_num <- tapply(twostage_sample$number.trees, twostage_sample$region, mean))
# estimate cluster totals 
(t_hat <- m * avg_num)
# (A). average survived trees per region
(avg_region <- mean(t_hat))
# (B). average survived trees per stand
(avg_stand <- mean(t_hat) / mean(m))
# (C). total trees survived
(total <- N * avg_region)

