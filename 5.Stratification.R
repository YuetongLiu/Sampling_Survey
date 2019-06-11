agpop <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/agpop.csv", header = TRUE)

#1. Generate a stratified sample
attach(agpop)
N.h <- tapply(acres92, region, length) #population size for different regions
(regions <- names(N.h)) # name of the regions 

length(acres92[region == "NC"]) # the result is 1054 
N <- sum(N.h) # population size is 3078 
detach(agpop)

n <- 400 # total sample size
n.h <- c(100,100,100,100) # specify the sample size for each region
set.seed(0)
STR.sample <- NULL
for (i in 1: length(regions)) {
  row.indices <- which(agpop$region == regions[i]) 
  sample.indices <- sample(row.indices, n.h[i], replace = F) 
  STR.sample <- rbind(STR.sample, agpop[sample.indices, ])
}

#2. STR estimation
ybar.h <- tapply(STR.sample$acres92, STR.sample$region, mean) 
var.h <- tapply(STR.sample$acres92, STR.sample$region, var) 
se.h <- sqrt((1 - n.h / N.h) * var.h / n.h)
rbind(ybar.h, se.h)
ybar.str <- sum(N.h / N * ybar.h)
se.str <- sqrt(sum((N.h / N)^2 * se.h^2)) 
c(ybar.str, se.str)

#3. Compare SRS and STR
(true.value <- mean(agpop$acres92))
n <- 600

set.seed(10)
SRS.indices <- sample.int(N, n, replace = F) 
SRS.sample <- agpop[SRS.indices , ] 
ybar.srs <- mean(SRS.sample$acres92)
se.srs <- sqrt((1 - n / N) * var(SRS.sample$acres92) / n) 
srs <- c(ybar.srs, se.srs)

neq <- n/4
n.h.eq <- c(neq,neq,neq,neq) 
set.seed(10)
STR.sample.eq <- NULL
for (i in 1: length(regions))
{
  row.indices <- which(agpop$region == regions[i])
  sample.indices <- sample(row.indices, n.h.eq[i], replace = F) 
  STR.sample.eq <- rbind(STR.sample.eq, agpop[sample.indices, ])
}
ybar.h.eq <- tapply(STR.sample.eq$acres92, STR.sample.eq$region, mean) 
var.h.eq <- tapply(STR.sample.eq$acres92, STR.sample.eq$region, var) 
se.h.eq <- sqrt((1 - n.h.eq / N.h) * var.h.eq / n.h.eq)
rbind(ybar.h.eq, se.h.eq)

ybar.str.eq <- sum(N.h / N * ybar.h.eq) 
se.str.eq <- sqrt(sum((N.h / N)^2 * se.h.eq^2)) 
str.eq <- c(ybar.str.eq, se.str.eq)

n.h.prop <- round( (N.h/N) * n) 
STR.sample.prop <- NULL 
set.seed(10)
for (i in 1: length(regions))
{
  row.indices <- which(agpop$region == regions[i])
  sample.indices <- sample(row.indices, n.h.prop[i], replace = F) 
  STR.sample.prop <- rbind(STR.sample.prop, agpop[sample.indices, ])
}
ybar.h.prop <- tapply(STR.sample.prop$acres92, STR.sample.prop$region, mean) 
var.h.prop <- tapply(STR.sample.prop$acres92, STR.sample.prop$region, var) 
se.h.prop <- sqrt((1 - n.h.prop / N.h) * var.h.prop / n.h.prop)
rbind(ybar.h.prop, se.h.prop)

ybar.str.prop <- sum(N.h / N * ybar.h.prop) 
se.str.prop <- sqrt(sum((N.h / N)^2 * se.h.prop^2)) 
str.prop <- c(ybar.str.prop, se.str.prop)

rbind(srs, str.eq, str.prop)

