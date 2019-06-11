#1. Data preprocessing and visualization
# Read the data set into R: 
agpop <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Sampling_Survey/master/Data/agpop.csv")
#select those states with more than 20 records: 
a <- tapply(agpop$farms92,agpop$state,length) 
b <- a[a > 20]; b <- names(b)
agpop <- agpop[agpop$state %in% b,]
#A trick to “re-level” a factor a R: 
agpop$state <- as.character(agpop$state) 
agpop$state <- as.factor(agpop$state)
# Select relevant columns
agpop <- agpop[, c("county", "state", "acres92", "farms92")]

boxplot(acres92 ~ state, data=agpop) 
plot(acres92 ~ farms92, data=agpop)

# Total acreages devoted in 1992
(t_acres92_pop <- sum(agpop$acres92))
# Average acreages per county in 1992
(ave_acres92_pop <- mean(agpop$acres92))
# Total number of counties in 1992
(num_county_pop <- length(agpop$county))
# Total number of states in 1992
(num_state_pop <- length(levels(agpop$state)))

#2. One stage cluster sampling
n <- 10
state_name <- levels(agpop$state)
set.seed(1)
state_sample <- sample(state_name, n, replace=F) 
agsample_1 <- agpop[agpop$state %in% state_sample,] 
agsample_1$state <- as.character(agsample_1$state) 
agsample_1$state <- as.factor(agsample_1$state)

#Total acreage for each state
(tol_acre_state <- tapply(agsample_1$acres92, agsample_1$state, sum))
#To estimate the total acreage, we need the estimate of average total acreage per state: 
(ave_acre_state <- mean(tol_acre_state))
#So the estimate of total acreage in 1992 is
(tol_acre_1992_hat <-num_state_pop*ave_acre_state)
#S.E of the above estimate
(se_tol_acre_1992_hat <- num_state_pop*sqrt((1 - n/num_state_pop)*var(tol_acre_state)/n))

