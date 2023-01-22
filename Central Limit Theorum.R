#Sampling Distribution 
#Sample mean is unbiased because the mean of all possible sample means of a given sample size, n, is equal to the population mean, mu. 
#Standard error of the mean is the value of the standard deviation of all possible sample means 
#To compute the standard error of the mean for a population the following is required: 
#population standard deviation 
#population size 
#Formula = sigma x-bar = sigma/(sqrt(n))
#Step 1: Create a function sigma_x_bar to compute the standard error of the mean 
#This function takes input parameters 
#sigma = population standard deviation 
#n = sample size 
#and outputs the standard error of the mean 
sigma_x_bar <- function(sigma, n) {
  sigma_xbar = (sigma/sqrt(n))
  
  return(sigma_xbar)
}
#e.g. 1
#Given that sigma = 19 and n = 49, calculate the standard error of the mean and assign it to the vector eg1 
eg1 <- sigma_x_bar(19,49)
eg1
#e.g. 2 Data set: mtcars 
#Step 1: Load the data set 
data(mtcars)
head(mtcars)
# Step 2: Create a vector consisting of the desired column using dataset$columnname 
# and assign it to the vector mtcarsmpg
mtcarsmpg <- mtcars$mpg
#Step 3: use functions sd and count within the function sigma_x_bar to calculate the standard error of the mean for the data set 
#assign it to mtcars_sigmaxbar
mtcars_sigmaxbar <- sigma_x_bar(sd(mtcarsmpg), length(mtcarsmpg))
mtcars_sigmaxbar
#When sampling from normally distributed populations, the sampling distribution of the mean is normally distributed 
#This means mu x-bar = mu 
#The standard error of the mean decreases as sample size increases
#To compute the z-score for the sampling distribution of the mean the following is required:
#sample mean 
#population mean 
#population standard deviation
#sample size 
#Formula: z = ((x-bar)- mu)/(sigma/(sqrt(n)))
#The Central Limit Theorem states that as the sample size gets larger, the sampling destitution of the mean is approximately normally distributed. 
#This is regardless of the shape of the distribution of the individual values in the population. 
#The minimum sample size needed to apply the Central Limit Theorem is 30 
#To check if the data set mtcars is normally distributed plot a histogram 
#Step 4: Install the ggplot2 package 
install.packages("ggplot2")
library("ggplot2")
#Step 5: Plot a histogram for mtcarsmpg
ggplot(mtcars, aes(x = mtcars$mpg))+
  geom_histogram(binwidth = 5)
#As seen from the histogram the data is positively skewed
#Central Limit Theorem can be applied because the sample size is 32
#Let's now breakdown the process of Central Limit Theorem 
#Create samples of 4 from the data
sample_1 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_1
sample_2 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_2 
sample_3 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_3
sample_4 <- sample(mtcars$mpg, 4, replace=FALSE)
sample_4
sample_5 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_5
sample_6 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_6
sample_7 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_7
sample_8 <- sample(mtcars$mpg, 4, replace = FALSE)
sample_8
#Calculate the mean of each sample and assign it to the vector mtcarsmpg_samplemeans
mtcarsmpg_samplemeans <- data.frame("Sample Means" = c(mean(sample_1), 
                           mean(sample_2),
                           mean(sample_3),
                           mean(sample_4),
                           mean(sample_5),
                           mean(sample_6),
                           mean(sample_7),
                           mean(sample_8)))
mtcarsmpg_samplemeans
#Plot a histogram of mtcarsmpg_samplemeans
ggplot(mtcarsmpg_samplemeans, aes(x = mtcarsmpg_samplemeans))+
  geom_histogram(binwidth = 1)
