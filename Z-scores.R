# z-scores are used to calculate the number of standard deviations away (in either direction) the data point is 
# z-scores are graphically represented using a normal distribution curve
# z-scores are used to convert a normally distributed variable, "x" into a standardized normal variable, "z"
# To compute a z-score for a population the following is required: 
# population statistic,
# population mean, 
# population standard deviation
# Formula: z = (x - mu/sigma)
# To compute a z-score for a sample the following is required: 
# sample statistic,
# sample mean, 
# sample standard deviation 
# and outputs the z-score.
# Formula: z = (xi - x-bar/s)
# Step 1: Create function z_score() to compute the z-score
# This function takes input parameters 
# xi = sample statistic,
# xbar = sample mean, 
# s = sample standard deviation
# and outputs the Z-score.
z_score <- function(xi, xbar, s) {
  z = (xi - xbar)/s
  
  return(z)
}
# e.g. 1 
# Given that xi = 170, xbar = 56, and s = 32, calculate the z-score and assign it to the vector eg1
eg1 <- z_score(170, 56, 32)
eg1
# e.g. 2 Data set: mtcars
# Step 1: Load the data set
data(mtcars)
head(mtcars)
# Step 2: Create a vector consisting of the desired column using dataset$columnname 
# and assign it to the vector mtcarsmpg
mtcarsmpg <- mtcars$mpg
# Step 3: use functions mean and sd within the function z_score to calculate the
# z-score for each x-value in the mpg column and assign it to mtcarsmpgz_scores
mtcarsmpgz_scores <- z_score(mtcarsmpg, mean(mtcarsmpg), sd(mtcarsmpg))
head(mtcarsmpgz_scores)
# Step 4: Install the dplyr package 
install.packages("dplyr")
library("dplyr")
# Step 5: Use mutate to add the z-scores to the orignal data set 
mtcarsz <- mtcars %>%
  mutate(mpgz_scores = mtcarsmpgz_scores)
head(mtcarsz)
#Step 6: Select and filter to find the number of rows with outliers 
  mtcarsz %>%
    select(mpg, mpgz_scores) %>%
    arrange(desc(mpg, mpgz_scores)) %>%
    filter(mpgz_scores > 1.5)

  mtcarsz %>%
    select(mpg, mpgz_scores) %>%
    arrange(desc(mpg, mpgz_scores)) %>%
    filter(mpgz_scores < -1.5)
#Step 7: Plot the z-score graph 
plot(mtcarsmpgz_scores, type = "o", col = "green")


  
  
  
  
    
    
    
    

