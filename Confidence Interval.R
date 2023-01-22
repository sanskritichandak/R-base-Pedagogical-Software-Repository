#Confidence Intervals 
#Confidence Interval estimates a range of numbers constructed around the point estimate
#The point estimate is the value of a single sample statistic, usually the sample mean 
#The confidence interval is constructed such that the probability that the interval includes the population mean is known 
#To compute the margin of error for a population the following is required 
#critical z value 
#population standard deviation 
#sample size 
#Formula: E = (Zc*sigma)/(sqrt(n))
#Create function margin_of_error() to compute the margin of error
#This function takes the input parameters 
#Zc = critical z value 
#sigma = population standard deviation 
#n = sample size 
#and outputs the margin of error 
margin_of_error <- function(Zc, sigma, n) {
  E = (Zc*sigma)/(sqrt(n))

return(E)
  
}
#e.g. 1 
#Given that Zc = 2.58, sigma = 7.90, and n = 43 calculate the margin of error and assign it to the vector eg1
eg1 <- margin_of_error(2.58, 7.90, 43)
eg1
#Confidence Interval Formula: x-bar - E =< mu =< x-bar + E
#x-bar - E represents the lower limit
#x-bar + E represents the upper limit
#The data must be normally distributed and n must be greater than or equal to 30 
#e.g.2
#Given that E = eg1 and x-bar = 37.5, calculate the lower limit and upper limit of the confidence interval
x_bar <- 37.5 
lower_limit <- x_bar - eg1 
lower_limit
upper_limit <- x_bar + eg1
upper_limit
#To compute the margin of error for a sample the following is required 
#critical t value 
#sample standard deviation 
#sample size 
#Formula: E = (tc*s)/(sqrt(n))
#Create function margin_of_error_sample() to compute the margin of error
#This function takes the input parameters 
#tc = critical t value 
#s = sample standard deviation 
#n = sample size 
#and outputs the margin of error for the sample 
margin_of_error_sample <- function(tc, s, n) {
  E_s = (tc*s)/(sqrt(n))
  
  return(E_s)
  
}
#e.g. 3 
#Given that tc = 1.729, s = 28.8843, and n = 20 calculate the margin of error and assign it to the vector eg3
eg3 <- margin_of_error_sample(1.729, 28.8843, 20)
eg3
#Confidence Interval Formula: x-bar - E =< mu =< x-bar + E
#x-bar - E represents the lower limit
#x-bar + E represents the upper limit
#The data must be normally distributed and n must be greater than or equal to 30 
#e.g.4
#Given that E = eg3 and x-bar = 81.9, calculate the lower limit and upper limit of the confidence interval
x_bar <- 81.9
lower_limit <- x_bar - eg3 
lower_limit
upper_limit <- x_bar + eg3
upper_limit
#e.g. 3 Data set: Mtcars
# Step 1: Load the data set
data(mtcars)
head(mtcars)
# Step 2: Create a vector consisting of the desired column using dataset$columnname 
# and assign it to the vector mtcarsmpg
mtcarsmpg <- mtcars$mpg
#Step 3: Use functions sd and length within function margin_of_error_sample to find the margin of error for mtcars$mpg 
#and assign it to the vector mtcars_MoE
#assume its a two tail test with a confidence level of 90% and d.f. = 31
#tc = 1.697 
mtcars_MoE <- margin_of_error_sample(1.697, sd(mtcars$mpg), length(mtcars$mpg))
mtcars_MoE
#Step 4: Given that E = mtcars_MoE, calculate the lower limit and upper limit of the confidence interval 
x_bar <- mean(mtcars$mpg)
lower_limit <- x_bar - mtcars_MoE
lower_limit
upper_limit <- x_bar + mtcars_MoE
upper_limit
#In conclusion, there is a 90% chance that the true mean of mtcars$mpg lies between 18.2826 and 21.89865
