
##############################################################################################################
# 
# (1) Setup
#
##############################################################################################################

# Set Working Directory 
setwd("/Users/Harry Patria/Documents/MS981-EITSIE")  # Change this!

# Install and Load Package(s)
install.packages("tidyverse")
library(tidyverse)

# Get Data (in this case via another package)
install.packages("ISLR2")        # Package from James et al. (2021, An Introduction to Statistical Learning 
library(ISLR2)                   #      with applications in R, 2nd ed.)

# Create a data frame object from dataset "Bikeshare"
bikeshare.df <- Bikeshare

##############################################################################################################
# 
# (2) Calculating Descriptive Statistics
#
##############################################################################################################

#=========================================================================================================
# 2.1 Mean
#=========================================================================================================

# The Mean Function ---------------------------------
# The mean is calculated by taking the sum of the values and dividing with the number of values in a data series.
# The function mean() is used to calculate this in R.

# Create a vector. 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find Mean.
result.mean <- mean(x)
result.mean

# Applying Trim Option ----------------------------
# When trim parameter is supplied, the values in the vector get sorted and then the required numbers of 
#    observations are dropped from calculating the mean.
# When trim = 0.3, 30% of the values from each end will be dropped from the calculations to find mean.

# Create a vector.
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# In this case the sorted vector is (−21, −5, 2, 3, 4.2, 7, 8, 12, 18, 54) and the values removed 
#    from the vector for calculating mean are (−21,−5,2) from left and (12,18,54) from right.

# Find Mean.
result.mean <-  mean(x,trim = 0.3)
result.mean

# Applying NA Option ----------------------------------
# If there are missing values, then the mean function returns NA.
# To drop the missing values from the calculation use na.rm = TRUE, which means remove the NA values.

# Create a vector. 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5,NA)

# If we use the normal mean function, this will result in 'NA'.
result.mean <-  mean(x)
result.mean

# Find mean dropping NA values.
result.mean <-  mean(x,na.rm = TRUE)
result.mean

# You can also combine Trim and the NA option.


#=========================================================================================================
# 2.2 Median
#=========================================================================================================

# The Median Function ------------------------------
# The middle most value in a data series is called the median. 
# The median() function is used in R to calculate this value.

# Create the vector.
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find the median
result.median <- median(x)
result.median

# Applying NA Option ----------------------------------
# If there are missing values, then the mean function returns NA.
# na.rm is used to remove the missing values from the input vector.

# Create a vector. 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5,NA)

# Find mean dropping NA values.
result.median <-  median(x,na.rm = TRUE)
result.median


#=========================================================================================================
# 2.3 Mode
#=========================================================================================================

# There is NO Mode Function --------------------------
# The mode is the value that has highest number of occurrences in a set of data. 
# Unike mean and median, mode can have both numeric and character data.
# R does not have a standard in-built function to calculate mode. 

# Create a Mode Function ------------------------------
# This function takes the vector as input and gives the mode value as output.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Using the Mode Function for Numeric Data -------------

# Create the vector with numbers.
v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)

# Calculate the mode using the user function.
result.mode <- getmode(v)
result.mode

# Using the Mode Function for Character Data -------------

# Create the vector with characters.
charv <- c("o","it","the","it","it")

# Calculate the mode using the user function.
result.mode <- getmode(charv)
result.mode

#=========================================================================================================
# 2.4 Calculating the Range
#=========================================================================================================

# Calculating the Range of a Vector -----------------------------
# The range is the difference between the largest and the smallest value in a dataset.

# Create the vector
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Calculate Range
result.range <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
result.range

# The Range Function --------------------------------------------
# R has a function called Range(), which displays the smallest and largest value in a daatset.

# Show Smallest and Largest Value
range(x, na.rm=TRUE)


#=========================================================================================================
# 2.5 Standard Deviation
#=========================================================================================================

# The standard deviation of an observation variable is the square root of its variance.
# R has a function called SD() that calculates this for us.

# Create the vector
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Calculate SD
result.sd <- sd(x, na.rm=TRUE)
result.sd


#=========================================================================================================
# 2.6 Quartiles
#=========================================================================================================

# Quartiles are special percentiles that occur after a certain percent of data has been covered.
# R has a function quantiles() that can help us here.

# Create the vector
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Apply the quantile function
results.quartile <- quantile(x)
results.quartile

# We can also customise this function
results.quartile <- quantile(x, prob=c(.25,.5,.75))
results.quartile

#=========================================================================================================
# 2.7 Calculating Descriptive Statistics from Data Frames
#=========================================================================================================

# Using a Single Column -----------------------------------

bikeshare.df

df.mean <- mean(bikeshare.df$bikers)
df.mean

df.median <- median(bikeshare.df$bikers)
df.median

df.mode <- getmode(bikeshare.df$weathersit)
df.mode

print(paste0("The mean is: ",df.mean," and the median is: ",df.median))

# Using Multiple Columns -----------------------------------

df.mean2 <- sapply(bikeshare.df[c('casual', 'registered')], function(x) mean(x, trim=0.05))
df.mean2

##############################################################################################################
# 
# (3) Visualising Averages and Variations
#
##############################################################################################################

#=========================================================================================================
# 3.1 Consider this Problem
#=========================================================================================================

ggplot(Bikeshare) +
  geom_point(mapping=aes(hr, bikers)) +   # Plots a scatterplot with casual riders on the x axis and 
  theme_bw()                                      #     registered bikers on the y axis. There are a range 
                                                  #     of themes called theme_* that you can choose from.

#=========================================================================================================
# 3.2 Create a Box Plot
#=========================================================================================================
install.packages("ggproto")
library(ggplot)
plot_1 <- ggplot(Bikeshare) +
  geom_boxplot(mapping=aes(hr, bikers)) +   # Plots a scatterplot with casual riders on the x axis and 
  theme_bw()                          
plot_1

#=========================================================================================================
# 3.3 Saving Plots
#=========================================================================================================

ggsave("plot.png", width = 10, height = 7)

# Saves last plot as 10’ x 7’ file named "plot.png" in working directory. 
# Matches file type to file extension (i.e., "plot.pdf" produces a pdf with the same content).


##############################################################################################################
# 
# (4) Exercise
#
##############################################################################################################

# Make sure that you correctly changed your working directory!

# Explore the following datasets and visualise information from at least one of them:
#   College
#   Wage
#   OJ
