# R-assignment 1: Titanic

##### Problem Statement #####

# The "Titanic" was an ocean liner that sunk on its first attempt at crossing 
# the Atlantic Ocean in 1912.This assessment uses a sample of data from the 
# Titanic passenger manifest. Variables include:

# - Name of the passenger
# - Ticket class of the passenger (First, Second or Third)
# - Age of the passenger, in years
# - Sex of the passenger (male or female)
# - Did the passenger survive? (1="Yes", 0="No")


##### Analysis #####

# Complete the following data manipulation and analysis tasks in order using 
# data available from the following website:
# http://www.asayanalytics.com/titanic

# You are only expected to use the commands covered in R Tutorials 1 and 2! 
# Do not go looking for new functions to complete these tasks!


##### Task 1 (0.05 Points): #####
# Import the data into a data frame named: "titanic"
library(readr)
titanic<-read_csv("http://www.asayanalytics.com/titanic")


##### Task 2 (0.05 Points): #####
# Report the maximum, minimum and range for the age variable. 
# Set each of these to be a new variable object of the same name. 
# For example, the 'max' variable should be stored as an object that always 
# returns the max age.
max_age<-max(titanic$age, na.rm = T)
min_age<-min(titanic$age, na.rm = T)
range_age<-range(titanic$age, na.rm = T)

print(max_age)
print(min_age)
print(range_age)

##### Task 3 (0.05 Points): #####
# Age was originally unknown for the passenger Mr. Michael Connaghton.
# It has since been determined that Mr. Connaghton was 31 years old when the 
# Titanic sunk. Update the titanic data frame to reflect this new information.
# Use direct identifiers [] here with the row and column identifiers.
titanic[216,3] <- 31	


##### Task 4 (0.05 Points): #####
# Create a histogram from the age variable.
# Properly label the chart with a main title,
# X label, Y label and set the color to "cyan".  
hist(titanic$age,
     col = "cyan",
     main = "Histogram for Titanic Passenger Age",
     xlab = "Age",
     ylab = "Number of Passengers")



##### Task 5 (.1 Points): #####
# Create a new column vector named "child" that indicates whether the passenger 
# was a child using TRUE or FALSE. 
# All passengers under 18 are considered children.
titanic$child <- titanic$age < 18



##### Task 6 (.2 Points): #####
# Find the percent of Titanic passengers that survived
# Store this calculation as an object named "percent.survived"
# This object should return the percent in percent form--not decimal form.
percent.survived <- mean(titanic$survived) *100
print(percent.survived)


##### Task 7 (.3 Points): #####
# Use simple math operations and the other objects you have already created to 
# calculate the average age of survivors. 
# Store this calculation as an object named "avg_age"

# HINT: 
# This is a tricky question! Think conceptually what this calculation requires.
# You need to sum the total age of all passengers who survived and divide it 
# by the the total number of survivors. 
# To calculate the numerator, consider using the "survived" column where 
# everyone who did not survive is a zero.

age_survivors <- (sum(titanic$age[titanic$survived == 1]))
survivors <- sum(titanic$survived == 1)
#print(survivors)
avg_age <- age_survivors/survivors
print(avg_age)

##### Task 8 (.1 Points): #####
# Make a boxplot showing age by survival. Properly label the chart with a 
# main title, x axis and y axis label. 
# Does there appear to be any difference in age between those who survived and 
# those who did not? Write your responses as comments below your code!

boxplot(titanic$age~titanic$survived,   
        #col = "blue",
        main = "Titanic Age by Survival",
        xlab = "Survival 
        0 = did not survive, 1 = survived",
        ylab = "Age")

# The median age of those who survived and those who did not survive, seems to 
# be the same. About 28-29 years old. However, for those who did not survive, 
# there are some higher aged outliers. The survived boxplot has a wider IQR than
# the did not survive boxplot.


##### Task 9 (.1 Points): #####
# Make a boxplot showing age by class. 
# Properly label the chart with a main title, x axis and y axis label. 
# Does there appear to be a relationship between the age of an individual and 
# the passenger class he or she purchased? Why do you think this is? 
# Write your responses as comments below your code!

boxplot(titanic$age~titanic$class,   
        #col = "blue",
        main = "Titanic Age by Class",
        xlab = "Class",
        ylab = "Age")

# Yes, the average and median age is higher, the higher the class you are in. 
# I think this is because the longer you work, the more you get promoted, and the
# more money you make. Therefore, older people can afford to buy 1st class. There
# are outliers in the 2nd and 3rd class boxplots towards the older ages. This shows
# that younger people bought lower class tickets, while older people bought 1st class
# tickets. 