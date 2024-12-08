# Load readr library
library(readr)

# Data explanation
# This is data taken from an airline that includes information on flights 
# and surveyed customers on their overall satisfaction of parts of the flight

# Import the dataset
flight_data <- 
  read_csv("https://myxavier-my.sharepoint.com/:x:/g/personal/malloyc5_xavier_edu/EdehjmF6CyxAk54Th0dlFgUBIqhpHPgQ2aCtQhDFFX90dQ?download=1")


# Create and explain at least 1 new variable from the data that is a calculation
# or logical test of the other(s). 
flight_data$departure_delayed <- ifelse(flight_data$`Departure Delay in Minutes` > 0, "Delayed", "On time")


# Create and explain at least 1 new object that summarizes some aspect of the 
# data. Be sure to explain the new object.
avg_distance <- (mean(flight_data$`Flight Distance`))
# This is the average distance flown (miles) based on all the flights in this
# dataset. 


# Create a histogram, scatterplot or boxplot from the data that you find 
# interesting and explain the result.
hist(flight_data$`Ease of Online booking`,
     main = "Survey Results Based on Ease of Online Booking a Flight",
     col = "navy",
     xlab = "Response from Survery:
     1 = bad 5 = good",
     ylab = "Number of People")
## Kind of interesting, but typical bell curve result for a survey so I tried 
## another kind of graph:

boxplot(flight_data$Age~flight_data$Class,
        main = "Boxplot for Age by Class on Airplane",
        xlab = "Class",
        ylab = "Age")

## I should rearrange so Eco Plus is in the middle, but this shows that the median
## age is higher, the higher the class you buy a ticket for a flight in. It is also
## interesting that there are outliers in the older ages of the business boxplot. 