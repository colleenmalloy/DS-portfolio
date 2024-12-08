## Part 1
# Load readr library
library(readr)

# Data explanation
# This is data taken from an airline that includes information on flights 
# and surveyed customers on their overall satisfaction of parts of the flight

# Load my personal data
flight_data <- 
  read_csv("https://myxavier-my.sharepoint.com/:x:/g/personal/malloyc5_xavier_edu/EdehjmF6CyxAk54Th0dlFgUBIqhpHPgQ2aCtQhDFFX90dQ?download=1")

## Topic/Question/Observation
# Are there more male or female Loyal customers? 

## How do you intend to answer the question
# I can find this by a bar chart with gender on my x
# and number of Loyal customer on my y. I will have to 
# filter the Customer Type column.

## Analysis
library(ggplot2)
library(dplyr)
summary_data <- flight_data %>%
  group_by(Gender, `Customer Type`) %>%
  summarise(Count = n())

ggplot(summary_data, aes(x = Gender, y = Count, fill = `Customer Type`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  labs(title = "Customer Loyalty by Gender",
       x = "Gender",
       y = "Frequency") +
  theme_minimal()

# I was surprised at these results because the female and male Loyal Customer
# count are almost exactly the same. There are large totals for female and male
# and those numbers are not the same so I was surprised how close they are. 
# (Less than 10 customers). 


## Topic/Question/Observation
# What is the distribution of ages of flyers?

## How do you intend to answer the question
# I want to make a bar graph to see the distribution.

## Analysis
flight_data$AgeGroup <- cut(flight_data$Age, 
                            breaks = seq(0, max(flight_data$Age, na.rm = TRUE), 
                                         by = 9), right = FALSE, 
                            labels = c("0-9", "10-19", "20-29", "30-39",
                                       "40-49", "50-59", "60-69", "70-79", 
                                       "80-89"))
ggplot(flight_data, aes(x = AgeGroup)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Age Distribution of Flyers", x = "Age Group", y = "Frequency") +
  theme_minimal()

# The distribution looks bell curved. There are most flyers in the 40-49 age
# range. My assumption is that most middle aged flyers are on business or work
# trips. There are many younger flyers, so I would assume those are for 
# vacations or family trips. Also, there are 17 flyers without ages recorded. 


## Topic/Question/Observation
#Are all the survey answers bell curved? Are there some that feel strongly 
# good/bad than others?

## How do you intend to answer the question
# I want to try and facet wrap the columns with survey answers in bargraphs.

## Analysis
# load libraries
library(ggplot2)
library(dplyr)
library(grid)

# I want survey answers so columns 9 through 22.
survey_answers <- colnames(flight_data)[9:22]

# This is so all the plots are created.
plot_list <- list()

# wrap for X in columns 9-22
for (X in survey_answers) {
  temp <- flight_data %>%
    select(Inflight_wifi_service = !!sym(X))
  
  p <- ggplot(temp, aes(x = Inflight_wifi_service)) +
    geom_bar(fill = "lightgreen", color = "black") +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -.5) +
    labs(title = paste("Survey Results:", X), x = X, y = "Frequency") +
    theme_minimal()
  
  plot_list[[X]] <- p
}

# We can see a lot from this. Some are bell shaped, and the rest are mostly
# left skewed. This is good because a 5 on the scale was the best. Most
# graphs have very few values for 0, so maybe I would take the out for 
# better visual. Online boarding had a lot of 0s so maybe this did not
# apply to some passengers. 



##### Repeat this but with a different data set
# Load the reader library
library(readr)

#Data Explanation:
# This is data taken from ESPN Teams Page that describes
#NFL season games from 2002 to 2023.

# load my personal data
nfl_seasons <-
  read_csv("https://myxavier-my.sharepoint.com/:x:/g/personal/mclaughline3_xavier_edu/ERJXGZUS03JBlYMdrjIsP-cBTBkUA0LNAAdE-EAMZcgOMg?download=1")


## Topic/Question/Observation
# What is the correlation of score at home vs score away?

## How do you intend to answer the question
# Make a scatterplot of score home and score away.

## Analysis
ggplot(nfl_seasons, aes(x = score_home, y = score_away))+
  geom_point()+
  labs(title = "Scatterplot of Score at Home vs. Away",
       x = "Score at Home",
       y = "Score while Away")+
  theme_minimal()

# There does not seem to be a correlation, but the highest scores are while the 
# teams are at home. 


## Topic/Question/Observation
# I would assume this will look the same as the question above... What is the 
# correlation between yards at home vs away?

## How do you intend to answer the question
# I will make another scatterplot but with yards instead.

## Analysis
ggplot(nfl_seasons, aes(x = yards_home, y = yards_away))+
  geom_point()+
  labs(title = "Scatterplot of Yards at Home vs. Away",
       x = "Yards at Home",
       y = "Yards while Away")+
  theme_minimal()

# Okay, this is actually not what I expected. It looks like the average
# yards for a football game is between 300 and 450 yards. 


## Topic/Question/Observation
# Did the average scores for home games increase or decrease over the years?

## How do you intend to answer the question
# I need to find the average score of home games for each year.
# So I will make a new data frame and summarize the season and score columns.

## Analysis
summary_data <- nfl_seasons %>%
  group_by(season) %>%
  summarise(Average_Score = mean(score_home))

ggplot(summary_data, aes(x = factor(season), y = Average_Score))+
  geom_bar(stat = "identity", fill = "skyblue")+
  labs(title = "Average Scores by Football Season (2002-2023)",
       x = "Year",
       y = "Average Score")+
  theme_minimal()

# There is not much difference. All the averages are between 20 
# and 25 points. 2006 had the lowest average and 2013 had the highest average.