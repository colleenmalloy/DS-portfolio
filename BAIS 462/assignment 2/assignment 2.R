library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

##### 1 Data Preparation and Simple Questions:
# 1.1 
housing <- 
  read_csv("http://asayanalytics.com/pierce_housing-pool")

# 1.2
yrs_since_2009 <- (2009-housing$yr_blt)

#1.3
greater_bedrooms <- ifelse(housing$bedrms > housing$bath, "TRUE", "FALSE")

#1.4
greater_baths <- sum(greater_bedrooms == FALSE)
print(greater_baths) #247
# 247 homes have more bathrooms than bedrooms


##### 2 Directed Analysis:
# 2.1
hist(housing$trans_value,
     breaks = 12,
     main = "Distribution of Transaction Value of Houses",
     col = "navy",
     xlab = "Transaction Value ($)",
     ylab = "Count of Houses",
     xlim=c(0,1000000),
     ylim=c(0, 1800))

# The median of transaction value of the most recent sale of the property falls
# within $150,000 to $250,000. The histogram is right skewed because some 
# outliers, fancy large houses are sold for a lot more than the average home. 

#2.2
pools <- housing %>% 
  select(pool, lotsize) %>% 
  mutate(homeType = factor(pool, levels = c(1, 0),
                           labels = c("With Pool", "Without Pool")))
pools %>%
  group_by(homeType) %>% 
  summarise(averageLotSize = mean(lotsize)) %>% 
  ggplot(aes(x = homeType, y = averageLotSize, fill = homeType))+
  geom_bar(stat = "identity")+
  labs(title = "Average Lot Size of Homes With a Pool and Without a Pool", 
       x = "Home Type", y = "Average Lot Size (sq ft)")+
  scale_fill_manual(values = c("blue", "lightblue"))
# The average lot size of houses without a pool is greater than that 
# of houses with a pool.

#2.3
housing %>% 
  ggplot(aes(x = sqft, y = trans_value))+
    geom_point()+
     labs(x = "Square Footage of Home",
          y = "Transaction Value ($)",
          title = "Square Footage by Transaction Value")

# There seems to be an increasing trend in this visual. The more square footage,
# the more the transaction value of the house. There are some outliers, but for
# the most part, this is an increasing linear model. Also, most of our data sits 
# between 1000 to 2000 square footage. 

# 2.4
pools <- housing %>% 
  select(pool, lotsize, trans_value) %>% 
  mutate(homeType = factor(pool, levels = c(1, 0),
                           labels = c("With Pool", "Without Pool")))
pools %>% 
  ggplot(aes(x = homeType, y = trans_value,)) +
  geom_boxplot()+
  labs(x = "Type of Home", y = "Transaction Value ($)", 
       title = "Transaction Value by Whether a House has a Pool or Not")

# The medians of these boxplots are very close, both a little over $250,000, 
# however, houses without a pool have a larger range with more houses with
# higher transaction values. Both types of houses are right skewed, which is 
# typical. There will be some houses that sell for more money than average 
# (regardless of whether it has a pool or not). 


# 2.5
housing %>% 
  mutate(bath_full = floor(bath)) %>% 
  ggplot(aes(x = bath_full, y = bedrms))+
  geom_jitter(width = .2, height = .2)+
  scale_x_continuous(breaks = seq(0,6,1))+
  scale_y_continuous(breaks = seq(0,12,2))+
  labs(title = "Scatter Plot of Full Bathrooms vs. Bedrooms",
       x = "Number of Full Bathrooms",
       y = "Number of Bedrooms")

# There is a positive linear trend, the more full baths, the more bedrooms.
# There are most points around 1 to 3 bathrooms and 2 to 5 bathrooms.
# The average is somewhere within these points because this is where most
# points are located.


##### 3 Self-Directed Analysis:
# 3.1
# housing %>% 
#   ggplot(aes(x = yr_blt, y = trans_value))+
#   geom_point()+
#   labs(title = "Year House was Built vs. Value of House", 
#        x = "Year House was Built", 
#        y = "Price ($)")
# tried this but didnt like how it looked


housing %>% 
  mutate(group = cut(yr_blt, breaks = c(0, 1900, 1920, 1940, 1960, 1980, 2000, 
                                        2010, 2020), labels = c("0-1900", 
                                                                "1900-1920",
                                                                "1920-1940",
                                                                "1940-1960",
                                                                "1960-1980",
                                                                "1980-2000",
                                                                "2000-2010",
                                                                "2010+"))) %>% 
  ggplot(aes(x = group, y = trans_value))+
  geom_boxplot()+
  labs(title = "Year House was Built vs. Value of House", 
       x = "Year House was Built", 
       y = "Price ($)")
# The median house prices slightly increase as time goes on.
# There aren't any drastic changes. There are higher outliers as time goes on.
# Also, the IQRs seem relatively similar in size. So, yes, older homes
# are worth less than newer homes, but I would use this visualization to show 
# this is not true for every home and it is only a slight increase in price 
# by years.

# 3.2
housing %>% 
  ggplot(aes(x = as.factor(bedrms), y = sqft))+
  geom_boxplot()+
  geom_smooth(aes(x = bedrms, y = sqft), method = 'lm')+
  labs(title = "", 
       x = "Number of Bedrooms", 
       y = "Squarefoot of House")
# From this visualization, yes, as the number of bedrooms increase, the 
# square footage of the home also increases. This is not a necessity or true for 
# every home. For example, 6 bedrooms has a higher median square footage than 7 
# bedrooms. So, there are some 7 bedroom houses smaller than 6 bedroom houses.
# For the most part though, the more bedrooms, the more square footage. 

# 3.3
housing %>% 
  mutate(pool = ifelse(pool == 1, "Yes", "No")) %>% 
  ggplot(aes(x = as.factor(bath), y = sqft, fill = pool)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "Home Size and Number of Bathrooms by if the House has a Pool",
       x = "Number of Bathrooms",
       y = "Home Size (sq ft)",
       fill = "Pool")
# I made side by side boxplots and compared the houses with pools and without
# pools. The medians for many of them are very close or even the same. Houses
# with 3.5 bathrooms seem to have bigger houses without pools. I would say no to
# this statement. The more bathrooms, the larger the house gets, but the less
# houses with a pool.
