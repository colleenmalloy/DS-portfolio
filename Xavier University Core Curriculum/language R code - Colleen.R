install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

#rename spreadsheet
data <- read_excel('CoreRequirements2015-2023.xlsx')
View(data)
##_____________________________________________________________________________________________________________________________
### How many required language credits do students typically take???
# filter language == 1

#length(unique(data$`Student ID-Random`))

language <- data %>% 
  filter(language == "1") %>% 
  filter(`Cohort Code` == "NF") 

#View(language)

#length(unique(language$`Student ID-Random`))


library(dplyr)
total_langcredit <- language %>% 
  group_by(`Student ID-Random`) %>% 
  summarise(total_value = sum(`Credits earned for this course`))
print(total_langcredit)

##Total Language Requirement Credits Earned at Xavier Barplot (filtered by NF)
#library(ggplot2)
ggplot(data = total_langcredit, aes(x = total_value))+
  geom_bar(fill = "pink", color = "black")+
  scale_x_continuous(breaks = seq(0,21, by =3))+
  geom_text(stat = 'count', aes(label= after_stat(count)), vjust = -.5)+
  labs(title = "Total Language Requirement Credits Earned at Xavier", x = "earned credits", y = "Count of Students")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))

#________________________________________________________________________________________________________________________________________
##language credits earned amongst those who took language courses at Xavier
##number of attempted (enrolled) language courses by student 

language <- data %>% 
  filter(language == "1") %>% 
  filter(`Cohort Code` == "NF") 

#View(language)

#length(unique(language$`Student ID-Random`))


#library(dplyr)
total_langcourse <- language %>% 
  group_by(`Student ID-Random`) %>% 
  summarise(total_rows = n())
print(total_langcourse)

##Total Language Requirement Credits Earned at Xavier Barplot (filtered by NF)
#library(ggplot2)
ggplot(data = total_langcourse, aes(x = total_rows))+
  geom_bar(fill = "pink", color = "black")+
  scale_x_continuous(breaks = seq(0,10, by =1))+
  geom_text(stat = 'count', aes(label= after_stat(count)), vjust = -.5)+
  labs(title = "Total Language Requirement Courses Attempted by Student at Xavier", x = "Number of Attempted Language Courses", y = "Count of Students")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 12))

##____________________________________________________________________________________________________________________________

language1 <- data %>% 
  select(`Student ID-Random`, `Cohort Code`, `Credits earned for Core this Semester`, `Credits earned for this course`, Course, Grade, language) %>% 
  filter(`Cohort Code` == "NF") 

yes <- language1 %>% 
  filter(language == "1") %>% 
  filter(`Cohort Code` == "NF") 

no <- language1 %>% 
  filter(language == "0") %>% 
  filter(`Cohort Code` == "NF") 
#View(language)

#length(unique(language$`Student ID-Random`))


#library(dplyr)
total_langcourse <- language %>% 
  group_by(`Student ID-Random`) %>% 
  summarise(total_rows = n())
print(total_langcourse)

##Total Language Requirement Credits Earned at Xavier Barplot (filtered by NF)
#library(ggplot2)
ggplot(data = total_langcourse, aes(x = total_rows))+
  geom_bar(fill = "pink", color = "black")+
  scale_x_continuous(breaks = seq(0,10, by =1))+
  geom_text(stat = 'count', aes(label= after_stat(count)), vjust = -.5)+
  labs(title = "Total Language Requirement Courses Attempted by Student at Xavier", x = "Number of Attempted Language Courses", y = "Count of Students")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 12))



#__________________________________________________________________________________________________________________________
length(unique(data$`Student ID-Random`))
length(unique(language$`Student ID-Random`))
12089-8235

language <- data %>% 
  filter(language == "1") %>% 
  filter(`Cohort Code` == "NF") 

total_langcourse <- language %>% 
  group_by(`Student ID-Random`) %>% 
  summarise(total_rows = n())

#find how many students in language
number_students = unique(data$`Student ID-Random`) %in% unique(language$`Student ID-Random`)

#find how many students not in language
no.language = unique(data$`Student ID-Random`)[!number_students]

#give all students 0 
no_langcourse = cbind(no.language,0)

#give foo same column names as total_langcourse
colnames(no_langcourse) = colnames(total_langcourse)

#bind total_langcourse and no_langcourse
combination = rbind(total_langcourse, no_langcourse)


##Total Language Requirement Credits Earned at Xavier Barplot (filtered by NF)
#library(ggplot2)
ggplot(data = combination, aes(x = total_rows))+
  geom_bar(fill = "pink", color = "black")+
  scale_x_discrete(breaks = seq(0,10, by =1))+
  geom_text(stat = 'count', aes(label= after_stat(count)), vjust = -.5)+
  labs(title = "Total Language Requirement Courses Attempted by Student at Xavier", x = "Number of Attempted Language Courses", y = "Count of Students")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 12))