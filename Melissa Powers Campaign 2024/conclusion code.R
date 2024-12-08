library(readr)
library(tidyverse)

library(sf)
library(readxl)
library(RColorBrewer)
library(ggplot2)

summary_2024 <- read_xlsx("data/summary2024.xlsx")
map <- st_zm(st_read("data/maps/precincts_2024.shp"))

COMBINED <- 
  left_join(summary_2024, map, by = c("Precinct" = "PRECINCT"))

COMBINED$Total <- as.numeric(COMBINED$Total)
COMBINED$`Total Votes Powers` <- as.numeric(COMBINED$`Total Votes Powers`)


COMBINED <- COMBINED %>% 
  mutate(powers = `Total Votes Powers`/(`Total`)) 


m = min(COMBINED$powers, na.rm = TRUE)
M = max(COMBINED$powers, na.rm = TRUE)

COMBINED1 <- COMBINED %>% 
  mutate(powers = `Total Votes Powers`/(`Total`)) %>% 
  ggplot(aes(fill=powers)) +
  geom_sf()+
  labs(title = "", 
       subtitle = "Melissa Powers vs Connie Pillich",
       fill = "% of Registered Voters", 
       caption = "")+
  scale_fill_gradientn(colours=brewer.pal(n=6,name="RdBu"),na.value = "transparent",                        
                       breaks=c(0,.25,0.5,.75,1),labels=c("0%","25%","50%","75%","100%"),
                       limits=c(0,1))
view(COMBINED1)



COMBINED <- left_join(summary_2024, map, by = c("Precint" = "PRECINCT"))

# Check if COMBINED is an sf object
if (!inherits(COMBINED, "sf")) {
  COMBINED <- st_as_sf(COMBINED)
}




#-----------------------

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)
library(RColorBrewer)

# Read data
summary_2024 <- read_xlsx("data/summary2024.xlsx")
map <- st_zm(st_read("data/maps/precincts_2024.shp"))

# Join data frames
COMBINED <- left_join(summary_2024, map, by = c("Precint" = "PRECINCT"))

# Ensure the columns are numeric
COMBINED$Total <- as.numeric(COMBINED$Total)
COMBINED$`Total Votes Powers` <- as.numeric(COMBINED$`Total Votes Powers`)

# Check that COMBINED is an sf object
if (!inherits(COMBINED, "sf")) {
  COMBINED <- st_as_sf(COMBINED)
}

# Print the head of the COMBINED data frame to check
print(head(COMBINED))


# Create the plot
COMBINED %>%
  mutate(powers = `Total Votes Powers` / Total) %>%
  ggplot(aes(fill = powers)) +
  geom_sf() +
  labs(
    title = "", 
    subtitle = "Melissa Powers vs Connie Pillich (2024)",
    fill = "Vote for Powers (%)", 
    caption = ""
  ) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(n = 6, name = "RdBu")), 
    na.value = "transparent",
    breaks = c(0, .25, 0.5, .75, 1), 
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1)
  )




mapANDresults2020_1 <- mapANDresults2020 %>% rename(Deters = `Joseph T. Deters      (Rep)`)

#`Joseph T. Deters      (Rep)` /
  
  
mapANDresults2020_1 %>%
  mutate(Deters.prop = Deters/(Deters + `Fanon A. Rucker      (Dem)`))%>%
  ggplot(aes(fill = Deters.prop))+
  geom_sf() +
  labs(
    title = "", 
    subtitle = "Joe Deters vs Fanon Rucker (2020)",
    fill = "Vote for Deters (%)", 
    caption = ""
  ) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(n = 6, name = "RdBu")), 
    na.value = "transparent",
    breaks = c(0, .25, 0.5, .75, 1), 
    labels = c("0%", "25%", "50%", "75%", "100%"),
    limits = c(0, 1)
  )
