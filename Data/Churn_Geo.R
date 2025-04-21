
# Clear variables in memory
rm(list=ls())

# Load required libraries
library(gridExtra)
library(ggmap)
library(ggplot2)
library(sf)
library(terra)
library(dplyr)
library(tmap)    
library(leaflet) 
library(rnaturalearth)
library(maps)
library(tidyr)

Churn1 <- read.csv("C:/R/Telco_customer_churn.csv")

str(Churn1)
head(Churn1)
summary(Churn1)


theme_custom <- theme_minimal() +
  theme(plot.title = element_text(size=12, face="bold"),
        axis.title = element_text(size=10),
        legend.position = "bottom")


devtools::install_github("ropensci/rnaturalearthhires")
states <- ne_states(country = "united states of america", returnclass = "sf")
california <- states[states$name == "California", ]


geo <- ggplot() +
  borders("state", regions = "California", fill = "gray90") +
  geom_point(data = Churn1,
             aes(x = Longitude, y = Latitude, color = Churn.Label),
             alpha = 0.6,
             size = 1,  
             stroke = 0) +  
  coord_quickmap() +
  labs(title = "Churn Distribution in California") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "#00BFC4", "No" = "lightcoral"))

print(geo)


churn1_clean <- Churn1 %>%
  drop_na(Total.Charges)

summary(churn1_clean$Total.Charges)


churn_yes <- churn1_clean %>%
  filter(Churn.Label == "Yes")


geo_churn <- ggplot() +
  borders("state", regions = "California", fill = "gray90") +
  geom_point(data = churn_yes,
             aes(x = Longitude, y = Latitude, color = Churn.Label),
             alpha = 0.6,
             size = 1,  
             stroke = 0) +  
  coord_quickmap() +
  labs(title = "Churn Distribution in California") +
  theme_minimal() +
  scale_color_manual(values = c("Yes" = "#00BFC4"))

print(geo_churn)


summary(churn1_clean$Longitude) 
summary(churn1_clean$Latitude)   

  

