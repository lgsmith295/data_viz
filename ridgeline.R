#### Packages ####
library(tidyverse)
library(lubridate)
library(ggridges)
library(waterData)
library(ggplot2)
library(viridis)
library(hrbrthemes)

###### Data - Clinch River above Tazewell, TN 03528000 ####

clinch_raw <- importDVs(staid = "03528000", code = "00060") # arguments are station ID and the code for discharge
str(clinch) #check that dates column is classified as such

hist(clinch_raw$val) #quick check of distributions, verrry right skewed as expected
str(clinch) #check to make sure dates column is classified as "date" in order to use lubridate functions 

clinch <- clinch_raw %>%
  mutate(month = month(dates, label = TRUE)) %>% # make month a column, changing number to names 
  mutate(val = log(val)) #log transform to deal with skew

clinch$month <- factor(clinch$month, levels=rev(levels(clinch$month))) #reorder the months so that January is plotted on top


#### plot ####
ridge <- ggplot(clinch, aes(x = val, y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Discharge", option = "D") +
  labs(title = 'Streamflow at Clinch River above Tazewell, TN') +
  xlab("Log Cubic Feet/Second Discharge") +
  theme_ridges() +
  theme(
    legend.position="none", #remove legend
    axis.title.y = element_blank()) #remove y-axis title

ggsave(filename = "SF_ridgeline.tif", plot = ridge, device = "tiff")
