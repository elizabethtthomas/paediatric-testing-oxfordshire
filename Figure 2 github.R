####---------------------------------------INTRODUCTION---------------------------------------####

# Below is the code to create Figure 2 in the paper on paediatric testing trends in Oxfordshire #
# Graph for Figure 2 shows the proportion of children in Oxfordshire who had a test in General Practice, Outpatient and Inpatient settings as well as in any setting
# The data for this graph was drawn from the analyses which is provided in another file (Script for github)

#### -----------------------------------------------------------------------------------------####


#### Install packages ####

install.packages("dplyr")
install.packages("tidyverse")
install.packages("collapse")
install.packages("janitor")
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(collapse)
library(janitor)

install.packages('hrbrthemes')
install.packages('viridis')
install.packages('ggthemes')
install.packages('gtable')  
install.packages('grid')
install.packages('ggnewscale')
install.packages('geomtext')
install.packages('shadowtext')
install.packages('ggrepel')

library(hrbrthemes)
library(viridis)
library(ggthemes)
library(gtable)
library(gtable)
library(ggnewsscale)
library(ggtext)
library(shadowtext)
library(geomtext)
library(ggrepel)

#Step 1. 

#### Import excel file proportion of children having a test. xls called 'proportion'
library(readxl)
proportion <- read_excel("~/proportion.xlsx")
View(proportion)

# Change year and group to factor variable 
proportion$setting <- as.factor(proportion$setting)
proportion$year <- as.factor(proportion$year)

# Make a line graph 
proportion1 <- proportion %>%
  ggplot(aes(x=year, y=proportion, group=setting, color=setting))+
  scale_color_manual(values=c("mediumvioletred", "mediumpurple1", "goldenrod1", "black")) +
  geom_line(size=0.9)+
  ggtitle("Proportion of children in Oxfordshire who had at least one test from 2005 to 2019") +
  theme_bw()+
  xlab("Year")+
  scale_y_continuous(limits = c(0,15), name="Proportion of children in Oxfordshire (%)")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
  theme(axis.title = element_text(face= "bold", size=11))+
  theme(legend.title = element_text(face = "bold", size = 11))+
  theme(legend.position = "bottom")+
  theme(legend.title= element_blank())+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6, linetype = "dashed" ))

#Print graph
proportion1
