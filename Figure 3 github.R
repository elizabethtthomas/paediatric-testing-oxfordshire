####---------------------------------------INTRODUCTION---------------------------------------####

# Below is the code to create Figure 3 in the paper on paediatric testing trends in Oxfordshire #
# Graph for Figure 3 shows the proportion of each of the top 20 tests out of the total number of tests for each age group and setting
# The data for this graph was drawn from the analyses which is provided in another file (Script for github)

#### -----------------------------------------------------------------------------------------####


### Install packages###


install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("collapse")
install.packages("janitor")
install.packages("ggthemes")
library(ggthemes)
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
install.packages("scales")
install.packages("wesanderson")
install.packages("ggsci")
install.packages("ggpubr") 
install.packages("RColorBrewer")
install.packages("stringr")
library(stringr)
library(hrbrthemes)
library(viridis)
library(ggthemes)
library(gtable)
library(gtable)
library(ggnewsscale)
library(ggtext)
library(shadowtext)
library(scales)
library(wesanderson)
library(ggsci)
library(ggpubr)
library(RColorBrewer)

# Import data # 
library(readxl)
commontests3 <- read_excel("C:/Users/thomase/Documents/new numbers for figure 3 with top 20 tests2.xlsx")

# Create colour palette with 22 colours 
colors37 = c("#d1dc74", "#28f0ad","#ffb143", "#e7a9e9","#8cbc34","#01c298","#f979e4","#00b4f4","#00c069","#9187ff","#ff5574","#028eec","#bb5b00","#9247ba","#774c15","#a5001d","#8d1c00","#84005f","#1635a2","#5e003d","lightgrey")


# Create stacked, grouped bar chart showing proportion of top 20 tests that make up the total tests performed by age group and setting 

common_tests_graph_3 <- commontests3 %>%
  ggplot(aes(x=age_group, y=proportion, fill=test))+
  geom_bar(stat = "identity",col = 'white', size = 0.7)+
  facet_wrap(~ setting)+
  scale_fill_manual(values = colors37)+
  guides(fill = guide_legend(title = "Tests"))+
  ggtitle("Figure 3. Most common tests performed in each setting by age group")+
  theme_bw()+
  labs(x= "Age group (years)", y= "Proportion of tests (%)")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
  theme(axis.title = element_text(face= "bold", size=11))+
  theme(legend.title = element_text(face = "bold", size = 11))+
  theme(axis.text = element_text(size = 11))+
  theme( strip.text = element_text( size = 11, color = "black", face = "bold"),
         strip.background = element_rect( fill = "#d2e4ee", color = NA ),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6 ))

# Print graph

common_tests_graph_3