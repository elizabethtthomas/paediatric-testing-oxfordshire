####---------------------------------------INTRODUCTION---------------------------------------####

# Below is the code to create Figure 1 in the paper on paediatric testing trends in Oxfordshire #
# Graph for Figure 1 with four panels: overall test use, and rates broken down by setting,sex,age

#### -----------------------------------------------------------------------------------------####

### Install packages###

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
install.packages("scales")
install.packages("viridis")
install.packages("ggrepel")
install.packages("wesanderson")
install.packages("RColorBrewer")


library(hrbrthemes)
library(viridis)
library(ggthemes)
library(gtable)
library(gtable)
library(ggnewsscale)
library(ggtext)
library(shadowtext)
library(scales)
library(viridis)
library(ggrepel)
library(wesanderson)
library(RColorBrewer)



# Step1. Import Sheet 1 in setting_removed2020_forR
library(readxl)
trendsbysetting <- read_excel("C:/Users/thomase/Documents/setting_removed2020_forR.xlsx")
View(trendsbysetting)

# Step2. Change setting to factor variable 
trendsbysetting$setting <- as.factor(trendsbysetting$setting)
trendsbysetting$setting <- factor(trendsbysetting$setting, levels=c("Inpatient", "Outpatient", "General Practice"))

# Step 3. Make ggplot

trendsbysetting_graph <- trendsbysetting %>%
mutate(label = if_else(year == max(year), as.character(setting), NA_character_))%>%
  ggplot(aes(x=year, y=modelled_rate, color = setting,group=setting))+
  geom_line(size = 0.72)+
  scale_color_manual(values= c("darkorchid4", "aquamarine3", "goldenrod1"))+
  scale_y_continuous(trans = 'log10',limits = c(10,10000), name="Test rate (per 1,000 child-years) log axis")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("Figure 1b. Test rates by setting")+
  theme_bw()+
  labs(x= "Year")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16))+
  theme(axis.title.y = element_blank())+
  theme(axis.title = element_text(face= "bold", size=11))+
  theme(legend.title = element_text(face = "bold", size = 11))+
  theme(legend.position = "NONE")+
  theme(legend.title= element_blank())+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6, linetype = "dashed" ))+
  geom_label_repel(aes(label = label), point.padding = unit(2.5, 'lines'),label.size = NA, fill = NA, fontface = "bold", na.rm = TRUE, nudge_x = 45, direction = "y")

trendsbysetting_graph

# Step4. Import Sheet 2 in overall age adjusted rate.xlsx, called temporalbysex
temporalbysex <- read_excel("~/overall age adjusted rate.xlsx", 
                            sheet = "Sheet2")

# Check sex, segment, year are factor variables
temporalbysex <- overall_age_adjusted_rate
str(temporalbysex)
temporalbysex$sex <- as.factor(temporalbysex$sex)
temporalbysex$segment <- as.factor(temporalbysex$segment)



# Step 5. Make graph of temporal change by sex
trendbysex_graph <- temporalbysex %>%
  mutate(label = if_else(year == max(year), as.character(sex), NA_character_))%>%
  ggplot(aes(x=year, y=modeled_rate, group=sex, color=sex))+
  geom_line(size=0.72)+
  ggtitle("Figure 1c. Test rates by sex") +
  theme_bw()+
  scale_color_manual(values = c("mediumvioletred", "green4"))+
  xlab("Year")+
  ylab("Test rate (tests/1,000 child-years)") +
  scale_y_continuous(trans = 'log10',limits = c(10,10000), name="Test rate (per 1,000 child-years) log axis")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face= "bold"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "none")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6, linetype="dashed" ))+
  geom_label_repel(aes(label = label), fill = NA, label.size = NA, point.padding = unit(2, 'lines'),fontface = "bold", na.rm = TRUE, nudge_x = 45, direction = "y")

trendbysex_graph

# Step 6 import age group data 

rm(trendsbyagegroup)
library(readxl)
trendsbyagegroup <- read_excel("C:/Users/thomase/Documents/agegroup_removed2020_forR.xlsx")
View(trendsbyagegroup)

# Change age group to factor variable
trendsbyagegroup$age_group <- as.factor(trendsbyagegroup$age_group)
str(trendsbyagegroup$age_group)

# Step 7. Make graph of temporal change by age group 

trendbyagegroup_graph <- trendsbyagegroup %>%
  mutate(label = if_else(year == max(year), as.character(age_group), NA_character_))%>%
  ggplot(aes(x=year, y=modeled_rate, group=age_group, color=age_group))+
  geom_line(size=0.72)+
  ggtitle("Figure 1d. Test rates by age group") +
  theme_bw()+
  scale_color_manual(values=c("deeppink", "royalblue", "chartreuse4", "chocolate1"))+
  xlab("Year")+
  ylab("Test rate (tests/1,000 child-years)") +
  scale_y_continuous(trans = 'log10',limits = c(10,10000), name="Test rate (per 1,000 child-years) log axis")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(2005,2019)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face="bold"))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title.y = element_blank())+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "none")+
  theme(legend.key=element_blank())+
 guides(fill = guide_legend(override.aes = aes(color = NA)))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2",linetype="dashed", size = 0.6 ))+
  geom_label_repel(aes(label = label), fill = NA, hjust = 1, label.size = NA, fontface = "bold", na.rm = TRUE, nudge_x = 1000,
                   nudge_y = 0.2)

trendbyagegroup_graph

# Step 8. Overall rates of test use

library(readxl)
overall_apc_2019 <- read_excel("~/overall age adjusted rate.xlsx", 
                               sheet = "overall rates without 2020")
view(overall_apc_2019)

# Output put into excel sheet and imported here as overall_apc_2019
rm(overall_apc_2019)
#overall_apc_2019$year <- as.factor(overall_apc_2019$year)
overall_apc_2019$segment <- as.factor(overall_apc_2019$segment)
str(overall_apc_2019)

# Step 9. Make a line graph showing overall trends in test use with observed rates as points 

overall_apc_2019_graph <- overall_apc_2019 %>%
  ggplot(aes(x=year, y=modeled_rate))+
  geom_line(size=0.8)+
  geom_point(aes(y=observed_rate, color = "black"))+
  ggtitle("Figure 1a. Overall rates of test use") +
  theme_bw()+
  scale_color_manual(values=c("black")) +
  xlab("Year")+
  scale_y_continuous(trans = 'log10',limits = c(10,10000), name="Test rate (per 1,000 child-years) log axis")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "none")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2",linetype="dashed", size = 0.6 ))

overall_apc_2019_graph

# Step 10. Combine 4 graphs into one panelled figure 

install.packages("gridExtra")
library(gridExtra)

plot1 <- overall_apc_2019_graph 
plot2 <- trendsbysetting_graph
plot3 <- trendbysex_graph
plot4 <- trendbyagegroup_graph

plot1
plot2
plot3
plot4

Figure1 <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 4)
