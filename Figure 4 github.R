####---------------------------------------INTRODUCTION---------------------------------------####

# Below is the code to create Figure 4 in the paper on paediatric testing trends in Oxfordshire #
# Graph for Figure 4 shows the Average Annual Percentage Change (AAPC) of the top 25 tests overall performed in children aged 0-15 in Oxfordshire
# For this graph I want to create a dotplot with confidence intervals listing the tests in order from highest AAPC growth to the lowest
# The data contributing to this dotplot comes from joinpoint regression analysis that was run on the Joinpoint SEER program 
# The output from Joinpoint was added to a excel spreadsheet including the test name, AAPC, and 95% confidence intervals - called "aapcbytest.xlsx remove 2020"

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

library(hrbrthemes)
library(viridis)
library(ggthemes)
library(gtable)
library(gtable)
library(ggnewsscale)
library(ggtext)
library(shadowtext)
library(scales)

## Step 1. Import excel as AAPCbytest

aapcbytest <- read_excel("~/aapc by test.xlsx remove 2020.xlsx")
aapcbytest


## Step 2. Change order of factor variable according to average annual percent change (from highest to lowest)
aapcbytest$testname <- fct_relevel(aapcbytest$testname, levels=c("Vitamin D","Parathyroid hormone","Iron studies", "Folate", "Vitamin B12", "HbA1c", "IgA", "Coeliac", "Creatine Kinase", "CSF studies", "Lipids", "Thyroid function test", "IgG/IgM", "Medication level", "Full blood count", "C reactive protein", "Amylase", "Urea and electrolytes", "Urine creatinine","Liver function test", "Calcium Magnesium Phosphate", "Erythrocyte sedimentation rate", "Coagulation profile", "Gentamicin", "Monospot"))

## Step 3.  Create dot plot
aapc_by_test_graph <- aapcbytest %>%
  arrange(aapc)%>%
  mutate(testname = factor(testname, levels = testname))%>%
  ggplot(aes(x=testname, y=aapc)) + 
  geom_point(col="deeppink", size=3) +  # Draw points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, colour = "black")+
  geom_segment(aes(x=testname, 
                   xend=testname, 
                   y=min(-25), 
                   yend=max(30)), 
               linetype="dashed", color="white", 
               size=0.1) +   # Draw dashed lines
  labs(title="Temporal change of specific test use for children aged 0-15 years of age in Oxfordshire from 2005 to 2020", 
       subtitle="Average annual percentage change of 25 laboratory tests")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(x="Test name", y = "Average annual percentage change in test use (%)")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0, color="light grey", size = 1)+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=12,face="bold"))+
  coord_flip()


## Print graph
aapc_by_test_graph
