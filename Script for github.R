#####################################################################################################################################################################
############################## INTRODUCTION ##############################  


# This script contains the code for cleaning and doing the initial analyses for a paper on trends in paediatric laboratory testing in Oxfordshire from 2005 to 2019
# It uses laboratory data from the OUH NHS Trust clinical laboratories 
# The code to create the Figures is provided separately

##############################  Steps undertaken ##############################  
# 1. Import packages
# 2. Import data 
# 3. Cleaning raw data to get master data set
        - # merging each individual tests with panels and removing duplicates, changing date format and making year factor variable, removing void columns to get master dataset
# 4. Managing master data set
        - # removing admin tests
        - # removing 2020, including only those from 2005 to 2019
        - # combining ages of those under 1, as 0
        - # removing blood gas/glucose tests as these were done inconsistently across the Trust
        - # making age-groups
# 5. Data for Table 1
# 6. Data for Figure 1
# 7. Data for Figure 2
# 8. Data for Figure 3
# 9. Data for Figure 4
  

############### Import packages required ###############

install.packages("dplyr")
install.packages("tidyverse")
install.packages("readxl")
install.packages("readr")
install.packages("ggplot2")
install.packages("collapse")
install.packages("janitor")


library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
library(collapse)
library(janitor)

############### Import data ###############

raw_data <- read_csv("lizzie_data_20220308.csv")

# Understanding the data 
glimpse(raw_data)
summary(raw_data)

############### Data cleaning of raw data to get master dataset ############### 

# Change sex and location/setting variables to factor variables
raw_data$sex <- as.factor(raw_data$sex)
raw_data$loc2 <- as.factor(raw_data$loc2)

# De-duplicate rows and make new dataframe called raw_data2
raw_data2 <- distinct(raw_data)

-## Test counts ## -
  
# Number of tests ordered = 11044960 individual tests
# >11 million tests includes Haemoglobin, white cell count, neutrophil count separately as individual tests etc... however these would be usually ordered as one combined test

# To find out how many unique tests were ordered #
  
length(unique(raw_data2$tname)) 
# 1272 unique tests

#-#-#-#-#-#-#-#- Merge dataset with test panels (test_names_4.xlsx) to assign each test name to a panel #-#-#-#-#-#-#-#-
  
# The panel assigned to each test name is available as a supplementary file 

panel <- read_excel("test_names_4.xlsx") # Remove duplicates from panel and rename panel_2
panel_2<- distinct(panel)

#perform left join using dplyr 

raw_data3 <- left_join(raw_data2, panel_2, by='tname')
table(raw_data3$panel, useNA = "always") # check for completion of merging dataframes 


#change dts format to Date

raw_data3$dts = as.Date(raw_data3$dts)
str(raw_data3$dts)
raw_data3$dts_year <- substr (raw_data3$dts, 1, 4)
str(raw_data3$dts)
str(raw_data3$dts_year)
raw_data3$dts_year <- as.factor(raw_data3$dts_year)
str(raw_data3$dts_year)


# Make a new variable called no_test_perday
raw_data4 <- raw_data3 %>%
  group_by(id,dts,panel) %>%
  mutate(no_test_perday = 1:n())

table(raw_data4$no_test_perday, useNA = "always")   # TEST - check for completeness of new variable (how many of each test were done on each day)

dummy <- subset(raw_data4, raw_data4$id==50482)   # examining id 50482 which had 78 of same test on one day by creating dummy dataframe (blood gas - examined to be correct)
dummy <- dummy[order(dummy$dts, dummy$panel),]    # ordered dummy dataset by date and panel 

# New data frame with unique no_test_perday (deleting rows which are not 1)
raw_data5 <- subset(raw_data4, raw_data4$no_test_perday==1)

names(raw_data5)
raw_data5$dts <- NULL     # delete columns called dts, tname, val, val2, panel_id
raw_data5$tname <- NULL      
raw_data5$val <- NULL     
raw_data5$val2 <- NULL     
raw_data5$panel_id <- NULL     

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- Master data set #-#-#-#-#-#-#-#-#-#-#-#-
master <- raw_data5
save(master,file="master.Rda")




############### Data management of master dataset ############### 

# MASTER 1 
master_1 <- master
str(master_1$age)

# Remove Admin test codes i.e. test codes  # 
master_1 <-master_1[!grepl('Admin', master_1$panel),]
master_1 %>%
  group_by(panel) %>%
  count() %>%
  View() # to check no admin test codes are included

# Check counts by age 
master_1 %>%
  group_by(age) %>%
  count() %>%
  View()

# Under 1s contributed 1/3 of data, make new master dataset excluding those aged under 1
# MASTER 2
master_2 <-subset(master_1, age>0.95)

master_2 %>%
  group_by(panel) %>%
  count() %>%
  View()

# MASTER 3
#Master_3 combines all the tests done in <1 age category as 0
table(master$age)
master_3 <- master_1

master_3$age <- as.numeric(master_3$age)

master_3 <- mutate(master_3, age = ifelse(age < 1, 0, age))
table(master_3$age)

# MASTER 4 - This was not used 
# Master 4 included a column for results (combining raw data set and raw data results) 

# MASTER 5
### Master 5 is master 2 removing 2020 tests due to pandemic impacts ###

master_5 <-master_2[!grepl('2020', master_2$dts_year),]
master_5 %>%
  group_by(panel) %>%
  count() %>%
  View()

# MASTER 6
### Master_6 is master_3 removing 2020 tests due to pandemic impacts ### 
master_6 <-master_3[!grepl('2020', master_3$dts_year),]
master_6 %>%
  group_by(panel) %>%
  count() %>%
  View()

# MASTER 7
### Master_7 is master_6 removing 3 blood tests: POCT Blood gas, Glucose and IgM.
# POCT Blood gas and Glucose were inconsistently recorded/uploaded onto testing system. 
# IgM and IgG had extremely similar numbers so likely duplicates (and ordered as one test). We deleted IgM and assumed IgG was actually IgM/IgG

master_7 <-master_6[!grepl('POCT Blood gas', master_6$panel),]
master_7 <-master_7[!grepl('Glucose', master_7$panel),]
master_7 <-master_7[!grepl('IgM', master_7$panel),]


#### To gather input data for Joinpoint software, which aims to determine temporal trends over time, we need crude rates of tests stratified age, year and location ####
crude_age_year_sex_setting <- master_7
joinpoint_setting_age_year<- crude_age_year_sex_setting %>%
  group_by(age,sex, dts_year,loc2) %>%
  summarize(num_test = n())

# Import ONS estimates - age/sex/year based population estimates for Oxfordshire from 2005 to 2019
str(sex_year_age_population)
str(panel_crude_age_year_sex_setting)
sex_year_age_population$sex <- as.factor(sex_year_age_population$sex)
sex_year_age_population$age <- as.numeric(sex_year_age_population$age)
sex_year_age_population$dts_year <- as.factor(sex_year_age_population$dts_year)

# Merge population estimates with crude numbers data frame
joinpoint_setting_age_year_population <- joinpoint_setting_age_year
joinpoint_setting_age_year_population <- left_join(joinpoint_setting_age_year_population, sex_year_age_population, by = c("sex", "age", "dts_year"))

#### Making age groups #### 
# age groups: <1, 1-5, 6-10 and 11-15 years 
trial_run <- master_7
trial_run <- trial_run %>%
  group_by(sex, age, dts_year,loc2) %>%
  summarize(num_test = n())

joinpoint_setting_age_year_population_agegroups <- joinpoint_setting_age_year_population %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )


##### #### ### #### #### ####   Descriptive analyses #### ####  ####  ####  ####  ####  #### 

####================================ Table 1 ================================####

### Count number of females and males ###

masterid <- master_7[!duplicated(master_7$id),]
table(masterid$sex)


# Number of tests done by age
master_7 %>%
  group_by(age) %>%
  count() %>%
  View()

# Number of tests done in each location
master_7 %>%
  group_by(loc2) %>%
  count() %>%
  View()

### To get summary statistics on tests ordered per child/median/IQR overall
master_7 %>%                              
  group_by(id, sort = TRUE) %>%
  summarise(count = n_distinct(panel)) %>%
  summary()

### Median and IQR - number of tests per child in each setting ###
master_7_gp <- subset(master_7, master_7$loc2 == "GP")    
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  summarise(count = n_distinct(panel)) %>%
  summary()


master_7_ip <- subset(master_7, master_7$loc2 == "IP")   
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  summarise(count = n_distinct(panel)) %>%
  summary()


master_7_op <- subset(master_7, master_7$loc2 == "OP")   
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  summarise(count = n_distinct(panel)) %>%
  summary()

####================================ Figure 1 ================================####
# The results for Figure 1 were obtained by gathering the output from R, making a spreadsheet for input into Joinpoint

# Figure 1a. Overall test numbers per year 
### Adjusted numbers by year
### Due to adjustment by age we obtained the number of tests per year for each age-group for input into the Joinpoint spreadsheet


### Numbers of each test by age-group and year ###

trial_run_age <- master_7
trial_run_age <- trial_run_age %>%
  group_by(age, dts_year, panel) %>%
  summarize(num_test = n())
trial_run_age

trial_run_agegroups <- trial_run_age
trial_run_agegroups <- trial_run_agegroups %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )

dummy_trial_run_agegroups <- trial_run_agegroups %>%
  group_by(age_group, dts_year, panel) %>% # add loc2 if wanting age, sex and panel
  mutate(test_total = sum(num_test))


dummy_trial_run_agegroups_1 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "<1")
dummy_trial_run_agegroups_1 <- dummy_trial_run_agegroups_1[!duplicated(dummy_trial_run_agegroups_1[c("age_group","dts_year","panel")]),]

dummy_trial_run_agegroups_2 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "1-5")
dummy_trial_run_agegroups_2 <- dummy_trial_run_agegroups_2[!duplicated(dummy_trial_run_agegroups_2[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_3 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "6-10")
dummy_trial_run_agegroups_3 <- dummy_trial_run_agegroups_3[!duplicated(dummy_trial_run_agegroups_3[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_4 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "11-15")
dummy_trial_run_agegroups_4 <- dummy_trial_run_agegroups_4[!duplicated(dummy_trial_run_agegroups_4[c("age_group", "dts_year","panel")]),]


### 1b. Number of tests by setting and year 
# Due to adjustment by age we obtained the number of tests for each setting by age group each year 


trial_run_age_setting <- master_7
trial_run_age_setting <- trial_run_age_setting %>%
  group_by(age, dts_year, loc2, panel) %>%
  summarize(num_test = n())
trial_run_age_setting

trial_run_agegroups_setting <- trial_run_age_setting
trial_run_agegroups_setting <- trial_run_agegroups_setting %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )

dummy_trial_run_agegroups_setting <- trial_run_agegroups_setting %>%
  group_by(age_group, dts_year, loc2, panel) %>%
  mutate(test_total = sum(num_test))


dummy_trial_run_agegroups_setting_1 <- subset(dummy_trial_run_agegroups_setting, dummy_trial_run_agegroups_setting$age_group == "<1")
dummy_trial_run_agegroups_setting_1 <- dummy_trial_run_agegroups_setting_1[!duplicated(dummy_trial_run_agegroups_setting_1[c("age_group","dts_year","panel")]),]

dummy_trial_run_agegroups_setting_2 <- subset(dummy_trial_run_agegroups_setting, dummy_trial_run_agegroups_setting$age_group == "1-5")
dummy_trial_run_agegroups_setting_2 <- dummy_trial_run_agegroups_setting_2[!duplicated(dummy_trial_run_agegroups_setting_2[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_setting_3 <- subset(dummy_trial_run_agegroups_setting, dummy_trial_run_agegroups_setting$age_group == "6-10")
dummy_trial_run_agegroups_setting_3 <- dummy_trial_run_agegroups_setting_3[!duplicated(dummy_trial_run_agegroups_setting_3[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_setting_4 <- subset(dummy_trial_run_agegroups_setting, dummy_trial_run_agegroups_setting$age_group == "11-15")
dummy_trial_run_agegroups_setting_4 <- dummy_trial_run_agegroups_setting_4[!duplicated(dummy_trial_run_agegroups_setting_4[c("age_group", "dts_year","panel")]),]


### 1c. Test numbers by sex and year
# To adjust by age we obtained the number of tests for each sex by age group each year 


trial_run_age_sex <- master_7
trial_run_age_sex <- trial_run_age_sex %>%
  group_by(age, dts_year, sex, panel) %>%
  summarize(num_test = n())
trial_run_age_sex

trial_run_agegroups_sex <- trial_run_age_sex
trial_run_agegroups_sex <- trial_run_agegroups_sex %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )

dummy_trial_run_agegroups_sex <- trial_run_agegroups_sex %>%
  group_by(age_group, dts_year, sex, panel) %>%
  mutate(test_total = sum(num_test))


dummy_trial_run_agegroups_sex_1 <- subset(dummy_trial_run_agegroups_sex, dummy_trial_run_agegroups_sex$age_group == "<1")
dummy_trial_run_agegroups_sex_1 <- dummy_trial_run_agegroups_sex_1[!duplicated(dummy_trial_run_agegroups_sex_1[c("age_group","dts_year","sex", "panel")]),]

dummy_trial_run_agegroups_sex_2 <- subset(dummy_trial_run_agegroups_sex, dummy_trial_run_agegroups_sex$age_group == "1-5")
dummy_trial_run_agegroups_sex_2 <- dummy_trial_run_agegroups_sex_2[!duplicated(dummy_trial_run_agegroups_sex_2[c("age_group","dts_year","sex", "panel")]),]

dummy_trial_run_agegroups_sex_3 <- subset(dummy_trial_run_agegroups_sex, dummy_trial_run_agegroups_sex$age_group == "6-10")
dummy_trial_run_agegroups_sex_3 <- dummy_trial_run_agegroups_sex_3[!duplicated(dummy_trial_run_agegroups_sex_3[c("age_group","dts_year","sex", "panel")]),]

dummy_trial_run_agegroups_sex_4 <- subset(dummy_trial_run_agegroups_sex, dummy_trial_run_agegroups_sex$age_group == "11-15")
dummy_trial_run_agegroups_sex_4 <- dummy_trial_run_agegroups_sex_4[!duplicated(dummy_trial_run_agegroups_sex_4[c("age_group","dts_year","sex", "panel")]),]

### To get test numbers by age group and year.

trial_run_age <- master_7
trial_run_age <- trial_run_age %>%
  group_by(age, dts_year, panel) %>%
  summarize(num_test = n())
trial_run_age

trial_run_agegroups <- trial_run_age
trial_run_agegroups <- trial_run_agegroups %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )

dummy_trial_run_agegroups <- trial_run_agegroups %>%
  group_by(age_group, dts_year, panel) %>% 
  mutate(test_total = sum(num_test))


dummy_trial_run_agegroups_1 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "<1")
dummy_trial_run_agegroups_1 <- dummy_trial_run_agegroups_1[!duplicated(dummy_trial_run_agegroups_1[c("age_group","dts_year","panel")]),]

dummy_trial_run_agegroups_2 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "1-5")
dummy_trial_run_agegroups_2 <- dummy_trial_run_agegroups_2[!duplicated(dummy_trial_run_agegroups_2[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_3 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "6-10")
dummy_trial_run_agegroups_3 <- dummy_trial_run_agegroups_3[!duplicated(dummy_trial_run_agegroups_3[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_4 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "11-15")
dummy_trial_run_agegroups_4 <- dummy_trial_run_agegroups_4[!duplicated(dummy_trial_run_agegroups_4[c("age_group", "dts_year","panel")]),]

####================================ Figure 2 ================================####

# To get number of people who had at least one test in each setting, calculated the number per year, and then calculated the proportion by dividing the result each year by the population of 0-15 year olds in Oxfordshire that year (according to ONS estimates)
# Proportion of children who had a test requested in GP each year

master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2005") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2006") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2007") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2008") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2009") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2010") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2011") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2012") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2013") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2014") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2015") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2016") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2017") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2018") %>%
  summarise(count = n_distinct(id))
master_7_gp %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2019") %>%
  summarise(count = n_distinct(id))

# Proportion of children who had a test requested in inpatient setting each year

aster_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2005") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2006") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2007") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2008") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2009") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2010") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2011") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2012") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2013") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2014") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2015") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2016") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2017") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2018") %>%
  summarise(count = n_distinct(id))
master_7_ip %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2019") %>%
  summarise(count = n_distinct(id))

# Proportion of children who had a test requested in outpatient setting each year

master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2005") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2006") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2007") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2008") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2009") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2010") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2011") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2012") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2013") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2014") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2015") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2016") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2017") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2018") %>%
  summarise(count = n_distinct(id))
master_7_op %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2019") %>%
  summarise(count = n_distinct(id))

# Children who received a blood test in ANY setting during the time period (Overall)

master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2005") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2006") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2007") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2008") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2009") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2010") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2011") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2012") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2013") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2014") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2015") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2016") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2017") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2018") %>%
  summarise(count = n_distinct(id))
master_7 %>% 
  group_by(id, sort = TRUE) %>%
  filter(dts_year =="2019") %>%
  summarise(count = n_distinct(id))

####================================ Figure 3 ================================####


panel_crude_age_year_sex_setting_population_agegroups <- panel_crude_age_year_sex_setting_population %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )

iron_dummy <- subset(dummy_panel, dummy_panel$panel== "Iron studies") #### testing to see how the numbers add up. Need to input total_test numbers into the excel spreadsheet! 

dummy_panel <- panel_crude_age_year_sex_setting_population_agegroups %>%
  group_by(age_group, dts_year, sex, loc2, panel) %>%
  mutate(test_total = sum(num_test))

### the panel loc2 (GP) does not mean anything here as it is a combined value

# Count total number of tests per panel (Crude) to determine which ones to do joinpoint regression

dummy_panel_total <- dummy_panel %>%
  group_by(panel) %>%
  summarise(test_total = sum(test_total))

group_by(Category) %>% 
  summarise(Frequency = sum(Frequency))

# The top 20 tests were (in order from most to least)
# Full blood count, Urea and electrolytes, liver function test, CRP, CMP, TFT, Coagulation profile, ESR, Iron studies, IgA, coeliac test, HbA1c, Vitamin D, Medication level, Urine creatinine, Gentamicin level, Vitamin B12, CSF studies, Folate, Lipids

#### Panel counts stratified by age groups and setting

dummy_panel_1 <- subset(dummy_panel, dummy_panel$age_group == "<1")
dummy_panel_1 <- dummy_panel_1[!duplicated(dummy_panel_1[c("dts_year","loc2","panel")]),]

dummy_panel_2 <- subset(dummy_panel, dummy_panel$age_group == "1-5")
dummy_panel_2 <- dummy_panel_2[!duplicated(dummy_panel_2[c("dts_year","loc2","panel")]),]

dummy_panel_3 <- subset(dummy_panel, dummy_panel$age_group == "6-10")
dummy_panel_3 <- dummy_panel_3[!duplicated(dummy_panel_3[c("dts_year","loc2","panel")]),]

dummy_panel_4 <- subset(dummy_panel, dummy_panel$age_group == "11-15")
dummy_panel_4 <- dummy_panel_4[!duplicated(dummy_panel_4[c("dts_year","loc2","panel")]),]

# For each of the age groups and settings, the proportion of each test was calculated by dividing the number of each test by the total number of tests conducted in that age group and setting.
# Details on how each figure was made is published in a separate file. 

####================================ Figure 4 ================================####
# The number of each test, by age-group was determined from the following code
# The filter function was used to determine yearly counts of tests for each age group (as the results were adjusted by age) for the top 25 tests


trial_run_age <- master_7
trial_run_age <- trial_run_age %>%
  group_by(age, dts_year, panel) %>%
  summarize(num_test = n())
trial_run_age

trial_run_agegroups <- trial_run_age
trial_run_agegroups <- trial_run_agegroups %>%
  mutate(
    age_group = dplyr::case_when(
      age < 1 ~ "<1",
      age >= 1 & age <=5 ~ "1-5",
      age >= 6 & age <=10 ~ "6-10",
      age >= 11 & age <=15 ~ "11-15",),
    age_group = factor(age_group,
                       level = c("<1", "1-5", "6-10", "11-15"))
  )

dummy_trial_run_agegroups <- trial_run_agegroups %>%
  group_by(age_group, dts_year, panel) %>% 
  mutate(test_total = sum(num_test))


dummy_trial_run_agegroups_1 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "<1")
dummy_trial_run_agegroups_1 <- dummy_trial_run_agegroups_1[!duplicated(dummy_trial_run_agegroups_1[c("age_group","dts_year","panel")]),]

dummy_trial_run_agegroups_2 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "1-5")
dummy_trial_run_agegroups_2 <- dummy_trial_run_agegroups_2[!duplicated(dummy_trial_run_agegroups_2[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_3 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "6-10")
dummy_trial_run_agegroups_3 <- dummy_trial_run_agegroups_3[!duplicated(dummy_trial_run_agegroups_3[c("age_group","dts_year", "panel")]),]

dummy_trial_run_agegroups_4 <- subset(dummy_trial_run_agegroups, dummy_trial_run_agegroups$age_group == "11-15")
dummy_trial_run_agegroups_4 <- dummy_trial_run_agegroups_4[!duplicated(dummy_trial_run_agegroups_4[c("age_group", "dts_year","panel")]),]

# The filter function was used to find the test we were specifically interested in. 
# Then the total numbers for each test for each year were entered into a excel spreadsheet to be input in the Joinpoint program

# The JoinPoint settings to calculate the age-adjusted rates:
  #  Standard error (calculated) for each point
  #  Log transformation
  #  Up to 2 joinpoints given the number of data points (12—16), best fit model was selected.
  #  Tests of significance - Permutation test (significance level 0.05)
  #  Min number of observations from joinpoint to either end of data = 4
  #  Min number of observations between 2 joinpoints = 2


#================================ The End ================================#
