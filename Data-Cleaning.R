
# Running Needed Libraries 
library(readxl)
library(tidyverse)
library(writexl)
library(gtsummary)
library(janitor)
library(broom)
library(rstatix)
library(flextable)

# Reading and Uploading Excel Files 
# The Data Was separated into three excel files each has the data of a single group
high_group1 <- read_excel(path = "group 1.high.xlsx")
low_group2 <- read_excel(path = "group 2 low.xlsx")
placebo_group3 <- read_excel(path = "group 3 placeb.xlsx")

# Adding a column distinguishing groups in each excel file 
high_group1 <- high_group1 %>% mutate(Group = "High Intensity")
low_group2 <- low_group2 %>% mutate(Group = "Low Intensity")
placebo_group3 <- placebo_group3 %>% mutate(Group = "Placebo")

# Removing unneeded text that was present besides a numeric value and removing extra spaces 
high_group1$weight <- high_group1$weight %>% str_remove("KG") %>% str_trim() %>% as.numeric()

# Combingin and stacking data on top of each other to one data frame 
All_Data <- bind_rows(high_group1, low_group2, placebo_group3)

# Creating an ID column as it would be most needed when doing paried tests 
All_Data <- All_Data %>% mutate(name = row_number()) %>% rename("ID" = name)

# Renaming Column Names to be clearer 
All_Data <- All_Data %>% rename("Marital Status" = `marr/single`,
                    "Height" = length,
                    "RMDQ Before" = `R M befor(range =0-24)`,
                    "RMDQ After" = `R M after (0_ 24)`,
                    "ST Before" = `ST- befor`,
                    "ST After" = `ST- after`,
                    "OSW Before" = `OSW- before (range= 0 -100%)`,
                    "OSW After" = `OSW- after (range= 0 -100%)`,
                    "VAS-Activity Before" = `VAS (range= 0- 10)- ACTIVITY before`,
                    "VAS-Activity After" = `VAS (range= 0- 10)- ACTIVITY after`,
                    "VAS-Rest After" = `VAS (range= 0- 10)- REST after`,
                    "VAS-Rest Before" = `VAS (range= 0- 10)- REST before`)


# Converting Needed Variables into a factor 
All_Data <- All_Data %>% mutate(across(c("gender", "Marital Status", "Group"), ~ factor(.x)))

# Removing extra space and unneeded text 
All_Data <- All_Data %>% mutate(across(c("ST After", "ST Before"), ~str_remove(.x, "cm|CM")))

# Converting needed variables to numeric 
All_Data <- All_Data %>% mutate(across(c("ST After", "ST Before"), ~as.numeric(.x)))

# Creating a BMI column 
All_Data <- All_Data %>% mutate(BMI = round(weight/(Height/100)^2, digits = 0))

# Adjusting Location of BMI column to be at the beginninng just for aesthetics 
All_Data <- All_Data %>% relocate(BMI, .after = Height)

# Specifiying specific factor values by recode 
All_Data$`Marital Status` <- All_Data$`Marital Status` %>% fct_recode("Married" = "M",
                                         "Single" = "S")
All_Data$gender <- All_Data$gender %>% fct_recode("Female" = "F",
                               "Male" = "M")

