# installing/loading the latest installr package:
#install.packages("installr"); library(installr) # install+load installr

#updateR() # updating R.

#ONLY ONLY ONLY for first loading of a package, you must install

#load packages
library(tidyverse) # for ggplot2,tibble,tidyr,readr,purrr,dplyr,stringr,forcats
library(haven) # for reading Stata files
library(labelled) # for working with variable labels
library(magrittr) # for extra %$% pipe operator
library(readxl) # for reading the excel file
library(writexl) # for saving something as an Excel file
library(sas7bdat) # for opening a SAS file
library(dplyr) # gives rename function

#Set Working directory -----------------------------------------------------------------------------
setwd("C:/Users/artol/Documents/Fl19/CapstoneR")

#Name loaded file as "jail"
Jail1 <- read_sas("test1.sas7bdat")
Jail2 <- read_sas("test2.sas7bdat")

#Inspect the data
view(Jail1)
view(Jail2)

#save the r-file as an Excel file, so that we can manipulate data in spreadsheet form
#the package did not load propertly, so the following code did not work :(
write_xlsx(x = Jail1, path = "jail1.xlsx", col_names = TRUE)
write_xlsx(x = Jail2, path = "jail2.xlsx", col_names = TRUE)

#Open new document that was saved as Excel file. Open 3rd tab only 
jail<-read_xlsx("jail1.xlsx", sheet = 3)
summary(jail)

#DATA CLEANING ------------------------------------------------------------------------------------

#****Dependent Variable***
#First, create a dichotomous variable called "mental_illness"; create this by aggregating
#all the mental health variables. If ANY=1, the variable is marked 1. Otherwise, it is 0
jail2 <- jail %>%
  mutate(mental_illness=ifelse(V2022Depression == 1|V2023Bipolar==1|V2026OtherAnxiety==1|
                                 V2024PsychoticDisorder==1|V2027PersonalityDis==1|V2025PTSD==1|
                                 V2028Other==1, 1, 0))
names(jail2) #to get the variable/column names, so that I don't have to keep going back and forth
view(jail2$mental_illness)

#create crosstabs to inspect some of the variables to ensure the coding was completed successfully
table(jail2$mental_illness,jail2$V2022Depression)
xtabs(~jail2$mental_illness + jail2$V2023Bipolar + jail2$V2024PsychoticDisorder)

#****Effect Modifier***
#We are going to recode the disability variable, so that 1=yes, 0=no
#this is the code in base R
jail2$Disability <- ifelse(jail2$V2054Disability == 1,1,0)

#Check the data to make sure it was done correctly (baseR)
table(jail2$Disability, jail2$V2054Disability)

#Repeat the same code in tidyverse for cleaner look
jail3 <- jail2 %>%
  mutate(Disability2=ifelse(V2054Disability==1, 1, 0))
view(jail3$Disability)

#****Independent Variables



