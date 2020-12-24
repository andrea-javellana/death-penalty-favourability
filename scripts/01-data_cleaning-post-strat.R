#### Preamble ####
# Purpose: Prepare and clean the 2018 and 2015 ACS data downloaded from IPUMS
# Author: Andrea Javellana
# Data: 22 December 2020
# Contact: andrea.javellana@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from IPUMS and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("C:.../data/usa_00005.dta")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
#names(ACS)

#Select for variables interested in
ACS <- 
  raw_data %>% 
  select(
    stateicp,# (UNFINISHED)
    sex,  #
    age, #
    race, #
    hispan,#
    educd,#
    ftotinc) #



# Clean responses that are unecessary and don't correspond to survey
ACS <- subset(ACS, age > 17) #keep responses of age over 17  
ACS <- subset(ACS, age < 96) # keep responses of age under 95 
ACS <- subset(ACS, age != 93) # discard 91,93,95 y/o
ACS <- subset(ACS, age != 91) # 
ACS <- subset(ACS, age != 95) #  
ACS <- subset(ACS, ftotinc < 9999999) # keep eligible responses


# EDUCATION

ACS$educd = cut(ACS$educd,c(0,26,61,64,71,101,116))
levels(ACS$educd) = c('less than high school', 'some high school',
                      'completed high school', 'some post-secondary',
                      'post-secondary degree', 'post-graduate degree'
)
ACS$educd <- as.numeric(ACS$educd)



# INCOME

ACS$ftotinc = cut(ACS$ftotinc,c(-16400,9999,19999,29999,39999,49999,
                                74999,99999, 149999, 9999999))

levels(ACS$ftotinc) = c('Less than $10,000', '10 to under $20,000', '$20 to under $30,000',
                        '30 to under $40,000', '40 to under $50,000', '50 to under $75,000', 
                        '75 to under $100,000','100 to under $150,000 [OR]','$150,000 or more', NA
)
table(ACS$ftotinc)




# Add the labels
ACS <- labelled::to_factor(ACS)


# AGE

table(ACS$age)
ACS$age <- as.numeric(as.character(ACS$age))
ACS$age[is.na(ACS$age)] <- 90 
# forsome reason 90 y/o are the only ones labelled as '90(90+ born in ....)'
table(ACS$age)




# RACE

# Simplifying/grouping ACS races
ACS$race <- gsub("american indian or alaska native", "other", ACS$race)
ACS$race <- gsub("japanese", "other", ACS$race)
ACS$race <- gsub("other asian or pacific islander", "other", ACS$race)
ACS$race <- gsub("other race, nec", "other", ACS$race)
ACS$race <- gsub("two major races", "other", ACS$race)
ACS$race <- gsub("three or more major races", "other", ACS$race)
ACS$race <- gsub("chinese", "other", ACS$race)
ACS$race <- gsub("black/african american/negro", "black", ACS$race)
table(ACS$race)

#Making HISPAN a binary variable (hispanic vs not hispanic)
ACS$hispan <- gsub("mexican", "hispanic", ACS$hispan)
ACS$hispan <- gsub("puerto rican", "hispanic", ACS$hispan)
ACS$hispan <- gsub("cuban", "hispanic", ACS$hispan)
ACS$hispan <- gsub("other", "hispanic", ACS$hispan)
table(ACS$hispan)

#RACE including hispanics as a race
ACS$race <- ACS$race
ACS$race[ACS$hispan == 'hispanic'] <- "hispanic"
table(ACS$race)

# Discard HISPAN column
ACS <- 
  ACS %>% 
  select(
    stateicp,# (UNFINISHED)
    sex,  #done
    age, #done
    race, #done
    educd,#done
    ftotinc) #done


# STATE
ACS$stateicp <- labelled::to_factor(ACS$stateicp)
# Switch to character vector and then back to factor to remove unused levels 
# and sort new levels in alphabetical order
ACS$stateicp <- as.character(ACS$stateicp)
ACS$stateicp <- as.factor(ACS$stateicp)
# Assign levels a number from 1 to 51 corresponding to state and make a numeric variable
levels(ACS$stateicp) <- c(1:51)
ACS$stateicp <- as.numeric(ACS$stateicp)

table(ACS$stateicp)



#re-add labels 
ACS <- labelled::to_factor(ACS)

# RENAME COLUMN NAMES (VARIABLES) TO MATCH WITH SURVEY
ACS <- rename(ACS, c("education"="educd", "state"="stateicp", "income" = 'ftotinc',
                      ))
names(ACS)
write_csv(ACS, "C:.../data/ACS2018.csv")


##########################################################################
##########################################################################
# repeat for 2015 ACS

# Read in the raw data. 
raw_data <- read_dta("C:.../data/usa_00008.dta")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
#names(ACS)

#Select for variables interested in
ACS <- 
  raw_data %>% 
  select(
    stateicp,# (UNFINISHED)
    sex,  #
    age, #
    race, #
    hispan,#
    educd,#
    ftotinc) #



# Clean responses that are unecessary and don't correspond to survey
ACS <- subset(ACS, age > 17) #keep responses of age over 17  
ACS <- subset(ACS, age < 96) # keep responses of age under 98 
ACS <- subset(ACS, age != 88)
ACS <- subset(ACS, age != 90)
ACS <- subset(ACS, age != 93) # discard 88,70,91,93,95 y/o
ACS <- subset(ACS, age != 94) # 
ACS <- subset(ACS, ftotinc < 9999999) # keep eligible responses



# EDUCATION
ACS$educd = cut(ACS$educd,c(0,26,61,64,71,101,116))
levels(ACS$educd) = c('less than high school', 'some high school',
                      'completed high school', 'some post-secondary',
                      'post-secondary degree', 'post-graduate degree'
)
ACS$educd <- as.numeric(ACS$educd)



# INCOME

ACS$ftotinc = cut(ACS$ftotinc,c(-16400,9999,19999,29999,39999,49999,
                                74999,99999, 149999, 9999999))

levels(ACS$ftotinc) = c('Less than $10,000', '10 to under $20,000', '$20 to under $30,000',
                        '30 to under $40,000', '40 to under $50,000', '50 to under $75,000', 
                        '75 to under $100,000','100 to under $150,000 [OR]','$150,000 or more', NA
)
table(ACS$ftotinc)




# Add the labels
ACS <- labelled::to_factor(ACS)


# AGE

table(ACS$age)
ACS$age <- as.numeric(as.character(ACS$age))
ACS$age[is.na(ACS$age)] <- 90 
# forsome reason 90 y/o are the only ones labelled as '90(90+ born in ....)'
table(ACS$age)




# RACE

# Simplifying/grouping ACS races
ACS$race <- gsub("american indian or alaska native", "other", ACS$race)
ACS$race <- gsub("japanese", "other", ACS$race)
ACS$race <- gsub("other asian or pacific islander", "other", ACS$race)
ACS$race <- gsub("other race, nec", "other", ACS$race)
ACS$race <- gsub("two major races", "other", ACS$race)
ACS$race <- gsub("three or more major races", "other", ACS$race)
ACS$race <- gsub("chinese", "other", ACS$race)
ACS$race <- gsub("black/african american/negro", "black", ACS$race)
table(ACS$race)

#Making HISPAN a binary variable (hispanic vs not hispanic)
ACS$hispan <- gsub("mexican", "hispanic", ACS$hispan)
ACS$hispan <- gsub("puerto rican", "hispanic", ACS$hispan)
ACS$hispan <- gsub("cuban", "hispanic", ACS$hispan)
ACS$hispan <- gsub("other", "hispanic", ACS$hispan)
table(ACS$hispan)

#RACE including hispanics as a race
ACS$race <- ACS$race
ACS$race[ACS$hispan == 'hispanic'] <- "hispanic"
table(ACS$race)

# Discard HISPAN column
ACS <- 
  ACS %>% 
  select(
    stateicp,# (UNFINISHED)
    sex,  #done
    age, #done
    race, #done
    educd,#done
    ftotinc) #done


# STATE
ACS$stateicp <- labelled::to_factor(ACS$stateicp)
# Switch to character vector and then back to factor to remove unused levels 
# and sort new levels in alphabetical order
ACS$stateicp <- as.character(ACS$stateicp)
ACS$stateicp <- as.factor(ACS$stateicp)
# Assign levels a number from 1 to 51 corresponding to state and make a numeric variable
levels(ACS$stateicp) <- c(1:51)
ACS$stateicp <- as.numeric(ACS$stateicp)

table(ACS$stateicp)



#re-add labels 
ACS <- labelled::to_factor(ACS)

# RENAME COLUMN NAMES (VARIABLES) TO MATCH WITH SURVEY
ACS <- rename(ACS, c("education"="educd", "state"="stateicp", "income" = 'ftotinc',
))
names(ACS)
# Path to your repository
write_csv(ACS, "C:.../data/ACS2015.csv")
         