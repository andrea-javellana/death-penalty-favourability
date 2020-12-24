#### Preamble ####
# Purpose: Prepare and clean the 2018 and 2015 Political Survey data downloaded from Pew Research Centre
# Author: Andrea Javellana
# Data: 22 December 2020
# Contact: andrea.javellana@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the datasets from pewresearch.org and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
install.packages("tidyverse")
install.packages("haven")

library(tidyverse) 
library(haven)




# read in the data file downloaded from Pew Research
may18 <- read_sav("C:.../data/May18 public.sav", 
                  user_na = TRUE) 


may18 <- 
  may18 %>% 
  select(
    age,# 
    state,#symbol
    sex,  #
    income, #family income before tax
    racethn,
    educ,#
    q70) #



#taking out 'dont know/ refused' answers in each variable
may18 <- subset(may18, age < 99)
may18<- subset(may18, racethn < 5)
may18<- subset(may18, income < 10)
may18<- subset(may18, educ < 9)
may18<- subset(may18, q70 < 9)


# VARIABLE OF INTEREST: q70
# "Do you favour or oppose the death penalty for convicted murderers?"
# Make binary: favour, oppose
table(may18$q70)
may18$q70 = cut(may18$q70,c(0,2,4))
levels(may18$q70) = c('in favour', 'opposed'
)
may18$q70 <- as.numeric(may18$q70)

# Assign a vote for OPPOSED a value of 0
may18$q70[may18$q70 == 2] <- 0
table(may18$q70)




# EDUCATION
may18$educ = cut(may18$educ,c(0,1,2,3,4,7,8))
levels(may18$educ) = c('less than high school', 'some high school',
                      'completed high school', 'some post-secondary',
                      'post-secondary degree', 'post-graduate degree'
)
may18$educ <- as.numeric(may18$educ)



# SEX
may18$sex <- cut(may18$sex, c(0,1,2))
levels(may18$sex) <- c("male", "female")


#put dataset observations in words 
may18 <- labelled::to_factor(may18)



# RACE
# renaming race options to white, black, hispanic, or other
may18$racethn <- gsub("White non-Hispanic", "white", may18$racethn)
may18$racethn <- gsub("Black non-Hispanic", "black", may18$racethn)
may18$racethn <- gsub("Other", "other", may18$racethn)
may18$racethn <- gsub("Hispanic", "hispanic", may18$racethn)






# STATE
table(may18$state)
# STATE
# Replace state abbreviations with state names, adding "DC" to the 
# state.abb vector and "district of columbia" to the state.name vector
may18$state <- append(state.name, values=c("district of columbia"))[match(
  may18$state, append(state.abb, values=c("DC")))]
# Make all state names lowercase
may18$state <- tolower(may18$state)
# Assign state names a numeric value between 1 and 51 in alphabetical order
may18$state <- as.factor(may18$state)
levels(may18$state) <- c(1:51)
may18$state <- as.numeric(may18$state)


#Add labels
may18 <- labelled::to_factor(may18)



#make matching column names
may18 <- rename(may18, c("education"="educ", "Death_Penalty" = 'q70',
               'race' = 'racethn' ))
names(may18)

#create clean output file
write_csv(may18, "C.../data/may18.csv")

table(may18$state)


######################################################################################
######################################################################################
# repeat for 2015 pew dataset


# read in the data file downloaded from Pew Research
mar15 <- read_sav("C:.../data/March15 public.sav", 
                  user_na = TRUE) 


mar15 <- 
  mar15 %>% 
  select(
    age,# 
    sex,  #
    income,
    state,#family income before tax
    racethn,
    educ2,#
    q60) # death penalty question



#taking out 'dont know/ refused' answers in each variable
mar15 <- subset(mar15, age < 96)
mar15<- subset(mar15, racethn < 5)
mar15<- subset(mar15, income < 10)
mar15<- subset(mar15, educ2 < 9)
mar15<- subset(mar15, q60 < 9)


# VARIABLE OF INTEREST: q60
# "Do you favour or oppose the death penalty for convicted murderers?"
# Make binary: favour, oppose
table(mar15$q60)
mar15$q60 = cut(mar15$q60,c(0,2,4))
levels(mar15$q60) = c('in favour', 'opposed'
)
mar15$q60 <- as.numeric(mar15$q60)

# Assign a vote for OPPOSED a value of 0
mar15$q60[mar15$q60 == 2] <- 0
table(mar15$q60)




# EDUCATION
mar15$educ2 = cut(mar15$educ2,c(0,1,2,3,4,7,8))
levels(mar15$educ2) = c('less than high school', 'some high school',
                       'completed high school', 'some post-secondary',
                       'post-secondary degree', 'post-graduate degree'
)
mar15$educ2 <- as.numeric(mar15$educ2)



# SEX
mar15$sex <- cut(mar15$sex, c(0,1,2))
levels(mar15$sex) <- c("male", "female")


#put dataset observations in words 
mar15 <- labelled::to_factor(mar15)



# RACE
# renaming race options to white, black, hispanic, or other
table(mar15$racethn)
mar15$racethn <- gsub("White, non-Hisp", "white", mar15$racethn)
mar15$racethn <- gsub("Black, non-Hisp", "black", mar15$racethn)
mar15$racethn <- gsub("Other", "other", mar15$racethn)
mar15$racethn <- gsub("Hispanic", "hispanic", mar15$racethn)



# STATE
mar15$state <- as.numeric(mar15$state)
table(mar15$state)

#Add labels
mar15 <- labelled::to_factor(mar15)



#make matching column names
mar15 <- rename(mar15, c("education"="educ2", "Death_Penalty" = 'q60',
                         'race' = 'racethn' ))
names(mar15)

#create clean output file
write_csv(mar15, "C:../data/mar15.csv")


table(mar15$state)





