
install.packages("lme4")
install.packages("stargazer")
install.packages("plyr")
install.packages("usmap")
library(expss)
library(usmap)
library(lme4)
library(tidyverse)
library(stargazer)
#detach(package:plyr)


# Read in the clean survey data and clean post-strat data.
may18 <- read_csv(("C:.../data/may18.csv"), col_types = cols( 
                                                             race = col_character(),
                                                             sex = col_factor(),
                                                             income = col_factor(),
                                                             age = col_factor(),
                                                             Death_Penalty = col_number(),
                                                             education = col_number(),
                                                             state = col_number()
)
)

ACS2018 <- read_csv(("C:.../data/ACS2018.csv"), col_types = cols( 
                                                           race = col_character(),
                                                           sex = col_factor(),
                                                           income = col_factor(),
                                                           age = col_factor(),
                                                           education = col_number(),
                                                           state = col_number()
)
)



# SELECTED MODEL
model_2018 <- glmer(Death_Penalty ~ (1|state) + education + factor(age) + factor(sex) + 
                  factor(race), data=may18, family=binomial(link="logit"))

summary(model_2018)


# Estimate likelihood to FAVOUR penalty of each ACS respondent 
ACS2018$estimate <- model_2018 %>% predict(newdata=ACS2018, type="response") %>% round(digits=3)



# Construct table of cells and add estimates for each level
cells <- plyr::count(ACS2018[complete.cases(ACS2018),] %>% select(state, education, age, sex, race))
cells$estimate <- model_2018 %>% predict(newdata = cells, type="response")



# Change state variable back into state names
ACS2018$state <- as.factor(ACS2018$state)
levels(ACS2018$state) <- append(state.name, values="District of Columbia", after=8)




# Create summary tables for post-stratification outcome grouped by each variable
# for use in constructing plots
states2018 <- ACS2018 %>% group_by(state) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                sd=round(sd(estimate), digits=3), 
                                                lower=round(quantile(estimate, 0.025), 
                                                            digits=3), 
                                                upper=round(quantile(estimate, 0.975), digits=3))


sex <- ACS2018 %>% group_by(sex) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                 sd=round(sd(estimate), digits=3), 
                                                 lower=round(quantile(estimate, 0.025), 
                                                             digits=3), 
                                                 upper=round(quantile(estimate, 0.975), digits=3))
sex <- cbind(sex, may18 %>% group_by(sex) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


age <- ACS2018 %>% group_by(age) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                           sd=round(sd(estimate), digits=3), 
                                           lower=round(quantile(estimate, 0.025), 
                                                       digits=3), 
                                           upper=round(quantile(estimate, 0.975), digits=3))
age <- cbind(age, may18 %>% group_by(age) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


race <- ACS2018 %>% group_by(race) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                                 sd=round(sd(estimate), digits=3), 
                                                                 lower=round(quantile(estimate, 0.025), 
                                                                             digits=3), 
                                                                 upper=round(quantile(estimate, 0.975), digits=3))
race <- cbind(race, may18 %>% group_by(race) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))

education <- ACS2018 %>% group_by(education) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                       sd=round(sd(estimate), digits=3), 
                                                       lower=round(quantile(estimate, 0.025), 
                                                                   digits=3), 
                                                       upper=round(quantile(estimate, 0.975), digits=3))
education <- cbind(education, may18 %>% group_by(education) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


# Survey data summaries
#may18$state <- as.factor(may18$state)
levels(may18$state) <- append(state.name, values="District of Columbia", after=8)
may18_states <- may18 %>% group_by(state) %>% summarise(mean=round(mean(Death_Penalty), digits=4))


mean(may18$Death_Penalty)
mean(ACS2018$estimate)

overall2018 <- cbind(states2018, raw_data_mean=may18_states$mean)




# TABLES
ACS2018 %>% summarise(DP=mean(ACS2018$estimate), 
                  'Standard Deviation' = sd(ACS2018$estimate)) %>% 
  rbind(summarise(may18, DP = mean(may18$Death_Penalty), 
                  "Standard Deviation" = sd(may18$Death_Penalty)))

# PLOTS


# proportion of favour by state on map
states2018$state <- as.character(states2018$state)
plot_usmap(data=states2018, values="post_strat_mean") + 
  scale_fill_continuous(name= "Darkly coloured states indicates higher proportion of those in favour", 
                        low="white", high="black") + theme(legend.position="top") +
  ggtitle("Death Penalty Favourability by State 2018 (Post-Stratified)")
ggsave("C:.../visuals/2018map.pdf")





# State post-stratification estimates versus survey estimates
ggplot(states2018, aes(x=reorder(state, post_strat_mean), y=post_strat_mean)) + geom_point() + geom_point(data=may18_states, aes(x=state, y=mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("State") + 
  geom_hline(yintercept=0.5) + 
  ggtitle("2018 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="Survey versus Post-Stratified Estimates by State")
ggsave("C:.../visuals/2018survey_versus_poststrat_STATE.pdf")





par(mfrow=c(2,2))


# Race post-stratification estimates
race$race <- str_to_title(race$race)
race %>% ggplot(aes(x=race, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=race, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=race, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Racial or Ethnic Identity") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("2018 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Race or Ethnicity, Survey versus Post-Stratified Estimates")
ggsave("C:.../visuals/2018survey_versus_poststrat_RACE.pdf")

# Education post-stratification estimates
education$education <- as.factor(education$education)
levels(education$education) <- c('Less than high school', 'Some high school',
                                 'Completed high school', 'Some post-secondary',
                                 'Post-secondary degree', 'Post-graduate degree')
education %>% ggplot(aes(x=education, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=education, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=education, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + 
  xlab("Highest Level of Education Completed") + 
  ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("2018 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Educational Attainment, Survey versus Post-Stratified Estimates")
ggsave("C:.../visuals/2018survey_versus_poststrat_EDUCATION.pdf")


# sex post-stratification estimates
sex$sex <- str_to_sentence(sex$sex)
sex %>% ggplot(aes(x=sex, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=sex, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=sex, y=survey_mean), color="dark grey") +
  xlab("sex") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("2018 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Sex, Survey versus Post-Stratified Estimates")
ggsave("C:.../visuals/2018survey_versus_poststrat_SEX.pdf")









###########################################################
###########################################################
# Repeat for 2015 data




# Read in the clean survey data and clean post-strat data.
mar15 <- read_csv(("C:.../data/mar15.csv"), col_types = cols( 
  race = col_character(),
  sex = col_factor(),
  income = col_factor(),
  age = col_factor(),
  Death_Penalty = col_number(),
  education = col_number(),
  state = col_number()
)
)

ACS2015 <- read_csv(("C:.../data/ACS2015.csv"), col_types = cols( 
  race = col_character(),
  sex = col_factor(),
  income = col_factor(),
  age = col_factor(),
  education = col_number(),
  state = col_number()
)
)



# SELECTED MODEL
model_2015 <- glmer(Death_Penalty ~ (1|state) + education + factor(age) + factor(sex) + 
                      factor(race), data=mar15, family=binomial(link="logit"))

summary(model_2015)


# Estimate likelihood to FAVOUR penalty of each ACS respondent 
ACS2015$estimate <- model_2015 %>% predict(newdata=ACS2015, type="response") %>% round(digits=3)



# Construct table of cells and add estimates for each level
cells <- plyr::count(ACS2015[complete.cases(ACS2015),] %>% select(state, education, age, sex, race))
cells$estimate <- model_2015 %>% predict(newdata = cells, type="response")



# Change state variable back into state names
ACS2015$state <- as.factor(ACS2015$state)
levels(ACS2015$state) <- append(state.name, values="District of Columbia", after=8)




# Create summary tables for post-stratification outcome grouped by each variable
# for use in constructing plots
states2015 <- ACS2015 %>% group_by(state) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                    sd=round(sd(estimate), digits=3), 
                                                    lower=round(quantile(estimate, 0.025), 
                                                                digits=3), 
                                                    upper=round(quantile(estimate, 0.975), digits=3))


sex <- ACS2015 %>% group_by(sex) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                               sd=round(sd(estimate), digits=3), 
                                               lower=round(quantile(estimate, 0.025), 
                                                           digits=3), 
                                               upper=round(quantile(estimate, 0.975), digits=3))
sex <- cbind(sex, mar15 %>% group_by(sex) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


age <- ACS2015 %>% group_by(age) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                               sd=round(sd(estimate), digits=3), 
                                               lower=round(quantile(estimate, 0.025), 
                                                           digits=3), 
                                               upper=round(quantile(estimate, 0.975), digits=3))
age <- cbind(age, mar15 %>% group_by(age) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


race <- ACS2015 %>% group_by(race) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                 sd=round(sd(estimate), digits=3), 
                                                 lower=round(quantile(estimate, 0.025), 
                                                             digits=3), 
                                                 upper=round(quantile(estimate, 0.975), digits=3))
race <- cbind(race, mar15 %>% group_by(race) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))

education <- ACS2015 %>% group_by(education) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                           sd=round(sd(estimate), digits=3), 
                                                           lower=round(quantile(estimate, 0.025), 
                                                                       digits=3), 
                                                           upper=round(quantile(estimate, 0.975), digits=3))
education <- cbind(education, mar15 %>% group_by(education) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


# Survey data summaries
#mar15$state <- as.factor(mar15$state)
levels(mar15$state) <- append(state.name, values="District of Columbia", after=8)
mar15_states <- mar15 %>% group_by(state) %>% summarise(mean=round(mean(Death_Penalty), digits=4))


mean(mar15$Death_Penalty)
mean(ACS2015$estimate)

overall2015 <- cbind(states2015, raw_data_mean=mar15_states$mean)






# TABLES
ACS2015 %>% summarise(DP=mean(ACS2015$estimate), 
                      'Standard Deviation' = sd(ACS2015$estimate)) %>% 
  rbind(summarise(mar15, DP = mean(mar15$Death_Penalty), 
                  "Standard Deviation" = sd(mar15$Death_Penalty)))

# PLOTS

# proportion of favour by state on map
states2015$state <- as.character(states2015$state)
plot_usmap(data=states2015, values="post_strat_mean") + 
  scale_fill_continuous(name= "Darkly coloured states indicates higher proportion of those in favour", 
                        low="white", high="black") + theme(legend.position="top") +
  ggtitle("Death Penalty Favourability by State 2015 (Post-Stratified)")
ggsave("C:.../visuals/2015map.pdf")





# State post-stratification estimates versus survey estimates
ggplot(states2015, aes(x=reorder(state, post_strat_mean), y=post_strat_mean)) + geom_point() + geom_point(data=mar15_states, aes(x=state, y=mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("State") + 
  geom_hline(yintercept=0.5) + 
  ggtitle("2015 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="Survey versus Post-Stratified Estimates by State")
ggsave("C:.../visuals/2015survey_versus_poststrat_STATE.pdf")





par(mfrow=c(2,2))


# Race post-stratification estimates
race$race <- str_to_title(race$race)
race %>% ggplot(aes(x=race, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=race, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=race, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Racial or Ethnic Identity") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("2015 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Race or Ethnicity, Survey versus Post-Stratified Estimates")
ggsave("C:.../visuals/2015survey_versus_poststrat_RACE.pdf")

# Education post-stratification estimates
education$education <- as.factor(education$education)
levels(education$education) <- c('Less than high school', 'Some high school',
                                 'Completed high school', 'Some post-secondary',
                                 'Post-secondary degree', 'Post-graduate degree')
education %>% ggplot(aes(x=education, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=education, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=education, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + 
  xlab("Highest Level of Education Completed") + 
  ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("2015 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Educational Attainment, Survey versus Post-Stratified Estimates")
ggsave("C:.../visuals/2015survey_versus_poststrat_EDUCATION.pdf")


# sex post-stratification estimates
sex$sex <- str_to_sentence(sex$sex)
sex %>% ggplot(aes(x=sex, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=sex, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=sex, y=survey_mean), color="dark grey") +
  xlab("sex") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("2015 Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Sex, Survey versus Post-Stratified Estimates")
ggsave(".../visuals/2015survey_versus_poststrat_SEX.pdf")



# FINDING ACERAGE FAVOURBILITY PER STATE 
#rename to specify estimates
states2018$post_strat_mean_2018 <- states2018$post_strat_mean
states2015$post_strat_mean_2015 <- states2015$post_strat_mean

# New dataset with favouibility estimates from 2018 and 2018
states_comb <- cbind(states2018, states2015)

#find the difference, put into absolute values, save as a new column
states_comb$mean_change <- abs(states_comb$post_strat_mean_2018 - states_comb$post_strat_mean_2015)
states_comb$percent_change <- (states_comb$post_strat_mean_2018 - states_comb$post_strat_mean_2015)*100

#get the average, then multiply by 100 to get percent 
mean(states_comb$mean_change)*100

States_data <- data.frame("state" = states_comb$state, 
                  "% change in favourability" = states_comb$percent_change, 
                  "2015 favourability" = states_comb$post_strat_mean_2015,
                  "2018 favourability" = states_comb$post_strat_mean_2018)
head(States_data)
write_csv(States_data, "C:.../data/States_data.csv")






#SUMMARY of Post-strat means

summary<- ACS2015 %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                 sd=round(sd(estimate), digits=3), 
                                 lower=round(quantile(estimate, 0.025), 
                                             digits=3), 
                                 upper=round(quantile(estimate, 0.975), digits=3)) %>% 
  rbind(ACS2018 %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                              sd=round(sd(estimate), digits=3), 
                              lower=round(quantile(estimate, 0.025), 
                                          digits=3), 
                              upper=round(quantile(estimate, 0.975), digits=3)))%>% 
  cbind(year = c(2015, 2018))

head(summary)
write_csv(summary, "C:.../data/summary.csv")

