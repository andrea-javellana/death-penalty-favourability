
install.packages("lme4")
install.packages("stargazer")
install.packages("plyr")
library(lme4)
library(tidyverse)
library(stargazer)
detach(package:plyr)


# Read in the clean survey data and clean post-strat data.
may18 <- read_csv(("outputs/may18.csv"), col_types = cols( 
                                                             race = col_character(),
                                                             sex = col_factor(),
                                                             income = col_factor(),
                                                             age = col_factor(),
                                                             Death_Penalty = col_number(),
                                                             education = col_number(),
                                                             state = col_number()
)
)

ACS <- read_csv(("outputs/ACS.csv"), col_types = cols( 
                                                           race = col_character(),
                                                           sex = col_factor(),
                                                           income = col_factor(),
                                                           age = col_factor(),
                                                           education = col_number(),
                                                           state = col_number()
)
)



# SELECTED MODEL
model2 <- glmer(Death_Penalty ~ (1|state) + education + factor(age) + factor(sex) + 
                  factor(race), data=may18, family=binomial(link="logit"))

summary(model2)


# Estimate likelihood to FAVOUR penalty of each ACS respondent 
ACS$estimate <- model2 %>% predict(newdata=ACS, type="response") %>% round(digits=3)



# Construct table of cells and add estimates for each level
cells <- plyr::count(ACS[complete.cases(ACS),] %>% select(state, education, age, sex, race))
cells$estimate <- model2 %>% predict(newdata = cells, type="response")








# Change state variable back into state names
#ACS$state <- as.factor(ACS$state)
#levels(ACS$state) <- append(state.name, values="District of Columbia", after=8)

# Create summary tables for post-stratification outcome grouped by each variable
# for use in constructing plots
states <- ACS %>% group_by(state) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                sd=round(sd(estimate), digits=3), 
                                                lower=round(quantile(estimate, 0.025), 
                                                            digits=3), 
                                                upper=round(quantile(estimate, 0.975), digits=3))


sex <- ACS %>% group_by(sex) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                 sd=round(sd(estimate), digits=3), 
                                                 lower=round(quantile(estimate, 0.025), 
                                                             digits=3), 
                                                 upper=round(quantile(estimate, 0.975), digits=3))
sex <- cbind(sex, may18 %>% group_by(sex) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


age <- ACS %>% group_by(age) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                           sd=round(sd(estimate), digits=3), 
                                           lower=round(quantile(estimate, 0.025), 
                                                       digits=3), 
                                           upper=round(quantile(estimate, 0.975), digits=3))
age <- cbind(age, may18 %>% group_by(age) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


race <- ACS %>% group_by(race) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                                 sd=round(sd(estimate), digits=3), 
                                                                 lower=round(quantile(estimate, 0.025), 
                                                                             digits=3), 
                                                                 upper=round(quantile(estimate, 0.975), digits=3))
race <- cbind(race, may18 %>% group_by(race) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))

education <- ACS %>% group_by(education) %>% summarise(post_strat_mean=round(mean(estimate), digits=4), 
                                                       sd=round(sd(estimate), digits=3), 
                                                       lower=round(quantile(estimate, 0.025), 
                                                                   digits=3), 
                                                       upper=round(quantile(estimate, 0.975), digits=3))
education <- cbind(education, may18 %>% group_by(education) %>% summarise(survey_mean=round(mean(Death_Penalty), digits=4)) %>% select(survey_mean))


# Survey data summaries
may18$state <- as.factor(may18$state)
levels(may18$state) <- append(state.name, values="District of Columbia", after=8)
may18_states <- may18 %>% group_by(state) %>% summarise(mean=round(mean(Death_Penalty), digits=4))


mean(may18$Death_Penalty)
mean(ACS$estimate)

overall <- cbind(states, raw_data_mean=may18_states$mean)






# TABLES
ACS %>% summarise(DP=mean(ACS$estimate), 
                  'Standard Deviation' = sd(ACS$estimate)) %>% 
  rbind(summarise(may18, DP = mean(may18$Death_Penalty), 
                  "Standard Deviation" = sd(may18$Death_Penalty)))

# PLOTS
install.packages("usmap")
library(usmap)

# Republican proportion of vote by state on map
plot_usmap(data=states, values="post_strat_mean") + 
  scale_fill_continuous(name= "Darkly coloured states indicates higher proportion of those in favour", 
                        low="white", high="black") + theme(legend.position="top") +
  ggtitle("Death Penalty Favourability by State 2018 (Post-Stratified)")
ggsave("visuals/2018map.pdf")






















# State post-stratification estimates versus survey estimates
ggplot(states, aes(x=reorder(state, post_strat_mean), y=post_strat_mean)) + geom_point() + geom_point(data=may18_states, aes(x=state, y=mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("State") + 
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="Survey versus Post-Stratified Estimates by State")
ggsave("visuals/survey_versus_poststrat_STATE.pdf")





par(mfrow=c(2,2))


# Race post-stratification estimates
race$race <- str_to_title(race$race)
race %>% ggplot(aes(x=race, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=race, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=race, y=survey_mean), color="dark grey") +
  theme(axis.text.x = element_text(angle=90)) + xlab("Racial or Ethnic Identity") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Race or Ethnicity, Survey versus Post-Stratified Estimates")
ggsave("visuals/survey_versus_poststrat_RACE.pdf")

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
  ggtitle("Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Educational Attainment, Survey versus Post-Stratified Estimates")
ggsave("visuals/survey_versus_poststrat_EDUCATION.pdf")


# sex post-stratification estimates
sex$sex <- str_to_sentence(sex$sex)
sex %>% ggplot(aes(x=sex, y=post_strat_mean)) + geom_path(group=1) + 
  geom_point() + geom_path(aes(x=sex, y=survey_mean, group=1), 
                           linetype="dashed") +
  geom_point(aes(x=sex, y=survey_mean), color="dark grey") +
  xlab("sex") + ylab("") +
  geom_hline(yintercept=0.5) + 
  ggtitle("Predicted Proportion in favour of Death Penalty") +
  labs(subtitle="By Sex, Survey versus Post-Stratified Estimates")
ggsave("visuals/survey_versus_poststrat_SEX.pdf")













