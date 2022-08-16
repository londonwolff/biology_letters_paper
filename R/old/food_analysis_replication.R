#load libraries--------
library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(BayesFactor)
library(broom)
library(bayestestR)
library(performance)
library(here)
library(papaja)

#upload individual bird sheets off the excel sheet------

basil <- read_excel(here("data/phase_2_nonsocial_complete.xlsx"),
                     sheet = "Basil")
basil <- basil %>%
  select(subject_bird, small_num, large_num, large_side, Choice) %>%
  na.omit(df)

#upload data Uno

dill <- read_excel(here("data/phase_2_nonsocial_complete.xlsx"),
                  sheet = "Dill")

dill$Choice <- dill$Choice...12

dill <- dill %>%
  select(subject_bird, small_num, large_num, large_side, Choice) %>%
  na.omit(df)

#upload data Fern

dart <- read_excel(here("data/phase_2_nonsocial_complete.xlsx"),
                   sheet = "Dart")
dart <- dart %>%
  select(subject_bird, small_num, large_num, large_side, Choice) %>%
  na.omit(df)

#upload data Mork

robin <- read_excel(here("data/phase_2_nonsocial_complete.xlsx"),
                   sheet = "Robin")
robin <- robin %>%
  select(subject_bird, small_num, large_num, large_side, Choice) %>%
  na.omit(df)

#upload data Prudence

rooster <- read_excel(here("data/phase_2_nonsocial_complete.xlsx"),
                       sheet = "Rooster")
rooster <- rooster %>%
  select(subject_bird, small_num, large_num, large_side, Choice) %>%
  na.omit(df)

#upload data Dumbledore

saffron <- read_excel(here("data/phase_2_nonsocial_complete.xlsx"),
                         sheet = "Saffron")
saffron <- saffron %>%
  select(subject_bird, small_num, large_num, large_side, Choice) %>%
  na.omit(df)


#Data Clean UP----------------------------------------------
#bind them all together into 1 sheet

df_food_rep <- bind_rows(dart, dill, rooster, saffron)

#Creating binary choice, ratio, and pair columns

df_food_rep$pair <- paste(df_food_rep$small_num, df_food_rep$large_num, sep = "/")

df_food_rep$large_choice <- ifelse(df_food_rep$Choice == df_food_rep$large_side, "Y", "N")

df_food_rep$choice_num <- ifelse(df_food_rep$large_choice == "Y", 1, 0)

#changing data types of columns in our data frame

df_food_rep$large_num <- as.numeric(df_food_rep$large_num)
df_food_rep$small_num <- as.numeric(df_food_rep$small_num)

df_food_rep$subject_bird <- as.factor(df_food_rep$subject_bird)
df_food_rep$pair <- as.factor(df_food_rep$pair)

#create bird sex column

female_birds_food_rep <- c("saffron", "robin")

df_food_rep$bird_sex <- ifelse(df_food_rep$subject_bird %in% female_birds_food_rep, "female", "male")


#creating the ratio and difference columns

options(digits = 2)

df_food_rep$ratio <- df_food_rep$small_num/df_food_rep$large_num

df_food_rep$difference <-  df_food_rep$large_num - df_food_rep$small_num


#Summarizing Data------------------

summary_food_rep <- df_food_rep %>%
  group_by(choice_num) %>%
  summarise(n())

pairsummary_food_rep <- df_food_rep %>%
  group_by(pair) %>%
  summarise(perc = mean(choice_num)*100)

birdsummary_food_rep <- df_food_rep %>%
  group_by(subject_bird) %>%
  summarise(n = n(),
            perc = mean(choice_num)*100,
            sd = sd(choice_num)) %>%
  mutate(se=sd/sqrt(8)) %>%
  mutate(ic=se*qt((1-0.05)/2 + .5, n-1))

diffsummary_food_rep <- df_food_rep %>%
  group_by(difference) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_food_rep <- df_food_rep %>%
  group_by(difference, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_means_food_rep <- diff_bird_summary_food_rep %>%
  group_by(difference) %>%
  summarise(perc = mean(perc))

ratiosummary_food_rep <- df_food_rep %>%
  group_by(ratio) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_food_rep <- df_food_rep %>%
  group_by(ratio, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_means_food_rep <- ratio_bird_summary_food_rep %>%
  group_by(ratio) %>%
  summarise(perc = mean(perc))

diff_ratio_summary_food_rep <- df_food_rep %>%
  group_by(difference, ratio) %>%
  summarise(perc = mean(choice_num)*100)


#Data Analysis----------------------

#1 sample t-test -------
#First want to test if birds choose larger over smaller

large_pref_ttest_food_rep <- t.test(birdsummary_food_rep$perc, mu= 50, alternative = "two.sided")

large_pref_ttest_food_rep

large_pref_ttest_bf_food_rep <- ttestBF(birdsummary_food_rep$perc, mu= 50, alternative = "two.sided")


# Model selection ----------------------------------------------

#Now we've looked at things individually lets put it all together folks!

# First find best-fitting random effect model, then test for fixed effect predictors
# Process is backward by eliminating weakest terms sequentially, starting with full model, until only significant effects remain
# Nested model comparisons (likelihood ratio tests using anova command) are used to select best fitting models

# Random effects Structure selection------------------

random_effect_intercept_food_rep <- glm(formula = choice_num ~ 1, data = df_food_rep, family = binomial())

random_effect_sub_food_rep <- glmer(formula = choice_num ~ (1 | subject_bird), data = df_food_rep, family = binomial()) #only subject bird as random effect

random_effect_pair_food_rep <- glmer(formula = choice_num ~ (1 | pair), data = df_food_rep, family = binomial())

random_effect_sub_pair_food_rep <- glmer(formula = choice_num ~ (1|subject_bird) + (1|pair), data = df_food_rep, family = binomial) #subject bird and pair as random effect

nonsocial_random_comparison_rep <- compare_performance(random_effect_intercept_food_rep, random_effect_sub_food_rep, random_effect_pair_food_rep, random_effect_sub_pair_food_rep )

nonsocial_random_bayes_comparison_rep <- bayesfactor_models(random_effect_sub_food_rep, random_effect_pair_food_rep, random_effect_sub_pair_food_rep, denominator = random_effect_intercept_food_rep)

### Extract BICs
bic1_random_rep <- nonsocial_random_comparison_rep$BIC[1]  # empty random model
bic2_random_rep <- nonsocial_random_comparison_rep$BIC[2]
bic3_random_rep <- nonsocial_random_comparison_rep$BIC[3]
bic4_random_rep <- nonsocial_random_comparison_rep$BIC[4]

# Convert BIC values to Bayes factor
bf_values_random_rep <- bic_to_bf(c(bic1_random_rep, bic2_random_rep, bic3_random_rep, bic4_random_rep ), denominator = bic1_random_rep)

nonsocial_random_comparison_rep_table <- nonsocial_random_comparison_rep %>%
  mutate(BF = bf_values_random_rep)


# Fixed effects----------------------
#create fixed-effects models

nonsocial_full_rep <- glm(formula = choice_num ~ difference * ratio, data = df_food_rep, family = binomial)  #full model

nonsocial_no_interaction_rep <- glm(formula = choice_num ~ difference + ratio , data = df_food_rep, family = binomial) #changing ratio from a term that adds an interaction to a main effect without the interaction.

nonsocial_difference_rep <- glm(formula = choice_num ~ difference, data = df_food_rep, family = binomial) #drop ratio from the model, difference is the main IV

nonsocial_ratio_rep <- glm(formula = choice_num ~ ratio, data = df_food_rep, family = binomial) #drop difference from the model and add ratio back in as the main IV

## Likelihood ratio tests for model comparison

nonsocial_fixed_comparison_rep <- compare_performance(random_effect_intercept_food_rep, nonsocial_ratio_rep, nonsocial_difference_rep, nonsocial_no_interaction_rep, nonsocial_full_rep)

nonsocial_fixed_bayes_comparison_rep <- bayesfactor_models(nonsocial_ratio_rep, nonsocial_difference_rep, nonsocial_no_interaction_rep, nonsocial_full_rep, denominator = random_effect_intercept_food_rep)


## Bayes factors for fixed effects
### Extract BICs
bic1_food_rep <- nonsocial_fixed_comparison_rep$BIC[1]  # empty random model
bic2_food_rep <- nonsocial_fixed_comparison_rep$BIC[2]
bic3_food_rep <- nonsocial_fixed_comparison_rep$BIC[3]
bic4_food_rep <- nonsocial_fixed_comparison_rep$BIC[4]
bic5_food_rep <- nonsocial_fixed_comparison_rep$BIC[5]

### Convert BICs to BFs

# Convert BIC values to Bayes factor
bf_values_food_rep <- bic_to_bf(c(bic1_food_rep, bic2_food_rep, bic3_food_rep, bic4_food_rep, bic5_food_rep), denominator = bic1_food_rep)

nonsocial_fixed_comparison_rep_table <- nonsocial_fixed_comparison_rep %>%
  mutate(BF = bf_values_food_rep)

#Calculating within subject confidence intervals----------------

confidence_intv_difference_food_rep <- wsci(data = diff_bird_summary_food_rep,
                                        id = "subject_bird",
                                        dv = "perc",
                                        factors = "difference",
                                        method = "Morey")


diff_bird_summary_means_food_rep$upper <- confidence_intv_difference_food_rep$perc + diff_bird_summary_means_food_rep$perc

diff_bird_summary_means_food_rep$lower <-  diff_bird_summary_means_food_rep$perc - confidence_intv_difference_food_rep$perc

confidence_intv_ratio_food_rep <- wsci(data=
                                     ratio_bird_summary_food_rep,
                                   id = "subject_bird",
                                   dv = "perc",
                                   factors= "ratio",
                                   method = "Morey")

ratio_bird_summary_means_food_rep$upper <- confidence_intv_ratio_food_rep$perc + ratio_bird_summary_means_food_rep$perc

ratio_bird_summary_means_food_rep$lower <-  ratio_bird_summary_means_food_rep$perc - confidence_intv_ratio_food_rep$perc

#GRAPHING TIME!!!-----------------------------------

#Bird Mean preference. Hypothesis 1 graph

bird_graph_food_rep <- ggplot(data = birdsummary_food_rep, aes(x=subject_bird, y= perc)) +
  labs( y="Percent larger choosen")+
  geom_bar(stat = 'identity')+
  theme_bw(base_size = 22)+
  theme(
    axis.title.x = element_blank()
  )+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,75)

bird_graph_food_rep


#Ratio Graph with the ratio in proportion form. as in 1 "/" 3 etc.

pair_graph_food_rep <- ggplot(data = pairsummary_food_rep, aes(x=pair, y= perc)) +
  labs(title = "Food Preference by Pair", y="% of trials larger option choosen", x = "Pair")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

pair_graph_food_rep

# Ratio graph with the ratio in numeric form so 0.13. Some conditions collapsed together.

ratio_graph_food_rep <- ggplot(data = ratiosummary_food_rep, aes(x=ratio, y= perc)) +
  labs( y="% of trials larger option choosen", x = "Ratio")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

ratio_graph_food_rep

#Difference Graph

diff_graph_food_rep <- ggplot(data = diffsummary_food_rep, aes(x=(difference), y = perc)) +
  labs(y="% of trials larger option choosen", x = "Difference")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

diff_graph_food_rep

#Graph Ratio on the X axis with Difference as the grouping variable

ratio_difference_graph_food_rep <- ggplot(data = diff_ratio_summary_food_rep, aes(x=ratio, y= perc)) +
  labs( y="Percent larger choosen", x = "Ratio")+
  geom_line(aes(colour = factor(difference)),
            size = 1)+
  geom_point(aes(colour = factor(difference)),
             size = 2)+
  labs(color = "difference")+
  theme_bw(base_size = 24)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = c(0.85, 0.75),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(.3, 'cm'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)+
  scale_x_continuous(breaks = c(.17, .2, .25,.33,.4, .5, .6, .67, .75, .8, .83))

ratio_difference_graph_food_rep



#Graph Ratio grouped by subject bird

ratio_bird_graph_food_rep <- ggplot(data = ratio_bird_summary_food_rep, aes(x=ratio, y= perc)) +
  labs(y="Percent larger choosen", x = "Ratio")+
  geom_point(data = ratiosummary_food_rep, size = 2)+
  geom_errorbar(data = ratio_bird_summary_means_food_rep, aes(x=ratio, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject_bird, color = subject_bird), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)+
  scale_x_continuous(breaks = c(.17, .2, .25,.33,.4, .5, .6, .67, .75, .8, .83))
#scale_x_continuous(c(0.15, 0.85, .1))
#xlim(0.15, 0.85)

ratio_bird_graph_food_rep



#Graph Difference by subject bird

diff_bird_graph_food_rep <- ggplot(data = diff_bird_summary_food_rep, aes(x=difference, y= perc)) +
  geom_point(data = diffsummary_food_rep, size = 2) +
  geom_errorbar(data = diff_bird_summary_means_food_rep, aes(x=difference, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject_bird, color = subject_bird), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs( y="Percent larger choosen", x = "Difference")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)

diff_bird_graph_food_rep




