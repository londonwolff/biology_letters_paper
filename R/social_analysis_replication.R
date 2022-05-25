#load libraries------------
library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(BayesFactor)
library(broom)
library(bayestestR)
library(here)
library(papaja)
library(BMA)
library(performance)
library(patchwork)

#input data
data_phase_rep <- read_csv(here("data/phase_2_social_complete.csv"))

#Data Clean Up-----------------------------------

df_social_rep <- data_phase_rep %>%
  select(id, subject_bird, small_num, large_num, large_side, choice, initials, date, time_in, time_out) %>%
  drop_na()

df_social_rep <- df_social_rep[order(df_social_rep$subject_bird),]

#Creating binary choice columns and difference columns

df_social_rep$pair <- paste(df_social_rep$small_num, df_social_rep$large_num, sep = "/")

df_social_rep$large_choice <- ifelse(df_social_rep$choice == df_social_rep$large_side, "Y", "N")

df_social_rep$difference <-  df_social_rep$large_num - df_social_rep$small_num

df_social_rep$choice_num <- ifelse(df_social_rep$large_choice == "Y", 1, 0)

#creating the ratio columns

options(digits = 2)

df_social_rep$small_num_corrected <- ifelse(df_social_rep$small_num == "0", 0.1, df_social_rep$small_num)

df_social_rep$ratio <- df_social_rep$small_num_corrected / df_social_rep$large_num

#standardize bird capitalizations and take out Baloo because he escaped too often for his data to be usable

df_social_rep <- df_social_rep %>%
  mutate(subject_bird = str_to_title(subject_bird))

#create bird sex column

female_birds_social <- c("Uno")

df_social_rep$bird_sex <- ifelse(df_social_rep$subject_bird %in% female_birds_social, "female", "male")


df_social_rep$subject_bird <- as.factor(df_social_rep$subject_bird)
df_social_rep$pair <- as.factor(df_social_rep$pair)

#ordering columns and printing the data frame

df_social_rep <- df_social_rep %>%
  select(id, subject_bird, bird_sex, everything())


#Summarizing Data-------------------------

summary_social_rep <- df_social_rep %>%
  group_by(choice_num) %>%
  summarise(n())

pairsummary_social_rep <- df_social_rep %>%
  group_by(pair) %>%
  summarise(perc = mean(choice_num)*100)

birdsummary_social_rep <- df_social_rep %>%
  group_by(subject_bird) %>%
  summarise(n = n(),
            perc = mean(choice_num)*100,
            sd = sd(choice_num)) %>%
  mutate(se=sd/sqrt(10)) %>%
  mutate(ic=se*qt((1-0.05)/2 + .5, n-1))

diffsummary_social_rep <- df_social_rep %>%
  group_by(difference) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_social_rep <- df_social_rep %>%
  group_by(difference, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_means_social_rep <- diff_bird_summary_social %>%
  group_by(difference) %>%
  summarise(perc = mean(perc))

ratiosummary_social_rep <- df_social_rep %>%
  group_by(ratio) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_social_rep <- df_social_rep %>%
  group_by(ratio, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_means_social_rep <- ratio_bird_summary_social %>%
  group_by(ratio) %>%
  summarise(perc = mean(perc))

diff_ratio_summary_social_rep <- df_social_rep %>%
  group_by(difference, ratio) %>%
  summarise(perc = mean(choice_num)*100)

#Data Analysis------------------------------------

#1 sample t-test ---------
#test if birds choose larger over smaller

large_pref_ttest_social_rep <- t.test(birdsummary_social$perc, mu= 50, alternative = "two.sided")

large_pref_ttest_social_rep

large_pref_ttest_bf_social_rep <- ttestBF(birdsummary_social$perc, mu= 50, alternative = "two.sided")

# Model selection: Subject_bird Random effect ----------------------------------------------

#Now that I've looked at things individually lets put it all together folks!

# First find best-fitting random effect model, then test for fixed effect predictors
# Process is backward by eliminating weakest terms sequentially, starting with full model, until only significant effects remain
# Nested model comparisons (likelihood ratio tests using anova command) are used to select best fitting models


# Random effects Structure selection------------------

random_effect_intercept_social_rep <- glm(formula = choice_num ~ 1, data = df_social_rep, family = binomial())

random_effect_sub_social_rep <- glmer(formula = choice_num ~ (1 | subject_bird), data = df_social_rep, family = binomial()) #only subject bird as random effect

random_effect_pair_social_rep <- glmer(formula = choice_num ~ (1 | pair), data = df_social_rep, family = binomial())

random_effect_sub_pair_social_rep <- glmer(formula = choice_num ~ (1|subject_bird) + (1|pair), data = df_social_rep, family = binomial) #subject bird and pair as random effect

social_random_comparison_rep <- compare_performance(random_effect_intercept_social_rep, random_effect_sub_social_rep, random_effect_pair_social_rep, random_effect_sub_pair_social_rep)

social_random_bayes_comparison_rep <- bayesfactor_models(random_effect_sub_social_rep, random_effect_pair_social_rep, random_effect_sub_pair_social_rep, denominator = random_effect_intercept_social_rep)

### Extract BICs
bic1_random_s_rep <- social_random_comparison_rep$BIC[1]
bic2_random_s_rep <- social_random_comparison_rep$BIC[2]
bic3_random_s_rep <- social_random_comparison_rep$BIC[3]
bic4_random_s_rep <- social_random_comparison_rep$BIC[4]

# Convert BIC values to Bayes factor
bf_values_random_s_rep <- bic_to_bf(c(bic1_random_s_rep, bic2_random_s_rep, bic3_random_s_rep, bic4_random_s_rep ), denominator = bic1_random_s_rep)

social_random_comparison_rep_table_rep <- social_random_comparison_rep %>%
  mutate(BF = bf_values_random_s_rep)


# Fixed effects------------------
#create fixed-effects models

base_model_rep <- glm(formula = choice_num ~ 1, data = df_social_rep, family = binomial) #simplest model to use as comparison

social_full_rep <- glm(formula = choice_num ~ difference * ratio, data = df_social_rep, family = binomial())  #full model

social_no_interaction_rep <- glm(formula = choice_num ~ difference + ratio, data = df_social_rep, family = binomial) #changing ratio from a term that adds an interaction to a main effect without the interaction.

social_difference_rep <- glm(formula = choice_num ~ difference, data = df_social_rep, family = binomial) #drop ratio from the model, difference is the main IV

social_ratio_rep <- glm(formula = choice_num ~ ratio, data = df_social_rep, family = binomial) #drop difference from the model and add ratio back in as the main IV

## Likelihood ratio tests for model comparison

social_model_comparison_rep <- compare_performance(base_model_rep,  social_ratio_rep, social_difference_rep, social_no_interaction_rep, social_full_rep)

social_fixed_bayes_comparison_rep <- bayesfactor_models(social_ratio_rep, social_difference_rep, social_no_interaction_rep, social_full_rep, denominator = base_model_rep)

## Likelihood ratio tests for model comparison

## Bayes factors for fixed effects
### Extract BICs
bic1_social_rep <- social_model_comparison_rep$BIC[1]
bic2_social_rep <- social_model_comparison_rep$BIC[2]
bic3_social_rep <- social_model_comparison_rep$BIC[3]
bic4_social_rep <- social_model_comparison_rep$BIC[4]
bic5_social_rep <- social_model_comparison_rep$BIC[5]

# Convert BIC values to Bayes factor
bf_values_social_rep <- bic_to_bf(c(bic1_social_rep, bic2_social_rep, bic3_social_rep, bic4_social_rep, bic5_social_rep), denominator = bic1_social_rep)

social_model_comparison_rep_table <- social_model_comparison_rep %>%
  mutate(BF = bf_values_social_rep)

#comparing top 2 BF values models together

bf_difference_social_rep <- social_model_comparison_rep_table$BF[3]/social_model_comparison_rep_table$BF[2]

#Calculating within subject confidence intervals----------------

confidence_intv_difference_social_rep <- wsci(data = diff_bird_summary_social,
                                          id = "subject_bird",
                                          dv = "perc",
                                          factors = "difference",
                                          method = "Morey")


diff_bird_summary_means_social$upper <- confidence_intv_difference_social_rep$perc + diff_bird_summary_means_social$perc

diff_bird_summary_means_social$lower <-  diff_bird_summary_means_social$perc - confidence_intv_difference_social_rep$perc

confidence_intv_ratio_social_rep <- wsci(data= ratio_bird_summary_social,
                                     id = "subject_bird",
                                     dv = "perc",
                                     factors= "ratio",
                                     method = "Morey")

ratio_bird_summary_means_social$upper <- confidence_intv_ratio_social_rep$perc + ratio_bird_summary_means_social$perc

ratio_bird_summary_means_social$lower <-  ratio_bird_summary_means_social$perc - confidence_intv_ratio_social_rep$perc


#GRAPHING TIME!!!-----------------------------

#Bird Mean preference. Hypothesis 1 graph

bird_graph_social_rep <- ggplot(data = birdsummary_social_rep, aes(x=subject_bird, y= perc)) +
  labs( y="Percent larger choosen")+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(x=subject_bird, ymin = perc - 1.5, ymax = perc + 1.5))+
  theme_bw(base_size = 22)+
  theme(
    axis.title.x = element_blank()
  )+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,75)

bird_graph_social_rep


#Ratio Graph with the ratio in proportion form. as in 1 "/" 3 etc.

pair_graph_social_rep <- ggplot(data = pairsummary_social_rep, aes(x=pair, y= perc)) +
  labs(title = "Food Preference by Pair", y="% of trials larger option choosen", x = "Pair")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

pair_graph_social_rep


# Ratio graph with the ratio in numeric form

ratio_graph_social_rep <- ggplot(data = ratiosummary_social_rep, aes(x=ratio, y= perc)) +
  labs(title = "Social Preference by Ratio", y="% of trials larger option choosen", x = "Ratio")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

ratio_graph_social_rep

#Difference Graph

diff_graph_social_rep <- ggplot(data = diffsummary_social_rep, aes(x=(difference), y = perc)) +
  # geom_smooth(method = "lm") +
  labs(title = "Social Preference by Difference", y="% of trials larger option choosen", x = "Difference")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

diff_graph_social_rep

#Graph Ratio on the X axis with Difference as the grouping variable


ratio_difference_graph_social_rep <- ggplot(data = diff_ratio_summary_social_rep, aes(x=ratio, y= perc)) +
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

ratio_difference_graph_social_rep

#Graph Ratio grouped by suject bird


ratio_bird_graph_social_rep <- ggplot(data = ratio_bird_summary_social_rep, aes(x=ratio, y= perc)) +
  labs(y="Percent larger choosen", x = "Ratio")+
  geom_point(data = ratiosummary_social_rep, size = 2)+
  geom_errorbar(data = ratio_bird_summary_means_social, aes(x=ratio, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject_bird, color = subject_bird), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(10,100)+
  scale_x_continuous(breaks = c(.17, .2, .25,.33,.4, .5, .6, .67, .75, .8, .83))
#scale_x_continuous(c(0.15, 0.85, .1))
#xlim(0.15, 0.85)

ratio_bird_graph_social_rep

#Graph Difference by subject bird

diff_bird_graph_social_rep <- ggplot(data = diff_bird_summary_social_rep, aes(x=difference, y= perc)) +
  geom_point(data = diffsummary_social_rep, size = 2) +
  geom_errorbar(data = diff_bird_summary_means_social, aes(x=difference, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject_bird, color = subject_bird), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs( y="Percent larger choosen", x = "Difference")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(10,100)

diff_bird_graph_social_rep



