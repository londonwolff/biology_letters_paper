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
data_phase_1 <- read_csv(here("data/phase_2_social_complete.csv"))

#Data Clean Up-----------------------------------

df_social <- data_phase_1 %>%
  select(id, subject_bird, small_num, large_num, large_side, choice, initials, date, time_in, time_out) %>%
  drop_na()

df_social <- df_social[order(df_social$subject_bird),]


#Creating binary choice columns and difference columns

df_social$pair <- paste(df_social$small_num, df_social$large_num, sep = "/")

df_social$large_choice <- ifelse(df_social$choice == df_social$large_side, "Y", "N")

df_social$difference <-  df_social$large_num - df_social$small_num

df_social$choice_num <- ifelse(df_social$large_choice == "Y", 1, 0)

#creating the ratio columns

options(digits = 2)

df_social$small_num_corrected <- ifelse(df_social$small_num == "0", 0.1, df_social$small_num)

df_social$ratio <- df_social$small_num_corrected / df_social$large_num

#standardize bird capitalizations and take out Baloo because he escaped too often for his data to be usable

df_social <- df_social %>%
  mutate(subject_bird = str_to_title(subject_bird))

#create bird sex column

female_birds_social <- c("Uno")

df_social$bird_sex <- ifelse(df_social$subject_bird %in% female_birds_social, "female", "male")


df_social$subject_bird <- as.factor(df_social$subject_bird)
df_social$pair <- as.factor(df_social$pair)

#ordering columns and printing the data frame

df_social <- df_social %>%
  select(id, subject_bird, bird_sex, everything())


#Summarizing Data-------------------------

summary_social <- df_social %>%
  group_by(choice_num) %>%
  summarise(n())

pairsummary_social <- df_social %>%
  group_by(pair) %>%
  summarise(perc = mean(choice_num)*100)

birdsummary_social <- df_social %>%
  group_by(subject_bird) %>%
  summarise(n = n(),
            perc = mean(choice_num)*100,
            sd = sd(choice_num)) %>%
  mutate(se=sd/sqrt(10)) %>%
  mutate(ic=se*qt((1-0.05)/2 + .5, n-1))

diffsummary_social <- df_social %>%
  group_by(difference) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_social <- df_social %>%
  group_by(difference, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_means_social <- diff_bird_summary_social %>%
  group_by(difference) %>%
  summarise(perc = mean(perc))

ratiosummary_social <- df_social %>%
  group_by(ratio) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_social <- df_social %>%
  group_by(ratio, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_means_social <- ratio_bird_summary_social %>%
  group_by(ratio) %>%
  summarise(perc = mean(perc))

diff_ratio_summary_social <- df_social %>%
  group_by(difference, ratio) %>%
  summarise(perc = mean(choice_num)*100)

#Data Analysis------------------------------------

#1 sample t-test ---------
#test if birds choose larger over smaller

large_pref_ttest_social <- t.test(birdsummary_social$perc, mu= 50, alternative = "two.sided")

large_pref_ttest_social

large_pref_ttest_bf_social <- ttestBF(birdsummary_social$perc, mu= 50, alternative = "two.sided")

# Model selection: Subject_bird Random effect ----------------------------------------------

#Now that I've looked at things individually lets put it all together folks!

# First find best-fitting random effect model, then test for fixed effect predictors
# Process is backward by eliminating weakest terms sequentially, starting with full model, until only significant effects remain
# Nested model comparisons (likelihood ratio tests using anova command) are used to select best fitting models


# Random effects Structure selection------------------

random_effect_intercept_social <- glm(formula = choice_num ~ 1, data = df_social, family = binomial())

random_effect_sub_social <- glmer(formula = choice_num ~ (1 | subject_bird), data = df_social, family = binomial()) #only subject bird as random effect

random_effect_pair_social <- glmer(formula = choice_num ~ (1 | pair), data = df_social, family = binomial())

random_effect_sub_pair_social <- glmer(formula = choice_num ~ (1|subject_bird) + (1|pair), data = df_social, family = binomial) #subject bird and pair as random effect

social_random_comparison <- compare_performance(random_effect_intercept_social, random_effect_sub_social, random_effect_pair_social, random_effect_sub_pair_social)

social_random_bayes_comparison <- bayesfactor_models(random_effect_sub_social, random_effect_pair_social, random_effect_sub_pair_social, denominator = random_effect_intercept_social)

### Extract BICs
bic1_random_s <- social_random_comparison$BIC[1]
bic2_random_s <- social_random_comparison$BIC[2]
bic3_random_s <- social_random_comparison$BIC[3]
bic4_random_s <- social_random_comparison$BIC[4]

# Convert BIC values to Bayes factor
bf_values_random_s <- bic_to_bf(c(bic1_random_s, bic2_random_s, bic3_random_s, bic4_random_s ), denominator = bic1_random_s)

social_random_comparison_table <- social_random_comparison %>%
  mutate(BF = bf_values_random_s)


# Fixed effects------------------
#create fixed-effects models

base_model <- glm(formula = choice_num ~ 1, data = df_social, family = binomial) #simplest model to use as comparison

social_full <- glm(formula = choice_num ~ difference * ratio, data = df_social, family = binomial())  #full model

social_no_interaction <- glm(formula = choice_num ~ difference + ratio, data = df_social, family = binomial) #changing ratio from a term that adds an interaction to a main effect without the interaction.

social_difference <- glm(formula = choice_num ~ difference, data = df_social, family = binomial) #drop ratio from the model, difference is the main IV

social_ratio <- glm(formula = choice_num ~ ratio, data = df_social, family = binomial) #drop difference from the model and add ratio back in as the main IV

## Likelihood ratio tests for model comparison

social_model_comparison <- compare_performance(base_model,  social_ratio, social_difference, social_no_interaction, social_full)

social_fixed_bayes_comparison <- bayesfactor_models(social_ratio, social_difference, social_no_interaction, social_full, denominator = base_model)

## Likelihood ratio tests for model comparison

## Bayes factors for fixed effects
### Extract BICs
bic1_social <- social_model_comparison$BIC[1]
bic2_social <- social_model_comparison$BIC[2]
bic3_social <- social_model_comparison$BIC[3]
bic4_social <- social_model_comparison$BIC[4]
bic5_social <- social_model_comparison$BIC[5]

# Convert BIC values to Bayes factor
bf_values_social <- bic_to_bf(c(bic1_social, bic2_social, bic3_social, bic4_social, bic5_social), denominator = bic1_social)

social_model_comparison_table <- social_model_comparison %>%
  mutate(BF = bf_values_social)

#comparing top 2 BF values models together

bf_difference_social <- social_model_comparison_table$BF[3]/social_model_comparison_table$BF[2]

#Calculating within subject confidence intervals----------------

confidence_intv_difference_social <- wsci(data = diff_bird_summary_social,
                                          id = "subject_bird",
                                          dv = "perc",
                                          factors = "difference",
                                          method = "Morey")


diff_bird_summary_means_social$upper <- confidence_intv_difference_social$perc + diff_bird_summary_means_social$perc

diff_bird_summary_means_social$lower <-  diff_bird_summary_means_social$perc - confidence_intv_difference_social$perc

confidence_intv_ratio_social <- wsci(data= ratio_bird_summary_social,
                                     id = "subject_bird",
                                     dv = "perc",
                                     factors= "ratio",
                                     method = "Morey")

ratio_bird_summary_means_social$upper <- confidence_intv_ratio_social$perc + ratio_bird_summary_means_social$perc

ratio_bird_summary_means_social$lower <-  ratio_bird_summary_means_social$perc - confidence_intv_ratio_social$perc


#GRAPHING TIME!!!-----------------------------

#Bird Mean preference. Hypothesis 1 graph

bird_graph_social <- ggplot(data = birdsummary_social, aes(x=subject_bird, y= perc)) +
  labs( y="Percent larger choosen")+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(x=subject_bird, ymin = perc - 1.5, ymax = perc + 1.5))+
  theme_bw(base_size = 22)+
  theme(
    axis.title.x = element_blank()
  )+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,75)

bird_graph_social


#Ratio Graph with the ratio in proportion form. as in 1 "/" 3 etc.

pair_graph_social <- ggplot(data = pairsummary_social, aes(x=pair, y= perc)) +
  labs(title = "Food Preference by Pair", y="% of trials larger option choosen", x = "Pair")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

pair_graph_social


# Ratio graph with the ratio in numeric form

ratio_graph_social <- ggplot(data = ratiosummary_social, aes(x=ratio, y= perc)) +
  labs(title = "Social Preference by Ratio", y="% of trials larger option choosen", x = "Ratio")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

ratio_graph_social

#Difference Graph

diff_graph_social <- ggplot(data = diffsummary_social, aes(x=(difference), y = perc)) +
  # geom_smooth(method = "lm") +
  labs(title = "Social Preference by Difference", y="% of trials larger option choosen", x = "Difference")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

diff_graph_social

#Graph Ratio on the X axis with Difference as the grouping variable


ratio_difference_graph_social <- ggplot(data = diff_ratio_summary_social, aes(x=ratio, y= perc)) +
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

ratio_difference_graph_social

#Graph Ratio grouped by suject bird


ratio_bird_graph_social <- ggplot(data = ratio_bird_summary_social, aes(x=ratio, y= perc)) +
  labs(y="Percent larger choosen", x = "Ratio")+
  geom_point(data = ratiosummary_social, size = 2)+
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

ratio_bird_graph_social

#Graph Difference by subject bird

diff_bird_graph_social <- ggplot(data = diff_bird_summary_social, aes(x=difference, y= perc)) +
  geom_point(data = diffsummary_social, size = 2) +
  geom_errorbar(data = diff_bird_summary_means_social, aes(x=difference, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject_bird, color = subject_bird), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs( y="Percent larger choosen", x = "Difference")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(10,100)

diff_bird_graph_social

#Saving plots for use in merp
#library(patchwork)
#ratio_bird_graph_social + diff_bird_graph_social + ratio_difference_graph_social + plot_annotation(tag_levels = "a") + plot_layout(ncol = 2)
#ggsave("figures/social_figures.png", scale = 2, height = 5, width = 7)

#Appendix Tables------------

factorial_pairs_df <- data.frame(Pair=c("1/2","1/3","1/4","1/5","1/6","2/3","2/4","2/5","2/6","3/4","3/5","3/6","4/5","4/6","5/6"),
                                 Ratio=c("0.50","0.33","0.25","0.20","0.17","0.67","0.50","0.40","0.33","0.75","0.60","0.50","0.80","0.67","0.83"),
                                 Difference=c("1","2","3","4","5","1","2","3","4","1","2","3","1","2","1"))

library(formattable)

fp_table <- formattable(factorial_pairs_df,
                        align =c("c","c","c"))


#Model Selection table

fixed_effect_df <- data.frame(Model = c("Intercept Only Model", "Ratio Only Model","Difference Only Model", "Both Fixed Effects, No Interaction", "Both Fixed Effects, With Interaction"),
                              Formula = c("Choice~1","Choice~Ratio","Choice~Difference","Choice~Ratio+Difference","Choice~Ratio*Difference"))


fixed_effect_structure_table <- formattable(fixed_effect_df,
                                            align =c("l","l"))

random_effect_df <- data.frame(Model = c("Intercept Only Model", "Subject Only Model","Pair Only Model", "Both Subject and Pair"),
                               Formula = c("Choice~1","Choice~(1|Subject)","Choice~(1|Pair)","Choice~(1|Subject)+(1|Pair)"))

random_effect_structure_table <- formattable(random_effect_df,
                                             align =c("l","l"))

#Making BF tables fro random & fixed for food and social

random_models <- c("(1|Subject)","(1|Pair)","(1|Subject)+(1|Pair)")

fixed_models <- c( "ratio", "difference","difference + ratio", "difference * ratio")

options(digits = 2)

random_social_bf_df <- data.frame(Model = random_models,
                                  AIC = c(social_random_comparison_table$AIC[2:4]),
                                  BIC = c(social_random_comparison_table$BIC[2:4]),
                                  BF = c(social_random_comparison_table$BF[2:4]))
random_social_bf_df[4] <- round(random_social_bf_df[4], digits = 2)

fixed_social_bf_df <- data.frame(Model = fixed_models,
                                 AIC = c(social_model_comparison_table$AIC[2:5]),
                                 BIC = c(social_model_comparison_table$BIC[2:5]),
                                 BF = c(social_model_comparison_table$BF[2:5]))
fixed_social_bf_df[4] <- round(fixed_social_bf_df[4], digits = 2)




random_social_bf_table <- formattable(random_social_bf_df ,
                                      align =c("l","l","l","l"))

fixed_social_bf_table <- formattable(fixed_social_bf_df,
                                     align= c("l","l","l","l"))

