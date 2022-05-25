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

fozzie <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                     sheet = "Fozzie")
fozzie <- fozzie %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data Uno

uno <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                  sheet = "Uno")
uno <- uno %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data Fern

fern <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                   sheet = "Fern")
fern <- fern %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data Mork

mork <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                   sheet = "Mork")
mork <- mork %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data Prudence

prudence <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                       sheet = "Prudence")
prudence <- prudence %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data Dumbledore

dumbledore <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                         sheet = "Dumbledore")
dumbledore <- dumbledore %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data Mote

mote <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                   sheet = "Mote")
mote <- mote %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#upload data He-man

heman <- read_excel(here("data/phase_1_nonsocial_complete.xlsx"),
                    sheet = "He-man")
heman <- heman %>%
  select(subject_bird, small_num, large_num, large_side, Choice...12) %>%
  na.omit(df)

#Data Clean UP----------------------------------------------
#bind them all together into 1 sheet, we took out Mulder and dart because they didn't complete the project.

df_food <- bind_rows(fozzie, uno, fern, mork, prudence, dumbledore, mote, heman)

#Creating binary choice, ratio, and pair columns

df_food$pair <- paste(df_food$small_num, df_food$large_num, sep = "/")

df_food$large_choice <- ifelse(df_food$Choice == df_food$large_side, "Y", "N")

df_food$choice_num <- ifelse(df_food$large_choice == "Y", 1, 0)

#changing data types of columns in our data frame

df_food$large_num <- as.numeric(df_food$large_num)
df_food$small_num <- as.numeric(df_food$small_num)

df_food$subject_bird <- as.factor(df_food$subject_bird)
df_food$pair <- as.factor(df_food$pair)

#create bird sex column

female_birds_food <- c("Uno")

df_food$bird_sex <- ifelse(df_food$subject_bird %in% female_birds_food, "female", "male")


#creating the ratio and difference columns

options(digits = 2)

df_food$ratio <- df_food$small_num/df_food$large_num

df_food$difference <-  df_food$large_num - df_food$small_num


#creating vector of bird ages for the entire colony at time of studies

bird_ages <- c(17,12,13,14,12,12,17,13,16,17,16,13,14,13,14,16,16,17,14,16,13,16,14,12,16,16,14,14,14,14,13)

#Summarizing Data------------------

summary_food <- df_food %>%
  group_by(choice_num) %>%
  summarise(n())

pairsummary_food <- df_food %>%
  group_by(pair) %>%
  summarise(perc = mean(choice_num)*100)

birdsummary_food <- df_food %>%
  group_by(subject_bird) %>%
  summarise(n = n(),
            perc = mean(choice_num)*100,
            sd = sd(choice_num)) %>%
  mutate(se=sd/sqrt(8)) %>%
  mutate(ic=se*qt((1-0.05)/2 + .5, n-1))

diffsummary_food<- df_food %>%
  group_by(difference) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_food <- df_food %>%
  group_by(difference, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

diff_bird_summary_means <- diff_bird_summary_food %>%
  group_by(difference) %>%
  summarise(perc = mean(perc))

ratiosummary_food <- df_food %>%
  group_by(ratio) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_food <- df_food %>%
  group_by(ratio, subject_bird) %>%
  summarise(perc = mean(choice_num)*100)

ratio_bird_summary_means <- ratio_bird_summary_food %>%
  group_by(ratio) %>%
  summarise(perc = mean(perc))

diff_ratio_summary_food <- df_food %>%
  group_by(difference, ratio) %>%
  summarise(perc = mean(choice_num)*100)


#Data Analysis----------------------

#1 sample t-test -------
#First want to test if birds choose larger over smaller

large_pref_ttest_food <- t.test(birdsummary_food$perc, mu= 50, alternative = "two.sided")

large_pref_ttest_food

large_pref_ttest_bf_food <- ttestBF(birdsummary_food$perc, mu= 50, alternative = "two.sided")


# Model selection ----------------------------------------------

#Now that I've looked at things individually lets put it all together folks!

# First find best-fitting random effect model, then test for fixed effect predictors
# Process is backward by eliminating weakest terms sequentially, starting with full model, until only significant effects remain
# Nested model comparisons (likelihood ratio tests using anova command) are used to select best fitting models

# Random effects Structure selection------------------

random_effect_intercept_food <- glm(formula = choice_num ~ 1, data = df_food, family = binomial())

random_effect_sub_food <- glmer(formula = choice_num ~ (1 | subject_bird), data = df_food, family = binomial()) #only subject bird as random effect

random_effect_pair_food<- glmer(formula = choice_num ~ (1 | pair), data = df_food, family = binomial())

random_effect_sub_pair_food <- glmer(formula = choice_num ~ (1|subject_bird) + (1|pair), data = df_food, family = binomial) #subject bird and pair as random effect

nonsocial_random_comparison <- compare_performance(random_effect_intercept_food, random_effect_sub_food, random_effect_pair_food, random_effect_sub_pair_food )

nonsocial_random_bayes_comparison <- bayesfactor_models(random_effect_sub_food, random_effect_pair_food, random_effect_sub_pair_food, denominator = random_effect_intercept_food)

### Extract BICs
bic1_random <- nonsocial_random_comparison$BIC[1]  # empty random model
bic2_random <- nonsocial_random_comparison$BIC[2]
bic3_random <- nonsocial_random_comparison$BIC[3]
bic4_random <- nonsocial_random_comparison$BIC[4]

# Convert BIC values to Bayes factor
bf_values_random <- bic_to_bf(c(bic1_random, bic2_random, bic3_random, bic4_random ), denominator = bic1_random)

nonsocial_random_comparison_table <- nonsocial_random_comparison %>%
  mutate(BF = bf_values_random)


# Fixed effects----------------------
#create fixed-effects models

nonsocial_full <- glm(formula = choice_num ~ difference * ratio, data = df_food, family = binomial)  #full model

nonsocial_no_interaction <- glm(formula = choice_num ~ difference + ratio , data = df_food, family = binomial) #changing ratio from a term that adds an interaction to a main effect without the interaction.

nonsocial_difference <- glm(formula = choice_num ~ difference, data = df_food, family = binomial) #drop ratio from the model, difference is the main IV

nonsocial_ratio <- glm(formula = choice_num ~ ratio, data = df_food, family = binomial) #drop difference from the model and add ratio back in as the main IV

## Likelihood ratio tests for model comparison

nonsocial_fixed_comparison <- compare_performance(random_effect_intercept_food, nonsocial_ratio, nonsocial_difference, nonsocial_no_interaction, nonsocial_full)

nonsocial_fixed_bayes_comparison <- bayesfactor_models(nonsocial_ratio, nonsocial_difference, nonsocial_no_interaction, nonsocial_full, denominator = random_effect_intercept_food)


## Bayes factors for fixed effects
### Extract BICs
bic1_food <- nonsocial_fixed_comparison$BIC[1]  # empty random model
bic2_food <- nonsocial_fixed_comparison$BIC[2]
bic3_food <- nonsocial_fixed_comparison$BIC[3]
bic4_food <- nonsocial_fixed_comparison$BIC[4]
bic5_food <- nonsocial_fixed_comparison$BIC[5]

### Convert BICs to BFs

# Convert BIC values to Bayes factor
bf_values_food <- bic_to_bf(c(bic1_food, bic2_food, bic3_food, bic4_food, bic5_food), denominator = bic1_food)

nonsocial_fixed_comparison_table <- nonsocial_fixed_comparison %>%
  mutate(BF = bf_values_food)

#Calculating within subject confidence intervals----------------

confidence_intv_difference_food <- wsci(data = diff_bird_summary_food,
     id = "subject_bird",
     dv = "perc",
     factors = "difference",
     method = "Morey")


diff_bird_summary_means$upper <- confidence_intv_difference_food$perc + diff_bird_summary_means$perc

diff_bird_summary_means$lower <-  diff_bird_summary_means$perc - confidence_intv_difference_food$perc

confidence_intv_ratio_food <- wsci(data=
                                ratio_bird_summary_food,
                              id = "subject_bird",
                              dv = "perc",
                              factors= "ratio",
                              method = "Morey")

ratio_bird_summary_means$upper <- confidence_intv_ratio_food$perc + ratio_bird_summary_means$perc

ratio_bird_summary_means$lower <-  ratio_bird_summary_means$perc - confidence_intv_ratio_food$perc

#GRAPHING TIME!!!-----------------------------------

#Bird Mean preference. Hypothesis 1 graph

bird_graph_food <- ggplot(data = birdsummary_food, aes(x=subject_bird, y= perc)) +
  labs( y="Percent larger choosen")+
  geom_bar(stat = 'identity')+
  theme_bw(base_size = 22)+
  theme(
    axis.title.x = element_blank()
  )+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,75)

bird_graph_food


#Ratio Graph with the ratio in proportion form. as in 1 "/" 3 etc.

pair_graph_food <- ggplot(data = pairsummary_food, aes(x=pair, y= perc)) +
  labs(title = "Food Preference by Pair", y="% of trials larger option choosen", x = "Pair")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

pair_graph_food

# Ratio graph with the ratio in numeric form so 0.13. Some conditions collapsed together.

ratio_graph_food <- ggplot(data = ratiosummary_food, aes(x=ratio, y= perc)) +
  labs( y="% of trials larger option choosen", x = "Ratio")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

ratio_graph_food

#Difference Graph

diff_graph_food <- ggplot(data = diffsummary_food, aes(x=(difference), y = perc)) +
  labs(y="% of trials larger option choosen", x = "Difference")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

diff_graph_food

#Graph Ratio on the X axis with Difference as the grouping variable

ratio_difference_graph_food <- ggplot(data = diff_ratio_summary_food, aes(x=ratio, y= perc)) +
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

ratio_difference_graph_food



#Graph Ratio grouped by subject bird

ratio_bird_graph_food <- ggplot(data = ratio_bird_summary_food, aes(x=ratio, y= perc)) +
  labs(y="Percent larger choosen", x = "Ratio")+
  geom_point(data = ratiosummary_food, size = 2)+
  geom_errorbar(data = ratio_bird_summary_means, aes(x=ratio, ymin= lower, ymax = upper), width = 0)+
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

ratio_bird_graph_food



#Graph Difference by subject bird

diff_bird_graph_food <- ggplot(data = diff_bird_summary_food, aes(x=difference, y= perc)) +
  geom_point(data = diffsummary_food, size = 2) +
  geom_errorbar(data = diff_bird_summary_means, aes(x=difference, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject_bird, color = subject_bird), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs( y="Percent larger choosen", x = "Difference")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)

diff_bird_graph_food


#Saving plots for use in merp
#library(patchwork)
#ratio_bird_graph_food + diff_bird_graph_food + ratio_difference_graph_food + plot_annotation(tag_levels = "a") + plot_layout(ncol = 2)
#ggsave("figures/food_figures.png", scale = 2, height = 5, width = 7)

#creating tables for BF values

random_models <- c("(1|Subject)","(1|Pair)","(1|Subject)+(1|Pair)")

fixed_models <- c( "ratio", "difference","difference + ratio", "difference * ratio")

random_food_bf_df <- data.frame(Model = random_models,
                                AIC = c(nonsocial_random_comparison_table$AIC[2:4]),
                                BIC = c(nonsocial_random_comparison_table$BIC[2:4]),
                                BF = c(nonsocial_random_comparison_table$BF[2:4]))
random_food_bf_df[4] <- round(random_food_bf_df[4],digits = 2)

fixed_food_bf_df <- data.frame(Model = fixed_models,
                               AIC = c(nonsocial_fixed_comparison_table$AIC[2:5]),
                               BIC = c(nonsocial_fixed_comparison_table$BIC[2:5]),
                               BF = c(nonsocial_fixed_comparison_table$BF[2:5]))
fixed_food_bf_df[4] <- round(fixed_food_bf_df[4],digits=2)


random_food_bf_table <- formattable(random_food_bf_df ,
                                    align =c("l","l","l","l"))

fixed_food_bf_table <- formattable(fixed_food_bf_df,
                                   align= c("l","l","l","l"))
