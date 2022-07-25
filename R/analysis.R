
# Libraries--------
library(tidyverse)
library(lme4)
library(BayesFactor)
library(broom)
library(bayestestR)
library(performance)
library(here)
library(papaja)
# library(formattable)


# Functions ---------------------------------------------------------------

analyze_data <- function(df, type, rep) {
  df <- df |>
    mutate(difference = large_num - small_num,
           ratio = small_num / large_num)

  summary <- df %>%
    group_by(choose_larger) %>%
    summarise(n())

  pairsummary <- df %>%
    group_by(pair) %>%
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  birdsummary <- df %>%
    group_by(subject) %>%
    summarise(n = n(),
              percent_larger = mean(choose_larger, na.rm = TRUE) * 100,
              sd = sd(choose_larger)) %>%
    mutate(se=sd/sqrt(8)) %>%
    mutate(ic=se*qt((1-0.05)/2 + .5, n-1))

  diffsummary<- df %>%
    group_by(difference) %>%
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  diff_bird_summary <- df %>%
    group_by(difference, subject) %>%
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  diff_bird_summary_means <- diff_bird_summary %>%
    group_by(difference) %>%
    summarise(percent_larger = mean(percent_larger))

  ratiosummary <- df %>%
    group_by(ratio) %>%
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  ratio_bird_summary <- df %>%
    group_by(ratio, subject) %>%
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  ratio_bird_summary_means <- ratio_bird_summary %>%
    group_by(ratio) %>%
    summarise(percent_larger = mean(percent_larger))

  diff_ratio_summary <- df %>%
    group_by(difference, ratio) %>%
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  # t-test -------
  #First want to test if birds choose larger over smaller

  large_pref_ttest <- t.test(birdsummary$percent_larger, mu= 50, alternative = "two.sided")

  large_pref_ttest_bf <- ttestBF(birdsummary$percent_larger, mu= 50, alternative = "two.sided")

  output <- list(ttest = large_pref_ttest, ttestbf = large_pref_ttest_bf)
}


# Model selection ----------------------------------------------

#Now that I've looked at things individually lets put it all together folks!

# First find best-fitting random effect model, then test for fixed effect predictors
# Process is backward by eliminating weakest terms sequentially, starting with full model, until only significant effects remain
# Nested model comparisons (likelihood ratio tests using anova command) are used to select best fitting models

# Random effects Structure selection------------------

random_effect_intercept <- glm(formula = choose_larger ~ 1, data = df, family = binomial())

random_effect_sub <- glmer(formula = choose_larger ~ (1 | subject), data = df, family = binomial()) #only subject bird as random effect

random_effect_pair<- glmer(formula = choose_larger ~ (1 | pair), data = df, family = binomial())

random_effect_sub_pair <- glmer(formula = choose_larger ~ (1|subject) + (1|pair), data = df, family = binomial) #subject bird and pair as random effect

nonsocial_random_comparison <- compare_performance(random_effect_intercept, random_effect_sub, random_effect_pair, random_effect_sub_pair )

nonsocial_random_bayes_comparison <- bayesfactor_models(random_effect_sub, random_effect_pair, random_effect_sub_pair, denominator = random_effect_intercept)

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

nonsocial_full <- glm(formula = choose_larger ~ difference * ratio, data = df, family = binomial)  #full model

nonsocial_no_interaction <- glm(formula = choose_larger ~ difference + ratio , data = df, family = binomial) #changing ratio from a term that adds an interaction to a main effect without the interaction.

nonsocial_difference <- glm(formula = choose_larger ~ difference, data = df, family = binomial) #drop ratio from the model, difference is the main IV

nonsocial_ratio <- glm(formula = choose_larger ~ ratio, data = df, family = binomial) #drop difference from the model and add ratio back in as the main IV

## Likelihood ratio tests for model comparison

nonsocial_fixed_comparison <- compare_performance(random_effect_intercept, nonsocial_ratio, nonsocial_difference, nonsocial_no_interaction, nonsocial_full)

nonsocial_fixed_bayes_comparison <- bayesfactor_models(nonsocial_ratio, nonsocial_difference, nonsocial_no_interaction, nonsocial_full, denominator = random_effect_intercept)


## Bayes factors for fixed effects
### Extract BICs
bic1 <- nonsocial_fixed_comparison$BIC[1]  # empty random model
bic2 <- nonsocial_fixed_comparison$BIC[2]
bic3 <- nonsocial_fixed_comparison$BIC[3]
bic4 <- nonsocial_fixed_comparison$BIC[4]
bic5 <- nonsocial_fixed_comparison$BIC[5]

### Convert BICs to BFs

# Convert BIC values to Bayes factor
bf_values <- bic_to_bf(c(bic1, bic2, bic3, bic4, bic5), denominator = bic1)

nonsocial_fixed_comparison_table <- nonsocial_fixed_comparison %>%
  mutate(BF = bf_values)

#Calculating within subject confidence intervals----------------

confidence_intv_difference <- wsci(data = diff_bird_summary,
                                        id = "subject",
                                        dv = "percent_larger",
                                        factors = "difference",
                                        method = "Morey")


diff_bird_summary_means$upper <- confidence_intv_difference$percent_larger + diff_bird_summary_means$percent_larger

diff_bird_summary_means$lower <-  diff_bird_summary_means$percent_larger - confidence_intv_difference$percent_larger

confidence_intv_ratio <- wsci(data=
                                     ratio_bird_summary,
                                   id = "subject",
                                   dv = "percent_larger",
                                   factors= "ratio",
                                   method = "Morey")

ratio_bird_summary_means$upper <- confidence_intv_ratio$percent_larger + ratio_bird_summary_means$percent_larger

ratio_bird_summary_means$lower <-  ratio_bird_summary_means$percent_larger - confidence_intv_ratio$percent_larger

#GRAPHING TIME!!!-----------------------------------

#Bird Mean preference. Hypothesis 1 graph

bird_graph <- ggplot(data = birdsummary, aes(x=subject, y= percent_larger)) +
  labs( y="percent_largerent larger choosen")+
  geom_bar(stat = 'identity')+
  theme_bw(base_size = 22)+
  theme(
    axis.title.x = element_blank()
  )+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,75)

bird_graph


#Ratio Graph with the ratio in proportion form. as in 1 "/" 3 etc.

pair_graph <- ggplot(data = pairsummary, aes(x=pair, y= percent_larger)) +
  labs(title = "Food Preference by Pair", y="% of trials larger option choosen", x = "Pair")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

pair_graph

# Ratio graph with the ratio in numeric form so 0.13. Some conditions collapsed together.

ratio_graph <- ggplot(data = ratiosummary, aes(x=ratio, y= percent_larger)) +
  labs( y="% of trials larger option choosen", x = "Ratio")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

ratio_graph

#Difference Graph

diff_graph <- ggplot(data = diffsummary, aes(x=(difference), y = percent_larger)) +
  labs(y="% of trials larger option choosen", x = "Difference")+
  geom_point()+
  theme_bw(base_size = 22)+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(0,100)

diff_graph

#Graph Ratio on the X axis with Difference as the grouping variable

ratio_difference_graph <- ggplot(data = diff_ratio_summary, aes(x=ratio, y= percent_larger)) +
  labs( y="percent_largerent larger choosen", x = "Ratio")+
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

ratio_difference_graph



#Graph Ratio grouped by subject bird

ratio_bird_graph <- ggplot(data = ratio_bird_summary, aes(x=ratio, y= percent_larger)) +
  labs(y="percent_largerent larger choosen", x = "Ratio")+
  geom_point(data = ratiosummary, size = 2)+
  geom_errorbar(data = ratio_bird_summary_means, aes(x=ratio, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject, color = subject), alpha = 0.5)+
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

ratio_bird_graph



#Graph Difference by subject bird

diff_bird_graph <- ggplot(data = diff_bird_summary, aes(x=difference, y= percent_larger)) +
  geom_point(data = diffsummary, size = 2) +
  geom_errorbar(data = diff_bird_summary_means, aes(x=difference, ymin= lower, ymax = upper), width = 0)+
  geom_line(aes(group = subject, color = subject), alpha = 0.5)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs( y="Percent larger choosen", x = "Difference")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)

diff_bird_graph


#Saving plots for use in merp
#library(patchwork)
#ratio_bird_graph + diff_bird_graph + ratio_difference_graph + plot_annotation(tag_levels = "a") + plot_layout(ncol = 2)
#ggsave("figures/food_figures.png", scale = 2, height = 5, width = 7)

#creating tables for BF values

random_models <- c("(1|Subject)","(1|Pair)","(1|Subject)+(1|Pair)")

fixed_models <- c( "ratio", "difference","difference + ratio", "difference * ratio")

random_bf_df <- data.frame(Model = random_models,
                                AIC = c(nonsocial_random_comparison_table$AIC[2:4]),
                                BIC = c(nonsocial_random_comparison_table$BIC[2:4]),
                                BF = c(nonsocial_random_comparison_table$BF[2:4]))
random_bf_df[4] <- round(random_bf_df[4],digits = 2)

fixed_bf_df <- data.frame(Model = fixed_models,
                               AIC = c(nonsocial_fixed_comparison_table$AIC[2:5]),
                               BIC = c(nonsocial_fixed_comparison_table$BIC[2:5]),
                               BF = c(nonsocial_fixed_comparison_table$BF[2:5]))
fixed_bf_df[4] <- round(fixed_bf_df[4],digits=2)


random_bf_table <- formattable(random_bf_df ,
                                    align =c("l","l","l","l"))

fixed_bf_table <- formattable(fixed_bf_df,
                                   align= c("l","l","l","l"))


# Import data -------------------------------------------------------------

all_data <- read_csv(here("data/wolff_etal_2022_data.csv"))

food1 <- all_data |>
  filter(study == "food" & rep == 1) |>
  filter(!subject %in% c("Dartagnan", "Mulder"))

food2 <- all_data |>
  filter(study == "food" & rep == 2) |>
  filter(!subject %in% c("Dartagnan", "Mulder"))

social1 <- all_data |>
  filter(study == "social" & rep == 1) |>
  filter(!subject %in% c("Dartagnan", "Mulder"))

social2 <- all_data |>
  filter(study == "social" & rep == 2) |>
  filter(!subject %in% c("Dartagnan", "Mulder"))


# Analyze data ------------------------------------------------------------


food1_results <- analyze_data(food1, "food", "1")
food2_results <- analyze_data(food2, "food", "2")
social1_results <- analyze_data(social1, "social", "1")
social2_results <- analyze_data(social2, "social", "2")
