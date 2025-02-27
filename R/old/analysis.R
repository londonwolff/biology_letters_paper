
# Libraries--------
library(tidyverse)
library(lme4)
library(BayesFactor)
library(broom)
library(bayestestR)
library(performance)
library(here)
library(papaja)
# library(dplyr)

# Functions ---------------------------------------------------------------

analyze_data <- function(df, type, rep) {
  df <- df |>
    mutate(difference = large_num - small_num,
           ratio = small_num / large_num)

  birdsummary <- df %>%
    group_by(subject) %>%
    summarise(n = n(),
              percent_larger = mean(choose_larger, na.rm = TRUE) * 100,
              sd = sd(choose_larger, na.rm = TRUE)) %>%
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

  # t-tests------------------------------

  large_pref_ttest <- t.test(birdsummary$percent_larger, mu= 50, alternative = "two.sided")

  large_pref_ttest_bf <- ttestBF(birdsummary$percent_larger, mu= 50, alternative = "two.sided")

  # Model selection ----------------------------------------------
  # First we found the best-fitting random effect model, plug this random effect structure into the fixed effect models and then test for best fixed effect structure.


  # Random effects structure selection------------------

  random_effect_intercept <- glm(formula = choose_larger ~ 1, data = df, family = binomial()) #empty or intercept only model

  random_effect_sub <- glmer(formula = choose_larger ~ (1 | subject), data = df, family = binomial()) #only subject bird as random effect

  random_effect_pair<- glmer(formula = choose_larger ~ (1 | pair), data = df, family = binomial()) #only pair as random effect

  random_effect_sub_pair <- glmer(formula = choose_larger ~ (1|subject) + (1|pair), data = df, family = binomial) #subject bird and pair as random effect

  random_comparison <- compare_performance(random_effect_intercept, random_effect_sub, random_effect_pair, random_effect_sub_pair )

  random_bayes_comparison <- bayesfactor_models(random_effect_sub, random_effect_pair, random_effect_sub_pair, denominator = random_effect_intercept)

  ### Extract BICs
  bic1_random <- random_comparison$BIC[1]  # empty random model
  bic2_random <- random_comparison$BIC[2]
  bic3_random <- random_comparison$BIC[3]
  bic4_random <- random_comparison$BIC[4]

  # Convert BIC values to Bayes factor
  bf_values_random <- bic_to_bf(c(bic1_random, bic2_random, bic3_random, bic4_random ), denominator = bic1_random)

  random_comparison_table <- random_comparison %>%
    mutate(BF = bf_values_random)

  #All random effect structures examined found that the intercept only random effect model or an empty model was the best model structure so no random effect structure was added to the fixed effect models.


  # Fixed effects----------------------

  full_fixed_model <- glm(formula = choose_larger ~ difference * ratio, data = df, family = binomial)  #full model

  fixed_no_interaction_model <- glm(formula = choose_larger ~ difference + ratio , data = df, family = binomial) #no interaction term with main effects.

  fixed_difference_model <- glm(formula = choose_larger ~ difference, data = df, family = binomial) #difference as the IV

  fixed_ratio_model <- glm(formula = choose_larger ~ ratio, data = df, family = binomial) #ratio as the IV

  ## Likelihood ratio tests for model comparison

  fixed_model_comparison <- compare_performance(random_effect_intercept, fixed_ratio_model, fixed_difference_model, fixed_no_interaction_model, full_fixed_model)

  fixed_bayes_comparison <- bayesfactor_models(fixed_ratio_model, fixed_difference_model, fixed_no_interaction_model, full_fixed_model, denominator = random_effect_intercept)

  ## Extract BICs from table to convert to Bayes factors
  bic1 <- fixed_model_comparison$BIC[1]
  bic2 <- fixed_model_comparison$BIC[2]
  bic3 <- fixed_model_comparison$BIC[3]
  bic4 <- fixed_model_comparison$BIC[4]
  bic5 <- fixed_model_comparison$BIC[5]

  ## Convert BICs to Bayes Factorss

  # Convert BIC values to Bayes factor
  bf_values <- bic_to_bf(c(bic1, bic2, bic3, bic4, bic5), denominator = bic1)

  fixed_comparison_table <- fixed_model_comparison %>%
    mutate(BF = bf_values)

  #Determine the model of best fit

  bestfit <- eval(parse(text = fixed_comparison_table$Name[which(fixed_comparison_table$BIC == min(fixed_comparison_table$BIC))]))

  #Calculating within subject confidence intervals----------------

  confidence_intv_difference <- wsci(data = diff_bird_summary,
                                     id = "subject",
                                     dv = "percent_larger",
                                     factors = "difference",
                                     method = "Morey")


  diff_bird_summary_means$upper <- confidence_intv_difference$percent_larger + diff_bird_summary_means$percent_larger

  diff_bird_summary_means$lower <-  diff_bird_summary_means$percent_larger - confidence_intv_difference$percent_larger

  confidence_intv_ratio <- wsci(data=ratio_bird_summary,
                                id = "subject",
                                dv = "percent_larger",
                                factors= "ratio",
                                method = "Morey")

  ratio_bird_summary_means$upper <- confidence_intv_ratio$percent_larger + ratio_bird_summary_means$percent_larger

  ratio_bird_summary_means$lower <-  ratio_bird_summary_means$percent_larger - confidence_intv_ratio$percent_larger

#Plots-----------------------------------

#Graph Ratio grouped by subject bird

ratio_bird_graph <- ggplot(data = ratio_bird_summary, aes(x=ratio, y= percent_larger)) +
  labs(y = "Percent larger choosen", x = "Ratio") +
  geom_line(aes(group = subject, color = subject), alpha = 0.5) +
  geom_point(data = ratiosummary, size = 2) +
  geom_errorbar(data = ratio_bird_summary_means, aes(x=ratio, ymin= lower, ymax = upper), width = 0) +
  theme_bw(base_size = 22) +
  theme(legend.position =  "none",
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) +
        geom_hline(yintercept = 50, linetype = "dashed") +
  scale_x_continuous(breaks = c(.17, .2, .25,.33,.4, .5, .6, .67, .75, .8, .83)) +
  scale_y_continuous(breaks=seq(10,100,10)) +
  ylim(10,110)


#Graph Difference by subject bird

diff_bird_graph <- ggplot(data = diff_bird_summary, aes(x=difference, y= percent_larger)) +
  geom_line(aes(group = subject, color = subject), alpha = 0.5) +
   labs(y = "Percent larger choosen", x = "Difference") +
 geom_point(data = diffsummary, size = 2) +
  geom_errorbar(data = diff_bird_summary_means, aes(x=difference, ymin= lower, ymax = upper), width = 0) +
  theme_bw(base_size = 22) +
  theme(legend.position =  "none",
        panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  scale_y_continuous(breaks=seq(10,100,10)) +
  ylim(10,110)

diff_bird_graph

#creating tables for BF values

random_models <- c("(1|Subject)","(1|Pair)","(1|Subject)+(1|Pair)")

fixed_models <- c( "ratio", "difference","difference + ratio", "difference * ratio")

random_bf_df <- data.frame(Model = random_models,
                           AIC = c(random_comparison_table$AIC[2:4]),
                           BIC = c(random_comparison_table$BIC[2:4]),
                           BF = c(random_comparison_table$BF[2:4]))
random_bf_df[4] <- round(random_bf_df[4],digits = 2)

fixed_bf_df <- data.frame(Model = fixed_models,
                          AIC = c(fixed_comparison_table$AIC[2:5]),
                          BIC = c(fixed_comparison_table$BIC[2:5]),
                          BF = c(fixed_comparison_table$BF[2:5]))
fixed_bf_df[4] <- round(fixed_bf_df[4],digits=2)


random_bf_table <- apa_table(random_bf_df ,
                               align =c("l","l","l","l"))

fixed_bf_table <- apa_table(fixed_bf_df,
                              align= c("l","l","l","l"))

#Creating Output to use for manuscript

output <- list(ttest = large_pref_ttest, ttestbf = large_pref_ttest_bf, CI_difference = diff_bird_summary_means, CI_ratio = ratio_bird_summary_means,  best_model_fit = bestfit, diff_fig = diff_bird_graph, ratio_fig = ratio_bird_graph, random_table = random_bf_table, fixed_table = fixed_comparison_table)
}

# Import data -------------------------------------------------------------

all_data <- read_csv(here("data/wolff_etal_2022_data.csv"))

food1 <- all_data |>
  filter(study == "food" & rep == 1) |>
  filter(!subject %in% c("Mulder", "Dartagnan"))

food2 <- all_data |>
  filter(study == "food" & rep == 2) |>
  filter(!subject %in% c("Basil", "Rooster"))

social1 <- all_data |>
  filter(study == "social" & rep == 1) |>
  filter(!subject %in% c("Baloo")) |>
  filter(!small_num %in% 0)

social2 <- all_data |>
  filter(study == "social" & rep == 2) |>
  filter(!subject %in% c()) |>
  filter(!small_num %in% 0)


# Analyze data -----------------------------------------------------

food1_results <- analyze_data(food1, "food", "1")
food2_results <- analyze_data(food2, "food", "2")
social1_results <- analyze_data(social1, "social", "1")
social2_results <- analyze_data(social2, "social", "2")


# Build plots -------------------------------------------------------------

library(patchwork)
food_figures <- food1_results$diff_fig + food1_results$ratio_fig + food2_results$diff_fig + food2_results$ratio_fig + plot_annotation(tag_levels = 'A')
ggsave(here("figures/food_figure.png"), width = 14, height = 10)

social_figures <- social1_results$diff_fig + social1_results$ratio_fig + social2_results$diff_fig + social2_results$ratio_fig + plot_annotation(tag_levels = 'A')
ggsave(here("figures/social_figure.png"), width = 14, height = 10)
