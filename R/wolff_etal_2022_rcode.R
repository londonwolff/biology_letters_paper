## ---
##
## Script name: wolff_etal_2022_rcode.R
##
## Purpose of script: Analyze pinyon jay number preference data
##
## Authors: London Wolff (lmwolff3@gmail.com) & Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Created: 2022-08-12
##
## Date Finalized: 2022-08-12
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share — copy and redistribute the material in any medium or format
##   Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
##   No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
##
## ---

# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(lme4)
library(BayesFactor)
library(bayestestR)
library(performance)
library(papaja)
library(here)

# Define functions -------------------------------------------------------------

# Analyze an experiment's data
analyze_data <- function(df, type, rep) {

  # Calculate difference and ratio
  df <- df |>
    mutate(
      difference = large_num - small_num,
      ratio = small_num / large_num
    )

  ## Descriptive statistics ------------------------------

  # Summarize data per subject
  choice_means_subject <- df |>
    group_by(subject) |>
    summarise(
      n = n(),
      percent_larger = mean(choose_larger, na.rm = TRUE) * 100
    )

  # Difference
  # Summarize data per subject and difference level
  choice_means_subject_diff <- df |>
    group_by(difference, subject) |>
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  # Summarize data per difference level
  choice_means_diff_means <- choice_means_subject_diff |>
    group_by(difference) |>
    summarise(percent_larger = mean(percent_larger))

  # Within subject confidence intervals
  wsci_difference <- wsci(
    data = choice_means_subject_diff,
    id = "subject",
    dv = "percent_larger",
    factors = "difference",
    method = "Morey"
  )
  choice_means_diff_means$upper <- choice_means_diff_means$percent_larger + wsci_difference$percent_larger
  choice_means_diff_means$lower <- choice_means_diff_means$percent_larger - wsci_difference$percent_larger

  # Ratio
  # Summarize data per subject and ratio level
  choice_means_subject_ratio <- df |>
    group_by(ratio, subject) |>
    summarise(percent_larger = mean(choose_larger, na.rm = TRUE) * 100)

  # Summarize data per ratio level
  choice_means_ratio_means <- choice_means_subject_ratio |>
    group_by(ratio) |>
    summarise(percent_larger = mean(percent_larger))

  # Within subject confidence intervals
  wsci_ratio <- wsci(
    data = choice_means_subject_ratio,
    id = "subject",
    dv = "percent_larger",
    factors = "ratio",
    method = "Morey"
  )
  choice_means_ratio_means$upper <- choice_means_ratio_means$percent_larger + wsci_ratio$percent_larger
  choice_means_ratio_means$lower <- choice_means_ratio_means$percent_larger - wsci_ratio$percent_larger


  ## t-tests ------------------------------

  large_pref_ttest <- t.test(choice_means_subject$percent_larger, mu = 50, alternative = "two.sided")
  large_pref_ttest_bf <- ttestBF(choice_means_subject$percent_larger, mu = 50, alternative = "two.sided")


  ## Model selection ----------------------------------------------
  # First we find the best-fitting random effect model, plug this random effect structure into the fixed effect models, and then test for best fixed effect structure.

  ## Random effects structure selection ------------------

  random_effect_intercept <- glm(formula = choose_larger ~ 1, data = df, family = binomial()) # empty or intercept only model
  random_effect_sub <- glmer(formula = choose_larger ~ (1 | subject), data = df, family = binomial()) # only subject bird as random effect
  random_effect_pair <- glmer(formula = choose_larger ~ (1 | pair), data = df, family = binomial()) # only pair as random effect
  random_effect_sub_pair <- glmer(formula = choose_larger ~ (1 | subject) + (1 | pair), data = df, family = binomial) # subject bird and pair as random effect

  random_comparison <- compare_performance(random_effect_intercept, random_effect_sub, random_effect_pair, random_effect_sub_pair)
  bf_values_random <- bayesfactor_models(random_effect_intercept, random_effect_sub, random_effect_pair, random_effect_sub_pair)
  random_comparison_table <- random_comparison |>
    mutate(BF = as.numeric(bf_values_random))
  best_random_effect_model <- eval(parse(text = random_comparison_table$Name[which(random_comparison_table$BF == max(random_comparison_table$BF))]))
  best_random_effect <- sub("choose_larger ~ ", "", best_random_effect_model$formula)

  # All random effect structures examined found that the intercept only random effect model or an empty model was the best model structure so no random effect structure was added to the fixed effect models.


  ## Fixed effects ----------------------

  fixed_difference_model <- glm(formula = choose_larger ~ difference, data = df, family = binomial) # difference as the IV
  fixed_ratio_model <- glm(formula = choose_larger ~ ratio, data = df, family = binomial) # ratio as the IV
  fixed_no_interaction_model <- glm(formula = choose_larger ~ difference + ratio, data = df, family = binomial) # no interaction term with main effects.
  full_fixed_model <- glm(formula = choose_larger ~ difference * ratio, data = df, family = binomial) # full model

  # Likelihood ratio tests for model comparison
  fixed_model_comparison <- compare_performance(random_effect_intercept, fixed_ratio_model, fixed_difference_model, fixed_no_interaction_model, full_fixed_model)

  fixed_bayes_comparison <- bayesfactor_models(random_effect_intercept, fixed_ratio_model, fixed_difference_model, fixed_no_interaction_model, full_fixed_model)

  fixed_comparison_table <- fixed_model_comparison |>
    mutate(BF = as.numeric(fixed_bayes_comparison))

  # Determine the model of best fit
  bestfit <- eval(parse(text = fixed_comparison_table$Name[which(fixed_comparison_table$BIC == min(fixed_comparison_table$BIC))]))


  ## Plots -----------------------------------

  # Plot effects of difference on choice
  diff_bird_graph <- ggplot(data = choice_means_subject_diff, aes(x = difference, y = percent_larger)) +
    geom_line(aes(group = subject, color = subject), alpha = 0.5) +
    labs(y = "Percent larger choosen", x = "Difference") +
    geom_point(data = choice_means_diff_means, size = 2) +
    geom_errorbar(data = choice_means_diff_means, aes(x = difference, ymin = lower, ymax = upper), width = 0) +
    geom_hline(yintercept = 50, linetype = "dashed") +
    scale_y_continuous(breaks = seq(10, 100, 10)) +
    ylim(10, 110) +
    theme_bw(base_size = 22, base_family = "Arial") +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # Plot effects of ratio on choice
  ratio_bird_graph <- ggplot(data = choice_means_subject_ratio, aes(x = ratio, y = percent_larger)) +
    labs(y = "Percent larger choosen", x = "Ratio") +
    geom_line(aes(group = subject, color = subject), alpha = 0.5) +
    geom_point(data = choice_means_ratio_means, size = 2) +
    geom_errorbar(data = choice_means_ratio_means, aes(x = ratio, ymin = lower, ymax = upper), width = 0) +
    geom_hline(yintercept = 50, linetype = "dashed") +
    scale_x_continuous(breaks = c(0.17, 0.2, 0.25, 0.33, 0.4, 0.5, 0.6, 0.67, 0.75, 0.8, 0.83)) +
    scale_y_continuous(breaks = seq(10, 100, 10)) +
    ylim(10, 110) +
    theme_bw(base_size = 22, base_family = "Arial") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  ## Tables -----------------------------------

  # Create tables of BF values
  random_models <- c("1", "(1|Subject)", "(1|Pair)", "(1|Subject) + (1|Pair)")
  fixed_models <- c("intercept only", "ratio", "difference", "difference + ratio", "difference * ratio")

  random_bf_df <- tibble(
    Model = random_models,
    AIC = random_comparison_table$AIC,
    BIC = random_comparison_table$BIC,
    BF = random_comparison_table$BF
  )

  fixed_bf_df <- tibble(
    Model = fixed_models,
    AIC = fixed_comparison_table$AIC,
    BIC = fixed_comparison_table$BIC,
    BF = fixed_comparison_table$BF
  )

  random_bf_table <- apa_table(random_bf_df)
  fixed_bf_table <- apa_table(fixed_bf_df)

  # Create Output to use for manuscript
  output <- list(ttest = large_pref_ttest, ttestbf = large_pref_ttest_bf, CI_difference = choice_means_subject_diff, CI_ratio = choice_means_ratio_means, best_model_fit = bestfit, diff_fig = diff_bird_graph, ratio_fig = ratio_bird_graph, random_table = random_bf_table, fixed_table = fixed_comparison_table)
}


# Import data ------------------------------------------------------------------

all_data <- read_csv(here("data/wolff_etal_2022_data.csv"))

# Separate out data for each experiment
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


# Analyze data -----------------------------------------------------------------

food1_results <- analyze_data(food1, "food", "1")
food2_results <- analyze_data(food2, "food", "2")
social1_results <- analyze_data(social1, "social", "1")
social2_results <- analyze_data(social2, "social", "2")


# Build plots ------------------------------------------------------------------

# Food
food_figures <- food1_results$diff_fig + food1_results$ratio_fig +
  food2_results$diff_fig + food2_results$ratio_fig +
  plot_annotation(tag_levels = "A")
ggsave(here("figures/food_figure.png"), width = 14, height = 10)

# Social
social_figures <- social1_results$diff_fig + social1_results$ratio_fig +
  social2_results$diff_fig + social2_results$ratio_fig +
  plot_annotation(tag_levels = "A")
ggsave(here("figures/social_figure.png"), width = 14, height = 10)


