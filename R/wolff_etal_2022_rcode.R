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
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licencor endorses you or your use.
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
library(ggcorrplot)

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

  random_bf_table <- (random_bf_df)
  fixed_bf_table <- (fixed_bf_df)

  # Create Output to use for manuscript
  output <- list(ttest = large_pref_ttest, ttestbf = large_pref_ttest_bf, CI_difference = choice_means_subject_diff, CI_ratio = choice_means_ratio_means, best_model_fit = bestfit, diff_fig = diff_bird_graph, ratio_fig = ratio_bird_graph, random_table = random_bf_table, fixed_table = fixed_bf_table, fixed_bf_df = fixed_bf_df)
}


# Import data ------------------------------------------------------------------

all_data <- read_csv(here("data/wolff_etal_2022_data.csv"))

# Separate out data for each experiment
food1 <- all_data |>
  filter(study == "food" & rep == 1) |>
  filter(!subject %in% c("Mulder", "Dartagnan"))

food2 <- all_data |>
  filter(study == "food" & rep == 2) |>
  filter(!subject %in% c("Basil", "Robin"))

social1 <- all_data |>
  filter(study == "social" & rep == 1) |>
  filter(!subject %in% c("Baloo")) |>
  filter(!small_num %in% 0)

social2 <- all_data |>
  filter(study == "social" & rep == 2) |>
  filter(!subject %in% c()) |>
  filter(!small_num %in% 0)

combined_data <- bind_rows(food1, food2, social1, social2)


# Analyze data -----------------------------------------------------------------

food1_results <- analyze_data(food1, "food", "1")
food2_results <- analyze_data(food2, "food", "2")
social1_results <- analyze_data(social1, "social", "1")
social2_results <- analyze_data(social2, "social", "2")


# Build plots ------------------------------------------------------------------

# Food
food_figures <- food1_results$diff_fig + food1_results$ratio_fig +
  food2_results$diff_fig + food2_results$ratio_fig +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(here("figures/food_figure.png"), width = 14, height = 10)

# Social
social_figures <- social1_results$diff_fig + social1_results$ratio_fig +
  social2_results$diff_fig + social2_results$ratio_fig +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(here("figures/social_figure.png"), width = 14, height = 10)


# Supplementary materials ---------------------------------------

## Calculate mean trials per session ---------------------

sessions_avg_food1 <- food1 %>%
  group_by(subject, session) %>%
  summarize(n())

sessions_avg_food2 <- food2 %>%
  group_by(subject, session) %>%
  summarize(n())

session_mean <- mean(c(sessions_avg_food1$`n()`,sessions_avg_food2$`n()`))


## Demographic table ------------------------

bird_ages <- c(12, 10, 11, 15, 12, 12, 12, 14, 11, 10, 14, 12, 15, 15, 12, 14, 14, 15, 10, 12, 19)

subject_bird_info <- combined_data |>
  unite(unique_code, c(study, rep)) |>
  group_by(unique_code, sex, subject) |>
  summarise(n = n()) |>
  pivot_wider(names_from = unique_code, values_from = n, values_fill = 0) |>
  select(subject, everything()) |>
  mutate(across(contains("_"), ~ ifelse(.x == "0", "", "X"))) |>
  add_column(age = bird_ages) |>
  select(subject, sex, age, everything())


## Table of factorial pairs ------------

factorial_pairs_df <- data.frame(
  Pair = c("1/2", "1/3", "1/4", "1/5", "1/6", "2/3", "2/4", "2/5", "2/6", "3/4", "3/5", "3/6", "4/5", "4/6", "5/6"),
  Ratio = c("0.50", "0.33", "0.25", "0.20", "0.17", "0.67", "0.50", "0.40", "0.33", "0.75", "0.60", "0.50", "0.80", "0.67", "0.83"),
  Difference = c("1", "2", "3", "4", "5", "1", "2", "3", "4", "1", "2", "3", "1", "2", "1"),
  Social_2 = c("X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "X", "", "", "", "")
)


## Random effect models ----------------

random_effect_df <- data.frame(
  Model = c("Intercept Only Model", "Subject Only Model", "Pair Only Model", "Both Subject and Pair"),
  Formula = c("Choice~1", "Choice~(1|Subject)", "Choice~(1|Pair)", "Choice~(1|Subject)+(1|Pair)")
)


## Fixed effect models ---------------

fixed_effect_df <- data.frame(
  Model = c("Intercept Only Model", "Ratio Only Model", "Difference Only Model", "Both Fixed Effects, No Interaction", "Both Fixed Effects, With Interaction"),
  Formula = c("Choice~1", "Choice~Ratio", "Choice~Difference", "Choice~Ratio+Difference", "Choice~Ratio*Difference")
)


## Bird preference table--------------------------

# Create column of birds that were chosen and create columns showing how often each bird was chosen and not chosen.

individual_preference_df <- combined_data |>
  filter(study != "food") |>
  mutate(
    chosenbirds = ifelse(choose_larger == "1", largebirds, smallbirds),
    Cash = ifelse(str_detect(chosenbirds, "Cash"), 1, 0),
    Scully = ifelse(str_detect(chosenbirds, "Scully"), 1, 0),
    Mork = ifelse(str_detect(chosenbirds, "Mork"), 1, 0),
    Mulder = ifelse(str_detect(chosenbirds, "Mulder"), 1, 0),
    Ariel = ifelse(str_detect(chosenbirds, "Ariel"), 1, 0),
    Pease = ifelse(str_detect(chosenbirds, "Pease"), 1, 0),
    Hagrid = ifelse(str_detect(chosenbirds, "Hagrid"), 1, 0),
    Egeus = ifelse(str_detect(chosenbirds, "Egeus"), 1, 0),
    Comanche = ifelse(str_detect(chosenbirds, "Commanche"), 1, 0),
    Sapphire = ifelse(str_detect(chosenbirds, "Sapphire"), 1, 0),
    Zappa = ifelse(str_detect(chosenbirds, "Zappa"), 1, 0),
    Quince = ifelse(str_detect(chosenbirds, "Quince"), 1, 0),
    Sebastian = ifelse(str_detect(chosenbirds, "Sebastan"), 1, 0),
    Hermia = ifelse(str_detect(chosenbirds, "Hermia"), 1, 0),
    Saffron = ifelse(str_detect(chosenbirds, "Saffron"), 1, 0),
    Hippolyta = ifelse(str_detect(chosenbirds, "Hippo"), 1, 0),
    Chicklet = ifelse(str_detect(chosenbirds, "Chicklet"), 1, 0),
    rejectedbirds = ifelse(choose_larger == "0", largebirds, smallbirds),
    Cash_rejected = ifelse(str_detect(rejectedbirds, "Cash"), 1, 0),
    Scully_rejected = ifelse(str_detect(rejectedbirds, "Scully"), 1, 0),
    Mork_rejected = ifelse(str_detect(rejectedbirds, "Mork"), 1, 0),
    Mulder_rejected = ifelse(str_detect(rejectedbirds, "Mulder"), 1, 0),
    Ariel_rejected = ifelse(str_detect(rejectedbirds, "Ariel"), 1, 0),
    Pease_rejected = ifelse(str_detect(rejectedbirds, "Pease"), 1, 0),
    Hagrid_rejected = ifelse(str_detect(rejectedbirds, "Hagrid"), 1, 0),
    Egeus_rejected = ifelse(str_detect(rejectedbirds, "Egeus"), 1, 0),
    Comanche_rejected = ifelse(str_detect(rejectedbirds, "Commanche"), 1, 0),
    Sapphire_rejected = ifelse(str_detect(rejectedbirds, "Sapphire"), 1, 0),
    Zappa_rejected = ifelse(str_detect(rejectedbirds, "Zappa"), 1, 0),
    Quince_rejected = ifelse(str_detect(rejectedbirds, "Quince"), 1, 0),
    Sebastian_rejected = ifelse(str_detect(rejectedbirds, "Sebastan"), 1, 0),
    Hermia_rejected = ifelse(str_detect(rejectedbirds, "Hermia"), 1, 0),
    Saffron_rejected = ifelse(str_detect(rejectedbirds, "Saffron"), 1, 0),
    Hippolyta_rejected = ifelse(str_detect(rejectedbirds, "Hippo"), 1, 0),
    Chicklet_rejected = ifelse(str_detect(rejectedbirds, "Chicklet"), 1, 0)
  ) |>
  relocate(rejectedbirds, .before = Cash)


# creating table of values for replication 1

individual_preference_table_1 <- individual_preference_df |>
  filter(rep == "1") |>
  group_by(sex) |>
  summarize(across(Cash:Chicklet_rejected, sum)) |>
  pivot_longer(-sex, names_to = "individual", values_to = "presence") |>
  mutate(
    chosen = ifelse(grepl(x = individual, pattern = "_rejected"), "rejected", "chosen"),
    individual = str_replace(individual, "_rejected", "")
  ) |>
  unite(sex_chosen, c("sex", "chosen")) |>
  pivot_wider(individual, names_from = sex_chosen, values_from = presence) |>
  mutate(
    total_trials = Female_chosen + Male_chosen + Female_rejected + Male_rejected,
    female_percent = Female_chosen / (Female_chosen + Female_rejected) * 100,
    male_percent = Male_chosen / (Male_chosen + Male_rejected) * 100,
    overall_percent = (Female_chosen + Male_chosen) / (Female_chosen + Male_chosen + Female_rejected + Male_rejected) * 100
  ) |>
  mutate(experiment = 1, .before = 1) |>
  arrange(overall_percent) |>
  filter(overall_percent != 0)

# creating table of values for replication 2
individual_preference_table_2 <- individual_preference_df |>
  filter(rep == "2") |>
  group_by(sex) |>
  summarize(across(Cash:Chicklet_rejected, ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(-sex, names_to = "individual", values_to = "presence") |>
  mutate(
    chosen = ifelse(grepl(x = individual, pattern = "_rejected"), "rejected", "chosen"),
    individual = str_replace(individual, "_rejected", "")
  ) |>
  unite(sex_chosen, c("sex", "chosen")) |>
  pivot_wider(individual, names_from = sex_chosen, values_from = presence) |>
  mutate(
    total_trials = Female_chosen + Male_chosen + Female_rejected + Male_rejected,
    female_percent = Female_chosen / (Female_chosen + Female_rejected) * 100,
    male_percent = Male_chosen / (Male_chosen + Male_rejected) * 100,
    overall_percent = (Female_chosen + Male_chosen) / (Female_chosen + Male_chosen + Female_rejected + Male_rejected) * 100
  ) |>
  mutate(experiment = 2, .before = 1) |>
  arrange(overall_percent) |>
  filter(overall_percent != 0)

individual_preference_table <- bind_rows(individual_preference_table_1, individual_preference_table_2) |>
  mutate(sex = c("F", "F", "M", "M", "M", "M", "M", "F", "M", "M", "F", "F", "M", "M", "F", "M", "M", "F", "F", "M", "M", "M", "F"),
         sex = fct_relevel(sex, "M", "F"),
         .after = individual) |>
  arrange(experiment, sex, overall_percent)

#Creating corrplot for individual difference
heatmap_df <- individual_preference_df |>
  group_by(subject) |>
  summarize(across(Cash:Chicklet_rejected, ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(-subject, names_to = "individual", values_to = "presence") |>
  mutate(
    chosen = ifelse(grepl(x = individual, pattern = "_rejected"), "rejected", "chosen"),
    individual = str_replace(individual, "_rejected", "")
  ) |>
  unite(subject_chosen, c("subject", "chosen")) |>
  pivot_wider(individual, names_from = subject_chosen, values_from = presence) |>
  rename(Black_Elk_chosen = "Black Elk_chosen",
         Black_Elk_rejected = "Black Elk_rejected",
         Heman_chosen = "He-man_chosen",
         Heman_rejected = "He-man_rejected") |>
  mutate(
    Basil = Basil_chosen / (Basil_chosen + Basil_rejected) *100,
    Black_Elk = Black_Elk_chosen / (Black_Elk_chosen + Black_Elk_rejected) *100,
    Chicklet = Chicklet_chosen / (Chicklet_chosen + Chicklet_rejected) *100,
    Dartagnan = Dartagnan_chosen / (Dartagnan_chosen + Dartagnan_rejected) *100,
    Dill = Dill_chosen / (Dill_chosen + Dill_rejected) *100,
    Dumbledore = Dumbledore_chosen / (Dumbledore_chosen + Dumbledore_rejected) *100,
    Fern = Fern_chosen / (Fern_chosen + Fern_rejected) *100,
    Flute = Flute_chosen / (Flute_chosen + Flute_rejected) *100,
    Fozzie = Fozzie_chosen / (Fozzie_chosen + Fozzie_rejected) *100,
    Heman = Heman_chosen / (Heman_chosen + Heman_rejected) *100,
    Hippolyta = Hippolyta_chosen / (Hippolyta_chosen + Hippolyta_rejected) *100,
    Juan = Juan_chosen / (Juan_chosen + Juan_rejected) *100,
    Juniper = Juniper_chosen / (Juniper_chosen + Juniper_rejected) *100,
    Mork = Mork_chosen / (Mork_chosen + Mork_rejected) *100,
    Mote = Mote_chosen / (Mote_chosen + Mote_rejected) *100,
    Mulder = Mulder_chosen / (Mulder_chosen + Mulder_rejected) *100,
    Prudence = Prudence_chosen / (Prudence_chosen + Prudence_rejected) *100,
    Robin = Robin_chosen / (Robin_chosen + Robin_rejected) *100,
    Rooster = Rooster_chosen / (Rooster_chosen + Rooster_rejected) *100,
    Uno = Uno_chosen / (Uno_chosen + Uno_rejected) *100)|>
  select(individual, Basil:Uno)
