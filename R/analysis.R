
# Libraries--------
library(tidyverse)
library(lme4)
library(BayesFactor)
library(broom)
library(bayestestR)
library(performance)
library(here)
library(papaja)

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
  labs(y="percent_largerent larger choosen", x = "Ratio")+
  geom_line(aes(group = subject, color = subject), alpha = 0.5)+
  geom_point(data = ratiosummary, size = 2)+
  geom_errorbar(data = ratio_bird_summary_means, aes(x=ratio, ymin= lower, ymax = upper), width = 0)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)+
  scale_x_continuous(breaks = c(.17, .2, .25,.33,.4, .5, .6, .67, .75, .8, .83))


#Graph Difference by subject bird

diff_bird_graph <- ggplot(data = diff_bird_summary, aes(x=difference, y= percent_larger)) +
  geom_line(aes(group = subject, color = subject), alpha = 0.5)+
  geom_point(data = diffsummary, size = 2) +
  geom_errorbar(data = diff_bird_summary_means, aes(x=difference, ymin= lower, ymax = upper), width = 0)+
  theme_bw(base_size = 22)+
  theme(legend.position =  "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs( y="Percent larger choosen", x = "Difference")+
  geom_hline(yintercept = 50, linetype = "dashed")+
  ylim(20,100)

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

output <- list(ttest = large_pref_ttest, ttestbf = large_pref_ttest_bf, CI_difference = diff_bird_summary_means, CI_ratio = ratio_bird_summary_means,  best_model_fit = bestfit, diff_fig = diff_bird_graph, ratio_fig = ratio_bird_graph, random_table = random_bf_table, fixed_table = fixed_bf_table)
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
  filter(!subject %in% c("Baloo"))

social2 <- all_data |>
  filter(study == "social" & rep == 2) |>
  filter(!subject %in% c())


# Analyze data ------------------------------------------------------------


food1_results <- analyze_data(food1, "food", "1")
food2_results <- analyze_data(food2, "food", "2")
social1_results <- analyze_data(social1, "social", "1")
social2_results <- analyze_data(social2, "social", "2")


# Build plots -------------------------------------------------------------

library(patchwork)
food_figures <- food1_results$diff_fig + food1_results$ratio_fig + food2_results$diff_fig + food2_results$ratio_fig + plot_annotation(tag_levels = 'A')

social_figures <- social1_results$diff_fig + social1_results$ratio_fig + social2_results$diff_fig + social2_results$ratio_fig + plot_annotation(tag_levels = 'A')


# Stooge Preferences -------------------------------

#input data
data_phase_1 <- read_csv(here("data/phase_1_social_complete.csv"))

#data clean-up
df <- data_phase_1 %>%
  select(id, subject_bird, small_num, large_num, large_side, choice, smallbirds, largebirds) %>%
  drop_na()

df$large_choice <- ifelse(df$choice == df$large_side, "Y", "N")

df$choice_num <- ifelse(df$large_choice == "Y", 1, 0)

df <- df %>%
  mutate(subject_bird = str_to_title(subject_bird),
         choice_num = as.character(choice_num)) %>%
  filter(subject_bird != "Baloo")

#create sex labels for each of our subject birds

female_birds <- c("Robin", "Juniper", "Hippo", "Flute")

df$bird_sex <- ifelse(df$subject_bird %in% female_birds, "female", "male")

df <- df %>%
  select(id, subject_bird, bird_sex, everything())

View(df)

# Create column of birds that were choosen and create columns showing how often each bird was choosen and not choosen.

df$choosenbirds <- ifelse(df$large_choice == "Y", df$largebirds, df$smallbirds)

df <- df %>%
  mutate(Cash = ifelse(str_detect(choosenbirds, "Cash"), 1, 0),
         Scully = ifelse(str_detect(choosenbirds, "Scully"), 1, 0),
         Mork = ifelse(str_detect(choosenbirds, "Mork"), 1, 0),
         Mulder = ifelse(str_detect(choosenbirds, "Mulder"), 1, 0),
         Ariel = ifelse(str_detect(choosenbirds, "Ariel"), 1, 0),
         Pease = ifelse(str_detect(choosenbirds, "Pease"), 1, 0),
         Hagrid = ifelse(str_detect(choosenbirds, "Hagrid"), 1, 0),
         Egeus = ifelse(str_detect(choosenbirds, "Egeus"), 1, 0),
         Commanche = ifelse(str_detect(choosenbirds, "Commanche"), 1, 0),
         Sapphire = ifelse(str_detect(choosenbirds, "Sapphire"), 1, 0),
         Zappa = ifelse(str_detect(choosenbirds, "Zappa"), 1, 0),
         Quince = ifelse(str_detect(choosenbirds, "Quince"), 1, 0),
         Sebastan = ifelse(str_detect(choosenbirds, "Sebastan"), 1, 0),
         Hermia = ifelse(str_detect(choosenbirds, "Hermia"), 1, 0),
         Saffron = ifelse(str_detect(choosenbirds, "Saffron"), 1, 0))

# Create column of birds that were NOT chosen and create columns showing how often each bird was NOT choosen and not choosen.

df$loserbirds <- ifelse(df$large_choice == "N", df$largebirds, df$smallbirds)

df <- df %>%
  mutate(Cash_l = ifelse(str_detect(loserbirds, "Cash"), 1, 0),
         Scully_l = ifelse(str_detect(loserbirds, "Scully"), 1, 0),
         Mork_l = ifelse(str_detect(loserbirds, "Mork"), 1, 0),
         Mulder_l = ifelse(str_detect(loserbirds, "Mulder"), 1, 0),
         Ariel_l = ifelse(str_detect(loserbirds, "Ariel"), 1, 0),
         Pease_l = ifelse(str_detect(loserbirds, "Pease"), 1, 0),
         Hagrid_l = ifelse(str_detect(loserbirds, "Hagrid"), 1, 0),
         Egeus_l = ifelse(str_detect(loserbirds, "Egeus"), 1, 0),
         Commanche_l = ifelse(str_detect(loserbirds, "Commanche"), 1, 0),
         Sapphire_l = ifelse(str_detect(loserbirds, "Sapphire"), 1, 0),
         Zappa_l = ifelse(str_detect(loserbirds, "Zappa"), 1, 0),
         Quince_l = ifelse(str_detect(loserbirds, "Quince"), 1, 0),
         Sebastan_l = ifelse(str_detect(loserbirds, "Sebastan"), 1, 0),
         Hermia_l = ifelse(str_detect(loserbirds, "Hermia"), 1, 0),
         Saffron_l = ifelse(str_detect(loserbirds, "Saffron"), 1, 0))

#graph of values for the relationship of birds across the entire experiment

df <- relocate(df, loserbirds, .before = Cash)

#graph of values for the relationship of birds looking at the sex of subject birds

sum_stooge_rel_sex <- df %>%
  filter(small_num != 0) %>%
  group_by(bird_sex) %>%
  summarize(across(Cash:Saffron_l, sum)) %>%
  pivot_longer(-bird_sex, names_to = "individual", values_to="presence") %>%
  mutate(chosen = ifelse(grepl(x = individual, pattern = "_l"), "unchosen", "chosen"), individual=str_replace(individual, "_l", "")) %>%
  unite(sex_chosen, c("bird_sex", "chosen")) %>%
  pivot_wider(individual, names_from = sex_chosen, values_from = presence) %>%
  mutate(total_trials = female_chosen + male_chosen + female_unchosen + male_unchosen,
         female_percent = female_chosen/(female_chosen+ female_unchosen)*100,
         male_percent = male_chosen/(male_chosen + male_unchosen)*100,
         overall_percent = (female_chosen + male_chosen) / (female_chosen + male_chosen + female_unchosen + male_unchosen)*100) %>%
  arrange(overall_percent)


sum_stooge_rel_sex <- sum_stooge_rel_sex %>%
  select(individual, female_percent, male_percent, overall_percent)

