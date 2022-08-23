##
## Script name: wolff_etal_2022_supplementary.R
##
## Purpose of script: Analyze pinyon jay number preference data

library(tidyverse)
library(BayesFactor)
library(bayestestR)
library(papaja)
library(here)

# Create Visuals for Stooge Preference-----------------------------
#Analysis for individual preference per bird for the first repition

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

# View(df)

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

#Same code for 2nd repititon-----------------

#input data
data_rep_2 <- read_csv(here("data/phase_2_social_complete.csv"))

#data clean-up
df2 <- data_rep_2 %>%
  select(id, subject_bird, small_num, large_num, large_side, choice, smallbirds, largebirds) %>%
  drop_na()

df2$large_choice <- ifelse(df2$choice == df2$large_side, "Y", "N")

df2$choice_num <- ifelse(df2$large_choice == "Y", 1, 0)

df2 <- df2 %>%
  mutate(subject_bird = str_to_title(subject_bird),
         choice_num = as.character(choice_num))

#create sex labels for each of our subject birds

female_bird_rep_2 <- c("Uno")

df2$bird_sex <- ifelse(df2$subject_bird %in% female_bird_rep_2, "female", "male")

df2 <- df2 %>%
  select(id, subject_bird, bird_sex, everything())

# Create column of birds that were choosen and create columns showing how often each bird was choosen and not choosen.

df2$choosenbirds <- ifelse(df2$large_choice == "Y", df2$largebirds, df2$smallbirds)

df2 <- df2 %>%
  mutate(Cash = ifelse(str_detect(choosenbirds, "Cash"), 1, 0),
         Pease = ifelse(str_detect(choosenbirds, "Pease"), 1, 0),
         Egeus = ifelse(str_detect(choosenbirds, "Egeus"), 1, 0),
         Sapphire = ifelse(str_detect(choosenbirds, "Sapphire"), 1, 0),
         Zappa = ifelse(str_detect(choosenbirds, "Zappa"), 1, 0),
         Quince = ifelse(str_detect(choosenbirds, "Quince"), 1, 0),
         Hippo = ifelse(str_detect(choosenbirds, "Hippo"), 1, 0),
         Chicklet = ifelse(str_detect(choosenbirds, "Chicklet"), 1, 0),
  )

#graph of values for the relationship of birds across the entire experiment

df2$loserbirds <- ifelse(df2$large_choice == "N", df2$largebirds, df2$smallbirds)

df2 <- df2 %>%
  mutate(Cash_l = ifelse(str_detect(loserbirds, "Cash"), 1, 0),
         Pease_l = ifelse(str_detect(loserbirds, "Pease"), 1, 0),
         Egeus_l = ifelse(str_detect(loserbirds, "Egeus"), 1, 0),
         Sapphire_l = ifelse(str_detect(loserbirds, "Sapphire"), 1, 0),
         Zappa_l = ifelse(str_detect(loserbirds, "Zappa"), 1, 0),
         Quince_l = ifelse(str_detect(loserbirds, "Quince"), 1, 0),
         Hippo_l = ifelse(str_detect(loserbirds, "Hippo"), 1, 0),
         Chicklet_l = ifelse(str_detect(loserbirds, "Chicklet"), 1, 0),
  )

df2 <- relocate(df2, loserbirds, .before = Cash)

#graph of values for the relationship of birds looking at the sex of subject birds

ind_preference_table_2 <- df2 %>%
  filter(small_num != 0) %>%
  group_by(bird_sex) %>%
  summarize(across(Cash:Chicklet_l, sum)) %>%
  pivot_longer(-bird_sex, names_to = "individual", values_to="presence") %>%
  mutate(chosen = ifelse(grepl(x = individual, pattern = "_l"), "unchosen", "chosen"), individual=str_replace(individual, "_l", "")) %>%
  unite(sex_chosen, c("bird_sex", "chosen")) %>%
  pivot_wider(individual, names_from = sex_chosen, values_from = presence) %>%
  mutate(total_trials = female_chosen + male_chosen + female_unchosen + male_unchosen,
         female_percent = female_chosen/(female_chosen+ female_unchosen)*100,
         male_percent = male_chosen/(male_chosen + male_unchosen)*100,
         overall_percent = (female_chosen + male_chosen) / (female_chosen + male_chosen + female_unchosen + male_unchosen)*100) %>%
  arrange(overall_percent)


ind_preference_table_2 <- ind_preference_table_2 %>%
  select(individual, female_percent, male_percent, overall_percent)

#decdie if you want them in 2 tables or if you want it to be 2 large table. We talked about 1 large set up with every indivdiual birds sex and age listed. But should we maybe just do thtat for subjects. For example, make a different table with subject bird used, what experiments they were in and age, sex, etc. yes. I should make that.

#Create table of factorial pairs with corresponding differences and ratios------------

factorial_pairs_df <- data.frame(Pair=c("1/2","1/3","1/4","1/5","1/6","2/3","2/4","2/5","2/6","3/4","3/5","3/6","4/5","4/6","5/6"),
                                 Ratio=c("0.50","0.33","0.25","0.20","0.17","0.67","0.50","0.40","0.33","0.75","0.60","0.50","0.80","0.67","0.83"),
                                 Difference=c("1","2","3","4","5","1","2","3","4","1","2","3","1","2","1"))

##add row from rep 2 social pairs we completed.

factorial_pairs_table <- apa_table(factorial_pairs_df)


#Creating Fixed Effect Model Selection Table---------

fixed_effect_df <- data.frame(Model = c("Intercept Only Model", "Ratio Only Model","Difference Only Model", "Both Fixed Effects, No Interaction", "Both Fixed Effects, With Interaction"),
                              Formula = c("Choice~1","Choice~Ratio","Choice~Difference","Choice~Ratio+Difference","Choice~Ratio*Difference"))


fixed_effect_structure_table <-apa_table(fixed_effect_df)

#Creating Random Effect Model Selection Table-----------

random_effect_df <- data.frame(Model = c("Intercept Only Model", "Subject Only Model","Pair Only Model", "Both Subject and Pair"),
                               Formula = c("Choice~1","Choice~(1|Subject)","Choice~(1|Pair)","Choice~(1|Subject)+(1|Pair)"))

random_effect_structure_table <- apa_table(random_effect_df)

#Creating Bayes Factor tables for random & fixed effects

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

#add in 2nd replication to the above graphs and then change it so the numbers are pulling from a dataset on this page.

#How to pull from other dataset? It doesn't seem feasible to pull df in and then rerun all the modeling in this script to get the numbers I need. Or should I add this table to the end of the .rcode.R file?

random_social_bf_table <- apa_table(random_social_bf_df)

fixed_social_bf_table <- apa_table(fixed_social_bf_df)

#Subject Bird Demographic And Experiment Information--------------

all_data <- read_csv(here("data/wolff_etal_2022_data.csv"))

all_data <- all_data %>%
  unite(unique_code, c(study, rep))

code_summary <- all_data |>
  group_by(unique_code, sex, subject) |>
  summarise(n = n(),)

subject_bird_info <- code_summary %>%
  pivot_wider(names_from = unique_code, values_from = n, values_fill = 0) %>%
  select(subject, everything())

subject_bird_info$food_1 <-  ifelse(subject_bird_info$food_1 == "0", 0, 1)

subject_bird_info$food_2 <-  ifelse(subject_bird_info$food_2 == "0", 0, 1)

subject_bird_info$social_1 <-  ifelse(subject_bird_info$food_2 == "0", 0, 1)

subject_bird_info$social_2 <-  ifelse(subject_bird_info$food_2 == "0", 0, 1)

subject_bird_info_table <- apa_table(subject_bird_info)
