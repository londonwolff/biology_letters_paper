
# Stooge Preferences -------------------------------
#Analysis for individual preference per bird. Do not run with the regular script for right now when running all the graphs.

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

