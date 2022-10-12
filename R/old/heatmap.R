library(tidyverse)
library(here)

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

#Creating heatmap for individual difference
heatmap_df_1 <- individual_preference_df |>
  filter(individual_preference_df$rep == "1") |>
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
         Black_Elk_rejected = "Black Elk_rejected") |>
  mutate(
    Basil = Basil_chosen / (Basil_chosen + Basil_rejected) *100,
    Black_Elk = Black_Elk_chosen / (Black_Elk_chosen + Black_Elk_rejected) *100,
    Dill = Dill_chosen / (Dill_chosen + Dill_rejected) *100,
    Flute = Flute_chosen / (Flute_chosen + Flute_rejected) *100,
    Juan = Juan_chosen / (Juan_chosen + Juan_rejected) *100,
    Juniper = Juniper_chosen / (Juniper_chosen + Juniper_rejected) *100,
    Robin = Robin_chosen / (Robin_chosen + Robin_rejected) *100,
    Rooster = Rooster_chosen / (Rooster_chosen + Rooster_rejected) *100) |>
  select(individual, Basil:Rooster)|>
  na.omit()

heatmap_df_long_1 <- heatmap_df_1 |>
  pivot_longer(-individual, names_to = "subject", values_to = "percent") |>
  mutate(replicate = 1, .before = 1)

heatmap_1 <- heatmap_df_long_1 |>
  ggplot(aes(x = subject, y = individual, fill = percent))+
  geom_tile()+
  scale_fill_continuous(low = "yellow",
                        high = "blue",
                        name = "Percent Choosen")+
  labs(y = "Stooge Birds", x = "Subject Birds")

heatmap_1



#heatmap 2
heatmap_df_2 <- individual_preference_df |>
  filter(individual_preference_df$rep == "2") |>
  group_by(subject) |>
  summarize(across(Cash:Chicklet_rejected, ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(-subject, names_to = "individual", values_to = "presence") |>
  mutate(
    chosen = ifelse(grepl(x = individual, pattern = "_rejected"), "rejected", "chosen"),
    individual = str_replace(individual, "_rejected", "")
  ) |>
  unite(subject_chosen, c("subject", "chosen")) |>
  pivot_wider(individual, names_from = subject_chosen, values_from = presence) |>
  rename(Heman_chosen = "He-man_chosen",
         Heman_rejected = "He-man_rejected") |>
  mutate(
    Dartagnan = Dartagnan_chosen / (Dartagnan_chosen + Dartagnan_rejected) *100,
    Dumbledore = Dumbledore_chosen / (Dumbledore_chosen + Dumbledore_rejected) *100,
    Fern = Fern_chosen / (Fern_chosen + Fern_rejected) *100,
    Fozzie = Fozzie_chosen / (Fozzie_chosen + Fozzie_rejected) *100,
    Heman = Heman_chosen / (Heman_chosen + Heman_rejected) *100,
    Mork = Mork_chosen / (Mork_chosen + Mork_rejected) *100,
    Mote = Mote_chosen / (Mote_chosen + Mote_rejected) *100,
    Mulder = Mulder_chosen / (Mulder_chosen + Mulder_rejected) *100,
    Prudence = Prudence_chosen / (Prudence_chosen + Prudence_rejected) *100,
    Uno = Uno_chosen / (Uno_chosen + Uno_rejected) *100)|>
  select(individual, Dartagnan:Uno)|>
  na.omit()

heatmap_df_long_2 <- heatmap_df_2 |>
  pivot_longer(-individual, names_to = "subject", values_to = "percent") |>
  mutate(replicate = 2, .before = 1)

heatmap_2 <- heatmap_df_long_2 |>
  ggplot(aes(x = subject, y = individual, label = percent, fill = percent))+
  geom_tile()+
  scale_fill_continuous(low = "yellow",
                        high = "blue",
                        name = "Percent Choosen")+
  labs(y = "Stooge Birds", x = "Subject Birds")

heatmap_2


heatmap_df_long <- bind_rows(heatmap_df_long_1, heatmap_df_long_2)

heatmap_df_long |>
  ggplot(aes(x = subject, y = individual, label = percent, fill = percent)) +
  geom_tile() +
  facet_wrap(~replicate, scales = "free") +
  scale_fill_gradient2(midpoint = 50) +
  labs(y = "Stooge Birds", x = "Subject Birds") +
  theme_bw()
