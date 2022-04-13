# Load libraries ----------------------------------------------------------

library(dataReporter)
library(readxl)
library(tidyverse)
library(lubridate)
library(here)


# Create functions --------------------------------------------------------

# Import Excel file and combine separate sheets into one data frame
combine_sheets <- function(path, coltypes) {
  path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(~ read_excel(path = path, sheet = .x, range = "A1:R151", col_types = coltypes), .id = "sheet")
}


# Import and combine data -------------------------------------------------

coltypes_n1 <- c("text", "text", "numeric", "numeric", "text", "text", "text", "date", "numeric", rep("text", 9))
food_raw_rep1 <- combine_sheets(here("data/phase_1_nonsocial_complete.xlsx"), coltypes_n1) %>%
  select(subject = sheet, date = Date, session = Session...9, small_num, large_num, large_side, choice = Choice...12) %>%
  mutate(study = "food", .before = subject,
         rep = 1) %>%
  mutate(smallbirds = NA,
         largebirds = NA, .before = choice)
food_raw_rep1[which(food_raw_rep1$subject == "He-man" & food_raw_rep1$session == 35), ]$date <- as_date("2021-04-30")

coltypes_n2 <- c("text", "text", "numeric", "numeric", "text", "text", "text", "date", "numeric", rep("text", 9))
food_raw_rep2 <- combine_sheets(here("data/phase_2_nonsocial_complete.xlsx"), coltypes_n2) %>%
  select(subject = sheet, date = Date, session = Session...9,  small_num, large_num, large_side, choice = Choice) %>%
  mutate(study = "food", .before = subject,
         rep = 2) %>%
  mutate(smallbirds = NA,
         largebirds = NA, .before = choice)

social_raw_rep1 <- read_csv(here("data/phase_1_social_complete.csv"), col_types = "nccnnccccccccccc") %>%
  select(subject = subject_bird, date, session = id, small_num, large_num, large_side, smallbirds, largebirds, choice) %>%
  mutate(date = parse_date(date, "%m/%d/%Y"),
         session = trunc(session)) %>%
  mutate(study = "social", .before = subject,
         rep = 1)
social_raw_rep1[which(social_raw_rep1$subject == "hippo" & social_raw_rep1$session == 28), ]$large_side <- "R"

coltypes_s1 <- c("numeric", "text", "numeric", "text", "numeric", "text", "text", "text", "date", rep("text", 6))
social_raw_rep2 <- read_xlsx(here("data/phase_2_social_complete.xlsx")) %>%
  select(subject = subject_bird, date, session = id, small_num, large_num, large_side, smallbirds, largebirds, choice) %>%
  mutate(session = trunc(session)) %>%
  mutate(study = "social", .before = subject,
         rep = 2)

# *****Check this list!!!******
female_birds <- c("Flute", "Hippolyta", "Juniper", "Robin", "Saffron", "Uno")

all_data <- bind_rows(food_raw_rep1, food_raw_rep2, social_raw_rep1, social_raw_rep2) %>%
  fill(date) %>%
  fill(session) %>%
  filter(subject != "baloo") %>%
  mutate(subject = fct_collapse(subject,
                                Basil = "basil",
                                "Black Elk" = "black elk",
                                Chicklet = "chicklet",
                                Dartagnan = c("Dart", "Dartangun"),
                                Dill = "dill",
                                Flute = "flute",
                                Hippolyta = "hippo",
                                Juan = "juan",
                                Juniper = "juniper",
                                Robin = "robin"),
         pair = paste(small_num, large_num, sep = "-"), .before = small_num) %>%
  mutate(sex = ifelse(subject %in% female_birds, "Female", "Male"), .after = subject) %>%
  mutate(choice = str_replace(choice, "NEEDS TO BE REDONE, ANIMAL CARE STAFF CAME IN TO TALK TO ME AND ROBIN ATE FROM BOTH SIDES", NA_character_),
         choice = str_replace_all(choice, "NC", NA_character_),
         choice = str_replace_all(choice, "L\\*", "L"),
         choose_larger = ifelse(large_side == choice, 1, ifelse(is.na(choice), NA, 0))
         )

write_csv(all_data, here("data/wolff_etal_2022_data.csv"))

makeCodebook(all_data, file = here("data/codebook_number_data.Rmd"), reportTitle = "Codebook for number data", replace = TRUE)
