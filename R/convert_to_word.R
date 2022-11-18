library(tidyverse)
read_file("docs/wolff_etal_2022.Rmd") |>
  str_replace(": \"doc\"", ": \"man\"") |>
  # str_replace("figsintext        : yes", "floatsintext        : no") |>
  str_replace("papaja::apa6_pdf", "papaja::apa6_word") |>
  write_file("docs/wolff_etal_2022_word.Rmd")
rmarkdown::render("docs/wolff_etal_2022_word.Rmd")

# Still need to:
# 1. Remove figures
# 2. Move figure captions to end
# 3. Add line numbers.
