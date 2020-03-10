library(purrr)
library(here)
library(readr)

reading_file <- function(file_path, file_name) {
  list.files(path = here("data/raw/2020DataFiles/2020-Womens-Data/"), pattern = "*.csv") %>% 
    map_df(read_csv)
}

