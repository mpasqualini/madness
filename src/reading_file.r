reading_file <- function(file_path) {
  #list.files(path = here("data/raw/2020DataFiles/2020-Womens-Data"), pattern = "*.csv", full.names=T)
  list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>% 
    map(read_csv)
}

