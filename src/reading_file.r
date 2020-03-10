library(purrr)
library(here)
library(readr)

reading_file <- function(file_path, file_name) {
  
  list.files(path = "C:/Users/mariana.pasqualini/madness/data/raw/2020DataFiles/2020-Womens-Data/", pattern = "*.csv") %>% 
    lapply(read_csv)
}


arquivos <- list.files(path = here("data/raw/2020DataFiles/2020-Womens-Data"), pattern = "*.csv", full.names=T)
map_df(read_csv(file.path(here("data/raw/2020DataFiles/2020-Womens-Data"))), arquivos) %>% 
  
a <- map(arquivos, read_csv)

list.files(path = here("data/raw/2020DataFiles/2020-Womens-Data"), pattern = "*.csv", full.names=T) %>% 
  map(read_csv) %>% 
    do.call()
