# listando os nomes dos arquivos no diretorio passado na funcao (argumento)
# e, a partir deles, mapeando a funcao read_csv em cada um desses arquivos:s
reading_file <- function(file_path) {
list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>% 
    map(read_csv)
}

# equivalente com o lapply:
reading_file_base <- function(file_path) {
  list.files(path = file_path, pattern = "*.csv", full.names = TRUE) %>% 
    lapply(read_csv)
}

translate_wl <- function(dic, var, varname) {
  dic <- dic %>% filter()
}
