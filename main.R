# aire_madrid -------------------------------------------------------------
# packages ----------------------------------------------------------------
library(data.table)
library(stringr)
library(XML)
library(xml2)
library(config)
library(zoo)


# config ------------------------------------------------------------------
config <- config::get(file = "conf/main.yml")
dictionaries <- config$dictionaries
output <- config$output


# load dictionaries -------------------------------------------------------
col_structure    <- readRDS(dictionaries$columns_structure)
magnitudes_names <- readRDS(dictionaries$magnitudes_names)
station_names    <- readRDS(dictionaries$station_names)

# Data download -----------------------------------------------------------
page <- read_xml(config$datos_madrid_url)
data <- xmlParse(page)
xml_data <- xmlToList(data)
location <- as.list(xml_data[["Catalog"]][["dataset"]][["Dataset"]])
data <- location[names(location) == "distribution"]
links <- 
  data.table(
    year = sapply(X = data, function(x) x[["Distribution"]][["title"]][["text"]]),
    link = sapply(X = data, function(x) x[["Distribution"]][["accessURL"]][["text"]])
  )

links[, file_name := paste0('datos_aire_madrid_', year, '.zip')]

# Download. The last year always for update, check if there are previous years
for (i in links[, year]) {
  if (i == max(links[, year])) {
    download.file(url = links[year == i, link],
                  destfile = paste0(
                    config$raw_data_path,
                    links[year == i, file_name]))
    unzip(
      paste0(config$raw_data_path, links[year == i, file_name]),
      overwrite = TRUE,
      exdir = paste0(config$raw_data_path, i)
    )
  }
  
  if (!dir.exists(paste0(config$raw_data_path, i))) {
    download.file(url = links[year == i, link],
                  destfile = paste0(
                    config$raw_data_path,
                    links[year == i, file_name]))
    unzip(
      paste0(config$raw_data_path,
             links[year == i, file_name]),
      overwrite = TRUE,
      exdir = paste0(config$raw_data_path, i)
    )
  }
  
}

# list of all files
all_files <- list.files(path = config$raw_data_path, recursive = T)

# get only txt data files 
txt_data_files <- list.files(path = config$raw_data_path,
                             recursive = TRUE,
                             pattern = '.txt')

# remove unnecessary files 
file.remove(paste0(config$raw_data_path,
                   all_files[!(all_files %in% txt_data_files)]))

# oct 2017 change in data structure
txt_data_files_before_oct_2017 <-
  c(str_subset(txt_data_files, pattern = paste(2001:2016, collapse = '|')),
    paste0('2017/', str_subset(list.files(path = paste0(
      config$raw_data_path,
      '2017'), pattern = '.txt'),
      pattern = c('oct|nov|dic'), negate = TRUE))
  )

txt_data_files_after_oct_2017 <-
  txt_data_files[!(txt_data_files %in% txt_data_files_before_oct_2017)]

data_files <- 
  data.table(files = c(
    txt_data_files_before_oct_2017,
    txt_data_files_after_oct_2017),
    txt_format = c(
      rep('old', length(txt_data_files_before_oct_2017)),
          rep('new', length(txt_data_files_after_oct_2017))))
    
dt <- data.table()

for (i in data_files[, files]) {
  dt_aux <- fread(paste0(config$raw_data_path, i))
  names(dt_aux) <- col_structure[, name_cols]
  dt <- rbind(dt_aux, dt)
}

dt <- rbind(dt_before_oct_2017, dt_after_oct_2017)
cols_to_numeric <- str_subset(names(dt), pattern = '^V', negate = T)
dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]
dt[, ANO := fifelse(ANO < 100, ANO + 2000, ANO)]

# Save
saveRDS(dt, output$wide_data)


# Reshaping data for use --------------------------------------------------
dt_sub <- dt[magnitudes_names, on = 'MAGNITUD']
dt_sub <- dt_sub[station_names, on = c('ESTACION' = 'id_numeric')]

dt_sub[, fecha := as.Date(paste(ANO, MES, DIA, sep = '-'))]
hour_cols <- str_subset(names(dt_sub), pattern = '^H')
cols <- c(config$subset_id_cols, hour_cols)

dt_sub <- melt(
  dt_sub[, ..cols],
  id.vars = config$subset_id_cols,
  measure.vars = hour_cols,
  value.name = 'valor',
  variable.name = 'hora'
)

dt_sub[, hora := gsub('H', '', hora)]
# eso tarda un poco
dt_sub[, date := as.POSIXct(
  paste(fecha, hora),
  tz = 'Europe/Paris',
  format = '%Y-%m-%d %H')]


# Save --------------------------------------------------------------------
saveRDS(dt_sub, output$subseted_long_data)
