
# packages ----------------------------------------------------------------

library(data.table)
library(stringr)
library(XML)
library(zoo)

# Functions ---------------------------------------------------------------

#' xts2ts
#'
#' @param xts_data xts time series data in daily, monthly, quarterly and yearly frequency
#'
#' @return ts object

xts_ts <- function(xts_data) {
  freq_list <-
    data.table::data.table(
      freq = c('day', 'month', 'quarter', 'year'),
      freq_n = c(365, 12L, 4L, 1L),
      freq_format = c('%Y, %j' ,'%Y, %m', '%Y, %q', '%Y')
    )
  
  d_ferq <- xts::periodicity(xts_data)[["label"]]
  freq_n <- freq_list[freq == d_ferq, freq_n]
  freq_format <- freq_list[freq == d_ferq, freq_format]
  
  # Put NA if missing date
  empty <-
    zoo::zoo(order.by = seq.Date(zoo::index(xts_data)[1], zoo::index(xts_data)[nrow(xts_data)], by = d_ferq))
  no_misssing <- merge(xts_data, empty)
  
  if (d_ferq == 'quarter') {
    start_date <-
      format(zoo::as.yearqtr(xts::periodicity(xts_data)[["start"]]), freq_format)
    
  } else {
    start_date <-
      format(zoo::as.Date(xts::periodicity(xts_data)[["start"]]), freq_format)
  }
  
  stats::ts(zoo::coredata(no_misssing),
            start = as.integer(strsplit(start_date, split = ',')[[1]]),
            frequency = freq_n)
}


# working directory -------------------------------------------------------

# Create directory

if (!dir.exists('~/aire_madrid')) {
  dir.create('~/aire_madrid')  
}

if (!dir.exists('~/aire_madrid/raw_data')) {
  dir.create('raw_data')  
}

setwd('~/aire_madrid/')


# Stations Info data ------------------------------------------------------

# txt data structure
col_arrange <- 
  data.table(name_cols =
               c("PROVINCIA", "MUNICIPIO", "ESTACION", "MAGNITUD", "TECNICA", "PERIODO ANALISIS",
                 "ANO", "MES", "DIA", "H01", "V01", "H02", "V02", "H03", "V03", 
                 "H04", "V04", "H05", "V05", "H06", "V06", "H07", "V07", "H08", 
                 "V08", "H09", "V09", "H10", "V10", "H11", "V11", "H12", "V12", 
                 "H13", "V13", "H14", "V14", "H15", "V15", "H16", "V16", "H17", 
                 "V17", "H18", "V18", "H19", "V19", "H20", "V20", "H21", "V21", 
                 "H22", "V22", "H23", "V23", "H24", "V24"),
             var_length = c(2, 3, 3, rep(2, 6), rep(c(5, 1), 24))
  )

col_arrange[, sub_start := cumsum(var_length) - var_length + 1]
col_arrange[, sub_end := cumsum(var_length)]


magnitudes_names <-
  data.table(
    MAGNITUD = c(1, 6, 7, 8, 9, 10, 12, 20, 30, 35),
    nom_mag = c(
      'Dióxido de Azufre',
      'Monóxido de Carbono',
      'Monóxido de Nitrógeno',
      'Dióxido de Nitrógeno',
      'Partículas < 2.5 µm',
      'Partículas < 10 µm',
      'Óxidos de Nitrógeno',
      'Tolueno',
      'Benceno',
      'Etilbenceno'
    ),
    nom_abv = c(
      'SO2',
      'CO',
      'NO',
      'NO2',
      'PM2.5',
      'PM10',
      'NOx',
      'TOL',
      'BEN',
      'EBE'
    ), 
    ud_med = rep('µg/m3', 10)
  )



station_names <-
  data.table(
    id_numeric = c(
      8L,
      11L,
      16L,
      17L,
      18L,
      24L,
      27L,
      35L,
      36L,
      38L,
      39L,
      40L,
      47L,
      48L,
      49L,
      50L,
      54L,
      55L,
      56L,
      57L,
      58L,
      59L,
      60L
    ),
    estaciones = c(
      "E08: Escuelas Aguirre",
      "E11: Avda. Ramon y Cajal",
      "E16: Arturo Soria",
      "E17: Villaverde",
      "E18: Farolillo",
      "E24: Casa de Campo",
      "E27: Barajas Pueblo",
      "E35: Plaza del Carmen",
      "E36: Moratalaz",
      "E38: Cuatro Caminos",
      "E39: Barrio del Pilar",
      "E40: Puente de Vallecas",
      "E47: Mendez Alvaro",
      "E48: Castellana",
      "E49: Retiro",
      "E50: Plaza de Castilla",
      "E54: Ensanche de Vallecas",
      "E55: Urbanizacion Embajada",
      "E56: Plaza Elíptica",
      "E57: Sanchinarro",
      "E58: El Pardo",
      "E59: Juan Carlos I",
      "E60: Tres Olivos"
    ),
    id = c(
      "E08",
      "E11",
      "E16",
      "E17",
      "E18",
      "E24",
      "E27",
      "E35",
      "E36",
      "E38",
      "E39",
      "E40",
      "E47",
      "E48",
      "E49",
      "E50",
      "E54",
      "E55",
      "E56",
      "E57",
      "E58",
      "E59",
      "E60"
    ),
    id_name = c(
      "Escuelas Aguirre",
      "Avda. Ramon y Cajal",
      "Arturo Soria",
      "Villaverde",
      "Farolillo",
      "Casa de Campo",
      "Barajas Pueblo",
      "Plaza del Carmen",
      "Moratalaz",
      "Cuatro Caminos",
      "Barrio del Pilar",
      "Puente de Vallecas",
      "Mendez Alvaro",
      "Castellana",
      "Retiro",
      "Plaza de Castilla",
      "Ensanche de Vallecas",
      "Urbanizacion Embajada",
      "Plaza Elíptica",
      "Sanchinarro",
      "El Pardo",
      "Juan Carlos I",
      "Tres Olivos"
    ),
    longitud = c(
      -3.6823158,-3.6773491,-3.6392422,-3.7133167,-3.7318356,-3.7473445,-3.5800258,-3.7031662,-3.6453104,-3.7071303,-3.7115364,-3.6515286,-3.6868138,-3.6903729,-3.6824999,-3.6887449,-3.6121394,-3.5805649,-3.7187679,-3.6605173,-3.7746101,-3.6163407,-3.6897308
    ),
    latitud = c(
      40.4215533,
      40.4514734,
      40.4400457,
      40.347147,
      40.3947825,
      40.4193577,
      40.4769179,
      40.4192091,
      40.4079517,
      40.4455439,
      40.4782322,
      40.3881478,
      40.3980991,
      40.4398904,
      40.4144444,
      40.4655841,
      40.3730118,
      40.4623628,
      40.3850336,
      40.4942012,
      40.5180701,
      40.4607255,
      40.5005477
    )
  )


# Data download -----------------------------------------------------------

URL <- 'https://datos.madrid.es/egob/catalogo/201200-0-calidad-aire-horario.dcat'
page <- read_xml(URL)
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

# Download. The las year always for update, check if there are previous years
for (i in links[, year]) {
  if (i == max(links[, year])) {
    download.file(url = links[year == i, link],
                  destfile = paste0('~/aire_madrid/raw_data/', links[year == i, file_name]))
    unzip(
      paste0('~/aire_madrid/raw_data/', links[year == i, file_name]),
      overwrite = TRUE,
      exdir = paste0('~/aire_madrid/raw_data/', i)
    )
  }
  
  if (!dir.exists(paste0('~/aire_madrid/raw_data/', i))) {
    download.file(url = links[year == i, link],
                  destfile = paste0('~/aire_madrid/raw_data/', links[year == i, file_name]))
    unzip(
      paste0('~/aire_madrid/raw_data/', links[year == i, file_name]),
      overwrite = TRUE,
      exdir = paste0('~/aire_madrid/raw_data/', i)
    )
  }
  
}

# list of all files
all_files <- list.files(path = '~/aire_madrid/raw_data/', recursive = T)

# get only txt data files 
txt_data_files <- list.files(path = '~/aire_madrid/raw_data/', recursive = T, pattern = '.txt')

# remove unnecessary files 
file.remove(paste0('~/aire_madrid/raw_data/', all_files[!(all_files %in% txt_data_files)]))

# oct 2017 change in data structure
txt_data_files_before_oct_2017 <-
  c(str_subset(txt_data_files, pattern = paste(2001:2016, collapse = '|')),
    paste0('2017/', str_subset(list.files(path = '~/aire_madrid/raw_data/2017', pattern = '.txt'), pattern = c('oct|nov|dic'), negate = TRUE))
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
    

# Read data and rbind

dt_before_oct_2017 <- data.table()
dt_after_oct_2017 <- data.table()

for (i in data_files[, files]) {
  
  if (data_files[files == i, txt_format] == 'old') {
    lines <- readLines(paste0('~/aire_madrid/raw_data/', i))
    dt <- data.table(apply(X = col_arrange, MARGIN = 1, FUN = function(x) str_sub(string = lines, start = x[3], end = x[4])))
    names(dt) <- col_arrange[, name_cols]
    dt_before_oct_2017 <- rbind(dt_before_oct_2017, dt)
    
  } else {
    dt <- fread(paste0('~/aire_madrid/raw_data/', i))
    names(dt) <- col_arrange[, name_cols]
    dt_after_oct_2017 <- rbind(dt_after_oct_2017, dt)
  }
}


dt <- rbind(dt_before_oct_2017, dt_after_oct_2017)
cols_to_numeric <- str_subset(names(dt), pattern = '^V', negate = T)
dt[, (cols_to_numeric) := lapply(.SD, as.numeric), .SDcols = cols_to_numeric]
dt[, ANO := fifelse(ANO < 100, ANO + 2000, ANO)]


saveRDS(dt, 'dt.RDS')


rm(dt_before_oct_2017, dt_after_oct_2017, col_arrange)



# Reshaping data for use --------------------------------------------------
dt_sub <- dt[magnitudes_names, on = 'MAGNITUD']
dt_sub <- dt_sub[station_names, on = c('ESTACION' = 'id_numeric')]

dt_sub[, fecha := as.Date(paste(ANO, MES, DIA, sep = '-'))]
hour_cols <- str_subset(names(dt_sub), pattern = '^H')
cols <- c('estaciones', 'id', 'id_name', 'longitud', 'latitud', 'fecha', 'nom_mag', 'nom_abv', 'ud_med', hour_cols)

dt_sub <- melt(
  dt_sub[, ..cols],
  id.vars = c('estaciones', 'id', 'id_name', 'longitud', 'latitud', 'fecha', 'nom_mag', 'nom_abv', 'ud_med'),
  measure.vars = hour_cols,
  value.name = 'valor',
  variable.name = 'hora'
)

dt_sub[, hora := gsub('H', '', hora)]
dt_sub[, date := as.POSIXct(paste(fecha, hora), tz = '', format = '%Y-%m-%d %H')]


# Save --------------------------------------------------------------------

saveRDS(dt_sub, 'dt_sub.RDS')
