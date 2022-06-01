library(data.table)
library(zoo)

# Load data
dt_aire_madrid_sub <- readRDS("data/processed/dt_aire_madrid_sub.RDS")

# Load dictionaries
tipo <- readRDS("dictionaries/tipo.RDS")
zona <- readRDS("dictionaries/zona.RDS")

# subset data 
dt_aire_madrid_sub_2011 <- dt_aire_madrid_sub[fecha >= "2011-01-01"]
# media diaria
cols <- c("estaciones", "id", "id_name", "longitud", "latitud", "fecha", 
  "nom_mag", "nom_abv", "ud_med")
dt_aire_madrid_sub_2011 <- dt_aire_madrid_sub_2011[, .(daily_mean = mean(valor, na.rm = TRUE)), by = cols]
rm(dt_aire_madrid_sub)
gc()

# rellenar con NAs las fechas que faltan
min_date <- dt_aire_madrid_sub_2011[, min(fecha, na.rm = T)] 
max_date <- dt_aire_madrid_sub_2011[, max(fecha, na.rm = T)]
complete_dates <- seq.Date(from = min_date, to = max_date, by = "day")

cols <- c("estaciones", "id", "id_name", "longitud", "latitud", 
          "nom_mag", "nom_abv", "ud_med")
ref_table <- dt_aire_madrid_sub_2011[, .(fecha = complete_dates), by = cols]

dt_aire_madrid_sub_2011 <- merge(ref_table, dt_aire_madrid_sub_2011, by = c(cols, "fecha"), all = TRUE)

# > dt_aire_madrid_sub_2011[, .N, is.na(daily_mean)]
# is.na      N
# 1: FALSE 505773
# 2:  TRUE  15615

# Add dictionaries
dt_aire_madrid_sub_2011 <- dt_aire_madrid_sub_2011[zona, on = "id_name"]
dt_aire_madrid_sub_2011 <- dt_aire_madrid_sub_2011[tipo, on = "id_name"]

# Save
saveRDS(dt_aire_madrid_sub_2011, "data/processed/dt_daily_mean_2011.RDS")
