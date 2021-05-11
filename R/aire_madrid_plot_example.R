#' Script for graph example of Nitrogen dioxide (Dióxido de nitrógeno) [µg/m³]
#'

# initial
library(data.table)
library(zoo)
library(ggplot2)
library(ggthemes)
library(config)

config <- get(file = "conf/main.yml")

# load data
dt_sub <- readRDS(config$output$subseted_long_data)


# weekly graph
# data
mendez_alvaro_weekly_no2 <-
  dt_sub[id_name == 'Mendez Alvaro' &
           nom_abv == 'NO2' &
           fecha >= '2010-01-01'][, .(weekly_no2 = max(valor, na.rm = T)), .(year = year(fecha), week = week(fecha))]

mendez_alvaro_weekly_no2[, med_10y := mean(weekly_no2[year %in% 2010:2019]), week]

mendez_alvaro_weekly_no2 <- mendez_alvaro_weekly_no2[year >= 2019]

mendez_alvaro_weekly_no2 <-
  melt(
    mendez_alvaro_weekly_no2,
    id.vars = c('year', 'week'),
    measure.vars = c('weekly_no2', 'med_10y')
  )

mendez_alvaro_weekly_no2[, variable := fifelse(variable == 'weekly_no2',
                                               paste('weekly_no2', year, sep = '_'),
                                               'med_10y')]

mendez_alvaro_weekly_no2 <-
  unique(mendez_alvaro_weekly_no2[, .(week, variable, value)])


# plot
ggplot(mendez_alvaro_weekly_no2[week < 53], aes(week, value, colour = factor(variable))) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 11) +
  geom_text(
    aes(x = 10.5, label = "Estado de alarma", y = 225),
    colour = "red",
    angle = 90,
    vjust = -1,
    text = element_text(size = 11)
  ) +
scale_y_continuous(breaks = seq(0, 300, 25)) +
  scale_x_continuous(breaks = seq(0, 52, 4)) +
  labs(
    title = 'Datos del promedio semanal de la magnitud Dióxido de nitrógeno [µg/m³]',
    subtitle = 'Estación: Méndez Álvaro',
    caption = ' Estaciones Automáticas de Medición de la Red de Vigilancia de la Calidad del Aire del Ayuntamiento de Madrid',
    x = NULL,
    y = 'Dióxido de nitrógeno [µg/m³]',
    color = ''
  ) +
  ggthemes::theme_wsj(
    base_size = 12,
    color = 'blue',
    title_family = 'Helvetica',
    base_family = 'Helvetica'
  ) +
  ggthemes::scale_color_wsj(labels = c("Media de 10 años", "Año 2019", "Año 2020", "Año 2021")) +
  theme(
    legend.position = 'top',
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 14, face = 'bold'),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = 'bold'),
    plot.caption = element_text(size = 8),
    axis.title = element_text(size = 12, face = 'bold'),
    axis.text = element_text(size = 10, face = 'bold'),
    strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )
