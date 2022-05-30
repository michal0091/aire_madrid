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

# station plot ------------------------------------------------------------

# weekly graph
# data
mendez_alvaro_weekly_no2 <-
  dt_sub[id_name == 'Mendez Alvaro' &
           nom_abv == 'NO2' &
           fecha >= '2011-01-01'][, .(weekly_no2 = mean(valor, na.rm = T)), .(year = year(fecha), week = week(fecha))]

mendez_alvaro_weekly_no2[, med_10y := mean(weekly_no2[year %in% 2011:2020]), week]

mendez_alvaro_weekly_no2 <- mendez_alvaro_weekly_no2[year >= 2020]

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
    aes(x = 10.5, label = "Estado de alarma", y = 75),
    colour = "red",
    angle = 90,
    vjust = -1,
    text = element_text(size = 11)
  ) +
scale_y_continuous(breaks = seq(0, 125, 25), limits = c(0, 125)) +
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
  ggthemes::scale_color_wsj(labels = c("Media de 10 años", "Año 2020", "Año 2021", "Año 2022")) +
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


# Monthly plot ------------------------------------------------------------
monthly_plot <- dt_sub[, .(valor_promedio = mean(valor, na.rm = T)), .(m_y = as.yearmon(fecha), nom_mag, nom_abv, ud_med)]
months <-  seq.Date(from = as.Date("2022-01-01"), length.out = 12, by = "month") %>% format("%B")
monthly_plot[, `:=`(
  year = year(m_y),
  month = factor(format(m_y, "%B"), levels = months, labels = months)
)]

contaminantes <- monthly_plot[, unique(nom_mag)]
contaminante_i <- "Dióxido de Azufre"

monthly_plot[nom_mag == contaminante_i,
             ggplot(.SD, aes(x = year, y = valor_promedio)) +
               geom_line() +
               labs(x = NULL, y = paste(unique(nom_abv), unique(ud_med)), 
                    title = paste("Datos del promedio mensual de", contaminante_i)) +
               facet_wrap(month~., ncol = 3)]



# Calima plot -------------------------------------------------------------
particulas <- c("Partículas < 2.5 µm", "Partículas < 10 µm")

calima <- dt_sub[nom_mag %in% particulas &
                   between(fecha, "2022-03-01", "2022-03-31"),
                 .(valor_promedio = mean(valor, na.rm = T)), .(fecha, id, id_name, nom_mag, nom_abv, ud_med)]
max_2.5 <- calima[nom_mag == particulas[1]][valor_promedio == max(valor_promedio)]
max_10 <- calima[nom_mag == particulas[2]][valor_promedio == max(valor_promedio)]

calima[, ggplot(.SD, aes(fecha, valor_promedio, colour = nom_mag)) +
         geom_jitter() +
         geom_smooth(
           method = "loess",
           span = .5,
           se = FALSE,
           show.legend = FALSE
         ) +
         scale_x_date(breaks = seq.Date(as.Date("2022-03-01"), by = "week", length.out = 5), date_labels =  "%d-%b") +
         scale_color_manual(values = c("#261606", "#DD9C4A")) + 
         geom_label_repel(
           data = max_10,
           mapping = aes(fecha, valor_promedio, label = paste(id_name)),
           show.legend = FALSE
         ) +
         geom_label_repel(
           data = max_2.5,
           mapping = aes(fecha, valor_promedio, label = paste(id_name)),
           show.legend = FALSE
         ) +
         labs(title = "Registro de partículas durante el mes de marzo 2022", subtitle = "Madrid",
              x = NULL, y = unique(ud_med), color = NULL) +
         theme(
           legend.position = 'bottom',
           panel.background = element_rect(fill = "#f4dbb3",
                                           colour = "#f4dbb3",
                                           size = 0.5, linetype = "solid"),
           plot.background = element_rect(fill = "#DD9C4A"),
           text = element_text(family = "helvetica", colour = "#261606"),
           legend.background = element_rect(fill = "#DD9C4A"),
           legend.key = element_rect(fill = "#f4dbb3", color = NA)
         )
]

