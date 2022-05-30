#' Script for graph examples

# initial
library(data.table)
library(zoo)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(config)
library(dplyr)
# devtools::install_github("davidsjoberg/ggstream")
library(ggstream)

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
    caption = 'Fuente: Red de Vigilancia de la Calidad del Aire del Ayto. de Madrid\nElaboración: Michal Kinel',
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
ggsave(
  filename = "station_plot.png",
  device = "png",
  path = "data/output/",
  width = 1800,
  height = 1124,
  dpi = 170,
  units = "px"
)

# Monthly plot ------------------------------------------------------------
monthly_plot <-
  dt_sub[, .(valor_promedio = mean(valor, na.rm = T)), .(m_y = as.yearmon(fecha), nom_mag, nom_abv, ud_med)]
months <-
  seq.Date(from = as.Date("2022-01-01"),
           length.out = 12,
           by = "month") %>% format("%B")
monthly_plot[, `:=`(year = year(m_y),
                    month = factor(format(m_y, "%B"), levels = months, labels = months))]

contaminantes <- monthly_plot[, unique(nom_mag)]
contaminante_i <-
  c(
    "Dióxido de Azufre",
    "Monóxido de Carbono",
    "Monóxido de Nitrógeno",
    "Dióxido de Nitrógeno",
    "Óxidos de Nitrógeno",
    "Tolueno",
    "Benceno",
    "Etilbenceno"
  )

monthly_plot[nom_mag %in% contaminante_i & year >= 2012,
             ggplot(.SD, aes(x = year, y = valor_promedio)) +
               geom_line() +
               scale_x_continuous(breaks = 2012:2022) +
               labs(
                 x = NULL,
                 y = unique(ud_med),
                 title = "Datos del promedio mensual de contaminantes",
                 caption = "Fuente: Red de Vigilancia de la Calidad del Aire del Ayto. de Madrid\nElaboración: Michal Kinel"
               ) +
               facet_grid(rows = vars(nom_abv),
                          cols = vars(month),
                          scales = "free_y") +
               theme(axis.text.x = element_text(
                 angle = 90,
                 vjust = 0.5,
                 hjust = 1
               ))]
ggsave(
  filename = "monthly_plot.png",
  device = "png",
  path = "data/output/",
  width = 2400,
  height = 1480,
  dpi = 170,
  units = "px"
)


# Calima plot -------------------------------------------------------------
particulas <- c("Partículas < 2.5 µm", "Partículas < 10 µm")

calima <- dt_sub[nom_mag %in% particulas &
                   fecha %in% seq.Date(as.Date("2022-03-01"), by = "day", length.out = 31),
                 .(valor_promedio = mean(valor, na.rm = T)), .(fecha, id, id_name, nom_mag, nom_abv, ud_med)]
max_2.5 <-
  calima[nom_mag == particulas[1]][valor_promedio == max(valor_promedio)]
max_10 <-
  calima[nom_mag == particulas[2]][valor_promedio == max(valor_promedio)]

calima[, ggplot(.SD, aes(fecha, valor_promedio, colour = nom_mag)) +
         geom_jitter() +
         geom_smooth(
           method = "loess",
           span = .5,
           se = FALSE,
           show.legend = FALSE
         ) +
         scale_x_date(breaks = seq.Date(as.Date("2022-03-01"), by = "week", length.out = 5),
                      date_labels =  "%d-%b") +
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
         labs(
           title = "Registro de partículas durante el mes de marzo 2022",
           subtitle = "Madrid",
           x = NULL,
           y = unique(ud_med),
           color = NULL,
           caption = "Fuente: Red de Vigilancia de la Calidad del Aire del Ayto. de Madrid\nElaboración: Michal Kinel"
         ) +
         theme(
           legend.position = 'bottom',
           panel.background = element_rect(
             fill = "#f4dbb3",
             colour = "#f4dbb3",
             size = 0.5,
             linetype = "solid"
           ),
           plot.background = element_rect(fill = "#DD9C4A"),
           text = element_text(family = "helvetica", colour = "#261606"),
           legend.background = element_rect(fill = "#DD9C4A"),
           legend.key = element_rect(fill = "#f4dbb3", color = NA)
         )]
ggsave(
  filename = "calima_plot.png",
  device = "png",
  path = "data/output/",
  width = 1800,
  height = 1124,
  dpi = 170,
  units = "px"
)


# Calendar plot -----------------------------------------------------------
calendar_plot <-
  dt_sub[, .(valor_promedio = mean(valor, na.rm = T)), .(fecha, nom_mag, nom_abv, ud_med)]

# Dates as factors
months <-
  seq.Date(from = as.Date("2022-01-01"),
           length.out = 12,
           by = "month") %>% format("%B")
wdays <-
  seq.Date(from = as.Date("2022-05-30"),
           length.out = 7,
           by = "day") %>% format("%A")
calendar_plot[, `:=`(
  year = year(fecha),
  month = factor(format(fecha, "%B"), levels = months, labels = months),
  wday = factor(weekdays(fecha), levels = wdays, labels = wdays),
  week = as.numeric(format(fecha, "%W"))
)]

calendar_plot[, wmonth := 1 + week - min(week), .(year, month)]
calendar_plot <- calendar_plot[order(year, month, -wday)]
i_mag <- "Dióxido de Nitrógeno"
calendar_plot[nom_mag == i_mag & year >= 2012,
              ggplot(.SD, aes(
                x = wmonth,
                y = reorder(wday, -as.numeric(wday)),
                fill = valor_promedio
              )) +
                geom_tile(colour = "white") +
                facet_grid(year ~ month) +
                scale_fill_gradient(low = "yellow", high = "red", ) +
                scale_x_continuous(breaks = 1:5, limits = c(0, 6)) +
                labs(
                  x = "Semana del mes",
                  y = NULL,
                  title = paste0("Concentración de ", i_mag, " por día de la semana"),
                  fill = paste(unique(nom_abv), unique(ud_med)),
                  caption = "Fuente: Red de Vigilancia de la Calidad del Aire del Ayto. de Madrid\nElaboración: Michal Kinel"
                )]
ggsave(
  filename = "calendar_plot.png",
  device = "png",
  path = "data/output/",
  width = 2400,
  height = 2400,
  dpi = 170,
  units = "px"
)


# Stream plot -------------------------------------------------------------
stream_plot <-
  dt_sub[, .(valor_promedio = mean(valor, na.rm = T)), .(m_y = as.yearmon(fecha), nom_mag, nom_abv, ud_med)]

contaminante_i <-
  c(
    "Dióxido de Azufre",
    "Monóxido de Carbono",
    "Óxidos de Nitrógeno",
    "Tolueno",
    "Benceno",
    "Etilbenceno"
  )
my_order <-
  stream_plot[, max(valor_promedio), by = nom_abv][order(V1), nom_abv]
my_colors <-
  c("#5F3E3A",
    "#9f999d",
    "#4D4049",
    "#DCD9D3",
    "#8C9DA7",
    "#53627A")

stream_plot[, nom_abv_f := factor(nom_abv, levels = my_order, labels = my_order)]

stream_plot[m_y >= 2012 &
              nom_mag %in% contaminante_i, ggplot(.SD, aes(x = m_y, y = valor_promedio, fill = nom_abv_f)) +
              geom_stream(type = "ridge") +
              scale_fill_manual(values = my_colors) +
              scale_x_yearmon(n = 10) +
              labs(
                title = "Evolución de contaminantes",
                x = NULL,
                y = unique(ud_med),
                fill = "Contaminante",
                caption = "Fuente: Red de Vigilancia de la Calidad del Aire del Ayto. de Madrid\nElaboración: Michal Kinel"
              ) +
              theme(legend.position = "bottom")]
ggsave(
  filename = "stream_plot.png",
  device = "png",
  path = "data/output/",
  width = 1800,
  height = 1124,
  dpi = 170,
  units = "px"
)
