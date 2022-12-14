# Libraries and read data ----
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

df.vpi <- read_delim(
    file = "vpi.csv", 
    delim = ";", 
    escape_double = FALSE, 
    col_types = cols(
        Statistik_Code = col_skip(), 
        Statistik_Label = col_skip(), 
        Zeit_Code = col_skip(), 
        Zeit_Label = col_skip(), 
        `1_Merkmal_Code` = col_skip(), 
        `1_Merkmal_Label` = col_skip(), 
        `1_Auspraegung_Code` = col_skip(), 
        `1_Auspraegung_Label` = col_skip(), 
        `2_Merkmal_Code` = col_skip(), 
        `2_Merkmal_Label` = col_skip(), 
        `2_Auspraegung_Code` = col_skip(), 
        `CH0004__Veraenderung_zum_Vorjahresmonat__in_(%)` = col_skip(), 
        `CH0005__Veraenderung_zum_Vormonat__in_(%)` = col_skip()
    ), 
    trim_ws = TRUE
) |> 
    rename(
        y = Zeit,
        m = 2,
        vpi = 3
    ) |> 
    mutate(m = rep(1:12, n()/12)) |> 
    mutate(vpi = ifelse(vpi == "...", NA, vpi)) |> 
    mutate(vpi = as.numeric(str_replace_all(vpi, ",", "."))) |> 
    select(y, m, vpi) |> 
    group_by(m) |> 
    arrange(y) |> 
    mutate(vpi_vorjahresmonat = (vpi-lag(vpi))/lag(vpi)) |> 
    ungroup() |> 
    mutate(vpi_vormonat = (vpi-lag(vpi))/lag(vpi)) |> 
    mutate(date = as.POSIXct(strptime(sprintf("%s-%s-01", y, sprintf("%02d", m)), "%Y-%m-%d"))) |> 
    select(date, starts_with("vpi"))

list.vertragsdaten <- list(
    "grundmiete" = 1466.86,
    "vertragsstart" = date("2022-09-01"),
    "letze_erhöhung" = date("2022-09-01")
)
list.vertragsdaten$start_vpi <- df.vpi$vpi[which(df.vpi$date == list.vertragsdaten$vertragsstart)]

df.index_miete <- df.vpi |> 
    mutate(change_in_vpi = vpi/list.vertragsdaten$start_vpi) |> 
    mutate(rent = list.vertragsdaten$grundmiete*change_in_vpi) |> 
    mutate(rent_delta = rent - list.vertragsdaten$grundmiete) |> 
    filter(date >= list.vertragsdaten$vertragsstart)



df.index_miete |> 
    ggplot(aes(x = date, y = rent)) +
    geom_point() +
    geom_linerange(aes(x = date, ymin = rent, ymax = rent + rent_delta))

# Viz ----
df.vpi |> 
    ggplot(aes(x = date, y = vpi)) +
    geom_line() +
    geom_point(x = as.POSIXct(list.vertragsdaten$vertragsstart), y = list.vertragsdaten$start_vpi) +
    labs(
        title = "Verbraucherpreisindex über Zeit", 
        y = "VPI",
        x = ""
    )

ggsave(
	"Output/vpi_over_time.png", 
	last_plot(), 
	scale = .8, 
	height = 3000, 
	width = 4500, 
	units = "px", 
	limitsize = F
)

df.vpi |> 
    mutate(angestiegen = factor(vpi_vormonat > 0, levels = c(T,F))) |> 
    filter(date > max(date)-months(12)) |> 
    mutate(vpi_vormonat_kum = cumsum(vpi_vormonat)) |> 
    ggplot(aes(x = date, y = vpi_vormonat)) +
    geom_col(aes(fill = angestiegen)) +
    geom_line(aes(y = vpi_vormonat_kum)) +
    scale_fill_manual(values = c("#760E0E", "#298423")) +
    scale_y_continuous(labels = scales::percent) +
    labs(
        title = "Verbraucherpreisindex über die letzten 12 Monate", 
        subtitle = "Veränderung zum Vormonat in %",
        y = "",
        x = ""
    ) +
    ktools::theme_kudusch() +
    theme(legend.position = "none")

ggsave(
	"Output/change_in_vpi_over_time.png", 
	last_plot(), 
	scale = .8, 
	height = 3000, 
	width = 4500, 
	units = "px", 
	limitsize = F
)

df.vpi |> 
    mutate(m = month(date), y = year(date)) |> 
    select(-date) |> 
    filter(y > max(y)-3) |> 
    mutate(y = factor(y)) |> 
    ggplot(aes(x = m, y = vpi, color = y, group = y)) +
    geom_line()

ggsave(
	"Output/yearly_vpi.png", 
	last_plot(), 
	scale = .8, 
	height = 3000, 
	width = 4500, 
	units = "px", 
	limitsize = F
)

df.vpi |> 
    arrange(desc(date)) |> 
    slice(1:12) |> 
    mutate(angestiegen = factor(vpi_vorjahresmonat > 0, levels = c(T,F))) |> 
    ggplot(aes(x = date, y = vpi_vorjahresmonat)) +
    geom_col(aes(fill = angestiegen)) +
    scale_fill_manual(values = c("red", "darkgreen")) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "none") +
    labs(
        title = "Verbraucherpreisindex über Zeit", 
        subtitle = "Veränderung zum Vormonat in %",
        y = "",
        x = ""
    )

ggsave(
	"Output/change_in_vpi_over_time_2.png", 
	last_plot(), 
	scale = .8, 
	height = 3000, 
	width = 4500, 
	units = "px", 
	limitsize = F
)

df.vpi |> 
    mutate(y = year(date)) |> 
    group_by(y) |> 
    summarise(vpi = mean(vpi)) |> 
    ungroup() |> 
    mutate(vpi_vorjahr = (vpi-lag(vpi))/lag(vpi)) |> 
    mutate(vpi_vorjahr = replace_na(vpi_vorjahr, 0)) |> 
    filter(y >= 2000) |> 
    ggplot(aes(x = y, y = vpi_vorjahr)) +
    geom_col(aes(fill = vpi_vorjahr)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.06), breaks = seq(0, 0.06, 0.005)) +
    scale_fill_gradient(low = "darkgreen", high = "red")

ggsave(
	"Output/changes_in_vpi_over_time_3.png", 
	last_plot(), 
	scale = .8, 
	height = 3000, 
	width = 4500, 
	units = "px", 
	limitsize = F
)
