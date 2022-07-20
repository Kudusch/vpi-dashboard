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
    col_names = FALSE, 
    col_types = cols(X4 = col_skip()), 
    trim_ws = TRUE
) |> 
    mutate(X1 = sort(rep(min(X1, na.rm = T):max(X1, na.rm = T), 12))) |> 
    mutate(m = rep(1:12, n()/12)) |> 
    rename(y = X1) |> 
    filter(X3 != "...") |> 
    mutate(vpi = as.numeric(str_replace_all(X3, ",", "."))) |> 
    select(y, m, vpi) |> 
    group_by(m) |> 
    arrange(y) |> 
    mutate(vpi_vorjahresmonat = (vpi-lag(vpi))/lag(vpi)) |> 
    ungroup() |> 
    mutate(vpi_vormonat = (vpi-lag(vpi))/lag(vpi)) |> 
    mutate(date = as.POSIXct(strptime(sprintf("%s-%s-01", y, sprintf("%02d", m)), "%Y-%m-%d"))) |> 
    select(date, starts_with("vpi"))

df.vpi |> 
    ggplot(aes(x = date, y = vpi)) +
    geom_line() +
    labs(
        title = "Verbraucherpreisindex über Zeit", 
        y = "VPI",
        x = ""
    )

df.vpi |> 
    mutate(angestiegen = factor(vpi_vormonat > 0, levels = c(T,F))) |> 
    ggplot(aes(x = date, y = vpi_vormonat)) +
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

df.vpi |> 
    mutate(m = month(date), y = year(date)) |> 
    select(-date) |> 
    filter(y > max(y)-3) |> 
    mutate(y = factor(y)) |> 
    ggplot(aes(x = m, y = vpi, color = y, group = y)) +
    geom_line()

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
# ihoih ----
df.vpi |> 
    mutate(y = year(date)) |> 
    group_by(y) |> 
    summarise(vpi = mean(vpi)) |> 
    ungroup() |> 
    mutate(vpi_vorjahr = (vpi-lag(vpi))/lag(vpi)) |> 
    mutate(vpi_vorjahr = replace_na(vpi_vorjahr, 0)) |> 
    ggplot(aes(x = y, y = vpi_vorjahr)) +
    geom_col(aes(fill = vpi_vorjahr)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_gradient(low = "darkgreen", high = "red")