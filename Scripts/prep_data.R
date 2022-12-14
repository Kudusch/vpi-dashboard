# Libraries and read data ----
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

setwd("/Users/kudusch/Desktop/vpi-dashboard/")

df.vpi <- read_delim(
    file = "Data/vpi.csv", 
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
    select(date, starts_with("vpi")) |> 
    filter(!is.na(vpi))

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
    filter(date >= list.vertragsdaten$vertragsstart) |> 
    filter(!is.na(vpi))

saveRDS(df.index_miete, file = "Output/df_index_miete.RDS")
saveRDS(df.vpi, file = "Output/df_vpi.RDS")
saveRDS(list.vertragsdaten, file = "Output/list_vertragsdaten.RDS")