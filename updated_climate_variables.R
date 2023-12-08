library(tidyverse)
library(innovar)
library(janitor)
library(rgee)
library(sf)
ee_Initialize(user = "geografo.pe@gmail.com",drive = T)
# Temperature of ERA5LAND
get_temp_from_ee <- function(data, start_date, end_date) {
  lista_vacia <- list()
  lista_poligonos <- list(
    lista_poligons_01 = (lista_poligonos_pre[1] + 0):lista_poligonos_pre[2],
    lista_poligons_02 = (lista_poligonos_pre[2] + 1):lista_poligonos_pre[3],
    lista_poligons_03 = (lista_poligonos_pre[3] + 1):lista_poligonos_pre[4],
    lista_poligons_04 = (lista_poligonos_pre[4] + 1):lista_poligonos_pre[5]
  )
  
  for (i in 1:length(lista_poligonos)) {
    poligonos_ee <- data[lista_poligonos[[i]], ] |>
      st_transform(32718) |>
      st_simplify(dTolerance = 250, preserveTopology = TRUE) |>
      st_transform(4326) |>
      sf_as_ee()
    
    temp_values_pre <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") |>
      ee$ImageCollection$filterDate(start_date, end_date) |>
      ee$ImageCollection$select("temperature_2m") |>
      ee$ImageCollection$toBands() |>
      ee_extract(
        y = poligonos_ee,
        fun = ee$Reducer$mean(),
        scale = 11132,
        sf = FALSE
      ) |>
      clean_names()
    
    lista_vacia[[i]] <- list(temp_values_pre)
  }
  
  bbdd_pre <- lista_vacia |>
    map_df(.f = as.data.frame) |>
    pivot_longer(
      cols = names(temp_values_pre)[2]:tail(names(temp_values_pre), 1),
      names_to = "year",
      values_to = "temp"
    ) |>
    mutate(
      date = as.Date(paste0(str_extract(year, "\\d+"), "01"), format = "%Y%m%d"),
      temp = temp - 273.15
    ) |>
    select(ubigeo,date,temp)
  
  bbdd <- bbdd_pre |>
    mutate(temp = case_when(
      ubigeo == "140114" ~ mean(bbdd_pre |> filter(ubigeo %in% c("140112", "140106", "140108", "140103")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150805" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150812", "150806", "150810", "150801")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150803" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150805", "150812", "150806", "150810", "150801")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "110501" ~ mean(bbdd_pre |> filter(ubigeo %in% c("110507", "110508", "110506")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150108" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150104", "150140", "150133", "150142")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150124" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150505")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150138" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150127", "150129", "150505", "150124")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "070102" ~ mean(bbdd_pre |> filter(ubigeo %in% c("070101", "070103", "150101", "150121", "150120", "150136", "070104", "070105")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "070104" ~ mean(bbdd_pre |> filter(ubigeo %in% c("070101", "070102", "150121", "150136", "070103", "070105", "150101", "150135")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "070105" ~ mean(bbdd_pre |> filter(ubigeo %in% c("070101", "070102", "070104")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150121" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150141", "150131", "150120", "150105", "150101", "070101", "070103", "150135", "150128", "150115", "150122", "070102", "070104", "150136", "150116", "150113")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150120" ~ mean(bbdd_pre |> filter(ubigeo %in% c("150121", "150113", "150105", "150101", "150116", "150131", "150122", "150136", "150141", "070104", "070103", "070101", "070102")) |> select(temp) |> pull(), na.rm = TRUE),
      ubigeo == "150136" ~ mean(bbdd_pre |> filter(ubigeo %in% c("070103", "150101", "150105", "150113", "150120", "150131", "150116", "070102", "070101")) |> select(temp) |> pull(), na.rm = TRUE),
      TRUE ~ temp
    ))
  return(bbdd)
}

# Precipitation data
get_pp_from_ee <- function(data, start_date, end_date) {
  lista_vacia <- list()
  lista_poligonos <- list(
    lista_poligons_01 = (lista_poligonos_pre[1] + 0):lista_poligonos_pre[2],
    lista_poligons_02 = (lista_poligonos_pre[2] + 1):lista_poligonos_pre[3],
    lista_poligons_03 = (lista_poligonos_pre[3] + 1):lista_poligonos_pre[4],
    lista_poligons_04 = (lista_poligonos_pre[4] + 1):lista_poligonos_pre[5]
  )
  
  for (i in 1:length(lista_poligonos)) {
    poligonos_ee <- data[lista_poligonos[[i]], ] |>
      st_transform(32718) |>
      st_simplify(dTolerance = 250, preserveTopology = TRUE) |>
      st_transform(4326) |>
      sf_as_ee()
    
    chirps_pre <- ee$ImageCollection$Dataset$`UCSB-CHG_CHIRPS_DAILY`$
      select("precipitation")$
      filterDate(start_date,end_date)$toBands()
    
    pp_values_pre <- ee_extract(
      x = chirps_pre,
      y = poligonos_ee,
      fun = ee$Reducer$mean(),
      scale = 5566)

    lista_vacia[[i]] <- list(pp_values_pre)
    
  }
  
  bbdd <- lista_vacia |>
    map_df(.f = as.data.frame) |>
    pivot_longer(
      cols = names(pp_values_pre)[2]:tail(names(pp_values_pre), 1),
      names_to = "year",
      values_to = "pp"
    ) |>
    mutate(
      date = as.Date(paste0(str_extract(year, "\\d+"), "01"), format = "%Y%m%d"),
    ) |> 
    select(ubigeo, date,pp)
  return(bbdd)
}

# 1. Cooking the polygons for rgee ----------------------------------------
data("Peru")
peru_sf <- Peru |> st_as_sf() |> select(ubigeo) 
lista_poligonos_pre <- seq(
  from = 1,
  to = nrow(peru_sf),
  length.out = 5) |> 
  round()

start_date <- '2023-04-01'
end_date <- '2023-12-31'

# 3. Getting temperature data ---------------------------------------------
temp <- get_temp_from_ee(
  data = peru_sf,
  start_date = start_date,
  end_date = end_date)

# 4. Getting precipitation data -------------------------------------------
pp <- get_pp_from_ee(
  data = peru_sf,
  start_date = start_date,
  end_date = end_date)
