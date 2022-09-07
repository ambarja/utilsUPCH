library(tidyverse)
library(tidygeocoder)
library(sf)
library(mapview)
# 1. Cleaning data ------------------------------------------------------------
malaria <- read_csv(
  "data/ucds/malaria_NOTI_SEM14_2021_clean.csv"
  ) %>% 
  filter(ano == "2021"&distrito == "SAN JUAN BAUTISTA") %>% 
  drop_na(localidad) |> 
  select(
    ano,semana,diagnostic,diagno,ubigeo,departam,
    provincia,distrito,localidad,direccion,edad,
    sexo,cod_disa,disa,fecha_not
    ) %>%
  mutate(
    localidad = gsub("-","",localidad),
    direccion = gsub("├æ","Ñ",direccion),
    direccion = gsub("├ô","Ó",direccion),
    direccion = gsub("N┬░","LOTE",direccion)
    ) |> 
  mutate(localidad = case_when(
    localidad == "1ro ENERO" ~ "1 DE ENERO",
    localidad == "1  DE OCTUBRE" ~ "1 DE OCTUBRE",
    localidad == "ANDOAS NUEVO" ~ "ANDOAS",
    localidad == "ANDOAS VIEJO" ~ "ANDOAS",
    localidad == "NUEVA ALIANZA (CHAMBIRA)" ~ "NUEVA ALIANZA",
    localidad == "NUEVA ESPERANZA (CHAMBIRA" ~ "NUEVA ESPERANZA",
    localidad == "NUEVO LIMON(CHAPURI)" ~ "NUEVO LIMON",
    localidad == "NUEVO PROGRESO - MUYUY" ~ "NUEVO PROGRESO",
    localidad == "C. IQUITOS" ~ "IQUITOS",
    TRUE ~ localidad)
    ) %>%
  drop_na(localidad)

localidad <- c(
  "SAN JOSE","VARILLAL","CALIPSO","SAN CIRILO",
  "PEÑA NEGRA","UNION PROGRESO","LOS DELFINES","QUISTOCOCHA"
  )

db <- malaria |> 
  filter(
    str_detect(
      localidad,
      "SAN JOSE|VARILLAL|CALIPSO|SAN CIRILO|PEÑA NEGRA|
      UNION PROGRESO|LOS DELFINES|QUISTOCOCHA"
      )
    )

# Geocoding -------------------------------------------------
malaria_geo <- db |> 
  mutate(adress = paste(
    departam,
    provincia,
    distrito,
    localidad,
    direccion,
    sep = ",")) %>% 
  geocode(
  address = adress,
  method = "arcgis",
  lat = latitude,
  long = longitude
  ) |> 
  select(-c(adress))

malaria_geo |> 
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) |> 
  mapview(layer.name = "malaria",col.regions = "red")

# write_csv(malaria_geo,"malaria2021.csv")