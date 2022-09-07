library(tidyverse)
library(tidygeocoder)
library(sf)
library(mapview)
# 1. Cleaning data ------------------------------------------------------------
malaria <- read_csv(
  "data/ucds/malaria_NOTI_SEM14_2021_clean.csv"
  ) %>% 
  filter(ano == "2021") %>% 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  drop_na(localidad) |> 
  select(
    ano,semana,diagnostic,diagno,ubigeo,departam,provincia,
    distrito,localidad,direccion,edad,sexo,cod_disa,disa,
    fecha_not
    ) %>%
  mutate(localidad = case_when(
    localidad == "1ro ENERO" ~ "1 DE ENERO",
    localidad == "1  DE OCTUBRE" ~ "1 DE OCTUBRE",
    localidad == "ANDOAS NUEVO" ~ "ANDOAS",
    localidad == "ANDOAS VIEJO" ~ "ANDOAS",
    localidad == "NUEVA ALIANZA (CHAMBIRA)" ~ "NUEVA ALIANZA",
    localidad == "NUEVA ESPERANZA (CHAMBIRA" ~ "NUEVA ESPERANZA",
    localidad == "NUEVO LIMON(CHAPURI)" ~ "NUEVO LIMON",
    localidad == "NUEVO PROGRESO - MUYUY" ~ "NUEVO PROGRESO",
    localidad == "QUEBRADA SUNICA├æO" ~ "QUEBRADA SUNICAÑO",
    localidad == "QUEBRADA PA├æA" ~ "QUEBRADA PAÑA",
    localidad == "QUEBRADA LU├æO" ~ "QUEBRADA LUÑO",
    localidad == "PA├æAYACU" ~ "PAÑAYACU",
    localidad == "C. IQUITOS" ~ "IQUITOS",
    TRUE ~ localidad)
    ) %>%
  mutate(localidad = gsub("├æ","Ñ",localidad)) |> 
  drop_na(localidad) |> 
  mutate(direccion = gsub("N┬░","LOTE",direccion))

localidad <- c(
  "SAN JOSE","VARILLAL","CALIPSO","SAN CIRILO",
  "PEÑA NEGRA","UNION PROGRESO","LOS DELFINES","QUISTOCOCHA"
  )

c1 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"SAN JOSE"))

c2 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"VARILLAL"))

c3 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"CALIPSO"))

c4 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"SAN CIRILO"))

# "PEÑA NEGRA","UNION PROGRESO","LOS DELFINES","QUISTOCOCHA")
c5 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"PEÑA NEGRA"))

c6 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"UNION PROGRESO"))

c7 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"LOS DELFINES"))

c8 <- malaria |> 
  filter(distrito == "SAN JUAN BAUTISTA") |> 
  filter(str_detect(localidad,"QUISTOCOCHA"))

db_total <- bind_rows(c1,c2,c3,c4,c5,c6,c7,c8)
# write_csv(db_total,"Geocoding/dengue_rawdata.csv")
# Geocoding -------------------------------------------------
malaria <- read_csv("Geocoding/malaria_rawdata.csv")
malaria_geo <- malaria |> 
  mutate(direccion = gsub("├æ","Ñ",direccion)) |> 
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
)

malaria_geo |> 
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) |> 
  mapview(layer.name = "malaria",col.regions = "red")

# write_csv(malaria_geo,"Final/malaria2021.csv")