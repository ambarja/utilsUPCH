library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

# Ruta 1 ------------------------------------------------------------------
villages <- st_read("data/villages_1km.gpkg",quiet = T) %>%
  filter(CPINEI != " ")  %>% 
  select(NOMCP,DIST)  %>%
  mutate(NOMCP = toupper(NOMCP))

comunidad_1 <- c(
  "CAHUIDE","VARILLAL","LOS DELFINES",
  "PEÑA NEGRA","QUISTOCOCHA","AA.HH TERMINAL",
  "AA.HH. VILLA SELVA")

data <- read.csv(
  "data/cp_drones.csv") |>
  mutate(NOMCP = toupper(NOMCP)) |> 
  filter(NOMCP %in% comunidad_1) |> 
  st_drop_geometry() |> 
  select(XGD, YGD,NOMCP) |> 
  rename(lon = XGD,
         lat = YGD,
         cp = NOMCP
  ) |> 
  as_tibble()

nofind <- tibble(
  lat = c(-3.796194,-3.7987158),
  lon = c(-73.2992432,-73.310606),
  cp = c("AA.HH. VILLA SELVA","AA.HH SAN ANTONIO DEL TERMINAL")
)

eje_in <- bind_rows(data,nofind) |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326) |> 
  mutate(class = "old") |> 
  select(cp,class,geometry)

norepeat <- villages[which(!villages$NOMCP %in% eje_in$cp),]
norepeat <- norepeat |> 
  mutate(class = "new") |>
  select(-c(DIST)) |> 
  rename(cp = NOMCP,geometry = geom) |> 
  select(cp,geometry,class)

eje_n1 <- bind_rows(eje_in,norepeat)
iquitos_nauta <- eje_n1 |> mutate(ruta = "Iquitos-Nauta")
rm(comunidad_1)
rm(eje_n1)
rm(eje_in)
rm(nofind)
rm(data)
rm(norepeat)
rm(villages)
mapview(iquitos_nauta)
# Ruta 2 ------------------------------------------------------------------
#Santa Clara-Santo Tomas
comunidad_2 <- c("SANTA CLARA","SANTO TOMAS","RUMOCOCHA") 
data <- read.csv(
  "data/cp_drones.csv") |>
  mutate(NOMCP = toupper(NOMCP)) |> 
  filter(NOMCP %in% comunidad_2) |> 
  st_drop_geometry() |> 
  select(XGD, YGD,NOMCP) |> 
  rename(lon = XGD,
         lat = YGD,
         cp = NOMCP
  ) |> 
  as_tibble()

eje_in <- data |> 
  mutate(class = "old") |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

nuevos <- tibble(
  cp = c("GEDEONES","LA PAZ","31 DE AGOSTO"),
  lat = c(-3.7961646839999617,-3.7982742679999433,-3.806527537999955),
  lon = c(-73.35946499999994,-73.33290658799996,-73.32575276499995)
  ) |> 
  st_as_sf(coords = c("lon","lat"),crs= 4326) |> 
  mutate(class = "new")

StaClara_StoTomas <- bind_rows(eje_in,nuevos) |> 
  mutate(ruta = "Santa Clara - Santo Tomas")
rm(eje_in)
rm(data)
rm(nuevos)
rm(villages)
rm(comunidad_2)
rm(norepeat)
mapview(StaClara_StoTomas)
# Carretera Zungarococha --------------------------------------------------
comunidad_3 <- c("LLANCHAMA","NINA RUMI","PUERTO ALMENDRA","SANTA ISABEL DE ZUNGARO COCHA")
data <- read.csv(
  "data/cp_drones.csv") |>
  mutate(NOMCP = toupper(NOMCP)) |> 
  filter(NOMCP %in% comunidad_3) |> 
  st_drop_geometry() |> 
  select(XGD, YGD,NOMCP) |> 
  rename(lon = XGD,
         lat = YGD,
         cp = NOMCP
  ) |> 
  as_tibble()

eje_in <- data |> 
  mutate(class = "old") |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

nuevo <- tibble(
  cp = "VILLA CRUZ",
  lat = -3.853170524999939,
  lon = -73.37349268399998
  ) |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326) |> 
  mutate(class = "new")

zungarococha <- bind_rows(eje_in,nuevo) |> 
  mutate(ruta = "Zungarococha")
rm(data)
rm(eje_in)
rm(nuevo)
rm(comunidad_3)
mapview(zungarococha)
# Otros -------------------------------------------------------------------
comunidad_4 <- c("TARAPOTO","HUATURI","SAN PABLO DE CUYANA")
data <- tibble(
  cp = c("TARAPOTO","HUATURI","SAN PABLO DE CUYANA"),
  lat = c(-3.80528,-3.77443,-3.76640),
  lon = c(-73.40567,-73.40446,-73.37614)
) |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

otros <- data |> mutate(class = "old", ruta = "Other") 
rm(data)
rm(comunidad_4)
mapview(otros)
# Rutas em total ------------------------------------------------
db_drones <- bind_rows(iquitos_nauta,StaClara_StoTomas,zungarococha,otros) |> 
  select(cp,ruta,class,geometry)
write_sf(db_drones,"data/db_drones.gpkg")
mapview(db_drones)
