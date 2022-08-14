library(sf)
library(dplyr)
library(mapview)
# 1. Iquitos-Nauta ---------------------------------------------------------
comunidad_1 <- c(
  "CAHUIDE","VARILLAL","LOS DELFINES",
  "PEÑA NEGRA","QUISTOCOCHA","AA.HH TERMINAL",
  "AA.HH. VILLA SELVA")

data <- read.csv(
  "Documentos/GitHub/UPCH/UPCH-scripts/Bryan/cp.csv") |>
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
  st_as_sf(coords = c("lon","lat"),crs = 4326)

m1 <- mapview(eje_in, legend = F)

# 2. Santa Clara-Santo Tomas ---------------------------------------------
comunidad_2 <- c("SANTA CLARA","SANTO TOMAS","RUMOCOCHA") 
data <- read.csv(
  "Documentos/GitHub/UPCH/UPCH-scripts/Bryan/cp.csv") |>
  mutate(NOMCP = toupper(NOMCP)) |> 
  filter(NOMCP %in% comunidad_2) |> 
  st_drop_geometry() |> 
  select(XGD, YGD,NOMCP) |> 
  rename(lon = XGD,
         lat = YGD,
         cp = NOMCP
  ) |> 
  as_tibble()

eje_in <- bind_rows(data,nofind) |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

m2 <- mapview(eje_in, legend = F)

# 3. Zungarococha ---------------------------------------------------------
comunidad_3 <- c("LLANCHAMA","NINA RUMI","PUERTO ALMENDRA","SANTA ISABEL DE ZUNGARO COCHA")
data <- read.csv(
  "Documentos/GitHub/UPCH/UPCH-scripts/Bryan/cp.csv") |>
  mutate(NOMCP = toupper(NOMCP)) |> 
  filter(NOMCP %in% comunidad_3) |> 
  st_drop_geometry() |> 
  select(XGD, YGD,NOMCP) |> 
  rename(lon = XGD,
         lat = YGD,
         cp = NOMCP
  ) |> 
  as_tibble()

eje_in <- bind_rows(data,nofind) |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

m3 <- mapview(eje_in, legend = F)

# Other -----------------------------------------------------------------------
comunidad_4 <- c("TARAPOTO","HUATURI","SAN PABLO DE CUYANA")
data <- read.csv(
  "Documentos/GitHub/UPCH/UPCH-scripts/Bryan/cp_other.csv") |>
  mutate(NOMCP = toupper(NOMCP)) |> 
  filter(NOMCP %in% comunidad_4) |> 
  st_drop_geometry() |> 
  select(XGD, YGD,NOMCP) |> 
  rename(lon = XGD,
         lat = YGD,
         cp = NOMCP
  ) |> 
  as_tibble()

eje_in <- bind_rows(data,nofind) |> 
  st_as_sf(coords = c("lon","lat"),crs = 4326)

m4 <- mapview(eje_in, legend = F)
