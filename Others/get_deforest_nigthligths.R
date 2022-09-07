library(tidyverse)
library(sf)
library(innovar)
library(rgee)
ee_Initialize(user = "antony.barja@upch.pe",drive = T)
source("https://raw.githubusercontent.com/ambarja/utilsR/main/ubicode.R")

redatam <- read_csv("viv_hog_pob.csv") |> 
  mutate(manzana = gsub(" ","",manzana)) |> 
  separate(
    manzana,
    into = c("codmz","dep","prov","dist","cp","mz"),
    sep = ",") |>
  mutate_all(trimws) |> 
  mutate(codmz = lapply(codmz,ubicode_mz)|> unlist()) |> 
  filter(dist %in% c("SanJuanBautista","Nauta")) |> 
  mutate(CPINEI = substr(codmz,1,10)) |> 
  group_by(cp,CPINEI) |> 
  summarise(
    vivienda = sum(as.numeric(vivienda)),
    hogar = sum(as.numeric(hogar)),
    poblacion = sum(as.numeric(poblacion)))

villages <- st_read(
  "/home/am/Escritorio/propuesta_upch/SBD/villages.gpkg"
  ) |> 
  filter(CPINEI != " ") |> 
  mutate(CPINEI = lapply(CPINEI,ubicode_cp) |> unlist())

villages_inei <- st_read(
  "/home/am/Escritorio/propuesta_upch/SBD/villages_inei.gpkg"
  ) |> 
  mutate(IDCCPP_17 = lapply(IDCCPP_17,ubicode_cp) |> unlist())

villages_inei$IDCCPP_17 %in% redatam$CPINEI |> table()
villages$CPINEI %in% redatam$CPINEI |> table()

# Get information of deforestation by 5km  and 10 km 

villages_inei_ee_5km <- villages_inei |> 
  select(IDCCPP_17) |> 
  st_transform(crs = 32718) |> 
  st_buffer(dist = 5*1000) |> 
  sf_as_ee()

villages_inei_ee_10km <- villages_inei |> 
  select(IDCCPP_17) |> 
  st_transform(crs = 32718) |> 
  st_buffer(dist = 10*1000) |> 
  sf_as_ee()

deforestacion_5km <- get_def(
  from = "2015-01-01",
  to = "2021-12-31",
  region = villages_inei_ee_5km)

deforestacion_10km <- get_def(
  from = "2015-01-01",
  to = "2021-12-31",
  region = villages_inei_ee_10km)

# Get information of night lights by 5km  and 10 km 
ligths_noct_5km <- get_nlv2(
  from = "2015-01-01",
  to = "2021-12-31",
  region = villages_inei_ee_5km,
  fun = "mean",
  scale = 100
)

ligths_noct_10km <- get_nlv2(
  from = "2015-01-01",
  to = "2021-12-31",
  region = villages_inei_ee_5km,
  fun = "mean",
  scale = 100
)

# Tidy data by villages 
deforestacion_5km <- deforestacion_5km |> 
  pivot_longer(
    !IDCCPP_17,
    names_to = "year",
    values_to = "def_areakm_in_5km") |> 
  mutate(year = gsub("Adef_","",year) |> as.numeric())

deforestacion_10km <- deforestacion_10km |> 
  pivot_longer(
    !IDCCPP_17,
    names_to = "year",
    values_to = "def_areakm_in_10km") |> 
  mutate(year = gsub("Adef_","",year) |> as.numeric())

ligths_noct_5km <- ligths_noct_5km |> 
  pivot_longer(
    !IDCCPP_17,
    names_to = "year",
    values_to = "ligths_noct_5km") |> 
  mutate(year = gsub("ntl","",year) |> as.numeric())

ligths_noct_10km <- ligths_noct_10km |> 
  pivot_longer(
    !IDCCPP_17,
    names_to = "year",
    values_to = "ligths_noct_10km") |> 
  mutate(year = gsub("ntl","",year) |> as.numeric())

list_db_ee <- list(
  deforestacion_5km,
  deforestacion_10km,
  ligths_noct_5km,
  ligths_noct_10km
  )

db_ee <- list_db_ee %>% reduce(full_join, by=c("IDCCPP_17","year")) |> 
  rename(CODCP = IDCCPP_17)

villages_db <- villages_inei |> 
  select(IDCCPP_17,NOMCCPP_17,DEPARTAMEN,PROVINCIA,DISTRITO) |> 
  rename(
    CODCP = IDCCPP_17,
    NOMCP = NOMCCPP_17,
    DEP = DEPARTAMEN,
    PROV = PROVINCIA,
    DIST = DISTRITO
    ) |> 
  left_join(db_ee,by = "CODCP")

# write_sf(villages_db,"/home/am/Escritorio/propuesta_upch/SBD/villages_db.gpkg")