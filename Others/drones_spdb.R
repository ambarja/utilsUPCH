library(rgee)
library(innovar)
ee_Initialize(user = "antony.barja8@gmail.com",drive = T)
db_drones <- st_read(
  "/home/am/Documentos/GitHub/utilsUPCH/data/db_drones.gpkg"
  ) |>
  st_transform(32718) |> 
  st_buffer(dist = 5*5000) |> 
  st_transform(4326) |> 
  sf_as_ee()
deforestation <- get_def(
  from = "2014-12-01",
  to = "2021-12-31",
  region = db_drones
  )
night_lights <- get_viirs(
  from = "2014-01-12",
  to = "2021-12-31",
  region = db_drones,
  fun = "mean")

# Two datasets --------
deforestation <- deforestation |> 
  pivot_longer(
    !c("cp","ruta","class"),
    names_to = "year",
    values_to = "deforestacion_km2") |> 
  mutate(year = gsub("Adef_","",year) |> as.numeric()) |> 
  select(cp,ruta,class,year,deforestacion_km2)

night_lights <- night_lights |>
  pivot_longer(
    !c("class","cp","ruta"),
    names_to = "year",
    values_to = "nigth_lights") |> 
  mutate(year = gsub("ntl","",year) |> as.numeric()) |> 
  select(cp,ruta,class,year,nigth_lights)

night_lights <- night_lights |> 
  select(cp,year,nigth_lights)

spdb <- deforestation |> 
  left_join(y = night_lights,by = c("cp","year"))

write_csv(spdb,"/home/am/Documentos/GitHub/utilsUPCH/data/spdb_drones.csv")