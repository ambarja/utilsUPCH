library(rgee)
library(innovar)
ee_Initialize(user = "antony.barja8@gmail.com",drive = T)

db_ee <- db_drones |> 
  st_transform(32718) |> 
  st_buffer(dist = 5*1000) |> 
  sf_as_ee()

Map$addLayer(db_ee)
deforestation <- get_def(from = "2015-01-01",to = "2021-12-31",region = db_ee,scale = 30)
nigth_ligth <- get_viirs(from = "2015-01-01",to = "2021-12-31",region = db_ee,fun = "mean")

deforestation <- deforestation |> 
  pivot_longer(
    !c("class","cp","ruta"),
    names_to = "year",
    values_to = "Deforestation") |> 
  mutate(year = gsub("Adef_","",year) |> as.numeric()) |> 
  select(cp,ruta,class,year,Deforestation)

nigth_ligth <- nigth_ligth |> 
  pivot_longer(
    !c("class","cp","ruta"),
    names_to = "year",
    values_to = "Nigth_light") |> 
  mutate(year = gsub("ntl","",year) |> as.numeric()) |> 
  select(cp,ruta,class,year,Nigth_light)

deforestation <- deforestation |> 
  select(cp,year,Deforestation)

spdb <- left_join(
  deforestation,
  nigth_ligth,
  c("cp","year")
)

write_sf(db_drones,"data/db_drones.gpkg")
write_csv(spdb,"data/spdb_drones.csv")
