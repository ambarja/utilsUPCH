library(rgee)
library(tidyverse)
library(sf)
ee_Initialize(user = 'geografo.pe@gmail.com',drive = T)

house_gps_1km <- read_csv("household_gps_share_072022.csv") |> 
  st_as_sf(coords = c('ffi_gps_long','ffi_gps_lat'),crs = 4326) |> 
  st_transform(32718) |> 
  st_buffer(dist = 1*1000) |> 
  st_transform(4326) |> 
  select(ffi_h_code) |> 
  sf_as_ee()

house_gps_5km <- read_csv("household_gps_share_072022.csv") |> 
  st_as_sf(coords = c('ffi_gps_long','ffi_gps_lat'),crs = 4326) |> 
  st_transform(32718) |> 
  st_buffer(dist = 5*1000) |> 
  st_transform(4326) |> 
  select(ffi_h_code) |> 
  sf_as_ee()

urban_area <- ee$ImageCollection("users/ambarja/ghs_built_s")$
  toBands()$divide(10000)

house_gps_1km_csv <- ee_extract(
  x = urban_area,
  y = house_gps_1km,
  fun = ee$Reducer$sum(),
  scale = 100,
  sf = F
  )

house_gps_5km_csv <- ee_extract(
  x = urban_area,
  y = house_gps_5km,
  fun = ee$Reducer$sum(),
  scale = 100,
  sf = F
)

write_csv(house_gps_5km_csv,'urban_area_5km_ha.csv')
write_csv(house_gps_1km_csv,'urban_area_1km_ha.csv')
