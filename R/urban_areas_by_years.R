library(rgee)
library(tidyverse)
library(sf)
ee_Initialize(user = 'geografo.pe@gmail.com',drive = T)

house_gps_5km <- read_csv("household_gps_share_072022.csv") |> 
  st_as_sf(coords = c('ffi_gps_long','ffi_gps_lat'),crs = 4326) |> 
  st_transform(32718) |> 
  st_buffer(dist = 5*1000) |> 
  st_transform(4326) |> 
  select(ffi_h_code) |> 
  sf_as_ee()

house_gps_10km <- read_csv("household_gps_share_072022.csv") |> 
  st_as_sf(coords = c('ffi_gps_long','ffi_gps_lat'),crs = 4326) |> 
  st_transform(32718) |> 
  st_buffer(dist = 10*1000) |> 
  st_transform(4326) |> 
  select(ffi_h_code) |> 
  sf_as_ee()

# Función para calcular el área de cada píxel
calcPixelArea <- function(image) {
  # Obtener la resolución espacial de la imagen
  pixel_area <- image$multiply(ee$Image$pixelArea()) |> 
    ee$Image$divide(1e6)
  area_km2 = pixel_area$updateMask(pixel_area$neq(0))
  return(area_km2)
}

urban_area <- ee$ImageCollection("users/ambarja/urban_area")$
  map(calcPixelArea)$
  toBands()

urban_area_5km <- ee_extract(
  urban_area,
  house_gps_5km,
  fun = ee$Reducer$sum(),
  scale = 100,
  sf = FALSE,
  crs = 'EPSG:4326'
  )

urban_area_10km <- ee_extract(
  urban_area,
  house_gps_10km,
  fun = ee$Reducer$sum(),
  scale = 100,
  sf = FALSE,
  crs = 'EPSG:4326'
)

write_csv(urban_area_5km,'urban_area_5km.csv')
write_csv(urban_area_10km,'urban_area_10km.csv')
