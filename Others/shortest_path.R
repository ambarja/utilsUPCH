library(qgisprocess)
library(sf)
library(tidyverse)
library(purrr)

# Reading spatial data (input)
network <- st_read("rivers.gpkg")
dots <- st_read("centro_poblado_movido.gpkg")
table <- st_drop_geometry(dots)

# Preprocessing dataset for qgisprocess
distance_matrix <- st_read("matriz_distance.csv")
distance_matrix <- distance_matrix %>% 
  left_join(y = table,by = c("InputID"="id_loc"))
distance_matrix <- distance_matrix %>% 
  left_join(y = table,by = c("TargetID"="id_loc"))

db_distance_matrix <- distance_matrix %>% 
  select(InputID:province.x,Lat_new.x,
         Lon_new.x,Lat_new.y,Lon_new.y) %>% 
  rename(id = id2.x,
         nrohab = nrohab.x,
         village = village.x,
         district = district.x,
         province = province.x,
         Lat_ori = Lat_new.x,
         Lon_ori = Lon_new.x,
         Lat_des = Lat_new.y,
         Lon_des = Lon_new.y
  ) %>% 
  mutate(START_POINT = sprintf("%s,%s [EPSG:4326]",Lon_ori,Lat_ori),
         END_POINT = sprintf("%s,%s [EPSG:4326]",Lon_des,Lat_des),
         id = rownames(db_distance_matrix)) 

# Exploring qgisprocess
qgis_algorithms() %>% View()
qgis_show_help(algorithm = "native:shortestpathpointtopoint")
shortest_path <- qgis_function(algorithm = "native:shortestpathpointtopoint")

# Preparing function for loop or apply family

distance_by_river <- function(x){
  # Measure river distance 
  longitude_river <- shortest_path(
    INPUT = network, 
    STRATEGY = 0,
    DEFAULT_DIRECTION = 2,
    DEFAULT_SPEED = 50,
    TOLERANCE = 0,
    START_POINT = db_distance_matrix[["START_POINT"]][x],
    END_POINT = db_distance_matrix[["END_POINT"]][x],
    OUTPUT = qgis_tmp_vector(),
    PROJECT_PATH = "malaria-network.qgz" # A project of qgis
  ) %>% 
    qgis_output('OUTPUT') %>% 
    st_read()
    
  # Calculate longitude of river distance 
  longitude_river$distance_rivers <- st_length(longitude_river) %>%
    as.vector()
  newtable <- st_drop_geometry(longitude_river) %>% 
    mutate(id_computate = db_distance_matrix[["id"]][x])
}

lista_distancias <- map(
  1:nrow(db_distance_matrix),
  possibly(distance_by_river, NA)
  )
