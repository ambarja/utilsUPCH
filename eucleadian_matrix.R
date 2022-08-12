# 1. Requeriments -------------------------------------------------------
library(sf)
library(tidyverse)
# 2. Reading of vector layer of villages --------------------------------
cp <- st_read(
  "/home/am/Documentos/UPCH/MalariaNetwork/recursos/centros_poblados.gpkg"
  )
# 3. Calculate of matrix distance ---------------------------------------
mt <- cp %>% 
  st_transform(32718) %>% 
  st_distance(.,.) %>% 
  as.data.frame()
colnames(mt) <- cp$id_loc
rownames(mt) <- cp$id_loc

# 4. Final dataframe in three columns ----------------------------------
fun_fromid <- function(x){sprintf("%s",rep(rownames(mt)[x],1829))}
new_fromid <- lapply(1:length(rownames(mt)), fun_fromid) %>% 
  unlist() %>% 
  as.vector()

eucledian_mt <- tibble(
  fromid = c(t(row(mt))),
  toid = c(t(col(mt))),
  distance = c(t(mt))
  ) %>% 
  mutate(
    fromid = new_fromid,
    toid = rep(colnames(mt),1829)
    )
head(eucledian_mt)

