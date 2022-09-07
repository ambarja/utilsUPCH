library(tidyverse)
library(sf)

# 1. Reading spatial data -------------------------------------------------
cp <- read_rds("resources/malaria_basins.rds") |> 
  select(-c("id2","uid"))

defores <- def %>% 
  select(id_loc,HydroNameANA:HydroNameL7,RiverName,Adef_2009:Adef_2018) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "deforestation"
    ) %>% 
  mutate(year = gsub("Adef_","",year) %>% as.numeric()) %>% 
  slice(rep(1:n(), each = 12)) %>% 
  mutate(month = rep(1:12,18290)) %>% 
  mutate(year = as.numeric(year),month = as.numeric(month)) %>% 
  mutate(
    id_loc,
    HydroNameANA,
    HydroNameL6,
    HydroNameL7,
    RiverName,
    year,
    month,
    deforestation
    )
rm(def)

ETP <- etp %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,`ET2009-01`:`ET2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "evapotranspiration"
    ) %>% 
  mutate(year = gsub("ET","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,
    HydroNameL7,RiverName,year,
    month,evapotranspiration
    )
rm(etp)

GHM <- ghm %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,gHM
    ) %>% 
  mutate(
    global_human_mod = gHM,
    year = 2009
    ) %>% 
  slice(rep(1:n(), each = 10)) %>% 
  mutate(year = rep(2009:2018,1829)) %>% 
  slice(rep(1:n(), each = 12)) %>%  
  mutate(
    month = as.numeric(rep(1:12,18290)),
    year = as.numeric(year)
    ) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,
    RiverName,year,month,global_human_mod
    )
rm(ghm)

Humidity <- humidity %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,
    RiverName,`Qair_f_tavg2009-01`:`Qair_f_tavg2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "humidity"
    ) %>% 
  mutate(year = gsub("Qair_f_tavg","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,
    HydroNameL7,RiverName,year,month,humidity
    )
rm(humidity)

Pop <- pop %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,pop2009:pop2018
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "population",
  ) %>% 
  slice(rep(1:n(), each = 12)) %>%
  mutate(
    year = as.numeric(gsub("pop","",year)),
    month = as.numeric(rep(1:12,18290))
    ) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,year,month,population
    )
rm(pop)  

Pp <- pp %>%
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,`pr2009-01`:`pr2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "precipitation"
    ) %>% 
  mutate(year = gsub("pr","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  select(id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,   
         year,month,precipitation)
rm(pp)
  
Ro <- ro %>%
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,`ro2009-01`:`ro2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "runoff"
    ) %>% 
  mutate(year = gsub("ro","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(
    month = as.numeric(month),
    year = as.numeric(year)) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,year,month,runoff
    )
rm(ro)

Soil <- soil %>%
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,`soil2009-01`:`soil2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "soil_moisture"
    ) %>% 
  mutate(year = gsub("soil","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(
    month = as.numeric(month),
    year = as.numeric(year)) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,year,month,soil_moisture
    )
rm(soil)

Tmax <- tmax %>%
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,`tmmx2009-01`:`tmmx2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "maximum_temperature"
    ) %>% 
  mutate(year = gsub("tmmx","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(
    month = as.numeric(month),
    year = as.numeric(year)) %>% 
  mutate(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,year,month,maximum_temperature
    )
rm(tmax)

Tmin <- tmin %>%
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,`tmmn2009-01`:`tmmn2018-12`
    ) %>% 
  pivot_longer(
    !c("id_loc","HydroNameANA","HydroNameL6","HydroNameL7","RiverName"),
    names_to = "year",
    values_to = "minimum_temperature"
    ) %>% 
  mutate(year = gsub("tmmn","",year)) %>% 
  separate(col = year,into = c("year","month"),sep = "-") %>% 
  mutate(
    month = as.numeric(month),
    year = as.numeric(year)) %>% 
  select(
    id_loc,HydroNameANA,HydroNameL6,HydroNameL7,RiverName,year,month,minimum_temperature
    )

rm(tmin)

# 2. Joined dataset -------------------------------------------------------

m1 <- left_join(
  cp %>% select(province,district,village,id_loc,year,month,nrohab,fal,viv),
  defores,
  by = c("id_loc","year","month")
)

m2 <- left_join(
  m1,
  ETP %>%select(id_loc,evapotranspiration,year,month) ,
  by = c("id_loc","year","month")
)

m3 <- left_join(
  m2,
  GHM %>%select(id_loc,global_human_mod,year,month) ,
  by = c("id_loc","year","month")
)

m4 <- left_join(
  m3,
  Humidity %>%select(id_loc,humidity,year,month) ,
  by = c("id_loc","year","month")
)

m5 <- left_join(
  m4,
  Pop %>%select(id_loc,population,year,month) ,
  by = c("id_loc","year","month")
)

m6 <- left_join(
  m5,
  Pp%>%select(id_loc,precipitation,year,month) ,
  by = c("id_loc","year","month")
)

m7 <- left_join(
  m6,
  Ro %>% select(id_loc,runoff,year,month) ,
  by = c("id_loc","year","month")
)

m8 <- left_join(
  m7,
  Soil %>% select(id_loc,soil_moisture,year,month) ,
  by = c("id_loc","year","month")
)

m9 <- left_join(
  m8,
  Tmax %>% select(id_loc,maximum_temperature,year,month) ,
  by = c("id_loc","year","month")
)

m10 <- left_join(
  m9,
  Tmin %>% select(id_loc,minimum_temperature,year,month) ,
  by = c("id_loc","year","month")
)

# write_rds(m10,"data/db_varibles.rds")

# Export newdata in csv ---------------------------------------------------
final <- m10 %>% 
  select(
    province,district,village,HydroNameANA,
    HydroNameL6,HydroNameL7,RiverName,id_loc,
    year,month,nrohab,fal,viv,deforestation,
    evapotranspiration,global_human_mod,humidity,
    population,precipitation,runoff,soil_moisture,
    maximum_temperature,minimum_temperature
    ) %>% 
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
    ) %>% 
  st_drop_geometry()

# write_csv(final,"data/db_variables.csv")