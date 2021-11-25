library(tidyverse)
# Working with csv file ---------------------------------------------------
dengue <- read_csv('dengue2020.csv') %>% 
  mutate(
    index = str_detect(direccion,pattern = "S/N")
         ) %>% 
  filter(index != TRUE)

dengue <- dengue %>% 
  mutate(
    index = str_detect(
      direccion,pattern = "N┬░")
    ) %>% 
  filter(index != T) %>%  
  group_by(
    ano,ubigeo,departam,provincia,
    distrito,localidad,direccion
    ) %>% 
  summarise(
    total_casos = sum(table(direccion))
    )

dengue <- dengue %>% 
  mutate(
    index = str_detect(direccion,pattern = "[0-9]")
    ) %>% 
  filter(index == TRUE)

malaria <- read_csv('malaria2020.csv') %>% 
  mutate(
    index = str_detect(direccion,pattern = "S/N")
  ) %>% 
  filter(index != TRUE)
  
malaria <- malaria %>% 
  mutate(
    index = str_detect(direccion,pattern = "/")
  ) %>% 
  filter(index != TRUE)

malaria <- malaria %>% 
  group_by(
    ano,ubigeo,departam,provincia,
    distrito,localidad,direccion
  ) %>% 
  summarise(
    total_casos = sum(table(direccion))
  )
  
# Saving new csv file -----------------------------------------------------
if(dir.exists("FinalData")==0){
  dir.create(path = "FinalData")
  }

write.csv(dengue_geo,'FinalData/dengegeo2020.csv')
write.csv(dengue_geo,'FinalData/malariageo2020.csv')