library(tidyverse)
library(tidymodels)
library(sf)

# Reading spatial data ----------------------------------------------------
spdata <- st_read('GPKG/coeficientes.gpkg') %>% 
  st_set_geometry(NULL)

data <- read_csv('index_economics.csv') %>% 
  select(anio,DISTRITO,IDDIST,index_colocaciones,index_depositos) %>% 
  mutate(index_colocaciones = log(index_colocaciones+1) ,
         index_depositos = log(index_colocaciones+1))

finaldata <- left_join(
  data,
  spdata,
  by = "IDDIST"
)

names_rm <- c("NOMBPROV","NOMBDEP","DCTO","LEY","FECHA",
              "NOM_CAP","AREA_KM.","FEC_ACT","FUENTE" )
finaldata <- finaldata %>% 
  select(-c(names_rm))

finaldata <- finaldata %>% mutate_all(replace_na,0)
# write_rds(finaldata,'inputModels/basedata.rds')

# Models ------------------------------------------------------------------
model_reg <- lm(
  formula = prodef ~ index_colocaciones + index_depositos + riomean +
    viamean + cagmean + cpomean + slpmean + catmean + acrmean + 
    tpimean + demmean + anpmean + anio,
  data = finaldata
  )

summary(model_reg)$coefficient
confint(model_reg)










