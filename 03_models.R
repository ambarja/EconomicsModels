library(tidyverse)
library(tidymodels)
library(GWmodel)
library(spgwr)
library(sf)

# Reading spatial data ----------------------------------------------------
spdata <- st_read('GPKG/coeficientesv2.gpkg') %>% 
  st_set_geometry(NULL) %>% 
  drop_na()

data <- read_csv('index_economics.csv') %>% 
  select(anio,DISTRITO,IDDIST,index_colocaciones) %>% 
  mutate(index_colocaciones = log(index_colocaciones+1))

finaldata <- left_join(
  spdata,
  data,
  by = "IDDIST"
)

names_rm <- c("NOMBPROV","NOMBDEP","DCTO","LEY","FECHA",
              "NOM_CAP","AREA_KM.","FEC_ACT","FUENTE","NOMBDIST")

finaldata <- finaldata %>% 
  select(-c(names_rm))

finaldata <- finaldata %>% mutate_all(replace_na,0)
# write_rds(finaldata,'inputModels/basedata.rds')

# Models ------------------------------------------------------------------
model_reg <- lm(
  formula = defmean ~ index_colocaciones + riomean +
    viamean + cagmean + cpomean + slpmean + catmean + acrmean + 
    tpimean + demmean + anpmean + anio,
  data = finaldata
  )
summary(model_reg)
summary(model_reg)$coefficient
confint(model_reg)

# GWR ---------------------------------------------------------------------

spdata <- st_read('GPKG/coeficientesv2.gpkg') %>% 
  select(IDDIST)

# spdata <- left_join(spdata,finaldata,"IDDIST") %>% 
#   drop_na(index_colocaciones) %>% 
#   select(IDDIST,anio)

datamodel <- left_join(spdata,finaldata,"IDDIST") %>% 
  drop_na(index_colocaciones) %>% 
  as('Spatial')

bw1 <- gwr.sel(
  formula = defmean ~ index_colocaciones + riomean +
    viamean + cagmean + cpomean + slpmean + catmean + acrmean + 
    tpimean + demmean + anpmean + anio,
  data = datamodel,
  coords = coordinates(datamodel),
  longlat =TRUE,
  adapt=FALSE
)

mod2 <- gwr(
  formula = defmean ~ index_colocaciones + riomean +
    viamean + cagmean + cpomean + slpmean + catmean + acrmean + 
    tpimean + demmean + anpmean + anio,
  data = datamodel@data,
  coords = coordinates(datamodel),
  bandwidth = bw1,
  hatmatrix=TRUE,
  se.fit=TRUE
) 
summary(mod2)
mod2$SDF$localR2

# Maps --------------------------------------------------------------------

gwr.map <- cbind(spdata, as.matrix(results))
gwr.map2 <- st_as_sf(gwr.map)
qtm(gwr.map, fill = "localR2") 












