library(tidyverse)
library(lis)
library(rgee)
library(sf)
ee_Initialize()
# Total de colocaciones 
# Total de depositos 
# tipo de banco


# Economics data ----------------------------------------------------------
data <- readxl::read_xlsx('Tasas/Dep&ColocAmazonia 2010-2019.xlsx')
newdata <- data %>%
  mutate(PERIODO_FECHA = format(PERIODO_FECHA,"%Y-%m-%d")) %>% 
  rename(FECHA = PERIODO_FECHA) %>% 
  separate(FECHA,into = c('anio','mes','dia')) %>% 
  group_by(anio,DISTRITO,CODIGO_UBIGEO) %>% 
  summarise(total_colocaciones = sum(TOTAL_COLOCACIONES,na.rm = TRUE),
            total_depositos = sum(TOTAL_DEPOSITOS,na.rm = TRUE)) %>% 
  mutate(DISTRITO = toupper(DISTRITO)) %>% 
  rename(IDDIST = CODIGO_UBIGEO)
  
# Population data ---------------------------------------------------------
ubigeo <- unique(newdata$CODIGO_UBIGEO)
region <- st_read('./GPKG/distritos.gpkg') %>% 
  filter(IDDIST %in% ubigeo)

region_ee <- pol_as_ee(
  region,
  id = c("IDDIST"),
  simplify = 10)

pop <- region_ee %>% 
  get_pop(
    from = '2010-01-01',
    to = '2019-12-31',
    fun = 'sum',
    scale = 100
    ) 
names(pop) <- c("IDDIST",paste0(2010:2019))

pop <- region %>% 
  left_join(
    pop,
    "IDDIST"
    )

pop <- pop %>%
  st_set_geometry(NULL) %>% 
  gather(variable, value, `2010`:`2019`) %>% 
  select(IDDIST,variable,value) %>% 
  mutate(value = round(value)) %>% 
  rename(anio = variable)
  
# Finaldata ---------------------------------------------------------------

newdata <- newdata %>% 
  left_join(
    pop,
    by = c("IDDIST","anio")
    ) %>% 
  mutate(
    index_colocaciones = total_colocaciones/value,
    index_depositos = total_depositos/value
    )

# Exportdata --------------------------------------------------------------
write_csv(newdata,'index_economics.csv')