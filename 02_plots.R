library(tidyverse)
library(cptcity)
library(cowplot)
library(extrafont)

# Reading data ------------------------------------------------------------
data <- read_csv('index_economics.csv') %>% 
  mutate(anio = factor(anio)) %>% 
  select(anio,DISTRITO,IDDIST,index_colocaciones,index_depositos) %>% 
  mutate(index_colocaciones = log(index_colocaciones+1) ,
         index_depositos = log(index_colocaciones+1)) %>% 
  gather(variable,value, index_colocaciones:index_depositos) 

p1 <- data %>% 
  filter(variable == "index_colocaciones") %>% 
  ggplot(aes(x = anio, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = "yellow") +
  geom_jitter(alpha = .3, colour = "black", fill = "white")+ 
  theme_minimal() + 
  labs(x = "Años", y = "Índice",
       title = "Tasa de colocaciones percápita") + 
  theme(
    axis.title.x = element_text(
      size = 14,
      face = 'bold',
      family = 'Roboto Slab'),
    axis.title.y = element_text(
      size = 14,
      face = 'bold',
      family = 'Roboto Slab'
    ),
    title =  element_text(
      size = 14,
      face = 'bold',
      family = 'Roboto Slab' )
  ) 
  
p2 <- data %>% 
  filter(variable == "index_depositos") %>% 
  ggplot(aes(x = anio, y = value)) +
  geom_boxplot(outlier.shape = NA, fill = "green") +
  geom_jitter(alpha = .3, colour = "black", fill = "white")+ 
  theme_minimal() + 
  labs(x = "Años", y = "Índice",
       title = "Tasa de depósitos percápita") + 
  theme(
    axis.title.x = element_text(
      size = 14,
      face = 'bold',
      family = 'Roboto Slab'),
    axis.title.y = element_text(
      size = 14,
      face = 'bold',
      family = 'Roboto Slab'
    ),
    title =  element_text(
      size = 14,
      face = 'bold',
      family = 'Roboto Slab' )
  ) 


# FinalPLots --------------------------------------------------------------

end <- plot_grid(p1,p2,nrow = 2)
last_plot()
ggsave(filename = 'plots.png',
       plot = last_plot(),
       width = 10,height = 6)