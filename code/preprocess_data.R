# paquetes
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos"))

# Filter data
# 
rm(list = ls())
library(tidyverse)


dat <- read_csv("data/perfiles-2020-09-18.csv")
lentnames(dat)
d <- dat %>% 
  dplyr::select(idh = id,
                idp=perfil_id, 
                y = perfil_ubicacion_latitud, 
                x = perfil_ubicacion_longitud,
                top = profundidad_superior,
                bot = profundidad_inferior,
                colh = color_humedo_hvc,
                cols = color_seco_hvc,
                cec = analitico_t, 
                bsum = analitico_s,
                bsat = analitico_saturacion_t,
                oc = analitico_carbono_organico_c,
                ph = analitico_ph_h2o,
                clay = analitico_arcilla
  ) %>% 
  filter(top <= 20)

# algo


chum <- stringr::str_split(string = d$colh, pattern = "(?<=YR)", simplify = TRUE) %>% as_tibble()
chum <- cbind(chum,
              stringr::str_split(string = chum$V2, pattern = "/", simplify = TRUE)) %>% 
  as_tibble()

names(chum) <- c("h_humedo", "v_c_humedo", "misc_hum_1", "value_humedo", "chroma_humedo", "misc_hum_2")
chum$h_humedo <- gsub(pattern = " ", replacement = "", x = chum$h_humedo)
chum$v_c_humedo <- gsub(pattern = " ", replacement = "", x = chum$v_c_humedo)
chum$value_humedo <- gsub(pattern = " ", replacement = "", x = chum$value_humedo)

chum <- chum %>% dplyr::select(h_humedo, value_humedo, chroma_humedo)

d <- cbind(d,chum) %>% as_tibble()

csec <- stringr::str_split(string = d$cols, pattern = "(?<=YR)", simplify = TRUE) %>% as_tibble()
csec <- cbind(csec,
              stringr::str_split(string = csec$V2, pattern = "/", simplify = TRUE)) %>% 
  as_tibble()

names(csec) <- c("h_seco", "v_c_seco", "misc_sec_1", "value_seco", "chroma_seco", "misc_sec_2")
csec$h_sec <- gsub(pattern = " ", replacement = "", x = csec$h_seco)
csec$v_c_seco <- gsub(pattern = " ", replacement = "", x = csec$v_c_seco)
csec$value_seco <- gsub(pattern = " ", replacement = "", x = csec$value_seco)

csec <- csec %>% dplyr::select(h_seco, value_seco, chroma_seco)

d <- cbind(d,csec) %>% as_tibble()

write_csv(d, "data/datos_para_BlackSoil.csv")


write_csv2(d, "data/datos_para_BlackSoil2.csv")


library(tidyverse)
dat <- read_csv("data/datos_revisados_26_sep_20.csv")
dat$cec <- dat$cec/1000
dat$bsum <- dat$bsum/1000



#ACÁ ARRANCA EL SCRIPT
library(tidyverse)
dat <- read_csv("data/datos_revisados_26_sep_20.csv")

# creacioón del campo blacksoil2, de auquellos que cumplen las condiciones para ser Black Soils 2 Categoría

datbs2 <- dat %>% 
  mutate(bs_oc = if_else(condition = oc >= 1.2, true = 1, false = 0)) %>% 
  mutate(bs_cromah = if_else(condition = chroma_humedo <= 3, true = 1, false = 0)) %>%
  mutate(bs_valueh = if_else(condition = value_humedo <= 3, true = 1, false = 0)) %>%
  mutate(bs_values = if_else(condition = value_seco <= 5, true = 1, false = 0)) %>%
  mutate(bs_top = if_else(condition = top <= 20, true = 1, false = 0)) %>%
  mutate(blacksoil2 = if_else(condition = bs_oc == 1 & bs_cromah == 1 & bs_valueh == 1 | bs_values == 1 & bs_top == 1, true = 1, false = 0)) %>%
  View()


# error. no entiendo por qué no puedo hacer un subset con el nuevo campo. no es un objeto
  cat2 <- subset(datbs2, blacksoil2 == 1) 

 
  
# no logro exportar la nueva tabla, error.
  
  
  write_csv(datbs2, "datbs2.csv")
  
# creacioón del campo blacksoil1, de auquellos que cumplen las condiciones para ser Black Soils 1 Categoría. 
# no corre y no entiendo por qué. es igual al codigo anterior mas las nuevas condiciones
  
  datbs1 <- dat %>% 
    mutate(bs_oc = if_else(condition = oc >= 1.2, true = 1, false = 0)) %>% 
    mutate(bs_cromah = if_else(condition = chroma_humedo <= 3, true = 1, false = 0)) %>%
    mutate(bs_valueh = if_else(condition = value_humedo <= 3, true = 1, false = 0)) %>%
    mutate(bs_values = if_else(condition = value_seco <= 5, true = 1, false = 0)) %>%
    mutate(bs_top = if_else(condition = top <= 20, true = 1, false = 0)) %>%
    mutate(bs_cec = if_else(condition = cec >= 25, true = 1, false = 0)) %>%
    mutate(bs_bsat = if_else(condition = bsat >= 50, true = 1, false = 0)) %>%
    mutate(blacksoil1 = if_else(condition = bs_oc == 1 & bs_cromah == 1 & bs_valueh == 1 & bs_values == 1 & bs_top == 1 $bs_cec == 1 & bsat == 1, true = 1, false = 0)) %>%
    View()
  
  
  
  
  
datbs1 <- datbs2 %>%
  mutate(bs_cec = if_else(condition = cec >= 25, true = 1, false = 0)) %>%
  mutate(bs_bsat = if_else(condition = bsat >= 50, true = 1, false = 0)) %>%
  mutate(blacksoil1 = if_else(condition = balcksoil == 1 & bs_cec == 1 & bs_bsat == 1, true = 1, false = 0)) %>%
  View()
  print(datbs1)
length(datbs1)
attributes(datbs1)
 


#subset borrar luego. me sirve a mi para plotear, aunque no me está funcionando con los campos generados con 
# el if_else

bs2 <- subset(datbs2, blacksoil2 == 1) 
bs1 <- subset(dat, oc >= 1.2 & top >= 25  & chroma_humedo <= 3 & value_humedo <= 3)



print(datbs2)
length(datbs2)
attributes(datbs2)

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = datbs2, aes(x = x, y = y), size = 2, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-75, -53), ylim = c(-56, -20), expand = FALSE)



