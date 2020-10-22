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
rm(list = ls())
library(tidyverse)
library(dplyr)

dat <- read_csv("data/datos_revisados_26_sep_20.csv")
# Cuento número de filas o registros
count(dat)
# Cuento número de perfiles (mismo idp)
dat_nperfiles <- distinct(dat, idp)
count(dat_nperfiles)

# Selecciono aquellos con registros(horizontes) con co > 1.2. los cuento
# de 14680 quedan 2592
oc <- dat[dat$oc > 1.2,]
View(oc)

dat_nperfiles <- distinct(oc, idp)
count(dat_nperfiles)

# creación del campo blacksoil2, de aquellos que cumplen las condiciones
# para ser Black Soils Segunda Categoría

(datbs2 <- dat %>%
    mutate(bs_oc = if_else(condition = oc >= 1.2, true = 1, false = 0)) %>%
    mutate(bs_cromah = if_else(condition = chroma_humedo <= 3, true = 1, false = 0)) %>%
    mutate(bs_valueh = if_else(condition = value_humedo <= 3, true = 1, false = 0)) %>%
    mutate(bs_values = if_else(condition = value_seco <= 5, true = 1, false = 0)) %>%
    mutate(bs_top = if_else(condition = top <= 25, true = 1, false = 0)) %>%
    mutate(blacksoil2 = if_else(condition = bs_oc == 1 & bs_cromah == 1 & bs_valueh == 1 & bs_top == 1, true = 1, false = 0))) %>%
  View()


# Despues de esta operación hay que dejar _sólo_ una fila por perfil. Actualmente tiene más de una.
# 1ro. eliminar todos los horizontes que tengan top > 25. Esos no entran en el análisis.
# 2do. evaluar si algunos de los horizontes de cada perfil tiene blacksoil2 == 0. Si sí,
#      el perfil no es BS, si nó, es BS.
#      Pista: Usar función group_by y summarise. Estan en el capítulo 5 que te envié (creo)
#      Además tené en cuenta las Cheat Sheets de RStudio https://rstudio.com/resources/cheatsheets/
#      Acá la de dplyr https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf



# Me quedo solo con aquellos horizontes con top menor a 20 (es la 2 Cat, no la 1).
datbs2_f  <-  filter(datbs2, top < 25)  
count(datbs2_f)
View(datbs2_f)

# Con la función group_by evaluamos si alguno de los horizontes del perfil no es blacksoil 
# (blacksoil2 == 0). Si hay alguno, el perfil no es blacksoil, si nó, es BS 
# codigo de abajo: si bs2 no es == 1, ese perfil no es BS 

evalua_BS2 <- datbs2_f %>% group_by(idp) %>% summarise(n = n(),
                                                       x = x,
                                                       y = y,
                                       s = sum(blacksoil2),
                                       bs2 = mean(blacksoil2))

View(evalua_BS2)

# Selecciono aquellos con registros(horizontes) bs2 == 1
BlackSoils2 <- evalua_BS2[evalua_BS2$bs2 == 1,]
View(BlackSoils2)
Count(BlackSoils2)

# Eliminar filas con nulos en una columna concreta, en este caso bs2
BlackSoils2_sinNA <- BlackSoils2[!is.na(BlackSoils2$bs2),]
View(BlackSoils2_sinNA)
count(BlackSoils2_sinNA)

# borro filas duplicadas. dejo solo una por perfil
BS2 <- distinct(BlackSoils2_sinNA, idp, x, y, bs2)
View(BS2)
count(BS2)                                       


# creacioón del campo blacksoil1, de aquellos que cumplen las condiciones para ser Black Soils 1 Categoría.


(datbs1 <- dat %>%
    mutate(bs_oc = if_else(condition = oc >= 1.2, true = 1, false = 0)) %>%
    mutate(bs_cromah = if_else(condition = chroma_humedo <= 3, true = 1, false = 0)) %>%
    mutate(bs_valueh = if_else(condition = value_humedo <= 3, true = 1, false = 0)) %>%
    mutate(bs_values = if_else(condition = value_seco <= 5, true = 1, false = 0)) %>%
    mutate(bs_top = if_else(condition = top <= 25, true = 1, false = 0)) %>%
    mutate(bs_cec = if_else(condition = cec >= 25, true = 1, false = 0)) %>%
    mutate(bs_bsat = if_else(condition = bsat >= 50, true = 1, false = 0)) %>%
    mutate(blacksoil1 = if_else(condition = bs_oc == 1 & bs_cromah == 1 & bs_valueh == 1 & bs_top == 1 & bs_cec == 1 & bs_bsat == 1, true = 1, false = 0))) %>%
  View()


# Me quedo solo con aquellos horizontes con top menor a 25.
datbs1_f  <-  filter(datbs1, top < 25)  
count(datbs1_f)
View(datbs1_f)

# Con la función group_by evaluamos si alguno de los horizontes del perfil no es blacksoil 
# (blacksoil1 == 0). Si hay alguno, el perfil no es blacksoil, si nó, es BS 
# codigo de abajo: si bs2 no es == 1, ese perfil no es BS 

evalua_BS1 <- datbs1_f %>% group_by(idp) %>% summarise(n = n(),
                                                       x = x,
                                                       y = y,
                                                       s = sum(blacksoil1),
                                                       bs1 = mean(blacksoil1))

View(evalua_BS1)

# Selecciono aquellos con registros(horizontes) bs1 == 1
BlackSoils1 <- evalua_BS1[evalua_BS1$bs1 == 1,]
View(BlackSoils1)
count(BlackSoils1)

# Eliminar filas con nulos en una columna concreta, en este caso bs1
BlackSoils1_sinNA <- BlackSoils1[!is.na(BlackSoils1$bs1),]
View(BlackSoils1_sinNA)
count(BlackSoils1_sinNA)

# borro filas duplicadas. dejo solo una por perfil
BS1 <- distinct(BlackSoils1_sinNA, idp, x, y, bs1)
View(BS1)
count(BS1)                                       



library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = BS2, aes(x = x, y = y), size = 2,
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-75, -53), ylim = c(-56, -20), expand = FALSE)

# Si solo desea mantener las nuevas variables, use transmute ()
#bs2_3cols <- transmute (bs2,y ,x , blacksoil2)
#View(bs2_3cols)


# exportar tabla

#write_csv(bs2_3cols, "bs2_3cols.csv")

