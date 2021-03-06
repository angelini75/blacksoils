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
  filter(top <= 25)

d %>% ggplot(aes(x = cec)) + geom_histogram()

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

#### rescatar datos de bases
#### 
dat <- read_csv("data/perfiles-2020-09-18.csv")
names(dat)
d <- dat %>%
  group_by(id) %>% 
  transmute(idh = id,
            idp = perfil_id,
            top = profundidad_superior,
            cec = analitico_t,
            ca = analitico_base_ca,
            mg = analitico_base_mg,
            k = analitico_base_k,
            na = analitico_base_na,
            bsum_sisinta = analitico_s,
            bsum_calculada = sum(across(ca:na), na.rm = TRUE),
            bsat = analitico_saturacion_t,
            bsum_diff = bsum_sisinta - bsum_calculada
  ) %>%
  filter(top < 25) %>% 
  ungroup() %>% 
  select(-id)
d %>% pivot_longer(cec:na) %>% 
  ggplot(aes(y = value)) + facet_wrap("name", scales = "free") + geom_boxplot()

# el resultado (d) lo tiene que revisar Darío
write_csv(d, "data/datos_de_saturacion.csv")


#ACÁ ARRANCA EL SCRIPT
rm(list = ls())
library(tidyverse)

# CARGAR DATOS EDITADOS POR DARÍO
# 
sat <- read_delim("data/datos_saturacion_revisados_3_nov_2020.csv", delim = "\t")

sat %>% pivot_longer(cec:na) %>% 
  ggplot(aes(y = value)) + facet_wrap("name", scales = "free") + geom_boxplot()


sat <- sat %>% group_by(idh) %>% 
  mutate(bsat2 = if_else(condition = !is.na(cec) & !is.na(ca) &
                           !is.na(mg) & !is.na(k) & !is.na(na),
                         true = ((ca+mg+k+na)/cec)*100,
                         false = bsat)) %>%
  mutate(bsat = if_else(condition = bsat > 5000,
                         true = bsat2,
                         false = bsat)) %>% 
  mutate(bsat = if_else(condition = is.na(ca) &
                           is.na(mg) & !is.na(k) & !is.na(na),
                         true = 100,
                         false = bsat)) %>% 
  mutate(bsat = if_else(condition = is.na(bsat) & !is.na(bsat2) & bsat2 < 125,
                         true = bsat2,
                         false = bsat))
sat <- sat %>% 
  transmute(bsat = if_else(bsat>100, 100, bsat)) %>% 
  filter(bsat>=1)

library(tidyverse)
dat <- read_csv("data/datos_revisados_26_sep_20.csv") %>% 
  select(-bsum, -bsat) %>% 
  mutate(cec = cec/1000) %>% 
  left_join(sat) %>% 
  unique()

# Cuento número de filas o registros
length(unique(dat$idh)) # numero de horizontes
length(unique(dat$idp)) # numero de perfiles


# Selecciono aquellos con registros(horizontes) con co > 1.2. los cuento
# de 14680 quedan 2592
# oc <- dat[dat$oc > 1.2,]
# View(oc)
# dat_nperfiles <- distinct(oc, idp)
# count(dat_nperfiles)


# creación del campo blacksoil2, de aquellos que cumplen las condiciones
# para ser Black Soils Segunda Categoría

x <- dat %>%
    filter(top<25) %>% 
    mutate(bs_oc = if_else(condition = oc >= 1.2, true = 1, false = 0),
           bs_cromah = if_else(condition = chroma_humedo <= 3, true = 1, false = 0),
           bs_valueh = if_else(condition = value_humedo <= 3, true = 1, false = 0),
           bs_bot = if_else(condition = bot >= 25, true = 1, false = 0),
           bs_bsat = if_else(condition = bsat >= 50, true = 1, false = 0),
           bs_cec = if_else(condition = cec >= 25, true = 1, false = 0)) %>%
  #mutate(bs_values = if_else(condition = value_seco <= 5, true = 1, false = 0)) %>%
  
    select(idh:bot, bs_oc:bs_cec)

x <- x %>% 
  mutate(bs2 = if_else(bs_oc == 1 & 
                           bs_cromah == 1 & 
                           bs_valueh == 1,
                         true = 1, 
                         false = 0),
         bs1 = if_else(bs_oc == 1 & 
                         bs_cromah == 1 & 
                         bs_valueh == 1 &
                         bs_cec == 1 &
                         bs_bsat == 1,
                       true = 1, 
                       false = 0)) 
y <- x %>% 
  group_by(idp, x, y) %>% 
  summarise(x = first(x),
            y = first(y),
            n = n(),
            condbs2 = sum(bs2)/n,
            condbs1 = sum(bs1)/n,
            bottom = sum(bs_bot)) %>% 
  mutate(bs2 = if_else(condbs2 == 1 & bottom == 1, 1, 0),
         bs1 = if_else(condbs1 == 1 & bottom == 1, 1, 0)) %>% 
  select(-condbs1, -condbs2, -bottom, -n) %>%
  ungroup() %>%  
  group_by(idp, x, y) %>%
  summarize(z = ifelse(bs1 == 1,1,
                             ifelse(bs2 == 1 & bs1 == 0,2,
                                    ifelse(bs1==0 & bs2==0, 0, NA)))) %>% 
# select(-bs1, -bs2) %>%
  ungroup() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) 
y$z <- as.factor(y$z)
# bs21 <- datbs2 %>% 
#   group_by(idp, x, y) %>% 
#   summarise(x = first(x),
#             y = first(y),
#             n = n(),
#             cond = sum(cond1)/n,
#             bottom = sum(bs_bot)) %>% 
#   mutate(bs2 = if_else(cond == 1 & bottom == 1, 1, 0)) %>% 
#   select(-cond, -bottom) %>%
#   ungroup() %>% 
#   na.omit() %>% 
#   filter(bs2 == 1) %>%
#   sf::st_as_sf(coords = c("x", "y"), crs = 4326) 
# 
# bs2 <- datbs2 %>% 
#   group_by(idp, x, y) %>% 
#   summarise(x = first(x),
#             y = first(y),
#             n = n(),
#             cond = sum(cond1)/n,
#             bottom = sum(bs_bot)) %>% 
#   mutate(bs2 = if_else(cond == 1 & bottom == 1, 1, 0)) %>% 
#   select(-cond, -bottom) %>%
#   ungroup() %>% 
#   na.omit() %>% 
#   # filter(bs2 == 1) %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 4326) 

library(maps)
library(sf)
arg  <-  sf::st_as_sf(map('world', plot = FALSE, fill = TRUE)) %>% filter(ID == "Argentina")

ggplot() + geom_sf(data = arg) + geom_sf(data = y, )
y %>% #filter(z==1 |z==2) %>% 
ggplot() + geom_sf(data = arg) + geom_sf( aes(color = z), size = 0.7, alpha = 0.3)


x %>% 
  group_by(idp, x, y) %>% 
  summarise(x = first(x),
            y = first(y),
            n = n(),
            condbs2 = sum(bs2)/n,
            condbs1 = sum(bs1)/n,
            bottom = sum(bs_bot)) %>% 
  mutate(bs2 = if_else(condbs2 == 1 & bottom == 1, 1, 0),
         bs1 = if_else(condbs1 == 1 & bottom == 1, 1, 0)) %>% 
  select(-condbs1, -condbs2, -bottom, -n) %>%
  ungroup() %>%  
  group_by(idp, x, y) %>%
  summarize(z = ifelse(bs1 == 1,1,
                       ifelse(bs2 == 1 & bs1 == 0,2,
                              ifelse(bs1==0 & bs2==0, 0, NA)))) %>% 
write_csv("data/perfiles_para_modelado.csv")
# Despues de esta operación hay que dejar _sólo_ una fila por perfil. Actualmente tiene más de una.
# 1ro. eliminar todos los horizontes que tengan top > 25. Esos no entran en el análisis.
# 2do. evaluar si algunos de los horizontes de cada perfil tiene blacksoil2 == 0. Si sí,
#      el perfil no es BS, si nó, es BS.
#      Pista: Usar función group_by y summarise. Estan en el capítulo 5 que te envié (creo)
#      Además tené en cuenta las Cheat Sheets de RStudio https://rstudio.com/resources/cheatsheets/
#      Acá la de dplyr https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf



# Me quedo solo con aquellos horizontes con top menor a 25 (es la 2 Cat, no la 1).
# y el bottom no debería ser > a 20 ????
datbs2_f  <-  filter(datbs2, top < 25)  
count(datbs2_f)
View(datbs2_f)

# Con la función group_by evaluamos si alguno de los horizontes del perfil no es blacksoil 
# (blacksoil2 == 0). Si hay alguno, el perfil no es blacksoil, si nó, es BS 
# codigo de abajo: si bs2 no es == 1, ese perfil no es BS 

evalua_BS2 <- datbs2_f %>% 
  group_by(idp) %>% 
  summarise(n = n(), # calcula cuantos elementos hay dentro de cada grupo (idp)
            x = first(x), # toma el primer elemento de un grupo
            y = first(y), # idem
            s = sum(blacksoil2), # suma los valores del campo blacksoil2 dentro de cada grupo (idp)
            bs2 = if_else(s == n, 1, 0)) # condicion, si n == s (todos los horizontes cumplen
                                         # la condicion de BS) entonces el perfil es BS (1)
                                         # si no, no (0).
# veamos como queda
library(sf) # para convertir a objeto espacial
d <- st_as_sf(evalua_BS2, coords = c("x", "y"), crs = 4326) # definimos coordenadas y 
                                                            # sistema de referencia
library(mapview) # para mapas interactivos
d %>% 
  filter(bs2 == 1) %>% # nos quedamos sólo con los perfiles que son BS (sólo para visualizar)
  mapview(zcol = "bs2", cex = 1, color = "red") # mostrar en el mapa

# Selecciono aquellos con registros(horizontes) bs2 == 1
BlackSoils2 <- evalua_BS2[evalua_BS2$bs2 == 1,]
View(BlackSoils2)
count(BlackSoils2)

# Eliminar filas con nulos en una columna concreta, en este caso bs2
BlackSoils2_sinNA <- BlackSoils2[!is.na(BlackSoils2$bs2),]
View(BlackSoils2_sinNA)
count(BlackSoils2_sinNA)

nrow(BlackSoils2_sinNA[duplicated(BlackSoils2_sinNA),])

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
# y el bottom no debería ser > a 25???
datbs1_f  <-  filter(datbs1, top < 25 & bot > 25)  
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
  geom_point(data = BS1, aes(x = x, y = y), size = 2,
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-75, -53), ylim = c(-56, -20), expand = FALSE)

# Si solo desea mantener las nuevas variables, use transmute ()
#bs2_3cols <- transmute (bs2,y ,x , blacksoil2)
#View(bs2_3cols)


# exportar tabla

#write_csv(bs2_3cols, "bs2_3cols.csv")

