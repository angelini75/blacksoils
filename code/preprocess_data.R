# Filter data
# 

library(tidyverse)

dat <- read_csv("data/perfiles-2020-09-11.csv")
names(dat)
dat %>% 
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
  )
