# join soil data with covariates

library(sf)
library(tidyverse)
library(raster)
library(snowfall)

rm(list = ls())

# # set temporal folders
# pid <- Sys.getpid()
# # options for raster package (define working directory)
# rasterOptions(tmpdir=paste0("/run/media/marcos/_home/marcos/tmp/",
#                             # set temp-file directory (it should already exist)
#                             # the directory needs more than 100 GB free
#                             pid),
#               tmptime = 3, # time in hours that temp files will remain
#               progress= "text", 
#               timer=TRUE,
#               overwrite = T, 
#               chunksize=2e8, 
#               maxmemory=1e8)

s <- read_csv("data/perfiles_para_modelado.csv") %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  as_Spatial()

s$z <- as.numeric(s$z)


ls <- list.files("D:/Leo/1-COVARIABLES AMBIENTALES/1 KM Merit", recursive = TRUE, pattern = "\\.sdat$")
?list.files


r <- stack("/home/marcos/Documents/GDB/covs_Arg/all_covariates.tif")
load("/home/marcos/Documents/GDB/covs_Arg/all_covariates_names.RData")
names(r) <- namesCovarAll

# extract in parallel 
# from https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points

# Extract values to a data frame - multicore approach
# First, convert raster stack to list of single raster layers
r.list <- unstack(r)
names(r.list) <- names(r)

# Now, create a R cluster using all the machine cores minus one
sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

# Load the required packages inside the cluster
sfLibrary(raster)
sfLibrary(sp)
# Run parallelized 'extract' function and stop cluster
e.df <- sfSapply(r.list, extract, y=s)
sfStop()

# Fix resulting data frame
s <- st_as_sf(s)
DF <- st_bind_cols(s, e.df) 
DF <- DF %>% st_drop_geometry()
s <- read_csv("data/perfiles_para_modelado.csv")
s <- dplyr::left_join(s,DF)

write_csv(s, "data/soil_&_covariates.csv")



