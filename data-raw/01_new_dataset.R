################################################################################
#                                 New dataset                                  #
################################################################################

# New dataset sent by Vincent, eliminating wrong sites (poor amplication
# quality) or concatenate replicates of same site.

library(tidyverse)
library(readxl)
library(magrittr)
source(file = "../R/misc.R")
mypath <- rprojroot::find_package_root_file

#data_path <- "../data-raw/Matrix_spp3-1.xlsx"
data_path <- "Matrix_spp3-1.xlsx"
excel_sheets(data_path)


# Presence/absence data
mat <- read_excel(data_path, sheet = "Matrix_Alain")  
colnames(mat) %<>% tolower()

mat %<>% rename(site = site_nb...1) %>%
  mutate(site = as.character(site))

mat %<>%
  dplyr::select(site, aana:utum)
# Correct for values sup to 1 
mat %<>%
  mutate_if(is.double, ~ifelse(is.na(.), 0, .)) %>%
  mutate_if(is.double, ~ifelse(. >= 1, 1, 0)) %>%
  mutate_if(is.double, as.integer)

##################
#  Spatial data  #
##################
library(sf)
myload(region_polygon, the_8_hydrologic_basin, envir = environment(), dir = get_mypath("data"))
sf_station <- st_as_sf(caract_station, coords = c("longitude", "latitude"), crs = 4326)

sf_station %<>%
  mutate(in_france = lengths(st_within(sf_station, region_polygon)))
sf_station %>%
  filter(in_france != 1)
outside_france_site <- sf_station[sf_station$in_france == 0, ]$site 

mysave(caract_station, mat, dir = mypath("data"), overwrite = TRUE)


myload(caract_station, station_bivalves_stream_caract,
  envir = environment(), dir = mypath("data"))

## Add openstars data
station_bivalves_stream_caract %<>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}"))
station_analysis <- caract_station %>%
  dplyr::select(site, latitude, longitude) %>%
  left_join(station_bivalves_stream_caract,
    by = "site")


sf_station <- st_as_sf(station_analysis, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 2154)
mask_basin <- st_intersects(x = sf_station, y = the_8_hydrologic_basin)
mask_basin <- map_int(mask_basin, ~ifelse(any(.x), .x, NA))
station_analysis %<>%
  mutate(basin = map_chr(mask_basin, ~the_8_hydrologic_basin$NomDistric[.x]))

mysave(station_analysis, dir = mypath("data"), overwrite = TRUE)

#####################
#  testing site id  #
#####################
previous_mat <- read_excel(first_matrix_path, sheet = "MatrixSpp")
colnames(previous_mat) %<>% tolower()
previous_mat %<>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}")) %>%
  dplyr::select(-site_nb)

to_test <- filter(mat, site %in% previous_mat$site) %>%
  dplyr::select(site, latitude, longitude) %>%
  arrange(as.integer(site))
env_to_test <- caract_station %>%
  filter(site %in% to_test$site) %>%
  arrange(as.integer(site))
nrow(to_test); nrow(env_to_test)
which(!to_test$site %in% env_to_test$site)
to_test[-157, ]$latitude == env_to_test$latitude
to_test[-157, ]$longitude == env_to_test$longitude


