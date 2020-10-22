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

colnames(mat)[colnames(mat) == "acomp"] <- "ecomp"

##################
#  Spatial data  #
##################

myload(caract_station, station_bivalves_stream_caract,
  envir = environment(), dir = mypath("data"))

library(sf)
myload(region_polygon, the_8_hydrologic_basin, envir = environment(), dir = get_mypath("data"))
sf_station <- st_as_sf(caract_station, coords = c("longitude", "latitude"), crs = 4326)

sf_station %<>%
  mutate(in_france = lengths(st_within(sf_station, region_polygon)))
sf_station %>%
  filter(in_france != 1)
outside_france_site <- sf_station[sf_station$in_france == 0, ]$site 

mysave(caract_station, mat, dir = mypath("data"), overwrite = TRUE)



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
#first_matrix_path <- "MatrixDef.xlsx"
#first_matrix_path <- "../data-raw/MatrixDef.xlsx"
previous_mat <- read_excel(first_matrix_path, sheet = "MatrixSpp")
current_mat <- read_excel(data_path, sheet = "Matrix_Alain")
colnames(previous_mat) %<>% tolower()
previous_mat %<>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}")) %>%
  dplyr::select(-site_nb)
colnames(current_mat) %<>% tolower()
current_mat %<>% rename(site = site_nb...1) %>%
  mutate(site = as.character(site))

to_test <- filter(current_mat, site %in% previous_mat$site) %>%
  dplyr::select(site, latitude, longitude) %>%
  arrange(as.integer(site))
env_to_test <- caract_station %>%
  filter(site %in% to_test$site) %>%
  arrange(as.integer(site))
nrow(to_test); nrow(env_to_test)
which(!to_test$site %in% env_to_test$site)
all(to_test[-which(!to_test$site %in% env_to_test$site), ]$latitude == env_to_test$latitude)
all(to_test[-which(!to_test$site %in% env_to_test$site), ]$longitude == env_to_test$longitude)


########################
#  Species attributes  #
########################


myload(mat, envir = environment(), dir = mypath("data"))

big_spp <- c("aana", "acyg", "aexu", "paur", "mmar", "plit", "pcom", "swoo",
  "ucra", "uma", "upic", "utum") 
little_spp <- colnames(mat)[!colnames(mat) %in% c(big_spp, "site")] 

# Check
sp_tot <- colnames(mat)[-which(colnames(mat) == "site")] 

length(sp_tot) == length(big_spp) + length(little_spp)
#good

# Invasive species
invasive_sp <- c("cflum", "swoo", "dros", "dpol", "ecom", "stra")
invasive_sp[which(! invasive_sp %in% sp_tot)] # GOOD

# Put all in tibbles
species_attributes <- tibble(
  species = c(big_spp, little_spp),
  ) %>%
  mutate(
    size_group = ifelse(species %in% little_spp, "little", "big"),
    invasive = ifelse(species %in% invasive_sp, TRUE, FALSE),
    invasive_status = ifelse(species %in% c("swoo", "dros", "ecomp"), "ongoing", "done")
  )

mysave(species_attributes, dir = get_mypath("data"), overwrite = TRUE)
