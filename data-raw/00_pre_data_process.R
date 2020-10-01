######################
#  Pre-process data  #
######################

library(tidyverse)
library(readxl)
library(magrittr)
source(file = "../R/misc.R")
mypath <- rprojroot::find_package_root_file

data_path <- "MatrixDef.xlsx"
excel_sheets(data_path)


# Presence/absence data
mat <- read_excel(data_path, sheet = "MatrixSpp")  
colnames(mat) %<>% tolower()

# Correct for values sup to 1 
mat %<>%
  mutate_if(is.double, ~ifelse(. >= 1, 1, 0)) %>%
  mutate_if(is.double, as.integer)

# Get the good station id 
mat %<>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}")) %>%
  select(-site_nb)

caract_station <- read_excel(data_path, sheet = "MatrixCaract")
colnames(caract_station) %<>% tolower()
caract_station %<>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}")) %>%
  select(-site_nb)


##############################
#  Remove problematic sites  #
##############################

station_to_rm <- c()

# Rm sampling with the same loc:
tmp <- 302:315
caract_station %>%
  filter(site %in% tmp)
# Vincent recommended to keep the 312 based on the ADN amplification performance
same_samp_site <- caract_station[caract_station$site %in% tmp, ]$site
station_to_rm <- same_samp_site[-which(same_samp_site == "312")] 
rm(tmp)

# Rm samplings that did not work well
## 324 does not exist, why? To ask to Vincent 
tmp <- c(347, 348, 323, 324)
caract_station %>%
  filter(site %in% tmp)
station_to_rm <- c(station_to_rm, tmp)
rm(tmp)

# Rm sampling outside streams 
unique(caract_station$type)
caract_station %>%
  filter(type == "PlanEau")
pond_site <- caract_station[caract_station$type == "PlanEau", ]$site 
station_to_rm <- c(station_to_rm, pond_site) 

# Rm sampling outside France 
library(sf)
myload(region_polygon, the_8_hydrologic_basin, envir = environment(), dir = get_mypath("data"))
sf_station <- st_as_sf(caract_station, coords = c("longitude", "latitude"), crs = 4326)

sf_station %<>%
  mutate(in_france = lengths(st_within(sf_station, region_polygon)))
sf_station %>%
  filter(in_france != 1)
outside_france_site <- sf_station[sf_station$in_france == 0, ]$site 
station_to_rm <- c(station_to_rm, outside_france_site)

# Filter station to rm
mat %<>% 
  filter(!site %in% station_to_rm)
caract_station %<>%
  filter(!site %in% station_to_rm)
mysave(station_to_rm, caract_station, mat, dir = mypath("data"), overwrite = TRUE)

####################################
#  Adding environmental variables  #
####################################
myload(caract_station, station_bivalves_stream_caract,
  envir = environment(), dir = mypath("data"))

## Add openstars data
station_bivalves_stream_caract %<>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}"))
station_analysis <- caract_station %>%
  select(site, latitude, longitude) %>%
  left_join(station_bivalves_stream_caract,
    by = "site")


sf_station <- st_as_sf(station_analysis, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 2154)
mask_basin <- st_intersects(x = sf_station, y = the_8_hydrologic_basin)
mask_basin <- map_int(mask_basin, ~ifelse(any(.x), .x, NA))
station_analysis %<>%
  mutate(basin = map_chr(mask_basin, ~the_8_hydrologic_basin$NomDistric[.x]))

mysave(station_analysis, dir = mypath("data"), overwrite = TRUE)

########################################
#  List big species and invasive ones  #
########################################

data_path <- "MatrixDef.xlsx"
excel_sheets(data_path)

mat_big <- read_excel(data_path, sheet = "MatrixGrossesSpp")  
colnames(mat_big) %<>% tolower()
mat_little <- read_excel(data_path, sheet = "Matrix PetitesSpp")
colnames(mat_little) %<>% tolower()

big_spp <- colnames(mat_big)[-which(colnames(mat_big) == "site_nb")]
little_spp <- colnames(mat_little)[-which(colnames(mat_little) == "site_nb")]

# Check
myload(mat, envir = environment(), dir = mypath("data"))
sp_tot <- colnames(mat)[-which(colnames(mat) == "site")] 

length(sp_tot) == length(big_spp) + length(little_spp)
#good

# Invasive species
invasive_sp <- c("cflum", "swoo", "dros", "dpol", "pcomp", "stroum")
invasive_sp[which(! invasive_sp %in% sp_tot)]

# Put all in tibbles
species_attributes <- tibble(
  species = c(big_spp, little_spp),
  ) %>%
  mutate(
    size_group = ifelse(species %in% little_spp, "little", "big"),
    invasive = ifelse(species %in% invasive_sp, TRUE, FALSE),
    invasive_status = ifelse(species %in% c("swoo", "dpol", "pcomp"), "ongoing", "done")
  )

mysave(species_attributes, dir = get_mypath("data"))
