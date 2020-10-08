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

# Change species name
colnames(mat)[colnames(mat) == "mmer"] <- "mmar" 

##############################
#  Remove problematic sites  #
##############################

station_to_rm <- c()

# Rm sampling with the same loc:
tmp <- c(302:315, 94, 353)
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

# List to suppr based on double record
double_site_to_rm <- c(
  300, #301
  162, 163, #Eaux saumatre
  93, #94
  326, #327
  228:232, #233
  211, #210
  351, #352
  350, #349
  208, #209
  201, #202
  199, #200
  195, #196
  193, #194
  186 #185
)
station_to_rm <- c(station_to_rm, double_site_to_rm)

# Filter station to rm
mat %<>% 
  filter(!site %in% station_to_rm)
caract_station %<>%
  filter(!site %in% station_to_rm)

# Replicates to concatenate
rep_to_cat <- list(
  c(91,92),
  c(88, 89),
  c(84:86),
  c(284:286),
  c(97:98),
  c(213:215),
  c(216:218),
  c(219, 220),
  c(318, 319),
  c(342, 343),
  c(156:161),
  c(197, 198)
)
concatened_site <- map_dfr(rep_to_cat, function (x, .df) {

  x <- as.character(x)

  tmp <- .df %>%
    filter(site %in% x) %>%
    summarise_if(is.integer, sum) %>%
    mutate_if(is.integer, ~ifelse(. != 0, as.integer(1), as.integer(0)))

  # Keep the first site as reference 
  tmp$site <- x[1]

  return(tmp)

}, .df = mat)

# Suppr site to be concatenated and add the concatenated sites 
mat %<>%
  filter(! site %in% as.character(unlist(rep_to_cat))) %>%
  rbind(., concatened_site) %>%
  arrange(as.character(site))

caract_station %<>% #refilter site
  filter(site %in% mat$site)

mysave(station_to_rm, rep_to_cat, caract_station, mat, dir = mypath("data"), overwrite = TRUE)
write_csv2(x = select(mat, site, everything()),
  path = get_mypath("data-raw", "cleaned_matrix.csv")
)

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

###########
#  BASIN  #
###########

#shape_name <- "BassinDCE.shp"
#if (!shape_name %in% list.files(path = get_mypath("data-raw", "bassin_dce"))) {

  #dest_zip <- get_mypath("data-raw", "bassin_dce", "basin.zip")
  #curl::curl_download("https://www.data.gouv.fr/fr/datasets/r/566ebfc8-3722-49df-ab65-ac151169ed5e", destfile = dest_zip)
  #unzip(zipfile = dest_zip, exdir = get_mypath("data-raw", "bassin_dce"))
  #file.remove(dest_zip)
#}

#library(sf)
#basin <- read_sf(get_mypath("data-raw", "bassin_dce", shape_name))


