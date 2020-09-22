library(tidyverse)
library(readxl)
library(magrittr)
source(file = "R/misc.R")

data_path <- "data-raw/MatrixDef.xlsx"
excel_sheets(data_path)

mat <- read_excel(data_path, sheet = "MatrixSpp")  
colnames(mat) %<>% tolower()
caract_station <- read_excel(data_path, sheet = "MatrixCaract")  
colnames(caract_station) %<>% tolower()
station <- caract_station
station_bivalves <- caract_station
mysave(station, station_bivalves, dir = get_mypath("data"), overwrite = TRUE)

library(sf)
myload(region_polygon, envir = environment(), dir = get_mypath("data"))
station_bivalves %<>% 
  sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 2154)
plot(region_polygon)
ggplot(st_geometry(region_polygon))+
  geom_sf()+
  geom_sf(data = st_geometry(station_bivalves))

richness <- tibble(
  site_nb = mat$site_nb, 
  site = str_extract(site_nb, "[0-9]{1,3}"),
  richness = rowSums(mat[, 2:ncol(mat)])
)

#######################
#  Richness analysis  #
#######################

myload(station_bivalves_stream_caract, envir = environment(),
  dir = get_mypath("data"))

caract_station <- station_bivalves_stream_caract %>%
  left_join(select(caract_station, site_nb, latitude, longitude),
    by = "site_nb") %>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}"))
# Problème de match
unique(richness$site_nb)
unique(richness$site)
unique(caract_station$site_nb)
which(caract_station$site_nb %in% richness$site_nb)
pb <- map_lgl(caract_station$site, ~any(richness$site == .x))

caract_station$site[!pb]
richness$site[!pb]

rich_data <- richness %>%
  left_join(caract_station, by = "site")
summary(rich_data)
# Il manque 57 sites dans les données environmentales  

rich_data %>%
  pivot_longer(cols = c(latitude, longitude), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = richness)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x")

rich_data %>%
  pivot_longer(cols = c(upDist, rang, avSloA, avAltA, H2OAreaA, afv_area), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = richness)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales = "free_x")


rich_rang <- ggplot(filter(rich_data, rang != 0), aes(y = richness, x = as.factor(rang))) +
  geom_boxplot()
rich_rang



#################
#  Cooccurence  #
#################

library(cooccur)
library(beepr)

# Get species in row and sites in column 
species_names <- colnames(mat)[!colnames(mat) %in% "site_nb"] 
site_names <- mat$site_nb 

t_mat <- as.data.frame(t(mat[,-1]))
colnames(t_mat) <- site_names 
rownames(t_mat) <- species_names 

# Virer les sites vides
#t_mat <- t_mat[, colSums(t_mat) > 0]



rowSums(t_mat)
rowMeans(t_mat)
colSums(t_mat) %>% hist 


data("finches")
str(finches)
rownames(finches)
cooccur.finches <- cooccur(mat = finches, type = "spp_site",
  thresh = TRUE, spp_names = TRUE)
class(cooccur.finches)
plot(cooccur.finches)


test <- t_mat %>%
  mutate_all(.funs = as.integer)
rownames(test) <- species_names 

## Co-occurence pair
cooc_bivalve <- cooccur(mat = test, type = "spp_site",
 thresh = TRUE, spp_names = TRUE)
beep("fanfare")

## Co-occurence pair between species 
p <- plot(cooc_bivalve)
p

## Association profile by species
p2 <- pair.profile(cooc_bivalve)
p2

ggsave(filename = "fig/co-occurrence_bivalves.pdf", plot = p)
ggsave(filename = "fig/association_profile_bivalves.pdf", plot = p2)

######################
#  Checkboard score  #
######################

library(EcoSimR)


myModel <- cooc_null_model(speciesData=t_mat,suppressProg=TRUE)
summary(myModel)

plot(myModel,type = "hist")
plot(myModel,type = "cooc")
