library(tidyverse)
library(readxl)
library(magrittr)
source(file = "R/misc.R")

data_path <- "data-raw/MatrixDef.xlsx"
excel_sheets(data_path)

# Presence/absence
mat <- read_excel(data_path, sheet = "MatrixSpp")  
colnames(mat) %<>% tolower()
mat %<>%
  mutate_if(is.double, ~ifelse(. >= 1, 1, 0)) %>%
  mutate_if(is.double, as.integer)

# Environmental/spatial data 
myload(region_polygon, the_8_hydrologic_basin, envir = environment(), dir = get_mypath("data"))

caract_station <- read_excel(data_path, sheet = "MatrixCaract")
colnames(caract_station) %<>% tolower()
station <- caract_station
station_bivalves <- caract_station
mysave(station, station_bivalves, dir = get_mypath("data"), overwrite = TRUE)
myload(station_bivalves_stream_caract, envir = environment(),
  dir = get_mypath("data"))
caract_station <- select(caract_station, site_nb, latitude, longitude) %>%
  left_join(station_bivalves_stream_caract,
    by = "site_nb") %>%
  mutate(site = str_extract(site_nb, "[0-9]{1,3}"))

library(sf)
station_bivalves %<>% 
  sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 2154)
plot(region_polygon)
ggplot(st_geometry(the_8_hydrologic_basin))+
  geom_sf()+
  geom_sf(data = st_geometry(station_bivalves))

# Add basin to env
mask_basin <- st_intersects(x = station_bivalves, y = the_8_hydrologic_basin)
mask_basin <- map_int(mask_basin, ~ifelse(any(.x), .x, NA))
caract_station %<>%
  mutate(basin = map_chr(mask_basin, ~the_8_hydrologic_basin$NomDistric[.x]))

hist(caract_station$avAltA)
##########
#  NMDS  #
##########

mat_mat <- as.matrix(mat[,-1])
rownames(mat_mat) <- mat[[1]]
colnames(mat_mat) <- colnames(mat)[-1]

hist(rowSums(mat_mat))
summary(colSums(mat_mat))
mat_mat <- mat_mat[rowSums(mat_mat) > 0, colSums(mat_mat) > 10] 

library(vegan)
mynmds <- metaMDS(mat_mat,
distance = "bray",k=2,trymax=100)

plot(mynmds)
ordiplot(mynmds,type="n")
orditorp(mynmds,display="species",col="red",air=0.01)
ordisurf(mynmds,caract_station[rowSums(mat[,-1]) > 0,]$avAltA, main="",col="forestgreen")
orditorp(mynmds,display="species",col="red",air=0.01)
ordisurf(mynmds,caract_station[rowSums(mat[,-1]) > 0,]$upDist, main="",col="forestgreen")
orditorp(mynmds,display="species",col="red",air=0.01)
ordisurf(mynmds,caract_station[rowSums(mat[,-1]) > 0,]$latitude, main="",col="forestgreen")
orditorp(mynmds,display="species",col="red",air=0.01)
ordisurf(mynmds,caract_station[rowSums(mat[,-1]) > 0,]$longitude, main="",col="forestgreen")
orditorp(mynmds,display="species",col="red",air=0.01)
#orditorp(mynmds,display="sites",cex=1.25,air=0.01)
ordiplot(mynmds)
#ordihull(mynmds, groups = caract_station[rowSums(mat[,-1]) > 0,]$substrat, draw="polygon", col="grey90", label= TRUE)
ordiellipse(mynmds, groups = caract_station[rowSums(mat[,-1]) > 0,]$substrat, draw="polygon", col="grey90", label= TRUE)
orditorp(mynmds, display="species", col="red", air=0.01)

ordiplot(mynmds, type = "points")
ordiellipse(mynmds, groups = caract_station[rowSums(mat[,-1]) > 0,]$type, draw="polygon", col="grey90", label= TRUE)
orditorp(mynmds, display="species", col="red", air=0.01)

ordiplot(mynmds, type = "points")
ordiellipse(mynmds, groups = caract_station[rowSums(mat[,-1]) > 0,]$basin, draw="polygon", col="grey90", label= TRUE)
orditorp(mynmds, display="species", col="red", air=0.01)

env <- envfit(mynmds, select(caract_station[rowSums(mat[,-1]) > 0,], longitude:type, upDist, avAltA), permutations = 999, na.rm = TRUE)
env

plot(mynmds)
plot(env)

####################
#  Beta-diversity  #
####################

library(betapart)

core <- betapart.core(mat[,-1])
multi <- beta.multi(core, index.family = "sorensen")
pair <- beta.pair(core, index.family = "sorensen")
pair_mat <- map(pair, as.matrix)
mean_site_beta <- map(pair_mat, rowMeans, na.rm = TRUE)

test <- as.matrix(pair$beta.sim)

#######################
#  Richness analysis  #
#######################

richness <- tibble(
  site_nb = mat$site_nb, 
  site = str_extract(site_nb, "[0-9]{1,3}"),
  richness = rowSums(mat[, 2:ncol(mat)]),
  avg_beta_sim = mean_site_beta$beta.sim,
  avg_beta_sne = mean_site_beta$beta.sne,
  avg_beta_sor = mean_site_beta$beta.sor
)

# Problème de match
unique(richness$site_nb)
unique(richness$site)
unique(caract_station$site_nb)
which(caract_station$site_nb %in% richness$site_nb)
pb <- map_lgl(caract_station$site, ~any(richness$site == .x))

caract_station$site[!pb]
caract_station$site
richness$site[!pb]
richness$site

rich_data <- richness %>%
  left_join(caract_station, by = "site")
summary(rich_data)

rich_data %>%
  pivot_longer(cols = c(latitude, longitude), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = richness)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, scales = "free")

rich_data %>%
  filter(!is.na(basin)) %>%
  pivot_longer(cols = c(upDist, rang, avSloA, avAltA, H2OAreaA, afv_area), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = richness)) +
  geom_point() +
  geom_smooth() +
  facet_grid(basin~variable, scales = "free")

rich_data %>%
  pivot_longer(cols = c(upDist, rang, avAltA), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = richness, color = basin)) +
  geom_point() +
  #geom_smooth() +
  facet_grid(~variable, scales = "free")

rich_rang <- ggplot(filter(rich_data, rang != 0), aes(y = richness, x = as.factor(rang))) +
  geom_boxplot()
rich_rang


##########
#  Beta  #
##########

rich_data %>%
  pivot_longer(cols = c(upDist, rang, avSloA, avAltA, H2OAreaA, afv_area), names_to = "variable", values_to = "value") %>%
  pivot_longer(cols = c(avg_beta_sim, avg_beta_sne, avg_beta_sor), names_to = "beta_type", values_to = "beta") %>%
  ggplot(aes(x = value, y = beta)) +
  geom_point() +
  geom_smooth() +
  facet_grid(beta_type~variable, scales = "free")


#################
#  Cooccurence  #
#################

library(cooccur)
library(beepr)

# Get species in row and sites in column 
species_names <- colnames(mat)[!colnames(mat) %in% "site"] 
site_names <- mat$site 

t_mat <- as.data.frame(t(mat[,-ncol(mat)]))
colnames(t_mat) <- site_names 
rownames(t_mat) <- species_names 

# Virer les sites vides
#t_mat <- t_mat[, colSums(t_mat) > 0]



rowSums(t_mat)
rowMeans(t_mat)
colSums(t_mat) %>% hist 

test <- t_mat %>%
  mutate_all(.funs = as.integer)
rownames(test) <- species_names 

## Co-occurence pair
cooc_bivalve <- cooccur(mat = t_mat, type = "spp_site",
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
