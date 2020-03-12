library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)
library(viridis)
library(measurements)
library(PBSmapping)
library(cowplot)


tbl_PlantSpecies <-read.csv("raw_data/tbl_PlantSpecies.csv")

tblLocation<-read.csv("raw_data/tblLocation.csv")

tblMainBee <-read.csv("raw_data/tblMainBee.csv")

tblNames_Species <-read.csv("raw_data/tblNames_Species.csv")

##### fixing locations coordinates ######

origin_dd <- tblLocation %>% 
  select(New.LocID, DecimalLat, DecimalLon) %>% 
  extract(DecimalLat, into = "decimal_lat", regex = "(\\d+\\.\\d+)") %>% 
  extract(DecimalLon, into = "decimal_lon", regex = "(\\-\\d+\\.\\d+)") %>% 
  filter(!is.na(decimal_lat)) %>% 
  mutate(decimal_lat = as.numeric(decimal_lat), decimal_lon = as.numeric(decimal_lon))

degree_to_decimal <- tblLocation %>%
  select(New.LocID, Degree.Lat, Degree.Lon) %>% 
  filter(!(New.LocID %in% origin_dd$New.LocID)) %>% 
  extract(Degree.Lat, into = "deg_lat", regex = "(\\d+)", remove = FALSE) %>% 
  extract(Degree.Lat, into = "min_lat", regex = "(\\°\\s*\\d+)", remove = FALSE) %>% 
  extract(min_lat, into = "min_lat", regex = "(\\d+)") %>% 
  extract(Degree.Lat, into = "sec_lat", regex = "([\\'|\\’]\\s*\\d+\\.*\\d+)") %>% 
  extract(sec_lat, into = "sec_lat", regex = "(\\d+\\.*\\d+)") %>% 
  mutate(deg_min_sec_lat = paste(deg_lat, min_lat, sec_lat)) %>% 
  mutate(decimal_lat = conv_unit(deg_min_sec_lat, from = 'deg_min_sec', to = 'dec_deg')) %>% 
  extract(Degree.Lon, into = "deg_lon", regex = "(\\d+)", remove = FALSE) %>% 
  extract(Degree.Lon, into = "min_lon", regex = "(\\°\\s*\\d+)", remove = FALSE) %>% 
  extract(min_lon, into = "min_lon", regex = "(\\d+)") %>% 
  extract(Degree.Lon, into = "sec_lon", regex = "([\\'|\\’]\\s*\\d+\\.*\\d+)") %>% 
  extract(sec_lon, into = "sec_lon", regex = "(\\d+\\.*\\d+)") %>% 
  mutate(deg_min_sec_lon = paste(deg_lon, min_lon, sec_lon)) %>% 
  mutate(decimal_lon = conv_unit(deg_min_sec_lon, from = 'deg_min_sec', to = 'dec_deg')) %>% 
  select(New.LocID, decimal_lat, decimal_lon) %>% 
  filter(!is.na(decimal_lat)) %>% 
  mutate(decimal_lat = as.numeric(decimal_lat), decimal_lon = -1*as.numeric(decimal_lon)) 

utm_orgin <- tblLocation %>%
  select(New.LocID, UTMEW, UTMNS) %>% 
  filter(!is.na(UTMEW))

utm_tab <- tblLocation %>% 
  select(UTMEW, UTMNS) %>% 
  filter(!is.na(UTMEW)) %>% 
  mutate(UTMEW = as.numeric(UTMEW)) %>% 
  mutate(UTMNS = as.numeric(UTMNS)) %>% 
  rename(X = UTMEW, Y = UTMNS)

attr(utm_tab, "zone") <- 10
attr(utm_tab, "projection") <- "UTM"
utm_to_dec <- convUL(utm_tab, km=FALSE, southern=NULL)

utm_to_decimal_final <- cbind(utm_orgin, utm_to_dec) %>% 
  rename(decimal_lat = Y, decimal_lon = X) %>% 
  select(New.LocID, decimal_lat, decimal_lon) %>% 
  mutate(decimal_lat = as.numeric(decimal_lat), decimal_lon = as.numeric(decimal_lon)) 

final_locations <- bind_rows(origin_dd, utm_to_decimal_final, degree_to_decimal)

##### locations#######


#### plant bee table

plant_bee_table <- tblMainBee %>%
  select(BeeID, New.LocID = NewLocID, Yr0, SpID, PlantSpeciesID = FlorNum) %>% 
  filter(!is.na(PlantSpeciesID)) %>% 
  filter(!is.na(BeeID)) %>% 
  left_join(select(tblNames_Species, SpID, GenusName, Species, BeeGenusID)) %>% 
  filter(!is.na(BeeGenusID)) %>% 
  left_join(tbl_PlantSpecies) %>% 
  left_join(final_locations) %>% 
  left_join(select(tblLocation, New.LocID, Location_Desc))

write.csv(plant_bee_table, "data/site_net.csv", row.names = FALSE)

########### location names #######

db <- read.csv("data/site_net.csv", stringsAsFactors = FALSE)

locs <- unique(db$Location_Desc)

head(db)
dplyr::filter(db, Location_Desc == "Jefferson Farm, nr. Jefferson")

library(stringr)


locs[str_detect(locs, "Victoria|Saanich|Metchosin|Oak Bay|Esquimalt|Cowichan|Mesachie|Tzuhalem")] <- "Vancouver Island"

locs[str_detect(locs, "Vancouver|North Vancouver")] <- "Vancouver"

locs[str_detect(locs, "Richmond|Delta|Bates Farm")] <- "Richmond/Delta"

locs[str_detect(locs, "Abbotsford|Langley|Maple Ridge|Pitt Meadows|Coquitlam|Surrey")] <- "Lower Mainland"

locs[str_detect(locs, "Bella Bella")] <- "Campbell Island"

locs[str_detect(locs, "Haynes|White Lake|Mt. Oliver|Kennedy Bench|Okanagan")] <- "Okanagan"

locs[str_detect(locs, "Rocky Prairie|Table Rock|Lacamas Prairie|Eugene|Finley|Sutherlin|Stayton|Camas|Mima|North Bank|Jefferson")] <- "US"

locs[str_detect(locs, "Tsawwassen")] <- "Tsawwassen"

str_detect(locs, "Bella Bella")

db_lo <- db %>% 
  left_join(data.frame(locs, Location_Desc= unique(db$Location_Desc)))

write.csv(db_lo, "data/site_net_locs.csv", row.names = FALSE)


################ making sure it is species ########

## filter species that have less than 1 obsevation for plants and less than 5 observations for bees

db <- read.csv("data/site_net_locs.csv")

bee_traits <- read.csv("raw_data/elle_insects.csv")

unique(bee_traits$insect_guild)

plant_traits <- read.csv("raw_data/elle_plants_full/elle_plants_full-Table 1.csv")

bee_names <- read.csv("raw_data/common_names_pol.csv")

db_int <- db %>% 
  filter(!(Species == "")) %>% 
  filter(!str_detect(Species, "sp.")) %>% 
  unite(col = "bee_sp", GenusName, Species, sep = " ") %>% 
  rename(plant_sp = Species.Name) %>% 
  filter(!bee_sp == "Hemiptera misc.")

db_int %>% 
  filter(str_detect(bee_sp, "Stelis"))

bees_great <- db_int %>% 
  group_by(bee_sp) %>% 
  summarise(n = n()) %>% 
  filter(n >5)

unique(str_extract(bees_great$bee_sp, "\\w+")) %>% View()

bee_common <- sci2comm(unique(str_extract(bees_great$bee_sp, "\\w+")))

plant_great <- db_int %>% 
  group_by(plant_sp) %>% 
  summarise(n = n()) %>% 
  filter(n >1)

library(taxize)




#db2 
#  mutate(plant_sp = str_replace(plant_sp, " ", "\n"), bee_sp = str_replace(bee_sp, " ", "\n")) 
  


write.csv(db2, "data/site_net_loc_fil.csv", row.names = FALSE)

#########cleaning up names #####

library(purrr)

db <- read.csv("data/site_net_loc_fil.csv")

sps_loc <- db %>%  
  mutate(bee_sp = str_replace(bee_sp, "\n", " "), plant_sp = str_replace(plant_sp, "\n", " "))




