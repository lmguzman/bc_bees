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
library(taxize)
library(purrr)
library(lubridate)

tbl_PlantSpecies <-read.csv("raw_data/tbl_PlantSpecies.csv")

tblLocation<-read.csv("raw_data/tblLocation.csv")

tblMainBee <-read.csv("raw_data/tblMainBee.csv")

tblNames_Species <-read.csv("raw_data/tblNames_Species.csv")

##### fixing locations coordinates ######

origin_dd <- tblLocation %>% 
  dplyr::select(New.LocID, DecimalLat, DecimalLon) %>% 
  extract(DecimalLat, into = "decimal_lat", regex = "(\\d+\\.\\d+)") %>% 
  extract(DecimalLon, into = "decimal_lon", regex = "(\\-\\d+\\.\\d+)") %>% 
  filter(!is.na(decimal_lat)) %>% 
  dplyr::mutate(decimal_lat = as.numeric(decimal_lat), decimal_lon = as.numeric(decimal_lon))

degree_to_decimal <- tblLocation %>%
  dplyr::select(New.LocID, Degree.Lat, Degree.Lon) %>% 
  filter(!(New.LocID %in% origin_dd$New.LocID)) %>% 
  extract(Degree.Lat, into = "deg_lat", regex = "(\\d+)", remove = FALSE) %>% 
  extract(Degree.Lat, into = "min_lat", regex = "(\\°\\s*\\d+)", remove = FALSE) %>% 
  extract(min_lat, into = "min_lat", regex = "(\\d+)") %>% 
  extract(Degree.Lat, into = "sec_lat", regex = "([\\'|\\’]\\s*\\d+\\.*\\d+)") %>% 
  extract(sec_lat, into = "sec_lat", regex = "(\\d+\\.*\\d+)") %>% 
  dplyr::mutate(deg_min_sec_lat = paste(deg_lat, min_lat, sec_lat)) %>% 
  dplyr::mutate(decimal_lat = conv_unit(deg_min_sec_lat, from = 'deg_min_sec', to = 'dec_deg')) %>% 
  extract(Degree.Lon, into = "deg_lon", regex = "(\\d+)", remove = FALSE) %>% 
  extract(Degree.Lon, into = "min_lon", regex = "(\\°\\s*\\d+)", remove = FALSE) %>% 
  extract(min_lon, into = "min_lon", regex = "(\\d+)") %>% 
  extract(Degree.Lon, into = "sec_lon", regex = "([\\'|\\’]\\s*\\d+\\.*\\d+)") %>% 
  extract(sec_lon, into = "sec_lon", regex = "(\\d+\\.*\\d+)") %>% 
  dplyr::mutate(deg_min_sec_lon = paste(deg_lon, min_lon, sec_lon)) %>% 
  dplyr::mutate(decimal_lon = conv_unit(deg_min_sec_lon, from = 'deg_min_sec', to = 'dec_deg')) %>% 
  dplyr::select(New.LocID, decimal_lat, decimal_lon) %>% 
  filter(!is.na(decimal_lat)) %>% 
  dplyr::mutate(decimal_lat = as.numeric(decimal_lat), decimal_lon = -1*as.numeric(decimal_lon)) 

utm_orgin <- tblLocation %>%
  dplyr::select(New.LocID, UTMEW, UTMNS) %>% 
  filter(!is.na(UTMEW))

utm_tab <- tblLocation %>% 
  dplyr::select(UTMEW, UTMNS) %>% 
  filter(!is.na(UTMEW)) %>% 
  dplyr::mutate(UTMEW = as.numeric(UTMEW)) %>% 
  dplyr::mutate(UTMNS = as.numeric(UTMNS)) %>% 
  dplyr::rename(X = UTMEW, Y = UTMNS)

attr(utm_tab, "zone") <- 10
attr(utm_tab, "projection") <- "UTM"
utm_to_dec <- convUL(utm_tab, km=FALSE, southern=NULL)

utm_to_decimal_final <- cbind(utm_orgin, utm_to_dec) %>% 
  dplyr::rename(decimal_lat = Y, decimal_lon = X) %>% 
  dplyr::select(New.LocID, decimal_lat, decimal_lon) %>% 
  dplyr::mutate(decimal_lat = as.numeric(decimal_lat), decimal_lon = as.numeric(decimal_lon)) 

final_locations <- bind_rows(origin_dd, utm_to_decimal_final, degree_to_decimal)

##### locations#######


#### plant bee table

plant_bee_table <- tblMainBee %>%
  dplyr::select(BeeID, New.LocID = NewLocID, Yr0, SpID, PlantSpeciesID = FlorNum) %>% 
  filter(!is.na(PlantSpeciesID)) %>% 
  filter(!is.na(BeeID)) %>% 
  left_join(dplyr::select(tblNames_Species, SpID, GenusName, Species, BeeGenusID)) %>% 
  filter(!is.na(BeeGenusID)) %>% 
  left_join(tbl_PlantSpecies) %>% 
  left_join(final_locations) %>% 
  left_join(dplyr::select(tblLocation, New.LocID, Location_Desc))

write.csv(plant_bee_table, "data/site_net.csv", row.names = FALSE)

########### location names #######

db <- read.csv("data/site_net.csv", stringsAsFactors = FALSE)

regions <- read.csv("data/new_regions.csv", stringsAsFactors = FALSE)

db_lo <- db %>% 
  left_join(regions) %>% 
  filter(!region == "HEL") %>% 
  dplyr::mutate(ecosection_nm = ifelse(ecosection_nm == "Southern Okanogan Basin", "Southern Okanagan Basin", ecosection_nm))

write.csv(db_lo, "data/site_net_locs.csv", row.names = FALSE)


################ making sure it is species ########

## filter species that have less than 1 obsevation for plants and less than 5 observations for bees

db <- read.csv("data/site_net_locs.csv", stringsAsFactors = FALSE)

bee_traits <- read.csv("raw_data/elle_insects.csv", stringsAsFactors = FALSE)

plant_traits <- read.csv("raw_data/elle_plants_full/elle_plants_full-Table 1.csv", stringsAsFactors = FALSE) %>% 
  filter(!plant_sp == "Incidental Collection") %>% 
  filter(!plant_sp == "Miscellaneous white flower") %>% 
  filter(!plant_sp == "Mixed species") %>% 
  filter(!str_detect(plant_sp, "Nest")) %>% 
  filter(!plant_sp == "net") %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Spirea douglasii ssp. Douglasii", "Spiraea douglasii", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Symphoricarpus albus", "Symphoricarpos albus", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Syringia", "Syringa", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Alyssum", "Lobularia", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Convulvulus arvensis", "Convolvulus arvensis", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Craetagus douglasii", "Crataegus douglasii", plant_sp)) 

bee_names <- read.csv("raw_data/common_names_pol.csv", stringsAsFactors = FALSE)
colnames(bee_names) <- c("Genus", "Common.name")

plant_names <- read.csv("raw_data/common_names_plants.csv", stringsAsFactors = FALSE)
colnames(plant_names) <- c("plant_sp", "common.name")

db_int <- db %>% 
  filter(!(Species == "")) %>% 
  filter(!str_detect(Species, "sp.")) %>% 
  unite(col = "bee_sp", GenusName, Species, sep = " ") %>% 
  dplyr::rename(plant_sp = Species.Name) %>% 
  filter(!bee_sp == "Hemiptera misc.") %>% 
  filter(!plant_sp == "Incidental Collection") %>% 
  filter(!plant_sp == "Miscellaneous white flower") %>% 
  filter(!plant_sp == "Mixed species") %>% 
  filter(!str_detect(plant_sp, "Nest")) %>% 
  filter(!plant_sp == "net") %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Spirea douglasii ssp. Douglasii", "Spiraea douglasii", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Symphoricarpus albus", "Symphoricarpos albus", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Syringia", "Syringa", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Alyssum", "Lobularia", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Convulvulus arvensis", "Convolvulus arvensis", plant_sp)) %>% 
  dplyr::mutate(plant_sp = ifelse(plant_sp == "Craetagus douglasii", "Crataegus douglasii", plant_sp)) 

bees_great <- db_int %>% 
  group_by(bee_sp) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >5)

bee_name_clean <- bees_great %>% 
  dplyr::mutate(bee_gen = str_extract(bee_sp, "\\w+")) %>% 
  left_join(bee_names, by = c("bee_gen" = "Genus")) %>% 
  left_join(dplyr::select(bee_traits, insect_sp, insect_order, social, nest_location, larval_diet, insect_guild),
            by = c("bee_sp" = "insect_sp")) %>% 
  dplyr::select(-n, -bee_gen) %>% 
  dplyr::rename(bee_common = Common.name, bee_order = insect_order, bee_social = social, bee_nest_location = nest_location,
         bee_diet = larval_diet, bee_guild = insect_guild)

plant_great <- db_int %>% 
  group_by(plant_sp) %>% 
  dplyr::summarise(n = n()) %>% 
  filter(n >5)

plant_name_clean <- plant_great %>% 
  dplyr::mutate(plant_gen = str_extract(plant_sp, "\\w+")) %>% 
  left_join(plant_names, by = c("plant_gen" = "plant_sp")) %>% 
  left_join(plant_traits, by = c("plant_sp")) %>%
  dplyr::select(plant_sp, plant_common = common.name, plant_order, plant_family, plant_native = native_inv, 
         plant_life_form = Life.form) 


db2 <- db_int %>% 
  filter(bee_sp %in% bees_great$bee_sp) %>% 
  filter(plant_sp %in% plant_great$plant_sp) %>% 
  left_join(bee_name_clean) %>% 
  left_join(plant_name_clean) %>% 
  dplyr::select(-BeeID, -SpID, -PlantSpeciesID, -BeeGenusID, -Species.CODE) %>% 
  dplyr::mutate(plant_sp = str_replace(plant_sp, " ", "\n"), bee_sp = str_replace(bee_sp, " ", "\n")) %>% 
  dplyr::mutate(plant_native = ifelse(plant_native == "native*", "non-native", plant_native)) %>% 
  dplyr::mutate(plant_native =ifelse(is.na(plant_native), 'both', plant_native)) %>% 
  dplyr::mutate(plant_life_form = str_to_sentence(plant_life_form)) %>% 
  dplyr::mutate(plant_life_form = ifelse(plant_life_form == "Woody vine", "Vine", plant_life_form)) %>% 
  dplyr::mutate(plant_life_form = ifelse(plant_life_form == "Varies", "Various", plant_life_form)) %>% 
  dplyr::mutate(plant_life_form = ifelse(plant_life_form == "Shrub or tree", "Shrub", plant_life_form)) %>% 
  dplyr::mutate(plant_life_form = ifelse(plant_life_form == "Herb to shrub", "Shrub", plant_life_form)) %>% 
  dplyr::mutate(plant_life_form =ifelse(is.na(plant_life_form), 'Herb', plant_life_form))

db2 <- db2 %>%
  dplyr::mutate(bee_guild = case_when(bee_common == "Sand wasps" ~ "otherhym",
                                      TRUE ~  as.character(bee_guild))) %>%
  dplyr::mutate(bee_guild_otro = case_when(bee_guild == "bombyliidae" ~ "Flower flies", 
                                           bee_guild == "andrenidae" ~ "Mining bees",
                                           bee_guild == "syrphidae" ~ "Flower flies",
                                           bee_guild == "otherfly" ~ "Flies",
                                           bee_common == "Bumble bees" ~ "Bumble bees",
                                           bee_guild == "apidae" & bee_common != "Bumble bees" & bee_common != "Honey bee"~ "Other bees",
                                           bee_common == "Honey bee" ~ "Honey bees",
                                           bee_guild == "megachilidae" ~ "Mason & Leafcutter bees",
                                           bee_guild == "halictidae" ~ "Sweat bees",
                                           bee_guild == "colletidae" ~ "Other bees",
                                           bee_guild == "lepidoptera" ~ "Moths & Butterflies",
                                           bee_guild == "coleoptera" ~ "Beetles",
                                           bee_guild == "otherhym" ~ "Wasps",
                                           bee_guild == "aves" ~ "Birds",
                                           TRUE ~ "Uncommon visitors")) %>%
  dplyr::mutate(bee_guild_otro = factor(bee_guild_otro, levels = c("Honey bees", "Bumble bees", "Mason & Leafcutter bees", "Mining bees",
                                                                   "Sweat bees", "Other bees", "Flower flies", "Flies", "Wasps", "Beetles", "Moths & Butterflies", "Birds")))

write.csv(db2, "data/site_net_loc_fil.csv", row.names = FALSE)

### adding web links ###

db3 <- read.csv("data/site_net_loc_fil.csv")

bee_links <- read.csv("raw_data/pollinator_app - unique_bee.csv") %>% 
  dplyr::select(bee_sp, bee_wiki = Wikipedia, bee_other_web = Canada_bee, bee_wiki_common = Wikipedia_common, bee_other_web_common = Canada_bee_common)

plant_links <- read.csv("raw_data/pollinator_app - unique_plant.csv") %>% 
  dplyr::select(plant_sp, plant_wiki = Wikipedia, plant_other_web = pacific.north.west.consortium, plant_wiki_common = Wikipedia_common, plant_other_web_common = pacific.north.west.consortium_common)


db4 <- db3 %>% 
  left_join(bee_links) %>% 
  left_join(plant_links)

write.csv(db4, "data/site_net_loc_fil_links.csv", row.names = FALSE)

##### add blooming times  ###

db3 <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

plant_bee_table <- tblMainBee %>% 
  dplyr::select(BeeID, New.LocID = NewLocID,Day0, Mon0, Yr0, SpID, PlantSpeciesID = FlorNum) %>% 
  filter(!is.na(PlantSpeciesID)) %>% 
  filter(!is.na(BeeID)) %>% 
  left_join(dplyr::select(tblNames_Species, SpID, GenusName, Species, BeeGenusID)) %>% 
  filter(!is.na(BeeGenusID)) %>% 
  left_join(tbl_PlantSpecies) %>% 
  dplyr::select(New.LocID, Day0, Mon0,  Yr0, GenusName, Species, Species.Name) %>% 
  unite(col = "bee_sp", GenusName, Species, sep = "\n") %>% 
  dplyr::rename(plant_sp = Species.Name)
  
all_flying_times <- plant_bee_table %>% 
  dplyr::select(Day0, Mon0, Yr0, bee_sp) %>% 
  dplyr::mutate(date = paste(Day0, Mon0, Yr0, sep = "-")) %>% 
  dplyr::mutate(date = dmy(date)) %>% 
  dplyr::mutate(month = month(date), week = week(date)) %>%
  dplyr::group_by(bee_sp) %>% 
  dplyr::summarise(min = round(quantile(week, 0.1)), max = round(quantile(week, 0.9))) %>% 
  filter(bee_sp %in% unique(db3$bee_sp)) %>% 
  split(.$bee_sp) %>% 
  map(~seq(.x$min, .x$max, 1))

all_flowering_times <- plant_bee_table %>% 
  dplyr::select(Day0, Mon0, Yr0, plant_sp) %>% 
  dplyr::mutate(date = paste(Day0, Mon0, Yr0, sep = "-")) %>% 
  dplyr::mutate(date = dmy(date)) %>% 
  dplyr::mutate(month = month(date), week = week(date)) %>% 
  dplyr::group_by(plant_sp) %>% 
  dplyr::summarise(min = round(quantile(week, 0.1)), max = round(quantile(week, 0.9))) %>% 
  dplyr::mutate(plant_sp = str_replace(plant_sp, " ", "\n")) %>% 
  filter(plant_sp %in% unique(db3$plant_sp)) %>% 
  split(.$plant_sp) %>% 
  map(~seq(.x$min, .x$max, 1))

saveRDS(all_flying_times, "data/all_flying_times.rds")

saveRDS(all_flowering_times, "data/all_flowering_times.rds")

### For Sarah ## 

db2 <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

db2 %>% 
  head()

unique_bee <- db2 %>% 
  dplyr::select(bee_sp, bee_common) %>% 
  unique() %>% 
  arrange(bee_sp)

write.csv(unique_bee, "data/unique_bee.csv", row.names = FALSE)

unique_plant <- db2 %>% 
  dplyr::select(plant_sp, plant_common) %>% 
  unique() %>% 
  arrange(plant_sp)

write.csv(unique_plant, "data/unique_plant.csv", row.names = FALSE)


## For elizabeth ##

# db3 <- read.csv("data/site_net_loc_fil.csv")
# 
# plant_links <- read.csv("raw_data/pollinator_app - unique_plant.csv") %>% 
#   dplyr::select(plant_sp, Invasive_non_invasive) 
# 
# 
# native_invasive <- db3 %>% 
#   dplyr::select(plant_sp, plant_native) %>% 
#   distinct() %>% 
#   left_join(plant_links) %>% 
#   mutate(plant_sp = str_replace(plant_sp, "\n", " "))
# 
# write.csv(native_invasive, "Screen_shots_Lora/native_invasive.csv", row.names = FALSE)
# 
