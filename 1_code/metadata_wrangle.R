library(tidyverse)
library(readxl)
# data wrangle HBH 13/05/2024
# 

# combine into notebook----

## load site data
site_info <- read_csv('0_data/Keppels_Site_metadata.csv') %>% 
  dplyr::select(REEF, SITE, ZONING, LAT, LONG) %>% 
  group_by(REEF, ZONING) %>% 
  #Because multiple sites per reef
  summarise(LAT = round(mean(LAT, na.rm = T),3), 
            LONG = round(mean(LONG, na.rm = T), 3)) %>% 
  mutate(ZONING = str_to_title(ZONING))

## load juveniles
Juv_KI3.4 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.4.xlsx', sheet = 2) %>% 
  dplyr::select(INDIVID_ID, DATE, SPECIES, LIFE_STAGE, REEF, SITE, ZONING, FL, TL) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  left_join(site_info, by = c("REEF", "ZONING")) 

## load adults
Ad_KI3.4 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.4.xlsx', sheet = 1) %>% 
  dplyr::select(INDIVID_ID, DATE, SPECIES, LIFE_STAGE, REEF, SITE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  #convert to mm
  mutate(FL = FL*10,
         TL = TL*10) %>% 
  left_join(site_info, by = c("REEF")) 

### Combine
notebook <- 
  bind_rows(Juv_KI3.4, Ad_KI3.4) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(YEAR = year(DATE), 
         ZONING = factor(ZONING, levels = c("Green", "Blue", "Yellow")))

# Tidy notebook ----

## Check reef names
notebook %>% 
  group_by(REEF, ZONING) %>% 
  summarise() #%>% view

Sample.numbers <- notebook %>% 
  filter(! is.na(LIFE_STAGE)) %>% 
  group_by(LIFE_STAGE, REEF, ZONING, LAT, LONG) %>% 
  summarise(`Sample size` = n()) %>% 
  filter(! is.na(ZONING))

## Check sample distribution
load("2_output/Keppel Island google map.RData")
KI.map +
  geom_point(data = Sample.numbers, 
             aes(y = LAT, x = LONG, size = `Sample size`, col = ZONING), 
             alpha = .8) +
  facet_wrap(~LIFE_STAGE) + 
  coord_equal(ratio = 1, xlim = c(150.87, 151.11), ylim = c(-23.23, -23.02)) +
  scale_colour_manual(values = c("darkgreen","blue","gold"), guide = "none") +
  labs(y = "", x = "") +
  theme_light()
ggsave(filename = "2_output/fig_SampleSize.png", width = 6, height = 3)


## Check sizes
ggplot(notebook, aes(y = TL, x = FL)) +
  geom_point() 

## Check size distribution
ggplot(notebook %>% filter(!is.na(TL), !is.na(ZONING)), 
       aes(x = TL, fill = ZONING)) +
  geom_histogram(bins = 20) +
  facet_grid(ZONING~LIFE_STAGE, scale = "free_y") +
  scale_fill_manual(values = c("darkgreen","blue","gold")) +
  theme_light()
ggsave("2_output/fig_AdultSizeDistributionLIFE_STAGE.png")

ggplot(notebook %>% filter(!is.na(TL), !is.na(ZONING), LIFE_STAGE == "Adult"), 
       aes(x = TL, fill = ZONING)) +
  geom_histogram(bins = 20) +
  facet_wrap(~REEF) +
  scale_fill_manual(values = c("darkgreen","blue","gold")) +
  theme_light() +
  theme(legend.position = "none")
ggsave("2_output/fig_AdultSizeDistributionREEF.png")

ggplot(notebook %>% filter(!is.na(TL), !is.na(ZONING), LIFE_STAGE == "Adult"), 
       aes(x = TL, fill = ZONING)) +
  geom_histogram(bins = 20, position = position_identity(), alpha =.5) +
  #facet_wrap(~ZONING, ncol = 1) +
  scale_fill_manual(values = c("darkgreen","blue","gold")) +
  theme_light() +
  theme(legend.position = "none") + labs(y = "")
ggsave("2_output/fig_AdultSizeDistributionZONING.png")

# All seems good to me

# Merge with subsampling data ----
subsampling_hugo <- read_xlsx("2_output/sample_tracker_Extractions.xlsx", sheet = "sample_tracker") %>%
  dplyr::select(INDIVID_ID, WP_ID) %>% 
  filter(!is.na(WP_ID))

#There's a descrepency between the subsampling data used for extractions and the same tracker from Ben. 
#This is evident in PmAd10, which had additional samples (see lab photo Feb 2nd)
#We'll use Ben's version
subsampling_ben <- read_csv("2_output/sample_tracker.csv") %>%
  dplyr::select(INDIVID_ID, PLATE_ID, WELL_ID, TISSUE) %>% 
  mutate(PLATE_ID = gsub("PMAd21_", "PmAd", PLATE_ID), 
         PLATE_ID = gsub("PMJuv21_", "PmJuv", PLATE_ID), 
         WP_ID = paste(PLATE_ID, WELL_ID, sep = "_")) %>% 
  dplyr::select(INDIVID_ID, WP_ID)

combined4extractions <- read_xlsx("2_output/sample_tracker_Extractions.xlsx", sheet = "Combined4Extractions") %>%
  dplyr::select(WP_ID, Combined_ID)

genotyping_reference <- subsampling_ben %>% full_join(combined4extractions, by = "WP_ID") %>% 
  mutate(PlateID = ifelse(is.na(Combined_ID), WP_ID, Combined_ID)) %>% 
  separate(PlateID, into = c("Plate", "Well"), sep = "_", remove = FALSE) %>% 
  dplyr::select(-WP_ID, -Combined_ID)

Keppels2021.metadata <- notebook %>% full_join(genotyping_reference, by = "INDIVID_ID")

write.csv(Keppels2021.metadata, "2_output/Keppels2021.GenotypingMetadata.csv", quote = FALSE, row.names = FALSE)





 

#install.packages("ggmap")
library(ggmap)
#ggmap::register_google(key = "YOUR API KEY")
KI.map <- ggmap(get_googlemap(center = c(lon = mean(site_info$LONG), lat = mean(site_info$LAT)),
                              zoom = 11, scale = 2, maptype = 'terrain', color = 'color'))
save(KI.map, file = "2_output/Keppel Island google map.RData")  

  
  
