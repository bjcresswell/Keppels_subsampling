# Script to load notebook and subsampling data and bind together

library(readxl)
library(tidyverse)
library(arsenal)

# Housekeeping
rm(list=ls())
getwd()


# Load sub-sampling records:

# Adults
# Fin samples (these come from the occasional biopsy at fin-body intersection and also from dead adults)
ad_fins <- read_xlsx('0_data/fin_samples.xlsx') %>% 
  dplyr::select(INDIVID_ID, PLATE_ID,  WELL_ID, TISSUE) %>% 
  mutate_if(is.character, as.factor)

# Scale samples (quite a few of these)
ad_scale <- read_xlsx('0_data/scale_samples.xlsx') %>% 
  dplyr::select(INDIVID_ID, PLATE_ID,  WELL_ID, TISSUE) %>% 
  mutate_if(is.character, as.factor)

# Muscle tissue samples (these are the majority unsurprisingly)
ad_tissue <- read_xlsx('0_data/tissue_samples.xlsx') %>% 
  dplyr::select(INDIVID_ID, PLATE_ID,  WELL_ID, TISSUE) %>% 
  mutate_if(is.character, as.factor)

# Combine together for all adult samples - 813 in total
ad_samples <- ad_fins %>% 
  bind_rows(ad_scale, ad_tissue)

# Juvies
## ALl juvenile samples are fin clips - 1013 in total
juv_samples <- read_xlsx('0_data/juv_samples.xlsx') %>% 
  dplyr::select(INDIVID_ID, PLATE_ID,  WELL_ID, TISSUE) %>% 
  mutate_if(is.character, as.factor)


# Finally the empty slots
empties <- read_xlsx('0_data/Empty slots.xlsx', skip = 1) %>% 
  mutate(INDIVID_ID = "Empty",
         PLATE_ID = `Tray/plate`,  
         WELL_ID = `Empty slot`, 
         TISSUE = "Nil",
         .keep = "none") %>% 
  mutate_if(is.character, as.factor)

# And combine
well_plates <- 
  bind_rows(ad_samples, juv_samples, empties)


# Wrangle in notebook raw data

Juv_KI3.3 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.3.xlsx', sheet = 2) %>% 
  dplyr::select(INDIVID_ID, DATE, REEF, SITE, TISSUE, LIFE_STAGE, TL) %>% 
  mutate(DATE = as.Date(DATE),
         TL = as.numeric(TL)) %>%  
  rename(TISSUE_TYPE = TISSUE) %>% 
  mutate_if(is.character, as.factor)

Ad_KI3.3 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.3.xlsx', sheet = 1) %>% 
  dplyr::select(INDIVID_ID, DATE, REEF, SITE, TISSUE, LIFE_STAGE, TL) %>% 
  mutate(DATE = as.Date(DATE),
         TL = as.numeric(TL)) %>%  
  rename(TISSUE_TYPE = TISSUE) %>% 
  mutate_if(is.character, as.factor)

# Combine
notebook <- 
  bind_rows(Juv_KI3.3, Ad_KI3.3)


# Check for any differences 

# What does well_plates contain that notebook doesn't? -> only the empty slots
sample_check1 <- 
  anti_join(well_plates, notebook)

# What does notebook contain that well_plates doesn't?
sample_check2 <- 
  anti_join(notebook, well_plates)

# 3 individs:
# P.ad_5113 is a duplicate of P.ad_5111
# P.ad_5568 and P.ad_5569 were muddled up in processing and thrown out


# Combine together for master list
subsample_main <- 
  merge(notebook, well_plates, by = 'INDIVID_ID', all = TRUE) %>% 
  mutate(WP_ID = paste(PLATE_ID, WELL_ID)) %>% 
  arrange(WP_ID)


# Checks

subsample_main %>% 
  distinct(PLATE_ID)

subsample_main %>% 
  filter(is.na(PLATE_ID))




