
library(readxl)
library(tidyverse)
library(arsenal)

getwd()

Juv_KI3.2 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.2.xlsx', sheet = 2) %>% 
  #dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)


Juv_KI3.2a <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.2a.xlsx', sheet = 2) %>% 
  #dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)

Juv_KI3.3 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.3.xlsx', sheet = 2) %>% 
  #dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)


Ad_KI3.2 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.2.xlsx', sheet = 1) %>% 
  #dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)


Ad_KI3.2a <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.2a.xlsx', sheet = 1) %>% 
  #dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)

Ad_KI3.3 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.3.xlsx', sheet = 1) %>% 
  #dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)


comparedf(Juv_KI3.2, Juv_KI3.2a)
comparedf(Juv_KI3.2, Juv_KI3.3)

comparedf(Ad_KI3.2, Ad_KI3.2a)
comparedf(Ad_KI3.2, Ad_KI3.3)

# All vsns are the same - can just pick one.
