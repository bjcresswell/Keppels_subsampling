
library(RCurl)
library(readxl)
library(tidyverse)



keppels3.Juv <- read_xlsx('../0_data/Plec_Sample data_Keppels_ KI3.2.xlsx', sheet = 2) %>% 
  #filter(LIFE_STAGE != "Adult") %>%                                                   # Don't need this as we are only loading Juv worksheet
  #filter(TL <=250) %>%                                                                # Leave TL filter until after coalesce below
  dplyr::select(ID, INDIVID_ID, DATE, REGION, REEF, LIFE_STAGE, FL, TL) %>% 
  mutate(DATE = as.Date(DATE),
         FL = as.numeric(FL),
         TL = as.numeric(TL)) %>%  
  mutate_if(is.character, as.factor)


