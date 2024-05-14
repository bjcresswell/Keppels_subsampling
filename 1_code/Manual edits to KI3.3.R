library(tidyverse)
# data wrangle HBH 13/05/2024
# The data was too unruly to edit in R, particularly when it came to reef/sites
# The data was manually edited to generate KI3.4

# combine into notebook----

## load juveniles
Juv_KI3.3 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.3.xlsx', sheet = 2) %>% 
  dplyr::select(INDIVID_ID, DATE, SPECIES, LIFE_STAGE, TISSUE, REEF, SITE,  HABITAT, ZONING, TYPE, Ysite, Xsite, Yind, Xind, FL, TL) %>% 
  mutate(DATE = as.Date(DATE)) 

### Checks done:
#### life stage
table(Juv_KI3.3$LIFE_STAGE)
mean(Juv_KI3.3$TL, na.rm = T)
#### Check data are in mm
max(Juv_KI3.3$FL, na.rm = T)
max(Juv_KI3.3$TL, na.rm = T)

#### site
Juv_KI3.3 %>% group_by(REEF, SITE) %>% summarise(n = n()) %>% view

## load adults
Ad_KI3.3 <- read_xlsx('0_data/Plec_Sample data_Keppels_ KI3.3.xlsx', sheet = 1) %>% 
  dplyr::select(INDIVID_ID, DATE, SPECIES, LIFE_STAGE, TISSUE, REEF, SITE,  HABITAT, ZONING, Ysite, Xsite, Yind, Xind, FL, TL) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  mutate(LIFE_STAGE = replace(LIFE_STAGE, LIFE_STAGE == "-", "Adult")) %>% #Assuming if it has a Ad label, it was an adult
  #Fix TL
  mutate(LIFE_STAGE = replace(LIFE_STAGE, TL == "JUV", "Juvenile"), 
         TL = replace(TL, TL == "JUV", NA)) %>% # Checked, can't fix
  mutate(TL = replace(TL, TL == "-", NA),
         TL = replace(TL, TL == "30/56", 56)) %>% #Don't need to worry about FL
  mutate(FL = as.numeric(FL),
         TL = as.numeric(TL)) %>% 
  #convert to mm
  mutate(FL = FL*10,
         TL = TL*10) 
  

### Checks done:
#### life stage
table(Ad_KI3.3$LIFE_STAGE)
#### TL has character values
table(Ad_KI3.3$TL)
#### Double heck these in the book
Ad_KI3.3 %>% filter(TL == "JUV") %>% 
  mutate(LIFE_STAGE = replace(LIFE_STAGE, TL == "JUV", "Juvenile"), 
         TL = replace(TL, TL == "JUV", NA))
Ad_KI3.3 %>% filter(TL == "-")
mean(Ad_KI3.3$TL, na.rm = T)
#Check none are in mm
max(Ad_KI3.3$TL, na.rm = T)
max(Ad_KI3.3$FL, na.rm = T)

### all other check can be done on the combined notebook



### Combine
notebook <- 
  bind_rows(Juv_KI3.3, Ad_KI3.3)


# Tidy notebook ----
#REEF = str_replace(REEF, "EGG", "Egg Rock"),

## Align reef names
notebook %>% 
  #fix REEF names
  mutate(REEF = str_replace(REEF, "Is$", "Island"), 
         REEF = str_replace(REEF, "is$", "Island"), 
         REEF = str_replace(REEF, "Big Peninsular", "Big Peninsula"),
         REEF = str_replace(REEF, "EGG", "Egg Rock"),
         REEF = str_replace(REEF, "Middle Observatory", "Middle Island"),
         REEF = str_replace(REEF, "North Keppel$", "North Keppel Island"),
         REEF = str_replace(REEF, "Halfway$", "Halfway Island"),
         REEF = str_replace(REEF, " Leaward$", ""), 
         REEF = str_replace(REEF, "Monkey$", "Monkey Bay")) %>% 
  # mutate(REEF = gsub("Is", "Island", REEF)) %>% 
  group_by(REEF) %>% 
  summarise() %>% 
  view

notebook %>% 
 # mutate(REEF = gsub("Is", "Island", REEF)) %>% 
  group_by(LIFE_STAGE, REEF) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = LIFE_STAGE, values_from = n) %>% view
  
  
  
  
  
  