#Neyman allocation of the sample:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx"))


data<- read.table(file = here("data","survey","population information","04362 Alders- og kjonnsfordeling for grunnkretsenes befolkning 2024.txt"),
                    header = T,sep = "\t",fileEncoding = "UTF-8",encoding = "UTF-8") %>% 
  mutate(grunnkrets_kode = str_extract(pattern = "[[:digit:]]+",region))


postnummer<- read.xlsx(xlsxFile = here("data","survey","population information","admin_unit_recoder.xlsx"),sheet = 1)



# alder omkoding ----------------------------------------------------------

aldergruppe<- unique(data$alder)

data_alderomkodert<- data %>% 
  mutate(ny_aldergruppe = case_match(alder,
                                     c(aldergruppe[1],aldergruppe[2])~ "20-29 ar",
                                     c(aldergruppe[3],aldergruppe[4])~ "30-59 ar",
                                     c(aldergruppe[5],aldergruppe[6],aldergruppe[7],aldergruppe[8])~ "60+ ar"))


pop_omkodert_data<- left_join(data_alderomkodert,postnummer, by = "grunnkrets_kode") %>% 
  mutate(ny_poststed = poststed) %>% 
  mutate(ny_poststed = case_when(ny_poststed == "FEOY" ~ "HAVIK",
                                 ny_poststed == "KVALAVAG" ~ "HAVIK",
                                 ny_poststed == "SANDVE" ~ "SKUDENESHAVN",
                                 ny_poststed == "STOL" ~ "SKUDENESHAVN",
                                 ny_poststed == "ROYKSUND" ~ "KARMSUND",
                                 ny_poststed == "VORMEDAL" ~ "KARMSUND",
                                 ny_poststed == "SAEVELANDSVIK" ~ "VEAVAGEN",.default = ny_poststed)) %>% 
  rename(omrade = ny_poststed) %>% 
  group_by(omrade) %>% 
  mutate(ny_postkoder = paste(unique(postnummer),collapse = ", ")) %>% 
  mutate(gamle_postsone = paste(unique(poststed),collapse = ", ")) %>% 
  select(-all_of(c("admin_sone","postnummer","poststed")))
  
pop_omkodert_strata<- pop_omkodert_data %>% 
  filter(omrade != "9999") %>% 
  group_by(ny_aldergruppe,kjonn,omrade,ny_postkoder,gamle_postsone) %>% 
  summarise(antall = sum(X04362..Befolkning.)) %>% 
  ungroup() %>% 
  group_by(omrade,ny_postkoder) %>% 
  mutate(poststed_befolkning = sum(antall)) %>% 
  ungroup() %>% 
  mutate(total_population = sum(antall)) %>% 
  mutate(postkode_share = round(poststed_befolkning/total_population,2)) %>% 
  mutate(innenpostkode_share = round(antall/poststed_befolkning,2)) %>% 
  mutate(postkode_samplesize = round(postkode_share*400)) %>% 
  mutate(innenpostkode_samplesize = round(innenpostkode_share*postkode_samplesize)) %>% 
  ungroup()

write.xlsx(x = pop_omkodert_strata,file = here("data","survey","population information","27062024_3alder_gruppe_proportional_stratified_sample_size.xlsx"))
