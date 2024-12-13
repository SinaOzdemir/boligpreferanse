
library(here)
library(tidyverse)
library(openxlsx)
library(patchwork)

gk_befolkning<- read.table(file = here("data","forhold data","befolkning_grunnkrets_2001_2024.txt"),
                           header = T,
                           sep = "\t",
                           encoding = "UTF-8",
                           col.names = c("variabel","region","kjonn","alder","ar","befolkning")) %>% 
  filter(ar !="2001") %>% 
  mutate(region_kode = str_extract_all(string = region, pattern = "[[:digit:]]+",simplify = T)[,1]) %>% 
  mutate(region = trimws(str_remove_all(string = region, pattern = "[[:digit:]]+"))) %>% 
  group_by(region,region_kode,ar,alder) %>% 
  summarise(antall = sum(befolkning))

alder_grupper<- unique(gk_befolkning$alder)


gk_befolkning<- gk_befolkning %>% 
  mutate(ny_aldergruppe = case_match(alder,
                                     c(alder_grupper[1],alder_grupper[2],alder_grupper[7])~"0-19 ar",
                                     c(alder_grupper[3],alder_grupper[4])~"20-29 ar",
                                     c(alder_grupper[5],alder_grupper[6]) ~"30-60 ar",
                                     c(alder_grupper[8],alder_grupper[9],alder_grupper[10],alder_grupper[11]) ~ "60+ ar")) %>% 
  group_by(region,region_kode,ar,ny_aldergruppe) %>% 
  summarise(antall = sum(antall))


admin_unit<- read.xlsx(xlsxFile = here("data","forhold data","admin_unit_recoder.xlsx"),sheet = 1) %>% 
  mutate(admin_sone = paste0("Sone ", admin_sone))

gk_befolkning<- left_join(gk_befolkning,admin_unit,by = c("region_kode" = "grunnkrets_kode"))


as_graphic<- gk_befolkning %>% 
  group_by(ar,admin_sone) %>% 
  summarise(befolkning = sum(antall)) %>% 
  arrange(admin_sone,ar) %>% 
  group_by(admin_sone) %>% 
  mutate(roc_perc = round((befolkning - lag(befolkning))/lag(befolkning)*100,2)) %>% 
  mutate(roc_count = (befolkning - lag(befolkning)))

admin_sone_graph<-as_graphic %>%
  mutate(ar = as.character(ar)) %>% 
  ggplot(aes(x= ar,y=befolkning,group = as.character(admin_sone)))+
  geom_smooth(aes(linetype = as.character(admin_sone),color = as.character(admin_sone)),se=F,show.legend = T)+
  theme_classic()+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 45,hjust = 1))+
  guides(color = guide_legend(title = "Adm. Soner"),linetype = "none")+
  labs(x = "Ar", y = "Befolkning", subtitle = "Befolkningforendring i det siste 13 år etter admin soner")

annotated_admin_gr<-admin_sone_graph+
  annotate(geom = "text",x = "2024",y = max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 1",]$befolkning)+100,
           label =max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 1",]$befolkning))+
  annotate(geom = "text",x = "2024",y = max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 2",]$befolkning)+100,
           label =max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 2",]$befolkning))+
  annotate(geom = "text",x = "2024",y = max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 3",]$befolkning)+100,
           label =max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 3",]$befolkning))+
  annotate(geom = "text",x = "2024",y = max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 4",]$befolkning)+100,
           label =max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 4",]$befolkning))+
  annotate(geom = "text",x = "2024",y = max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 5",]$befolkning)+100,
           label =max(as_graphic[as_graphic$ar==2024 & as_graphic$admin_sone=="Sone 5",]$befolkning))


befolknin_ra<- as_graphic %>% 
  filter(ar != 2011) %>% 
  arrange(admin_sone,ar) %>% 
  group_by(admin_sone) %>% 
  mutate(subgroup_id = rep(1:3,each = 4)) %>% 
  group_by(subgroup_id) %>% 
  mutate(year_range = paste0(min(ar),"-",max(ar)))


fem_ar_avg <- befolknin_ra %>% 
  drop_na(roc_perc,roc_count) %>% 
  group_by(admin_sone,year_range) %>% 
  summarise(avg_change_perc = round(mean(roc_perc,rm.na=T),2),
            avg_change_count = round(mean(roc_count,rm.na=T))) %>% 
  mutate(labels = paste0(avg_change_count,"\n","(",avg_change_perc,"%",")"))


fem_ar_rate_graph<- fem_ar_avg %>%
  ggplot(aes(x = admin_sone, y = year_range))+
  geom_tile(aes(fill = avg_change_perc),show.legend = F)+
  geom_text(aes(label = labels),color = "white")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_continuous(low = "steelblue", high = "darkblue")+
  labs(x = "Adminsoner", y = "Ar",subtitle = "3-år gjennomsnitlige forendringer")



annotated_admin_gr+fem_ar_rate_graph
