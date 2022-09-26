library(tidyverse)
library(lubridate)

covoit <- readRDS("cours_04/data/covoiturage_filtre_idf.rda")
summary(covoit)
table(covoit$journey_end_country)
table(covoit$journey_start_country)
simplifie<- covoit %>%
  select(-has_incentive, -passenger_seats)

simplifie<-simplifie %>%
  mutate(JourDeLaSemaine= wday(journey_start_date, label=TRUE, abbr=FALSE, week_start=1))

qplot(simplifie$journey_end_time)
qplot(simplifie$journey_end_time, bins=60)
qplot(simplifie$journey_start_date)

aires <- read_csv("cours_03/data/bnlc.csv")

# Importation des départements et des régions
LibDep<-read_csv("cours_04/data/TouslesLibellesDesDepartements.txt", col_types = cols(reg = col_character()))

LibReg<-read_csv("cours_04/data/TouslesLibellesDesRegions.txt", col_types = cols(reg = col_character()))

simplifie_avec_jointure <- simplifie%>%
  left_join(LibDep %>%
              select(dep, reg), by=c("journey_end_department"="dep") ) %>%
  left_join(LibReg%>%
              select(reg, libelle), by=c("reg"="reg"))



# Région de destination sur l'ensemble du jeu de données
Destinations_Totales <- simplifie_avec_jointure %>%
  filter(reg!=11)%>%
  group_by(reg, libelle) %>%
  summarise(Nombre_Trajets = n())%>%
  arrange(desc(Nombre_Trajets))%>%
  ungroup()%>%
  mutate(Part_Trajets= Nombre_Trajets/sum(Nombre_Trajets)*100)
  
Destinations_Totales%>%
  mutate(libelle=fct_reorder(.f = libelle,
                             .x = Part_Trajets,
                             .fun = "median",
                             .desc = FALSE))%>%
  ggplot()+
  geom_col(aes(x=Part_Trajets, y =libelle))

####

library(sf) 

#Carte
# formats de données géographiques
shp 
geojson /json
gpkg

#####
# importation du geojson
Regions_Polygones<-st_read("cours_04/data/regions.geojson")

Carte_Destinations<-Destinations_Totales %>%
  left_join(Regions_Polygones, by=c("reg"="code")) %>%
  st_as_sf()

summary(Destinations_Totales$Part_Trajets)
CarteAExporter<-Carte_Destinations %>%
  ggplot()+
  geom_sf(aes(fill=cut(Part_Trajets, breaks=c(0, 0.5, 1, 5,10,30,43))), colour="white")+
  theme_void()+
  labs(title="Destinations en juillet des covoiturages au départ de l'Ile de France",
       subtitle="En part des trajets",
       caption="Source: Base de données du covoiturage")+
  scale_fill_viridis_d("Part en %", option="magma")+
  theme(text = element_text(size=5), legend.position = "top")

ggsave(filename = "Carte des regions de destinations de covoiturage.png",width = 90, height = 90, units = "mm")
