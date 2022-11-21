library(tidyverse)
library(readr)
library(sf)

deplacements <- read_delim("C:/Users/valexandre/Downloads/deplacements (1).csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
Monde<-st_read("C:/Users/valexandre/Downloads/countries.geojson")

Monde%>%
  ggplot()+
  geom_sf()

unique(deplacements$lieu_niveau_3)

JonctionParNom<-deplacements %>% left_join(Monde, by=c("lieu_niveau_3"="NAME_FR"))
sort(unique(Monde$NAME_FR))

JonctionParCoordonneesGeographiques <-deplacements%>%
  separate(col=geo_point_2d, into = c("latitude","longitude"),sep = ",")%>%
  mutate(longitude=as.numeric(longitude), latitude=as.numeric(latitude))%>%
  filter(!(is.na(longitude)))%>%
  st_as_sf(coords=c("longitude","latitude"),crs=4326)

JonctionParCoordonneesGeographiques%>%
  ggplot()+
  geom_sf()+
  geom_sf(data=Monde,fill=NA)

# Jonction géographique
JonctionGeog<-st_join(JonctionParCoordonneesGeographiques,
                      Monde)

LesquelsNontPasdeJonctionGeog<-JonctionGeog%>%filter(is.na(ISO_A3))%>%
  group_by(lieu_niveau_2, lieu_niveau_3)%>%count()


BilanSelonJonctionGeog<- JonctionGeog%>%
  group_by(lieu_niveau_3,ISO_A3,ADMIN)%>%
  summarise(NombreDeVisites=n())%>%
  st_drop_geometry()%>%
  filter(!is.na(ISO_A3))

BilanSelonJonctionGeog%>%left_join(Monde)%>%
  st_as_sf()%>%
  ggplot()+
  geom_sf(aes(fill=cut(NombreDeVisites,breaks=c(0,1,5,10,20,100,5000000))),colour=NA)+
  geom_sf(data=Monde,colour="white",fill=NA)+
  theme(legend.position="top")
