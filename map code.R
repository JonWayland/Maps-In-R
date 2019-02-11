

library(tidyverse)
library(zipcode)
data(zipcode)
head(zipcode)

zipcode %>%
  filter(longitude >= -124.7844079, longitude <= -66.9513812) %>%
  filter(latitude >= 24.7433195, latitude <= 49.3457868) %>%
  ggplot(aes(longitude, latitude, color = state))+
  geom_point(alpha = 0.2, size = 1.5)+
  theme_bw()+
  theme(legend.position="none")


filter(zipcode, city %like% "Hastings-Kearney")


# https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude

data<-read.csv("C:\\Users\\jwayland\\Desktop\\personal\\R\\us-zip-code-latitude-and-longitude.csv", sep = ";")

data %>%
  filter(Longitude >= -124.7844079, Longitude <= -66.9513812) %>%
  filter(Latitude >= 24.7433195, Latitude <= 49.3457868) %>%
  ggplot(aes(Longitude, Latitude, color = factor(Daylight.savings.time.flag)))+
  geom_point(alpha = 0.2, size = 1.5)+
  theme_bw()+
  theme(legend.position="none")

library(ggmap)
usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")
write.csv(counties, 'C:\\Users\\jwayland\\Desktop\\personal\\R\\counties_map.csv')

usa %>%
  ggplot(aes(x=long, y = lat, group = group)) + 
  geom_polygon(fill = "lightblue", color = "red") + 
  coord_fixed(1.3)+
  theme_bw()


states %>%
  ggplot(aes(x=long, y = lat, fill= region, group = group)) + 
  geom_polygon(color = "white") +
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")


states %>%
  ggplot(aes(x=long, y = lat, group = group)) + 
  geom_polygon(color = "white", fill = "lightgray") +
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")+
  ggplot(data = zipcode, aes(longitude, latitude, color = state))+
  geom_point()


nrow(states)
nrow(zipcode)
#library(stringr)
state.abb[grep(str_to_title("district of columbia"), state.name)]

state.abb[grep(str_to_title("District of Columbia"), state.name)]


abbreviations <- data.frame(abb = as.character(state.abb),state = as.character(tolower(state.name)))
abbreviations <- rbind(abbreviations, data.frame(abb = "DC", state = "district of columbia"))

states <- states %>%
  left_join(abbreviations, by = c("region" = "state"))

states %>% select(region,abb) %>% unique

states %>%
  ggplot(aes(x=long, y = lat, group = group, fill = region)) + 
  geom_polygon(color = "white", fill = "lightgray") +
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")


counties %>%
  ggplot(aes(x=long, y = lat, group = group, fill = subregion)) + 
  geom_polygon(color = "white", , fill = "lightgray") +
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")


continental <- filter(data,Longitude >= -124.7844079, Longitude <= -66.9513812,
                      Latitude >= 24.7433195, Latitude <= 49.3457868)


ggplot() + 
  geom_polygon(data = states, 
               aes(x=long, y = lat, group = group), 
               color = "black", fill = "lightgray") +
  geom_point(data=data, 
             aes(x=Longitude,y=Latitude, color = factor(Timezone)),
             alpha = 0.1)+
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")


### Maps for Quora Post ###

# https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude
data<-read.csv("C:\\Users\\jwayland\\Desktop\\personal\\R\\us-zip-code-latitude-and-longitude.csv", sep = ";")

library(ggmap)
usa <- map_data("usa")
states <- map_data("state")

continental <- filter(data,Longitude >= -124.7844079, Longitude <= -66.9513812,
                      Latitude >= 24.7433195, Latitude <= 49.3457868)

ggplot() + 
  geom_polygon(data = states, 
               aes(x=long, y = lat, group = group), 
               color = "black", fill = "lightgray") +
  geom_point(data=continental, 
             aes(x=Longitude,y=Latitude, color = factor(Timezone)),
             alpha = 0.1)+
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")



OD<-read.csv("C:\\Users\\jwayland\\Desktop\\personal\\R\\overdose.csv")

OD$Incident.Zip <- as.character(OD$Incident.Zip)
OD$Decedent.Zip <- as.character(OD$Decedent.Zip)
data$Zip <- as.character(data$Zip)

latlong <- data %>% select(Zip, Latitude, Longitude)

# https://catalog.data.gov/dataset/allegheny-county-fatal-accidental-overdoses/resource/dc760d35-18bb-4443-8c91-295559e0d8d4
OD <- OD %>% left_join(latlong, by = c("Incident.Zip" = "Zip")) %>%
  rename(inc_lat = Latitude, inc_long = Longitude)

OD <- OD %>% left_join(latlong, by = c("Decedent.Zip" = "Zip")) %>%
  rename(dec_lat = Latitude, dec_long = Longitude)

# Map of allegheny county
allegheny <- filter(counties, subregion == "allegheny")

# https://catalog.data.gov/dataset/pittsburgh-police-arrest-data
arrests<-read.csv("C:\\Users\\jwayland\\Desktop\\personal\\R\\arrests.csv") %>%
  rename(long = X, lat = Y) %>%
  filter(OFFENSES %like% 'Controlled Substance') %>%
  group_by(long,lat) %>%
  summarize(arrests = n()) %>%
  data.frame()


OD2 <- OD %>% 
  group_by(inc_long, inc_lat) %>%
  summarize(OverDose_Deaths = n()) %>%
  data.frame()

# ODs
ggplot() + 
  geom_polygon(data = allegheny, 
               aes(x=long, y = lat, group = group), 
               color = "black", fill = "lightgray") +
  #geom_point(data = filter(data, Zip %in% alleghenyZips),
  #           aes(x=Longitude, y = Latitude), alpha = 0.1)+
  geom_point(data = filter(arrests, lat != 0),
             aes(x=long, y=lat), color = "red", alpha = 0.25)+
  geom_point(data = filter(OD2, inc_long > -80.5),
             aes(x=inc_long, y = inc_lat, size = OverDose_Deaths), color = "blue", alpha = 0.2)+
  theme_bw() +
  coord_cartesian(xlim = c(min(allegheny$long), max(allegheny$long)),
                  ylim = c(min(allegheny$lat), max(allegheny$lat)))


test <- get_map(location = "united states", maptype = "terrain", source = "google")









# Pulling all zip codes in Allegheny County **** NEEDS TRIED AT HOME ****
url<-"http://www.ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?FIPS=42003"

d1<-read_html(url) %>% 
  html_nodes(xpath='//*[@id="MPBody"]/center/table/tbody/tr[2]/td/center/table/tbody/tr/td[2]') %>%
  html_text()
# **********

alleghenyZips <-
c('15006',
'15007',
'15014',
'15015',
'15017',
'15018',
'15020',
'15024',
'15025',
'15025',
'15025',
'15025',
'15028',
'15030',
'15031',
'15032',
'15034',
'15035',
'15037',
'15044',
'15045',
'15046',
'15046',
'15047',
'15049',
'15051',
'15056',
'15064',
'15065',
'15071',
'15071',
'15075',
'15076',
'15082',
'15084',
'15086',
'15088',
'15090',
'15091',
'15095',
'15095',
'15101',
'15102',
'15104',
'15104',
'15106',
'15106',
'15108',
'15108',
'15110',
'15112',
'15116',
'15120',
'15120',
'15120',
'15120',
'15122',
'15122',
'15123',
'15126',
'15127',
'15129',
'15129',
'15130',
'15131',
'15131',
'15132',
'15133',
'15134',
'15135',
'15135',
'15136',
'15137',
'15139',
'15140',
'15140',
'15142',
'15143',
'15143',
'15144',
'15145',
'15146',
'15147',
'15148',
'15148',
'15201',
'15201',
'15202',
'15202',
'15202',
'15202',
'15202',
'15202',
'15203',
'15203',
'15204',
'15204',
'15205',
'15205',
'15206',
'15206',
'15207',
'15207',
'15208',
'15208',
'15209',
'15209',
'15210',
'15210',
'15210',
'15211',
'15211',
'15211',
'15212',
'15212',
'15213',
'15213',
'15214',
'15214',
'15215',
'15215',
'15215',
'15216',
'15216',
'15217',
'15217',
'15218',
'15218',
'15219',
'15220',
'15220',
'15221',
'15221',
'15222',
'15223',
'15223',
'15224',
'15224',
'15225',
'15225',
'15226',
'15226',
'15227',
'15227',
'15228',
'15228',
'15229',
'15229',
'15230',
'15231',
'15232',
'15232',
'15233',
'15233',
'15234',
'15234',
'15235',
'15235',
'15236',
'15236',
'15236',
'15236',
'15237',
'15237',
'15238',
'15238',
'15239',
'15239',
'15240',
'15241',
'15241',
'15242',
'15242',
'15243',
'15243',
'15244',
'15244',
'15250',
'15251',
'15260',
'15264',
'15272',
'15274',
'15275',
'15276',
'15290',
'15295')
