
library(tidyverse)
library(rvest)



url<-"https://www.olympic.org/rio-2016/archery/individual-competition-men"

read_html(url) %>% 
  html_nodes(xpath='//*[@id="main"]/div/section/table') %>%
  html_text()


download.file(url, destfile = "C:\\Users\\jwayland\\Desktop\\personal\\R")

content <- read_html(proxyPath) 
misspells <- content %>%
  html_nodes(xpath='//*[@id="main"]/div[1]/table[1]') %>%
  html_table() %>%
  data.frame
} 

plot(zipcode$latitude, zipcode$longitude)







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
  ggplot(aes(x=long, y = lat, group = group)) + 
  geom_polygon(color = "white", fill = "lightgray") +
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")


counties %>%
  ggplot(aes(x=long, y = lat, group = group)) + 
  geom_polygon(color = "white") +
  coord_fixed(1.3)+
  theme_bw()+
  theme(legend.position="none")


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


