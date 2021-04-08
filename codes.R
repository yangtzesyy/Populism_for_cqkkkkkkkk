if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(tidyverse,readr,ggplot2,sf,ggmap,rnaturalearth,
       rnaturalearthdata,rgeos,ggspatial,rworldmap)
GPD_20190625 <- read_csv("dataverse_files/GPD_20190625.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))

summ_n_bycountry <- GPD_20190625%>%
  group_by(country)%>%
  summarise(speeches=n())


names(summ_n_bycountry) <- c("name","speeches")

world <- left_join(world,summ_n_bycountry)

ggplot(data=world)+
  geom_sf(aes(fill=speeches))+
  ggtitle("The number of speeches across country in GPD")+

