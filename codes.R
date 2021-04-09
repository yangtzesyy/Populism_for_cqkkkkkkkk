if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(tidyverse,readr,ggplot2,sf,ggmap,rnaturalearth,
       rnaturalearthdata,rgeos,ggspatial,rworldmap)
GPD_20190625 <- read_csv("dataverse_files/GPD_20190625.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")

summ_n_bycountry <- GPD_20190625%>%
  group_by(country)%>%
  summarise(speeches=n())

names(summ_n_bycountry) <- c("name","speeches")

world <- left_join(world,summ_n_bycountry)

ggplot(data=world)+
  geom_sf(aes(fill=speeches))+
  ggtitle("The number of speeches across country in GPD")

# 数据库概览

GPD_20190625 <- within(GPD_20190625,{
  speechtype <- tolower(speechtype)
  speechtype[speechtype=="ribboncutting"] <- "ribbon cutting"
})


ggplot(data = GPD_20190625%>%
         filter(averagerubric>0,yearbegin>2001))+
  geom_jitter(aes(x=yearbegin,y=averagerubric,color=speechtype))+
  geom_smooth(aes(x=yearbegin,y=averagerubric,color=speechtype))+
  ggtitle("The average degree of populism from 2001 to present, categorized by types of speech")

#民粹主义在2005-2010年曾一度辉煌，但在2010年后受欢迎程度减弱。在民粹主义领导者看来，「运动」与民粹主义的结合是“统御”大众的有效手段。能够看到，在国际场合，民粹主义领导人依然保持了相当程度的克制。

summ_averagerubic_bycountry <- GPD_20190625%>%
  filter(!is.na(averagerubric))%>%
  group_by(country)%>%
  summarise(averagerubric=mean(averagerubric))

names(summ_averagerubic_bycountry) <- c("name","averagerubric")

summ_averagerubic_bycountry <- left_join(world,summ_averagerubic_bycountry)

ggplot(data=summ_averagerubic_bycountry)+
  geom_sf(aes(fill=averagerubric))+
  ggtitle("The average degree of populism")  
# 民粹主义程度和国家政体存在强相关关系（南美洲高亮国家为委内瑞拉，亚洲高亮国家为伊朗）。对欧美国家来说，民粹主义抬头可能意味着政治制度的调整，如波兰在两位民粹主义领导人上台后，先后通过了一系列法案。主题包括欢迎美军入驻，拒绝非洲难民，提高移民门槛等。


summ_leader_yearend <- GPD_20190625%>%
  group_by(leader)%>%
  summarise(yearend= ifelse(yearend=="present","present",yearend))%>%
  unique()%>%
  arrange(yearend)

summ_leader_yearbegin <- GPD_20190625%>%
  group_by(leader)%>%
  summarise(yearbegin = yearbegin)%>%
  unique()%>%
  arrange(yearbegin)

summ_change_n_byyear <- left_join(summ_leader_yearend%>%
            filter(yearend>2000)%>%
            group_by(yearend)%>%
            summarise(down=n())%>%
            rename("year" = "yearend"),
          
          summ_leader_yearbegin%>%
            filter(yearbegin>2000)%>%
            group_by(yearbegin)%>%
            summarise(up=n())%>%
            rename("year" = "yearbegin")%>%
            within({
              year <- as.character(year)
            }),
          by = "year")%>%
  mutate(change=up-down, color = ifelse(change>0,"red","blue"))

ggplot(data = summ_change_n_byyear)+
  geom_col(aes(x=year,y=change,fill=color))+
  geom_line(aes(x=year,y=change))+
  ggtitle("Changes in the number of populist leaders")

# 民粹主义领导人多在21世纪初上台，近年来数量渐少


