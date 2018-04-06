library(dplyr)
library(ggplot2)
library(reshape2)
library(openxlsx)
library(ggmap)
library(tidyr)
library("ggthemes")

# The Quantity of terrorist attacks in Russia

terror.Russia <- read.csv("The Global Terrorism Database - Russia 1970-2017.csv")
terror.Russia <- subset(terror.Russia, select = c(iyear))
terror.Russia <- count(terror.Russia, iyear)
terror.Russia$Country <- "Россия"
# The Graph
ggplot(terror.Russia, aes(x = iyear, y = n))+
  geom_line(linetype = 1, size = 1.5)+
  geom_point()+
  ggtitle("Террористическая активность в России") +
  labs(x = "Годы\n
       Источник: Университет Мэриленда Автор кода: github.com/Myonin/", 
       y = "количество терактов", colour = " ")+
  theme_economist()

# Максимальное количество терактов в России - 251 в 2010 году
terror.Russia$iyear[terror.Russia$n == max(terror.Russia$n[terror.Russia$Country == "Россия"])]

# За всю новейшую историю России (с 1992 по 2016 гг.) произошло 2158 теракта 
sum(terror.Russia$n[terror.Russia$Country == "Россия"])


terror.Russia.nkill <- read.csv("The Global Terrorism Database - Russia 1970-2017.csv")
terror.Russia.nkill <- subset(terror.Russia.nkill, select = c(iyear, nkill))
terror.Russia.nkill <- summarise(group_by(terror.Russia.nkill, iyear), sum(nkill, na.rm = T))
terror.Russia.nkill$Country <- "Россия"
colnames(terror.Russia.nkill) <- c("iyear", "nkill", "Country")

# График "Количества жертв терактов в России"
ggplot(terror.Russia.nkill, aes(x = iyear, y = nkill))+
  geom_line(linetype = 1, size = 1)+
  geom_point()+
  ggtitle("Количество погибших в результате терактов в России") +
  labs(x = "Годы\n
       Источник: Университет Мэриленда; Автор кода: github.com/Myonin/", 
       y = "количество погибших")+
  theme_economist_white()


# Теракты в России по террористическим организациям
terror.RUS <- read.csv("The Global Terrorism Database - Russia 1970-2017.csv")

gnames <- subset(terror.RUS, select = c(iyear, gname))
gnames <- count(gnames, gname, iyear)

# Вычисление доли терактов, совершенных террористической организацией за всё время,
# от всего количества терактов за всё время
gnames.all <- subset(terror.RUS, select = c(gname))
gnames.all <- count(gnames, gname)
gnames.all$Percents <- round(gnames.all$nn/sum(gnames.all$nn)*100, digits = 0)
gnames.all <- subset(gnames.all, Percents > 1)
gnames.all$gname <- as.character(gnames.all$gname)
gnames.all$gname <- as.factor(gnames.all$gname)
levels(gnames.all$gname) <- c("Антисемитские экстремисты",
                              "Вооружённые формирования Чеченской Республики Ичкерия",
                              "Кавказский эмират",
                              "Вилаят Кавказ",
                              "Чеченские повстанцы / Chechen Rebels",
                              "Джамаат шариат (Дагестанский фронт)",
                              "Бандиты / Gunmen",
                              "Ингушские повстанцы",
                              "Катаиб аль-Хоул",
                              "Махачкалинская бандгруппа",
                              "Религиозные экстремисты",
                              "Незаконные вооруженные формирования (NVF)",
                              "Повстанцы / Rebels",
                              "Риядус-Салихийн",
                              "Исламский полк особого назначения",
                              "Неизвестно")
gnames.all$gname <- reorder(gnames.all$gname, gnames.all$Percents)

# График "Наиболее активные террористические организации в России за всё время"
ggplot(gnames.all, aes(x = gname, y = Percents))+
  geom_bar(stat = "identity", colour = "black", fill = "white")+
  coord_flip()+
  ggtitle("Топ 10 террористических организаций в России") +
  labs(y = "% от всех совершённых терактов, 1992-2016 гг.\n
       Источник: Университет Мэриленда; Автор кода: github.com/Myonin/", 
       x = " ")+
  geom_text(aes(label = paste0(Percents, "%")), 
            colour = "black", hjust=1.2)+
  theme_fivethirtyeight()

# Вычисление количества терактов по годам
for(i in 1992:2016){
  gnames$sum.per.year[gnames$iyear == i] <- sum(gnames$n[gnames$iyear == i])
}

# Вычисление доли терактов, совершенных террористической организацией за год,
# от всего количества терактов за год
gnames$Percents <- round(gnames$n/gnames$sum.per.year*100, digits = 0)

# Наиболее активные террористические организации в России (доля за год > 10%)
gnames.subset <- subset(gnames, Percents >= 10 & gname != "Unknown")
gnames.subset$gname <- as.character(gnames.subset$gname)
gnames.subset$gname <- as.factor(gnames.subset$gname)
levels(gnames.subset$gname) <- c("Вооружённые формирования Чеченской Республики Ичкерия",
                                 "Кавказский эмират", 
                                 "Вилаят Кавказ",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Чеченские повстанцы / Chechen Rebels",
                                 "Ингушские повстанцы",
                                 "Религиозные экстремисты")

# График "Наиболее активные террористические организации в России"
ggplot(gnames.subset, aes(x = iyear, y = Percents, fill = gname))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle(expression(atop("Активные террористические группы в России", 
                          atop(italic("вклад в террористическую активность >10% всех терактов за год")))))+
  labs(x = "Годы\n
       Источник: Университет Мэриленда; Автор кода: github.com/Myonin/", 
       y = "% от всех совершённых терактов за год", fill = " ")

#Данные по количеству терактах c географическими координатами
terror.geo <- subset(terror.RUS, select = c(iyear, latitude, longitude))
terror.geo <- na.omit(terror.geo)



############### IT'S VERY DIFFICULT TO UNDERSTAND A POINT OF GGMAP. THE RIGHT VERSION IS BELOW
map <- get_map(location = "Eurasia", maptype = "terrain", zoom = 1, source = "google")


ggmap(map)+
  geom_point(data = terror.geo, 
             aes(x = longitude, y = latitude),
             colour = "red", alpha = 0.5)+
  ylim(5, 90)+
  xlim(10, 170)+
  ggtitle("Террористическая активность в России с 1992 по 2016 гг.\n") +
  labs(x = "Источники: Университет Мэриленда; Автор кода: github.com/Myonin/", 
       y = " ")
russia <- c(left = 25, bottom = 37, right = 170, top = 90)
map <- get_openstreetmap(russia, zoom = 1, maptype = "toner-lite",  format = c("png"), messaging = FALSE, urlonly = T,
                         filename = "ggmapTemp", color = c("color"))
ggmap(map)
ggmap(map, extent = "device")
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)



####### THE RIGHT VERSION IS HERE
library(geosphere)
library(tidyverse)
library(maps)
library(ggmap)
library(rworldmap)
d <- data.frame(
  country=c("Russia","Belarus"),
  value=c(0, 1))
n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")



mapCountryData(n, nameColumnToPlot="value", mapTitle="",
               xlim=c(30, 160), ylim=c(50, 70),
               colourPalette=c('blue','springgreen3') ,
               addLegend=F,borderCol = 'black',
               oceanCol="lightblue", missingCountryCol="springgreen3")

points(x=terror.geo$longitude, y=terror.geo$latitude, col = "red", cex=1, pch = 20)
title("Террористическая активность в России с 1992 по 2016 гг.", sub = "Источники: Университет Мэриленда", line = 2)


###### For all former USSR members
# Uzbekistan
terror.UZB <- read.csv("The Global Terrorism Database - Uzbekistan 1992-2017.csv")
terror.geo_Uz <- subset(terror.UZB, select = c(iyear, latitude, longitude))
terror.geo_Uz <- na.omit(terror.geo_Uz)

#Kyrg
terror.KGT <- read.csv("The Global Terrorism Database - Kyrgyzstan 1992-2017.csv")
terror.geo_Kg <- subset(terror.KGT, select = c(iyear, latitude, longitude))
terror.geo_Kg <- na.omit(terror.geo_Kg)


# Tajik
terror.TJT <- read.csv("The Global Terrorism Database - Tajikistan 1992-2017.csv")
terror.geo_TJ <- subset(terror.TJT, select = c(iyear, latitude, longitude))
terror.geo_TJ <- na.omit(terror.geo_TJ)

#Ukraine
terror.UKR <- read.csv("The Global Terrorism Database - Ukraine 1992-2017.csv")
terror.geo_UKR <- subset(terror.UKR, select = c(iyear, latitude, longitude))
terror.geo_UKR<- na.omit(terror.geo_UKR)

# Kazakhstan
terror.KAZ <- read.csv("The Global Terrorism Database - Kazakhstan 1992-2017.csv")
terror.geo_KAZ <- subset(terror.KAZ, select = c(iyear, latitude, longitude))
terror.geo_KAZ<- na.omit(terror.geo_KAZ)


# Georgia
terror.GEO <- read.csv("The Global Terrorism Database - Georgia 1992-2017.csv")
terror.geo_GEO <- subset(terror.GEO, select = c(iyear, latitude, longitude))
terror.geo_GEO<- na.omit(terror.geo_GEO)

# Turkmenistan
terror.TUR <- read.csv("The Global Terrorism Database - Turkmenistan 1992-2017.csv")
terror.geo_TUR <- subset(terror.TUR, select = c(iyear, latitude, longitude))
terror.geo_TUR<- na.omit(terror.geo_TUR)


# Azerbaijan
terror.AZ <- read.csv("The Global Terrorism Database - Azerbaijan 1992-2017.csv")
terror.geo_AZ <- subset(terror.AZ, select = c(iyear, latitude, longitude))
terror.geo_AZ<- na.omit(terror.geo_AZ)


# Armenia
terror.AR <- read.csv("The Global Terrorism Database - Armenia 1992-2017.csv")
terror.geo_AR <- subset(terror.AR, select = c(iyear, latitude, longitude))
terror.geo_AR<- na.omit(terror.geo_AR)


# Moldavia
terror.ML <- read.csv("The Global Terrorism Database - Moldova 1992-2017.csv")
terror.geo_ML <- subset(terror.ML, select = c(iyear, latitude, longitude))
terror.geo_ML<- na.omit(terror.geo_ML)


#Belarus
terror.BL <- read.csv("The Global Terrorism Database - Belarus 1992-2017.csv")
terror.geo_BL <- subset(terror.BL, select = c(iyear, latitude, longitude))
terror.geo_BL<- na.omit(terror.geo_BL)


#Latvia
terror.LA <- read.csv("The Global Terrorism Database - Latvia 1992-2017.csv")
terror.geo_LA <- subset(terror.LA, select = c(iyear, latitude, longitude))
terror.geo_LA<- na.omit(terror.geo_LA)


#Lithuania
terror.LI <- read.csv("The Global Terrorism Database - Lithuania 1992-2017.csv")
terror.geo_LI <- subset(terror.LI, select = c(iyear, latitude, longitude))
terror.geo_LI<- na.omit(terror.geo_LI)


# Estonia
terror.ES <- read.csv("The Global Terrorism Database - Estonia 1992-2017.csv")
terror.geo_ES <- subset(terror.ES, select = c(iyear, latitude, longitude))
terror.geo_ES<- na.omit(terror.geo_ES)



terror.geo <- rbind(terror.geo_AR, terror.geo_AZ, terror.geo_BL, terror.geo_ES, terror.geo_GEO,terror.geo_KAZ,
                    terror.geo_Kg, terror.geo_LA, terror.geo_LI, terror.geo_ML, terror.geo_TJ, terror.geo_TUR, terror.geo_UKR,
                    terror.geo_Uz)


####### THE RIGHT VERSION IS HERE
library(geosphere)
library(tidyverse)
library(maps)
library(ggmap)
library(rworldmap)
d <- data.frame(
  country=c("Russia","Belarus"),
  value=c(0, 1))
n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")



mapCountryData(n, nameColumnToPlot="value", mapTitle="",
               xlim=c(20, 90), ylim=c(40, 70),
               colourPalette=c('springgreen3','yellow') ,
               addLegend=F,borderCol = 'black',
               oceanCol="lightblue", missingCountryCol="yellow")

points(x=terror.geo$longitude, y=terror.geo$latitude, col = "red", cex=1, pch = 20)
title("Террористическая активность в СНГ с 1992 по 2016 гг. (Приближение)", sub = "Источники: Университет Мэриленда", line = 2)
