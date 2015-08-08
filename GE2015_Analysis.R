


library(ggplot2)
library(xlsx)
library(grid)
library(plyr)
source("C:/Users/Susan Johnston/Desktop/R Functions/recoderFunc.R")
source("C:/Users/Susan Johnston/Desktop/R Functions/multiplot.R")


gbmap <- read.table("CartogramPolygons.txt", header = T, sep = "\t")
gbpaths <- read.table("CartogramPaths.txt", header = T, sep = "\t")
scotcart <- read.table("ScotlandPolygons.txt", header = T, sep = "\t")

rsc <- read.xlsx("parliament-counts-yes-list-06May15.xlsx", sheetIndex = 1)
gender <- read.table("genderGE2015Scotland.txt", header = T, sep = "\t", stringsAsFactors = F)

head(gender)
gender$StatTraining <- "No"
gender$StatTraining[which(gender$Constituency %in% c("Cumbernauld, Kilsyth & Kirkintilloch East", "Dundee West", "Edinburgh North & Leith",
                                        "Edinburgh South", "Glasgow North"))] <- "Yes"

library(maptools)
gpclibPermit() 
ukmap <- readShapeSpatial("westminster_const_region.shp") 
names(ukmap)
nameframe <- data.frame(Constituency = as.character(ukmap$NAME), id = (1:length(ukmap))-1)

ukmap <- fortify(ukmap)
head(ukmap)
ukmap2 <- ukmap[seq(1, nrow(ukmap), 200),]

head(nameframe)

nameframe$Constituency <- gsub(" Co Const", "", nameframe$Constituency)
nameframe$Constituency <- gsub(" Boro Const", "", nameframe$Constituency)
nameframe$Constituency <- gsub(" Burgh Const", "", nameframe$Constituency)

nameframe$Constituency <- gsub(" and ", " & ", nameframe$Constituency)

unique(gender$Constituency[which(gender$Constituency %in% nameframe$Constituency)])
dput(unique(gender$Constituency[which(!gender$Constituency %in% nameframe$Constituency)]))

grep("Aberdeen", nameframe$Constituency, value = T)
gender$Constituency <- recoderFunc(gender$Constituency,
                                   c("Ayrshire Central", "Dunfermline & Fife West", "Dunbartonshire East", 
                                     "Renfrewshire East", "Linlithgow & Falkirk East", "Na h-Eileanan an Iar (Western Isles)", 
                                     "Fife North East", "Ayrshire North & Arran", "Dunbartonshire West", 
                                     "Aberdeenshire West & Kincardine"),
                                   c("Central Ayrshire", "Dunfermline & West Fife", "East Dunbartonshire", 
                                     "East Renfrewshire", "Linlithgow & East Falkirk", "Na h-Eileanan an Iar", 
                                     "North East Fife", "North Ayrshire & Arran", "West Dunbartonshire", 
                                     "West Aberdeenshire & Kincardine"))


head(gender)
head(ukmap2)
head(nameframe)

gender2 <- join(gender, nameframe)
gender2 <- join(gender2, ukmap)
gender2 <- droplevels(gender2)
gender3 <- gender2[seq(1, nrow(gender2), 200),]



table(gender$Gender, gender$Year)
newdata <- data.frame(Year = c(2010, 2015),
                      Label = c("46 Men\n   13 Women", "39 Men\n   20 Women"))



head(scotcart)

gender4 <- join(gender, scotcart)
str(gender4)

gender2$lat2 <- gender2$lat
gender2$lat2[which(gender2$lat > 1100000)] <- gender2$lat[which(gender2$lat > 1100000)] - 200000
gender2$long2 <- gender2$long
gender2$long2[which(gender2$lat > 1100000)] <- gender2$long2[which(gender2$lat > 1100000)] + 0.3e5

gender3$lat2 <- gender3$lat
gender3$lat2[which(gender3$lat > 1100000)] <- gender3$lat[which(gender3$lat > 1100000)] - 200000
gender3$long2 <- gender3$long
gender3$long2[which(gender3$lat > 1100000)] <- gender3$long2[which(gender3$lat > 1100000)] + 0.3e5


ggplot(subset(gender3, Year == "2015"), aes(x = long2, y = lat2, group = group, fill = StatTraining)) +
  geom_polygon() +
  geom_rect(xmin = 3.8e5+ 0.3e5, xmax = 4.9e5+ 0.3e5, ymin = 895000, ymax = 1030000, alpha = 0, col = "grey") +
  scale_fill_manual(values = c("lemonchiffon2", "red")) +
  theme(legend.position  = "none",
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.ticks.x     = element_blank(),
        strip.text.x     = element_text(colour = "white", face = "bold", size = 16, vjust=-54, hjust = 0.65),
        panel.grid       = element_blank(),
        plot.background  = element_rect(fill = "black"),
        panel.background = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "", fill = "Commitment to Statistical Training") +
  theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), units = "cm")) 

beepr::beep()

multiplot(
ggplot() + 
  geom_polygon(data = gender3, aes(x = long, y = lat, group = group, fill = Gender)) +
  #geom_text(data = newdata, aes(x = 1e05, y = 1100000, label = Label), col = "white", fontface = "bold", size = 5) +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position  = "top",
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.ticks.x     = element_blank(),
        strip.text.x     = element_text(colour = "white", face = "bold", size = 16, vjust=-54, hjust = 0.65),
        panel.grid       = element_blank(),
        plot.background  = element_rect(fill = "black"),
        panel.background = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "") +
  theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), units = "cm")) +
  facet_wrap(~Year)

,

ggplot(gender4, aes(x = x, y = -y, group = factor(Constituency))) + 
  geom_polygon(aes(fill = Gender), col = "darkgrey") +
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position  = "none",
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.ticks.x     = element_blank(),
        strip.text.x     = element_text(colour = "white", face = "bold", size = 16, vjust=-54, hjust = 0.5),
        panel.grid       = element_blank(),
        plot.background  = element_rect(fill = "black"),
        panel.background = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "") +
  coord_cartesian(ylim = c(-700, 100), xlim = c(300, 900)) +
  theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), units = "cm")) +
  facet_wrap(~Year)
,cols = 1)

write.table(gender$Constituency, "test.txt", row.names = F, sep = "\t", quote = T)

      

ggplot(gender, aes(x = x, y = -y, group = Constituency)) + 
  geom_polygon(col = "grey") +
  #geom_path(data = gbpaths, aes(x = x, y = -y, group = variable), colour = "white", size = 1) +
  theme(legend.position  = "none",
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.grid       = element_blank(),
        plot.background  = element_rect(fill = "ivory"),
        panel.background = element_blank()) +
  labs(x = "", y = "") +
  theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), units = "cm"))
    