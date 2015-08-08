

library(lubridate)
library(ggplot2)
library(reshape)


polls <- read.table("C:/Users/Susan Johnston/Desktop/Blog/PoliticalPredictions.txt", sep = "\t", header = T, stringsAsFactors = F)


polls$NewDate <- sapply(polls$Sample.dates, function(x) strsplit(x, split = "-")[[1]][1])
polls$NewDate <- parse_date_time(polls$NewDate, "dmy")


polls$Sample.size <- as.numeric(gsub(",", "", polls$Sample.size ))

head(polls)


polls2 <- melt(polls, id.vars = c("Pollster", "Sample.dates", "Sample.size", "NewDate"))
head(polls2)

myColours <- c("#377eb8", "#e41a1c", "#ff7f00", "#984ea3", "#4daf4a", "#ffff33")
names(myColours) <- levels(polls2$variable)




ggplot(polls2, aes(NewDate, value, col = variable)) +
  stat_smooth(method = "lm", formula = y ~ I(x^3) + I(x^2) + x) +
  #geom_line(alpha = 0.4, col = "grey", aes(group = variable)) +
  geom_point(size = 4, alpha = 0.7, shape=21, solid = F, col = "black", aes(fill = variable)) +
  scale_fill_manual(values = myColours) +
  scale_colour_manual(values = myColours) +
  theme_bw() +
  theme(axis.text.x  = element_text (size = 16, vjust = 0),
        axis.text.y  = element_text (size = 14),
        strip.text.x = element_text (size = 16, vjust = 0.7),
        axis.title.y = element_text (size = 16, angle = 90, vjust = 0.9),
        axis.title.x = element_text (size = 16, vjust = 0.2)) +
  labs(x = "Polling Date", y = "Voting Intention (%)", fill = "", col = "")

