#------------Prep------------#

library(tidyverse)
library(haven)
library(ggplot2)

player = read_sav("MGM_Player_Database_2019.sav")

hotel = read_sav("MGM_Hotel_Database_2019.sav")

player$compratio = player$Total_comps / player$Total_Actual

playershort <- subset.data.frame(player, comp_ratio != "na" | compratio = NaN)

playershort$adjwin <- playershort$Total_Actual - playershort$Total_comps

#------------Summary stats to determine skewedness------------#

siteavg <- playershort %>%
  group_by(Site_ID) %>%
  summarise(avgcompratio = mean(compratio), medcompratio = median(compratio), avgadjwin = mean(adjwin), medadjwin = median(adjwin), totadjwin = sum(adjwin))

#------------Identifying positive and negative outlier ratios------------#

playershorterpos <- subset.data.frame(playershort, 1< playershort$compratio)

playershorterneg <- subset.data.frame(playershort, 0 > playershort$compratio)

#------------Visualizations------------#

ggplot(playershort, aes(x = playershort$Total_Actual, y = playershort$compratio)) +
  geom_line(na.rm = TRUE) +
  coord_fixed(ylim = c(-310,310), xlim = c(-100, 100)) +
  scale_x_continuous(breaks = 100)
