library(dplyr)
library(fmsb)
library(RColorBrewer)
library(scales)

# TO DO LEGENDA I CIEMNE TLO

# Color palette
coul <- c("#470D21", "#D67D3E")
colors_border <- coul
colors_in <- alpha(coul,0.3)

df <- read.csv("./data/merged_data_cleaned.csv")

characteristics <- c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness")

# prepare_data <- function (df1, df2) {
#   df1_vs_df2 <- rbind(df1, df2)
#   df1_vs_df2 <- rbind(rep(0, length(df1_vs_df2)), df1_vs_df2)
#   df1_vs_df2 <- rbind(rep(10, length(df1_vs_df2)), df1_vs_df2)
#!   df1_vs_df2 <- df1_vs_df2 %>% select(-Country.of.Origin)
#!  df1_vs_df2 <- data.frame(df1_vs_df2)
#   df1_vs_df2 <- df1_vs_df2[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]
#   return(df1_vs_df2)
# }

average_variety <- df %>%
  select(Variety, characteristics) %>%
  filter(!(Variety %in% c("", "Other"))) %>%
  na.omit() %>%
  group_by(Variety) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

average_country <- df %>%
  select(Country.of.Origin, characteristics) %>%
  filter(!(Country.of.Origin %in% c("", "Other"))) %>%
  na.omit() %>%
  group_by(Country.of.Origin) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

# BELOW 70 AND OVER 90 CUP POINTS
average_coffee_below_70 <- df %>%
  filter(`Total.Cup.Points` < 70) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_over_90 <- df %>%
  filter(`Total.Cup.Points` > 90) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_70_vs_90 <- rbind(average_coffee_below_70, average_coffee_over_90)
average_coffee_70_vs_90 <- rbind(rep(0, length(average_coffee_70_vs_90)), average_coffee_70_vs_90)
average_coffee_70_vs_90 <- rbind(rep(10, length(average_coffee_70_vs_90)), average_coffee_70_vs_90)
average_coffee_70_vs_90 <- data.frame(average_coffee_70_vs_90)
average_coffee_70_vs_90 <- average_coffee_70_vs_90[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/average_coffee_70_vs_90.png", width = 1000, height = 1000, units = "px")
radarchart(average_coffee_70_vs_90,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Coffee with <70 vs >90 cup points",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# ARABICA VS ROBUSTA
average_robusta <- df %>%
  filter(`Species` == "Robusta") %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_arabica <- df %>%
    filter(`Species` == "Arabica") %>%
    select(characteristics) %>%
    na.omit() %>%
    colMeans()

average_arabica_vs_robusta <- rbind(average_robusta, average_arabica)
average_arabica_vs_robusta <- rbind(rep(0, length(average_arabica_vs_robusta)), average_arabica_vs_robusta)
average_arabica_vs_robusta <- rbind(rep(10, length(average_arabica_vs_robusta)), average_arabica_vs_robusta)
average_arabica_vs_robusta <- data.frame(average_arabica_vs_robusta)
average_arabica_vs_robusta <- average_arabica_vs_robusta[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/arabica_vs_robusta.png", width = 1000, height = 1000, units = "px")
radarchart(average_arabica_vs_robusta,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Arabica vs Robusta",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# PAPUA NEW GUINEA VS HAITI
average_haiti <- df %>%
  select(Country.of.Origin, characteristics) %>%
  filter(Country.of.Origin == "Haiti") %>%
  na.omit() %>%
  group_by(Country.of.Origin) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

average_papua_new_guinea <- df %>%
  select(Country.of.Origin, characteristics) %>%
  filter(Country.of.Origin == "Papua New Guinea") %>%
  na.omit() %>%
  group_by(Country.of.Origin) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

haiti_vs_papua_new_guinea <- rbind(average_haiti, average_papua_new_guinea)
haiti_vs_papua_new_guinea <- rbind(rep(0, length(haiti_vs_papua_new_guinea)), haiti_vs_papua_new_guinea)
haiti_vs_papua_new_guinea <- rbind(rep(10, length(haiti_vs_papua_new_guinea)), haiti_vs_papua_new_guinea)
haiti_vs_papua_new_guinea <- haiti_vs_papua_new_guinea %>% select(-Country.of.Origin)
haiti_vs_papua_new_guinea <- haiti_vs_papua_new_guinea[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/haiti_vs_papua_new_guinea.png", width = 1000, height = 1000, units = "px")
radarchart(haiti_vs_papua_new_guinea,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "avarege coffe from Haiti vs Papua New Guinea",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# HAWAIIAN KONA VS ETHIOPIAN YIRGACHEFFE
average_hawaiian_kona <- df %>%
  select(Variety, characteristics) %>%
  filter(Variety == "Hawaiian Kona") %>%
  na.omit() %>%
  group_by(Variety) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

average_ethiopian_yirgacheffe <- df %>%
  select(Variety, characteristics) %>%
  filter(Variety == "Ethiopian Yirgacheffe") %>%
  na.omit() %>%
  group_by(Variety) %>%
  summarise_at(vars(characteristics), mean, na.rm = TRUE) %>%
  na.omit()

ethiopian_vs_kona <- rbind(average_ethiopian_yirgacheffe, average_hawaiian_kona)
ethiopian_vs_kona <- rbind(rep(0, length(ethiopian_vs_kona)), ethiopian_vs_kona)
ethiopian_vs_kona <- rbind(rep(10, length(ethiopian_vs_kona)), ethiopian_vs_kona)
ethiopian_vs_kona <- ethiopian_vs_kona %>% select(-Variety)
ethiopian_vs_kona <- ethiopian_vs_kona[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/yirgacheffe_vs_kona.png", width = 1000, height = 1000, units = "px")
radarchart(ethiopian_vs_kona,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Ethiopian Yirgacheffe vs Hawaiian Kona",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3)
dev.off()

# BELOW 1KM AND OVER 2KM
average_coffee_below_1000m <- df %>%
  filter(Altitude < 1000) %>%
  select(characteristics) %>%
  na.omit() %>%
  colMeans()

average_coffee_over_2000m <- df %>%
    filter(Altitude >= 2000) %>%
    select(characteristics) %>%
    na.omit() %>%
    colMeans()

average_coffee_1000m_vs_2000m <- rbind(average_coffee_below_1000m, average_coffee_over_2000m)
average_coffee_1000m_vs_2000m <- rbind(rep(0, length(average_coffee_1000m_vs_2000m)), average_coffee_1000m_vs_2000m)
average_coffee_1000m_vs_2000m <- rbind(rep(10, length(average_coffee_1000m_vs_2000m)), average_coffee_1000m_vs_2000m)
average_coffee_1000m_vs_2000m <- data.frame(average_coffee_1000m_vs_2000m)
average_coffee_1000m_vs_2000m <- average_coffee_1000m_vs_2000m[, c(9, 2, 3, 8, 5, 6, 7, 4, 1)]

png(filename ="./plots/spider_charts/average_coffee_1000m_vs_2000m.png", width = 1000, height = 1000, units = "px")
# DARK BACKGROUND, WORK IN PROGRESS
# par(bg = "black", mar = rep(0, 4))
radarchart(average_coffee_1000m_vs_2000m,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty = 1,
           centerzero = TRUE, maxmin = TRUE, na.itp = TRUE,
           title = "Coffee below 1000m vs over 2000m",
           seg = 1, cglcol="black", cglty=1, axislabcol="grey",
           vlcex=1.3, paxislabels = seq(0, 10, 2))
dev.off()



