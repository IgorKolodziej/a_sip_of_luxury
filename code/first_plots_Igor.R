library(dplyr)
library(ggplot2)
library(forcats)
library(pheatmap)

df <- read.csv("./data/merged_data_cleaned.csv")

# TO DO:
# Dodać kolorowanie jako trzeci wymiar wykresów
# Mapa świata pokolorowana wg. różnych cech
# Wykres pajęczynowy charakterystyk kawy

# Correlation matrix
df %>%
  select(altitude_mean_meters, Aroma, Flavor, Aftertaste, Acidity, Body,
         Balance, Uniformity, Clean.Cup, Sweetness, Cupper.Points, Moisture,
         Category.One.Defects, Quakers, Category.Two.Defects) %>%
  na.omit() %>%
  cor() %>%
  pheatmap(., show_rownames = TRUE, show_colnames = TRUE, fontsize = 8, display_numbers = TRUE)
ggsave("./plots/correlation_matrix.png")

# Geography vs altitude
df %>%
  mutate(`Country.of.Origin` = as.factor(`Country.of.Origin`), 'Altitude' = as.numeric(Altitude)) %>%
  group_by(Country.of.Origin) %>%
  summarize(mean_altitude = mean(altitude_mean_meters, na.rm = TRUE)) %>%
  ggplot(aes(x = fct_reorder(Country.of.Origin, mean_altitude), y = mean_altitude)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Altitude by Country", x = "Country", y = "Average Altitude") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./plots/altitude_by_country.png")

df %>%
  filter(altitude_mean_meters < 2000) %>%
  mutate(Region = as.factor(Region), altitude_mean_meters = as.numeric(altitude_mean_meters)) %>%
  na.omit() %>%
  group_by(Region) %>%
  summarize(mean = mean(altitude_mean_meters, na.rm = TRUE)) %>%
  top_n(10, mean) %>%
  ggplot(aes(x = fct_reorder(Region, mean), y = mean)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Regions with highest average altitude", x = "Region", y = "Average altitude") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./plots/top_10_regions_by_altitude.png")

# What influences characteristics?
characteristics <- c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness", "Cupper.Points", "Moisture", "Category.One.Defects", "Quakers", "Category.Two.Defects", "Total.Cup.Points")

# Distribution of characteristics
for (characteristic in characteristics) {
    df %>%
        filter(altitude_mean_meters < 2000) %>%
        ggplot(aes(x = get(characteristic))) +
        geom_density() +
        labs(title = paste("Histogram of", characteristic), x = characteristic, y = "Count")
    ggsave(paste("./plots/characteristics_densities/", characteristic, "_density.png", sep = ""))
}

# 1. Altitude
for (characteristic in characteristics) {
  df %>%
    filter(altitude_mean_meters < 3000) %>%
    ggplot(aes(x = altitude_mean_meters, y = get(characteristic))) +
    geom_point() +
    labs(title = paste("Scatterplot of", characteristic, "vs altitude"), x = "Altitude", y = characteristic)
  ggsave(paste("./plots/characteristics_vs_altitude/", characteristic, "_vs_altitude.png", sep = ""))
}

alt_intervals <- seq(0,2400, by=400)

for (characteristic in characteristics) {
  df %>%
    mutate(altitude_interval = cut(altitude_mean_meters, breaks = alt_intervals)) %>%
    ggplot(aes(x = altitude_interval, y = get(characteristic))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", characteristic, "vs altitude"), x = "Altitude", y = characteristic)
  ggsave(paste("./plots/characteristics_vs_altitude/", characteristic, "_vs_altitude_intervals.png", sep = ""))
}

# 2. Processing method
for (characteristic in characteristics) {
  df %>%
    mutate(`Processing.Method` = as.factor(`Processing.Method`)) %>%
    ggplot(aes(x = `Processing.Method`, y = get(characteristic))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", characteristic, "vs processing method"), x = "Processing method", y = characteristic)
  ggsave(paste("./plots/characteristics_vs_processing_method/", characteristic, "_vs_processing_method.png", sep = ""))
}

# 3. Country
for (characteristic in characteristics) {
    df %>%
        mutate(`Country.of.Origin` = as.factor(`Country.of.Origin`)) %>%
        ggplot(aes(x = `Country.of.Origin`, y = get(characteristic))) +
        geom_boxplot() +
        labs(title = paste("Boxplot of", characteristic, "vs country"), x = "Country", y = characteristic) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(paste("./plots/characteristics_vs_country/", characteristic, "_vs_country.png", sep = ""))
    }

# TO DO inne aspekty pochodzenia: region, farma itd.
# 4. Variety
for (characteristic in characteristics) {
    df %>%
        mutate(Variety = as.factor(Variety)) %>%
        ggplot(aes(x = Variety, y = get(characteristic))) +
        geom_boxplot() +
        labs(title = paste("Boxplot of", characteristic, "vs variety"), x = "Variety", y = characteristic) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(paste("./plots/characteristics_vs_variety/", characteristic, "_vs_variety.png", sep = ""))
    }

# Varieties in each country
df %>%
    mutate(`Country.of.Origin` = as.factor(`Country.of.Origin`), Variety = as.factor(Variety)) %>%
    group_by(`Country.of.Origin`, Variety) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = `Country.of.Origin`, y = count, fill = Variety)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of each variety in each country", x = "Country", y = "Number of each variety") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./plots/varieties_in_countries.png")

# TO DO: cechy pochodzenia
origin <- c("Country.of.Origin", "")

# EXPORTS
# Owners with most coffee produced
df %>%
    mutate("Owner.1" = as.factor(`Owner.1`),
           "Number.of.Bags" = as.numeric(`Number.of.Bags`),
           "Bag.Weight" = as.numeric(`Bag.Weight`),
           "Total.Weight" = `Number.of.Bags` * `Bag.Weight`) %>%
    na.omit() %>%
    group_by(`Owner.1`) %>%
    summarize("Total.Weight" = sum(`Total.Weight`, na.rm = TRUE)) %>%
    top_n(10, `Total.Weight`) %>%
    ggplot(aes(x = fct_reorder(`Owner.1`, `Total.Weight`), y = `Total.Weight`)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 owners with most coffee produced in kg", x = "Owner", y = "Total weight in kg") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./plots/top_10_owners.png")
# dlaczego zwraca tylko 6 ownerów???


# MAPS
# Load required libraries
library(ggplot2)
library(maps)

# Get world map data
world_map <- map_data("world")

# Your data frame (replace this with your actual data)
#df <- data.frame(country = c("France", "Germany", "China"),
                 #value = c(10, 20, 30))
colnames(df)

# Merge your data with the world map data
merged_data <- merge(world_map, df, by.x = "region", by.y = "Country.of.Origin", all.x = TRUE)
country_centroids <- aggregate(cbind(long, lat) ~ region, data = world_map, FUN=function(x) mean(range(x)))
# Replace NA values with a specific value (e.g., 0)
merged_data$Number.of.Bags[is.na(merged_data$value)] <- 0
merged_data$altitude_mean_meters[is.na(merged_data$altitude_mean_meters)] <- 0
# Create the map

merged_data %>%
  mutate("Number.of.Bags" = as.numeric(`Number.of.Bags`),
         "Bag.Weight" = as.numeric(`Bag.Weight`)) %>%
  na.omit() %>%
  group_by(region) %>%
  summarize(Total.Weight = sum(Number.of.Bags * Bag.Weight, na.rm = TRUE))

merged_data %>%
ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = altitude_mean_meters)) +
  scale_fill_gradient(low = "white", high = "brown") +


    geom_text(data = world_map, aes(x = long, y = lat, label = region)) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = "Average altitude by country")


    geom_text(data=country_centroids, aes(x=long, y=lat, label=region), size=3)
# Add country names
country_centroids <- aggregate(cbind(long, lat) ~ region, data = world_map, FUN=function(x) mean(range(x)))
p + geom_text(data=country_centroids, aes(x=long, y=lat, label=region), size=3)


# wersja z rworldmap
# Load required libraries
library(ggplot2)
library(maps)
library(rworldmap)

# Get world map data
world_map <- map_data("world")

# Your data frame (replace this with your actual data)
df <- data.frame(country = c("France", "Germany", "China"),
                 value = c(10, 20, 30))

# Merge your data with the world map data
merged_data <- merge(world_map, df, by.x = "region", by.y = "country", all.x = TRUE)

# Replace NA values with a specific value (e.g., 0)
merged_data$value[is.na(merged_data$value)] <- 0

# Create the map
p <- ggplot() +
  geom_polygon(data = merged_data,
               aes(x = long, y = lat, group = group, fill = value),
               color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

# Get a world map object from rworldmap
world_map_rworldmap <- getMap()

# Add country names
p + geom_text(data=world_map_rworldmap@data, aes(x=world_map_rworldmap@data$LON, y=world_map_rworldmap@data$LAT, label=NAME), size=3)



