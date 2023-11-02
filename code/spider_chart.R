library(dplyr)
library(ggplot2)
library(fmsb)

# TO DO: NAŁOŻYĆ KILKA RÓŻNYCH KAW NARAAZ
# ZROBIĆ WYPEŁNIENIE KOLORAMI

df <- read.csv("./data/merged_data_cleaned.csv")

characteristics <- c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body", "Balance", "Uniformity", "Clean.Cup", "Sweetness", "Cupper.Points", "Moisture", "Category.One.Defects", "Quakers", "Category.Two.Defects")

average_colombian_coffee <-
  df %>%
    filter(`Country.of.Origin` == "Colombia") %>%
    select(characteristics) %>%
    na.omit() %>%
    colMeans()

average_colombian_coffee <- as.data.frame(average_colombian_coffee)
average_colombian_coffee <- cbind(rep(10, length(average_colombian_coffee)), rep(0, length(average_colombian_coffee)), average_colombian_coffee)
average_colombian_coffee <- t(average_colombian_coffee)
average_colombian_coffee <- as.data.frame(average_colombian_coffee)
radarchart(average_colombian_coffee)
