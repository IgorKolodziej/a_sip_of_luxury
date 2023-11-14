library(wordcloud2)
library(RColorBrewer)
library(tm)
library(dplyr)

df <- read.csv("data/coffee_reviews_dataset/coffee_analysis.csv", stringsAsFactors = FALSE)
reviews <- df$desc_1
docs <- Corpus(VectorSource(reviews))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
words_df <- data.frame(word = names(words),freq=words)

# COLOR PALETTE
my_palette <- function(n) {
  # Get the original palette colors
  orig_colors <- brewer.pal(9, "YlOrBr")
  # Remove the first two colors, which are too light
  orig_colors <- orig_colors[-c(1, 2)]
  # Darken the remaining colors by reducing the brightness
  new_colors <- col2rgb(orig_colors) * 0.8
  # Convert the colors back to hexadecimal codes
  # Divide the values by 255 to make them between 0 and 1
  new_colors <- rgb(new_colors[1, ] / 255, new_colors[2, ] / 255, new_colors[3, ] / 255)
  # Return the interpolated colors
  colorRampPalette(new_colors)(n)
}

palette1 <- my_palette(5)
palette2 <- c("#CD7F32", "#A67B5B", "#836953", "#966919", "#704214")
palette3 <- c("#3e1e04", "#6a3005", "#965015", "#c4923e", "#cbac85")
palette4 <- brewer.pal(n = 5, name = "YlOrBr")

# WORD CLOUD
wordcloud2(words_df, size = 1.5, color = palette1, backgroundColor = "white", shape = "circle", minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.5)
wordcloud2(words_df, size = 1.5, color = palette2, backgroundColor = "white", shape = "circle", minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.5)
wordcloud2(words_df, size = 1.5, color = palette3, backgroundColor = "white", shape = "circle", minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.5)
wordcloud2(words_df, size = 1.5, color = palette4, backgroundColor = "white", shape = "circle", minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.5)

library(htmlwidgets)
install.packages("webshot")
webshot::install_phantomjs()
library(wordcloud2)
hw <- wordcloud2(demoFreq,size = 3)
saveWidget(w1,"./plots/word_cloud/palette1.png",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)
