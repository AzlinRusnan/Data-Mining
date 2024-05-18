# Loading libraries
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(ggplot2)

# Importing the dataset
lyrics <- read.csv(file.choose())

# Data cleaning and preprocessing
docs <- VCorpus(VectorSource(lyrics))
toSpace <- content_transformer(function(x,pattern){return(gsub(pattern," ",x))})
docs <- tm_map(docs,toSpace,"-")
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removeWords,stopwords("english"))
docs <- tm_map(docs,stripWhitespace)

# Convert to document-term matrix and find frequency
dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))

length(freq)
ord <- order(freq,decreasing=T)
head(ord)
freq[head(ord)]

#once we have all above, we can insert to data frame
wf <- data.frame(names(freq),freq)

# Renaming Column Name
names(wf) <- c("TERM","FREQ")
head(wf)

# Filter for at least five terms, sorted by frequency
findFreqTerms(dtm,lowfreq=5)
top_terms <- subset(wf,FREQ>=5)
top_terms_sorted <- top_terms[order(-top_terms$FREQ), ]

# Print top terms and their frequencies
print(top_terms_sorted)

# Term Frequency Analysis(>=5)
ggplot(top_terms_sorted, aes(x = reorder(TERM, -FREQ), y = FREQ, fill = FREQ)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  labs(title = "Frequency of Terms",
       x = "Terms",
       y = "Frequency",
       fill = "Frequency") +
  geom_text(aes(label = FREQ), vjust = -0.3, color = "white") +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Wordcloud
colors <- colorRampPalette(brewer.pal(9, "Blues"))(50)  # Creating a gradient from light to dark blue

set.seed(12)
wordcloud(words = wf$TERM, freq = wf$FREQ, min.freq = 1, max.words = 50,
          random.order = FALSE, rot.per = 0.3, colors = colors,
          family = "serif", scale = c(4, 0.5), bg = "black")


