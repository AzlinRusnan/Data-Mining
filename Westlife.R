lyric1 <- c(
  "An empty street, an empty house",
  "A hole inside my heart",
  "I'm all alone, the rooms are getting smaller",
  "I wonder how, I wonder why",
  "I wonder where they are",
  "The days we had, the songs we sang together",
  "Oh, yeah",
  "And oh, my love",
  "I'm holding on forever",
  "Reaching for the love that seems so far",
  "So, I say a little prayer",
  "And hope my dreams will take me there",
  "Where the skies are blue",
  "To see you once again, my love",
  "Overseas, from coast to coast",
  "To find a place I love the most",
  "Where the fields are green",
  "To see you once again",
  "My love",
  "I try to read, I go to work",
  "I'm laughing with my friends",
  "But I can't stop to keep myself from thinking, oh no",
  "I wonder how, I wonder why",
  "I wonder where they are",
  "The days we had, the songs we sang together", 
  "oh, yeah",
  "And oh, my love",
  "I'm holding on forever",
  "Reaching for the love that seems so far",
  "So, I say a little prayer",
  "And hope my dreams will take me there",
  "Where the skies are blue",
  "To see you l once again, my love",
  "Overseas, from coast to coast",
  "To find a place I love the most",
  "Where the fields are green",
  "To see you once again",
  "To hold you in my arms",
  "To promise you my love",
  "To tell you from the heart",
  "You're all I'm thinking of",
  "I'm reaching for the love that seems so far",
  "So, I say a little prayer",
  "And hope my dreams will take me there",
  "Where the skies are blue",
  "To see you once again, my love",
  "Overseas, from coast to coast",
  "To find the place I love the most",
  "Where the fields are green",
  "To see you once again",
  "(My love)",
  "Say a little prayer (my sweet love)",
  "Dreams will take me there",
  "Where the skies are blue (woah, yeah)",
  "To see you once again",
  "Overseas, from coast to coast",
  "To find the place I love the most",
  "Where the fields are green",
  "To see you once again",
  "My love"
)

#data frame
lyrics_df <- data.frame(
  Lyric = lyric1,
  stringsAsFactors = FALSE
)

#extracting the lyrics to csv file
write.csv(lyrics_df, "My_Love_Lyrics.csv", row.names = FALSE)

## Load the data
my_love <- read.csv(file.choose())
head(my_love)

#1. Data Cleaning/Preprocessing:
library(NLP)
library(tm)  # for text mining
library(SnowballC)  # for stopwords

# Create a corpus
corpus <- Corpus(VectorSource(my_love$Lyric))

# Data cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# View cleaned data
inspect(corpus)

#2. Document Term Matrix and Frequency Tabulation:
#Convert the text data to a document term matrix and find the frequency of terms
dtm <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm))
freq <- sort(freq, decreasing = TRUE)
top_freq <- head(freq, 5)  # adjust number for more terms

# Display top frequencies
print(top_freq)

#3. Word Cloud:
#Create a word cloud in any shape you prefer.
library(RColorBrewer)
library(wordcloud)

# Generate a word cloud
wordcloud(names(freq), freq, scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
