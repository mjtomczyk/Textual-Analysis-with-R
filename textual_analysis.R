# Install Packages
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

# Load Libraries
library("readr")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Import data
filePath <- "2019_ar.txt"
BoA2019 <- readLines(filePath)
docs <- Corpus(VectorSource(BoA2019)) # Load the data as a corpus

# Text Preprocessing & Cleaning Text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower)) # Convert all text to lower case
docs <- tm_map(docs, removePunctuation) # Remove punctuations
docs <- tm_map(docs, removeNumbers) # Remove numbers
docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords

# Build a Term-document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

# Generate Word Cloud
set.seed(1234)
png("wordcloud_BoA2019.png", width=12,height=8, units='in', res=300)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=c("#0076A8", "#00A3E0", "#C4D600", "#43B02A", 
                   "#A0DCFF", "#97999B", "#C4D600", "#62B5E5", 
                   "#009A44"))
dev.off()
# Plot Word frequencies
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="#0097A7", main ="Most frequent words",
        ylab = "Word frequencies")

#Find most common associations with the word credit
findAssocs(dtm, terms = "credit", corlimit = 0.3)
