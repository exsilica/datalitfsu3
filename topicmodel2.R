# Exploratory Text Analysis in R
# Workshop by Carolyn Moritz and Sarah Stanley
# Last updated 2017/04/12

# SECTION 3: TOPIC MODELING

# Text segmentation (also called "chunking")
makeFlexTextChunks <- function(play.word.l, chunk.size=1000, percentage=TRUE){
  play.word.v <- unlist(play.word.l)
  x <- seq_along(play.word.v)
  if(percentage){
    max.length <- length(play.word.v)/chunk.size
    chunks.l <- split(play.word.v, ceiling(x/max.length))
  } else {
    chunks.l <- split(play.word.v, ceiling(x/chunk.size))
    # Deal with small chunks at the end
    if(length(chunks.l[[length(chunks.l)]]) <=
       length(chunks.l[[length(chunks.l)]])/2){ chunks.l[[length(chunks.l)-1]] <-
         c(chunks.l[[length(chunks.l)-1]],
           chunks.l[[length(chunks.l)]])
       chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l, paste, collapse=" ") 
  chunks.df <- do.call(rbind, chunks.l) 
  return(chunks.df)
}

textname <- play.title[i]

# Install mallet package
library(mallet)

topic.m <- NULL
for(i in 1:length(play.word.l)){
  chunk.m <- makeFlexTextChunks(play.word.l, chunk.size=1000, percentage=FALSE)
  textname <- play.title[i]
  segments.m <- cbind(paste(textname, segment=1:nrow(chunk.m), sep="_"), chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}

# Create a topic trainer object 
topic.model <- MalletLDA(num.topics=43)

topic.model$loadDocuments(play.word.v)

# Create a wordcloud
install.packages("wordcloud")
library(wordcloud)
