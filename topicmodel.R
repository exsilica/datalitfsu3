# Install XML library
# Get files from folder XMLAuthorCorpus
# Create vector of file names
library(XML)
inputDir <- "XMLAuthorCorpus"
files.v <- dir(path=inputDir, pattern=".*xml")

# Text segmentation
chunk.size <- 1000 # number of words per chunk

# Explain!
makeFlexTextChunks <- function(doc.object, chunk.size=1000, percentage=TRUE){ 
  paras <- getNodeSet(doc.object, "/d:TEI/d:text/d:body//d:p", 
                      c(d = "http://www.tei-c.org/ns/1.0"))
words <- paste(sapply(paras,xmlValue), collapse=" ") 
words.lower <- tolower(words)
words.lower <- gsub("[^[:alnum:][:space:]']", " ", words.lower) 
words.l <- strsplit(words.lower, "\\s+")
word.v <- unlist(words.l)
x <- seq_along(word.v)
if(percentage){
  max.length <- length(word.v)/chunk.size
  chunks.l <- split(word.v, ceiling(x/max.length)) } else {
    chunks.l <- split(word.v, ceiling(x/chunk.size)) #deal with small chunks at the end 
    if(length(chunks.l[[length(chunks.l)]]) <=
    length(chunks.l[[length(chunks.l)]])/2){ 
      chunks.l[[length(chunks.l)-1]] <-
      c(chunks.l[[length(chunks.l)-1]],
        chunks.l[[length(chunks.l)]])
    chunks.l[[length(chunks.l)]] <- NULL }
  }
chunks.l <- lapply(chunks.l, paste, collapse=" ") 
chunks.df <- do.call(rbind, chunks.l) 
return(chunks.df)
}

# Grab file names, remove extensions
textname <- gsub("\\..*","", files.v[i])

# 
topic.m <- NULL
for(i in 1:length(files.v)){
  doc.object <- xmlTreeParse(file.path(inputDir, files.v[i]), useInternalNodes=TRUE)
  chunk.m <- makeFlexTextChunks(doc.object, chunk.size, percentage=FALSE)
  textname <- gsub("\\..*","", files.v[i]) 
  segments.m <- cbind(paste(textname, segment=1:nrow(chunk.m), sep="_"), chunk.m) 
  topic.m <- rbind(topic.m, segments.m)
}

# Convert to data frame
# Rename column headers
documents <- as.data.frame(topic.m, stringsAsFactors=F) 
colnames(documents) <- c("id", "text")

# Install mallet
library(mallet)

mallet.instances <- mallet.import(documents$id, 
                                  documents$text,
                                  "stoplist.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

## Create a topic trainer object 
topic.model <- MalletLDA(num.topics=43)

# Add instances into topic model
topic.model$loadDocuments(mallet.instances)

# Test it out - check out our vocabulary!
vocabulary <- topic.model$getVocabulary()

# Word frequencies
word.freqs <- mallet.word.freqs(topic.model)

# Set number of iterations
topic.model$train(400)

# Interpreting our topic model

# Determine matrix size
topic.words.m <- mallet.topic.words(topic.model, 
                                    smoothed=TRUE,
                                    normalized=TRUE)
dim(topic.words.m)

# Set column names with our vocabulary list
vocabulary <- topic.model$getVocabulary() 
colnames(topic.words.m) <- vocabulary 
topic.words.m[1:3, 1:3]

# Let's look at specific keywords
keywords <- c("california", "ireland") 
topic.words.m[, keywords]

# Which row has highest concentration of key terms?
imp.row <- which(rowSums(topic.words.m[, keywords]) == 
                   max(rowSums(topic.words.m[, keywords])))

# View top words from a topic
mallet.top.words(topic.model, topic.words.m[imp.row,], 10)

# Visualization

# Install wordcloud library
library(wordcloud)

# Grab top 100 words
topic.top.words <- mallet.top.words(topic.model, topic.words.m[imp.row,], 100)

# Customize aesthetic options
wordcloud(topic.top.words$words, topic.top.words$weights,
          c(4,.8), rot.per=0, random.order=F)

