library(XML)
library(tm)
directory <- "data"
files.v <- dir(path=directory, pattern=".*xml")
plays.lower.l <- list()
for(i in 1:length(files.v)){
  document <- xmlTreeParse(file.path(directory, files.v[i]), useInternalNodes = TRUE)
  play.texts.l <- paste(xpathSApply(document, "/tei:TEI//tei:sp/tei:l/text() | /tei:TEI//tei:sp/tei:ab/text() | /tei:TEI//tei:sp/tei:p/text()", xmlValue, namespaces = c(tei="http://www.tei-c.org/ns/1.0")), collapse=" ")
  plays.lower.l[[files.v[i]]] <- tolower(play.texts.l)
}
merchant.v <- plays.lower.l$mev.xml
merchant.words.v <- strsplit(merchant.v, "\\W")
merchant.words.v <- unlist(merchant.words.v)
merchant.words.v <- merchant.words.v[which(merchant.words.v!="")]
antonios.v <- merchant.words.v[which(merchant.words.v == "antonio")]
length(antonios.v)
merchant.words.v[1:10]
merchant.freqs.v <- table(merchant.words.v)