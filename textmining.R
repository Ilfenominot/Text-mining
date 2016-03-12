textmining <- function(x){
  review_text   <- paste(x, collapse=" ")
  review_source <- VectorSource(review_text)
  corpus        <- Corpus(review_source)
  corpus        <- tm_map(corpus, content_transformer(tolower))
  corpus        <- tm_map(corpus, removeWords, stopwords("english"))
  dtm           <- DocumentTermMatrix(corpus)
  dtm2          <- as.matrix(dtm)
  frequency     <- colSums(dtm2)
  frequency     <- sort(frequency, decreasing=TRUE)
  return(frequency)
}