#Source data sets
blogdata <- readLines("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt",warn = FALSE, skipNul = TRUE)
newsdata <- readLines("./Coursera-SwiftKey/final/en_US/en_US.news.txt",warn = FALSE, skipNul = TRUE)
twitdata <- readLines("./Coursera-SwiftKey/final/en_US/en_US.twitter.txt",warn = FALSE, skipNul = TRUE)

alldata <- c(blogdata,newsdata,twitdata)
rm(blogdata)
rm(newsdata)
rm(twitdata)
#sample portion of the data for analysis

set.seed(1382)
alldata <- sample(alldata,length(alldata)*.1, replace = FALSE)

library(tm)
datacorpus <- VCorpus(VectorSource(alldata))


#create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
datacorpus <- tm_map(datacorpus, toSpace, "-")
datacorpus <- tm_map(datacorpus, toSpace, ":")


datacorpus <-  tm_map(datacorpus, removePunctuation)
datacorpus <- tm_map(datacorpus, removeNumbers)
datacorpus <- tm_map(datacorpus, stripWhitespace)
datacorpus <- tm_map(datacorpus,content_transformer(tolower))
#datacorpus <- tm_map(datacorpus, removeWords, stopwords("english"))

save(datacorpus, file="datacorpus.Rdata")

options(mc.cores=2) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
library(RWeka)

BiTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # create n-grams
bigram <- TermDocumentMatrix(datacorpus, control = list(tokenize = BiTokenizer)) # create tdm from n-grams

TriTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) # create n-grams
trigram <- TermDocumentMatrix(datacorpus, control = list(tokenize = TriTokenizer)) # create tdm from n-grams

QuadTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4)) # create n-grams
quadgram <- TermDocumentMatrix(datacorpus, control = list(tokenize = QuadTokenizer)) # create tdm from n-grams

save(bigram, file="bigram.rdata")
save(trigram, file="trigram.rdata")
save(quadgram, file="quadgram.rdata")

library(dplyr)
rsf <- slam::row_sums(bigram)
bigramdf <- data.frame(term=names(rsf),freq=rsf, stringsAsFactors = FALSE)
bigramdf <- arrange(bigramdf,desc(freq))
rm(rsf)

rsf <- slam::row_sums(trigram)
trigramdf <- data.frame(term=names(rsf),freq=rsf, stringsAsFactors = FALSE)
trigramdf <- arrange(trigramdf,desc(freq))
rm(rsf)

rsf <- slam::row_sums(quadgram)
quadgramdf <- data.frame(term=names(rsf),freq=rsf, stringsAsFactors = FALSE)
quadgramdf <- arrange(quadgramdf,desc(freq))
rm(rsf)

gram2 <- bigramdf[bigramdf$freq>=1,]
gram3 <- trigramdf[trigramdf$freq>=1,]
gram4 <- quadgramdf[quadgramdf$freq>=1,]

s <- ls()
rm(list=s[-grep("^gram",ls())])

str_trim <- function(str_in){
    str_in <- trimws(str_in)
    str_in <- gsub("[[:space:]]+"," ",str_in, ignore.case = TRUE)
    str_in <- tolower(str_in)
    return(str_in)
}

gram2[,1] <- sapply(gram2[,1],str_trim, USE.NAMES = FALSE)
gram3[,1] <- sapply(gram3[,1],str_trim, USE.NAMES = FALSE)
gram4[,1] <- sapply(gram4[,1],str_trim, USE.NAMES = FALSE)

save(gram2,gram3,gram4,file="grams.rdata")

#spit into columns
library(tidyr)
ngram2 <- data.frame(term1=character(),term2=character(),freq=numeric(), stringsAsFactors = FALSE)
ngram2 <- tidyr::separate(gram2, term, c("term1", "term2"), " ", fill = "left")

ngram3 <- data.frame(term1=character(),term2=character(),term3=character(),freq=numeric(), stringsAsFactors = FALSE)
ngram3 <- tidyr::separate(gram3, term, c("term1", "term2","term3"), " ", fill = "left")

ngram4 <- data.frame(term1=character(),term2=character(),term3=character(),term4=character(),freq=numeric(), stringsAsFactors = FALSE)
ngram4 <- tidyr::separate(gram4, term, c("term1", "term2","term3","term4"), " ", fill = "left")

save(ngram2,ngram3,ngram4, file="ngram.rdata")

s.ngram2 <- ngram2[ngram2$freq>1,]
s.ngram3 <- ngram3[ngram3$freq>1,]
s.ngram4 <- ngram4[ngram4$freq>1,]

rm(list=ls()[-grep("^s.ngram",ls())])
