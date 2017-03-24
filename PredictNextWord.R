##SOURCE FUNCTION TO PREDICT NEXT WORD takes inputs from the GenerateNGrams.R
##Input: ngrams created by the GenerateNGrams.R source file

##Load the ngrams stored as RDS to conserve space
s.ngram2 <- readRDS("gram2.rds")
s.ngram3 <- readRDS("gram3.rds")
s.ngram4 <- readRDS("gram4.rds")

## function to clean the input text corpus of all punctionations, whitespace and numbers
CleanText <- function(txt, clean = TRUE) {
    if (clean){
        txt <- gsub("-", " ", txt)
        txt <- gsub(":", " ", txt)
        txt <- gsub("[[:punct:]]+"," ",txt, ignore.case = TRUE)
        txt <- gsub("[[:digit:]]+"," ",txt, ignore.case = TRUE)
        txt <- gsub("[[:space:]]+"," ",txt, ignore.case = TRUE)
        txt <- tolower(txt)
        txt <- trimws(txt)
    }
    txt
}

##function to get the words from the input phrase and return it in reverse order
##e.g. I love you will be returned as txt0[1] <- "you" txto[2] <- "love" txto[3] <- "I"
getWords <- function(txt, seperator = " ") {
    txto <- character()
    txt <- CleanText(txt)
    txtElems <- strsplit(txt, seperator)[[1]]
    lt <- length(txtElems)
    if(lt>0){
        for(i in lt:1){
            txto <- c(txto,txtElems[i])
        }
    }
    txto
}

##Main function to return the predicted words form saved ngrams
predictNextWord <- function(txt){
    txtin <- getWords(txt)
    predwords <- character()
    newbigram <- data.frame()
    if(is.na(txtin[3])==FALSE){ #checking if there are three words
        ##Match to the 3rd word from the last
        #store the matched indexex
        matindex <- which(s.ngram4$term1==txtin[3] & s.ngram4$term2==txtin[2] &  s.ngram4$term3==txtin[1])
        if(length(matindex)>0){
            predwords <- s.ngram4[matindex,  ]$term4
            #increase the frequency of the combination to remember the repeated queries
            s.ngram4[matindex,]$freq<- s.ngram4[matindex,]$freq + 1
            if(length(matindex)>3){
                predwords <- predwords[1:3]
            }
        }
        #store the input text as a bigram for future learning
        newbigram <- data.frame(term1=txtin[2], term2=txtin[1], stringsAsFactors = FALSE)
    }else if(is.na(txtin[2])==FALSE){ #checking if there are 2 words
        ##Match to the 2nd word from the last
        matindex <- which(s.ngram3$term1==txtin[2] & s.ngram3$term2==txtin[1])
        if(length(matindex)>0){
            predwords <- s.ngram3[matindex,  ]$term3
            s.ngram3[matindex,]$freq <- s.ngram4[matindex,]$freq+1
            if(length(matindex)>3){
                predwords <- predwords[1:3]
            }
        }
        #store the input text as a bigram for future learning
        newbigram <- data.frame(term1=txtin[2], term2=txtin[1], stringsAsFactors = FALSE)
    }
    else if(is.na(txtin[1])==FALSE){
        ##Match to the 1st word from the last
        matindex <- which(s.ngram2$term1==txtin[1])
        if(length(matindex)>0){
            predwords <- s.ngram2[matindex,  ]$term2
            s.ngram2[matindex,]$freq <- s.ngram2[matindex,]$freq +1
            if(length(matindex)>3){
                predwords <- predwords[1:3]
            }
        }
    }
    #if there are no predicted words then create a new bigram with the input text and store
    if(length(predwords)==0 & nrow(newbigram)>0){
        #check if the bigram already exists
        foundindex <- which(s.ngram2$term1==newbigram[1,]$term1 & s.ngram2$term2==newbigram[1,]$term2)
        if(length(foundindex)==0){ #if term not there then add the new term
            newfreq <- max(s.ngram2$freq)+1
            newbigram <- cbind(newbigram,freq=newfreq)
            assign("s.ngram2",rbind(newbigram,s.ngram2),envir = .GlobalEnv)
            predwords[4] <- paste(newbigram[1,]$term1,newbigram[1,]$term2, sep = " ")
        }
    }
    
    ## convert all NA outputs to empty strings to avoid publishing NA
    if(is.na(predwords[1])){
        predwords[1] <- ""
    }
    if(is.na(predwords[2])){
        predwords[2] <- ""
    }
    if(is.na(predwords[3])){
        predwords[3] <- ""
    }
    if(is.na(predwords[4])){
        predwords[4] <- ""
    }
    predwords
}
