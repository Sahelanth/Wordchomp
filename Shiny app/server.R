server <- function(input, output){
  
  #libraries
  library(stringr)
  library(data.table)
  library(wordcloud)
  library(xtable)
  
  #Acquire data
  load("sortedallquads100trimmed5.Rdata")

  #Define predictword
  predictword <- function(input){
    matchfound <- FALSE
    
    #Process input.
    #Tolower, remove everything that's not a letter or space, remove multiple spaces
    input <- input %>% tolower %>% str_remove_all("[^a-z ]") %>% 
      str_replace_all(" +", " ") %>%
      #Break input string into sub-strings, one sub-string for each word. 
      #Take the last 3 words.
      strsplit(split=" ") %>% unlist() %>% tail(3) %>% 
      #Then reassemble, adding a trailing space at the end.
      paste(collapse=" ") %>% paste(" ", sep="")
    
    #Implementation of stupid backoff. Katz backoff worked well for
    #rare phrases, but used too much memory when calculating the alphas for
    #common phrases: I decided to prioritize common phrases. Good-Turing smoothing
    #didn't improve my performance in the absence of Katz, and storing the
    #probabilities was more expensive than storing integer frequencies.
    
    
    #Find words.
    while(matchfound==FALSE){
      #Return the most frequent predictions
      prediction <- sortedall[startsWith(sortedall$feature, input),][1:20]
      
      #If prediction is good or it's chewed down to a blank, break the loop.
      if(!is.na(unlist(prediction[1,1]))) {matchfound <- TRUE}
      if(!isTRUE(str_detect(input, "[a-z]"))){matchfound <- TRUE}
      
      #If prediction is empty, chew off the first word and try again
      if (is.na(unlist(prediction[1,1])) & input !=" ") {
        input <- str_remove(input, "[a-z]+ ")
      }
    }
    
    
    #Return just the last word of the prediction.
    for(i in 1:nrow(prediction)){
      prediction[i,1] <- unlist(prediction[i,1]) %>% strsplit(split=" ") %>% unlist() %>% tail(1)
    }
    prediction[,2] <- (prediction[,2]*(-1))
    cloud <- wordcloud(words=na.omit(prediction$feature), freq=na.omit(prediction$frequency, min.freq=1, max.words=20))
    freqsums <- sum(na.omit(prediction[,2]))
    for(i in 1:nrow(prediction)){
    prediction[i,2] <- round(100*prediction[i,2]/freqsums, 2)
    }
    names(prediction) <- c("Word", "Confidence")

    result <- list(prediction, cloud)
    
    return(result)
    
  }

  #Produce output

  output$table <- renderTable({
    xtable(predictword(input$caption)[[1]])
  }, rownames=TRUE)
  
  output$cloud <- renderPlot({predictword(input$caption)[[2]]})
  
}
