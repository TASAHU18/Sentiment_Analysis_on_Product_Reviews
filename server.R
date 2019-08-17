list.of.packages <- c("shiny","DT","readr","dplyr", "tidytext", "tm", "textstem", "wordcloud", "data.table","ggplot2","ggthemes","plotly","pbapply")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(DT)
library(readr)
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(wordcloud)
library(data.table)
library(ggplot2)
library(ggthemes)
library(plotly)
library(pbapply)
op <- pboptions(type = "timer")

set.seed(1234)

dataBox=list()
sentiment_dict <- get_sentiments("afinn")
sentiment_dict_dt <- data.table(sentiment_dict)


options(shiny.maxRequestSize=3000*1024^2) 
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data <- reactive({
    file <- input$file1
    if(is.null(file)){return()} 
    reviews= read_csv(file=file$datapath)
    reviews
  })
  
  ############################
  #####      Outputs     ##### 
  ############################
  
  
  output$originalTableOutput <- renderDataTable(selection = 'single' ,options = list(scrollX = TRUE ,pageLength = 10), filter = 'top',{
    
    if( is.null (dataBox$reviews))
    {
      createAndAppendBox()
    }
    ####Displaying Id, ProductId, UserId, Review and Normalized review table####
    ####--------------------------------------------------------------------####
    
    ####Moved to createAndAppendBox
    ###------------------------------------------------------------------------------------####
    dataBox$reviews
  });
  
  
  output$normalizedTableOutput <- renderDataTable(selection = 'single' ,options = list(scrollX = TRUE ,pageLength = 10), filter = 'top',{
  
    if( is.null (dataBox$reviews))
    {
      createAndAppendBox()
    }
    ####Displaying Id, ProductId, UserId, Review and Normalized review table####
    ####--------------------------------------------------------------------####
   
    ####Moved to createAndAppendBox
    ###------------------------------------------------------------------------------------####
    dataBox$norm_table
  });
  
  
  output$rawWordFrequencyOutput<-renderDataTable(selection = 'single', class = 'table-bordered' ,options = list(scrollX = TRUE ,pageLength = 5), filter = 'top', { 
    if(is.null(dataBox$review_orig))
    {
      createAndAppendBox()
    }
    rawWordFrequency()
  });
  
  output$normalizedWordFrequencyOutput<- renderDataTable(selection = 'single' ,options = list(scrollX = TRUE ,pageLength = 5), filter = 'top', { 
    if(is.null(dataBox$review_norm))
    {
      createAndAppendBox()
    }
    normalizedWordFrequency()
  });
  
  observe({
    if(input$sentimentAnalysisSideBar  =="wordCloud")
    {
      if(is.null(dataBox$review_norm))
      {
        createAndAppendBox()
      }
      
      if(is.null(dataBox$word_freq_raw))
      {
        rawWordFrequency() 
      }
      output$origReviewsWCPlot<- renderPlot({ 
        wc(dataBox$word_freq_raw$word,dataBox$word_freq_raw$freq)
      })  ;
    }
  })
  
  observe({
    if(input$sentimentAnalysisSideBar  =="wordCloud")
    {
      if(is.null(dataBox$review_norm))
      {
        createAndAppendBox()
      }
      
      if(is.null(dataBox$word_freq_norm))
      {
        normalizedWordFrequency()
      }
      output$normalizedReviewsWCPlot<- renderPlot({ 
        wc(dataBox$word_freq_norm$word,dataBox$word_freq_norm$freq)
      })  ;
    }
  })
  
  
  output$reviewSentimentOutput <- renderDataTable(selection = 'single',options = list(scrollX = TRUE ,pageLength = 5), filter = 'top',
    {
      if(is.null(dataBox$review_norm))
      {
        createAndAppendBox()
      }
      getReviewSentiment()
    }
  )
  
  
  output$productSentimentOutput <- renderDataTable(selection = 'single' ,options = list(scrollX = TRUE ,pageLength = 5), filter = 'top',{
    if(is.null(dataBox$review_norm))
    {
      createAndAppendBox()
    }
    if( ! "Sentiment_Score" %in% colnames(dataBox$norm_table))
    {
      getReviewSentiment()
    }
    getProductSentiment() 
  })
  
  
  
  output$mostReviewedtOutput <- renderDataTable(selection = 'single' ,options = list(scrollX = TRUE ,pageLength = 5), filter = 'top',
    {
      if(is.null(dataBox$review_norm))
      {
        createAndAppendBox()
      }
      if( ! "Sentiment_Score" %in% colnames(dataBox$norm_table))
      {
        getReviewSentiment()
      }
      if(is.null(dataBox$reviewCounts))
      {
      getProductSentiment()
      }
      Top6 <- head(dataBox$reviewCounts %>% arrange(.,desc(User_Count) ),6)
      Top6
    }
  )
  
  
  output$scatterPlotstOutput <- renderPlotly(
    {
      if(is.null(dataBox$review_norm))
      {
        createAndAppendBox()
      }
      if( ! "Sentiment_Score" %in% colnames(dataBox$norm_table))
      {
        getReviewSentiment()
      }
      if(is.null(dataBox$reviewCounts))
      {
        getProductSentiment()
      }
      ggplotly(getTop6ProductsScatter())
    }
    )
  
  
  output$inferenceOutput <-  renderUI({
    print(getwd())
    my_test <- tags$iframe(src="Conclusion.htm", height=1200, width=1375)
    print(my_test)
    my_test
  })
  

      

      
  ############################
  #####     Functions    ##### 
  ############################
  
  
  
  
  rawWordFrequency <- function()
  {
    
    #mapped_words=  token_words(dataBox$review_orig)
    word_freq= freq_word(dataBox$review_orig)
    dataBox$word_freq_raw <<- word_freq
    dataBox$word_freq_raw
  }
  
  
  normalizedWordFrequency <-  function()
  {
    
    mapped_words=  token_words(dataBox$review_norm)
    stem_word=lemmatize_words(mapped_words, dictionary = lexicon::hash_lemmas)
    words_freq<- sort(table(stem_word), decreasing = TRUE)
    words_freq=as.data.frame(words_freq)
    names(words_freq) <- c("word","freq")
    dataBox$word_freq_norm <<- words_freq
    dataBox$word_freq_norm

  }

  freqLapplyRaw<- function(sentence)
  {
    conv <-  gsub('[[:punct:] ]+',' ',sentence)
    conv <- tolower(conv)
    tokenize_sentence(conv)
  }
  
  freqLapplyNormal<- function(sentence)
  {
    token_review <- strsplit(sentence, " ")
    mapped_words <- token_review[[1]]
    stem_word <-lemmatize_strings(mapped_words, dictionary = lexicon::hash_lemmas)
  }
  
  getTop6ProductsScatter <- function()
  {
   
    Top6 <- head(dataBox$reviewCounts %>% arrange(.,desc(User_Count) ),6)
    scatter_table <- merge(dataBox$reviews, dataBox$norm_table, by = "Id")
    scatter_table <- scatter_table[,c("Id","ProductId.x","Score","Sentiment_Score") ]
    names(scatter_table) <- c("Id", "ProductId","Score","AfinnScore")
    scatter_table <-scatter_table[ (scatter_table$ProductId %in%   Top6$ProductId) ,]
    dataBox$scatter_table <<-scatter_table
    ggplot(data = dataBox$scatter_table,aes(y=Score, x=AfinnScore))+ geom_point()+facet_wrap(~ProductId)+theme_stata()
    
  }
  
  getReviewSentiment <- function()
  {
    
    sum=pblapply(pblapply(dataBox$review_norm$Text, sentimentList) ,sum)
    df= do.call(rbind.data.frame, sum)
    names(df)<-"score"
    df$ID <- row.names(df)
    dataBox$norm_table$score <<- df$score
    names(dataBox$norm_table) <<- c("Id","ProductId","UserId","Actual Review", "Normalized Review","Sentiment_Score")
    dataBox$norm_table
    
  }
  
  getProductSentiment <- function()
  {
    reviewCounts=dataBox$norm_table %>% group_by(ProductId) %>% summarise(
      sentimentScore= mean(Sentiment_Score),
      count= n_distinct( UserId)
    )
    names(reviewCounts) <- c("ProductId","Average_Score","User_Count")
    dataBox$reviewCounts <<- reviewCounts
    as.data.frame(dataBox$reviewCounts)
    
  }
  
  sentimentList <- function(review_norm)
  {
    token_review <- lemmatize_words (tokenize_sentence(review_norm))
    vectorNew= vector();
    for(i in 1:length(token_review) )
    {
      word_score=sentiment_dict_dt[word==token_review[i] ]$score
      vectorNew <- c(vectorNew, word_score)
    }
    vectorNew
  }
  
 
  createAndAppendBox <- function()
    {
    
    dataBox$reviews <<- data()
    
    dataBox$review_orig <<- as.data.frame(dataBox$reviews[ ,c(10)])
    colnames(dataBox$review_orig) <<- c("Text")
    dataBox$review_norm <<- as.data.frame(dataBox$reviews[ ,c(1,10)])
    dataBox$review_norm$Text <<- cleanup(dataBox$review_norm$Text)
    
    norm_table <- merge(dataBox$reviews, dataBox$review_norm, by ='Id')
    norm_table <- norm_table[ ,c(1,2,3,10,11)]
    colnames(norm_table)[colnames(norm_table)=="Text.x"] <- "Actual review"
    colnames(norm_table)[colnames(norm_table)=="Text.y"] <- "Normalized review"
  
    names(norm_table)
    dataBox$norm_table <<- norm_table

    }
  
  
  
  cleanup <- function(tidy_txt){
    tidy_txt <- tolower(tidy_txt)
    tidy_txt <- gsub("<.*?>", " ", tidy_txt)
    tidy_txt <- gsub("<img src.*?>", " ", tidy_txt)
    tidy_txt <- gsub("[[:punct:]]","",tidy_txt)   
    tidy_txt <- gsub("[^a-zA-Z0-9 ]", " ", tidy_txt)
    tidy_txt <- gsub("http[^[:blank:]]+"," ",tidy_txt)     #removes html links
    tidy_txt <- gsub("[^[:alnum:]]"," ",tidy_txt)         #removes number (alphanumeric)
    tidy_txt <- gsub("[[:digit:]]","",tidy_txt) 
    tidy_txt <- gsub("\\[math\\]", " ", tidy_txt)        # text between [] refers to tags e.g. [math]
    tidy_txt <- gsub("\n", " ", tidy_txt)               # replace newline with a space
    tidy_txt <- gsub("\\s+", " ", tidy_txt)             # multiple spaces into one
    
    docs <- Corpus(VectorSource(tidy_txt))
    docs <- tm_map(docs, removeWords, stopwords("en"))
    rm_docs <- sapply(docs, function(i) i)
    normalized_docs <- data.frame(text = rm_docs, stringsAsFactors = FALSE)
    normalized_docs$text
    
  }
  
  tokenize_sentence <- function(review_data){
    token_review <- strsplit(review_data, " ")
    #tokenized_words <- as.list.data.frame(token_review$word)
    token_review[[1]]
  }
  


  token_words <- function(review_data){
    token_review <- review_data %>% unnest_tokens(word,Text)
    tokenized_words <- as.list.data.frame(token_review$word)
    tokenized_words
  }
  
  freq_word <- function(input_data) {
    tidy_txt <- paste(unlist(input_data), collapse =" ")
      tidy_txt <- tolower(tidy_txt)
      tidy_txt <- gsub("<.*?>", " ", tidy_txt)
      tidy_txt <- gsub("<img src.*?>", " ", tidy_txt)
      tidy_txt <- gsub("[^a-zA-Z0-9 ]", " ", tidy_txt)
      tidy_txt <- gsub("http[^[:blank:]]+"," ",tidy_txt)     #removes html links
      tidy_txt <- gsub("[[:punct:]]"," ",tidy_txt)          #removes punctuations
      tidy_txt <- gsub("[^[:alnum:]]"," ",tidy_txt)         #removes number (alphanumeric)
      tidy_txt <- gsub("\\[math\\]", " ", tidy_txt)        # text between [] refers to tags e.g. [math]
      tidy_txt <- gsub("\n", " ", tidy_txt)               # replace newline with a space
      tidy_txt <- gsub("\\s+", " ", tidy_txt)
      words<-strsplit(tidy_txt," ")
      words_freq<- sort(table(words), decreasing = TRUE)
       words_freq <- as.data.frame(words_freq)
      names(words_freq) <- c("word","freq")
      words_freq
  }
  
  as.sentiment.matrix <- function(x) {
    nr <- x$nrow
    nc <- x$ncol
    # nr and nc are integers. 1 is double. Double * integer -> double
    y <- matrix(vector(typeof(x$v), 1 * nr * nc), nr, nc)
    y[cbind(x$i, x$j)] <- x$v
    dimnames(y) <- x$dimnames
    y
  }
  
  
  wc <- function(words, freq){
    word_cloud <- wordcloud(words, freq, min.freq = 1,
                            max.words=200, random.order=FALSE, rot.per=0.35, 
                            colors=brewer.pal(8, "Dark2"))
  }
  
  
  
  
})
