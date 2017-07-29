library(shiny)

server <- function(input, output) {
  
  library(twitteR)
  library(ROAuth)
  library(tm)
  
  api_key <-       "6M0UlYMcAKwEKTemVhpncZUqK"
  api_secret <-   "kjHcODg4AEgM9pChZRUyENgAdJrACmOiZkWhrfgnoO6DdCCPbb"
  access_token <- "558595755-YTcAFgolY8qxzMpmLzkng7ML0NUVlYMtYxdc4CM1"
  access_token_secret <-  "JSPW0eRDLNcow8BFYMhNiVwgSdxj9qhE2uvP0a2RMCMfg"
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  
  output$distPlot <- renderPlot({
    tweets <- searchTwitter(input$text, n = 500, lang = "en")
    tweetsDF <- twListToDF(tweets)
    myCorpus <- Corpus(VectorSource(tweetsDF$text))
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
    # conver to lowercase
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    # remove URLs
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
    # remove anything other than English letters or space
    
    # remove stopwords
    myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                     "use", "see", "used", "via", "amp", "retweet", "follow", "notifications", "rt")
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    # remove extra whitespace
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    # keep a copy for stem completion later
    myCorpusCopy <- myCorpus
    wordFreq <- function(corpus, word) {
      results <- lapply(corpus,
                        function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
      )
      sum(unlist(results))
    }
    replaceWord <- function(corpus, oldword, newword) {
      tm_map(corpus, content_transformer(gsub),
             pattern=oldword, replacement=newword)
    }
    tdm <- TermDocumentMatrix(myCorpus,
                              control = list(wordLengths = c(1, Inf)))
    tdm
    idx <- which(dimnames(tdm)$Terms %in% c("alienware", "dell"))
    idx
    (freq.terms <- findFreqTerms(tdm, lowfreq = 10))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq >= 10)
    df <- data.frame(term = names(term.freq), freq = term.freq)
    df
    
    
    library(RColorBrewer)
    m <- as.matrix(tdm)
    # calculate the frequency of words and sort it by frequency
    word.freq <- sort(rowSums(m), decreasing = T)
    # colors
    pal <- brewer.pal(9, "BuGn")[-(1:4)]
    # plot word cloud
    library(wordcloud)
    wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, rot.per = 0.35,
              random.order = F, max.words = 1000, colors = brewer.pal(8, "Dark2"), scale = c(3,0.5))
  })
  
  output$histPlot <- renderPlot({
    
    tweets <- searchTwitter(input$text, n = 500, lang = "en")
    tweetsDF <- twListToDF(tweets)
    myCorpus <- Corpus(VectorSource(tweetsDF$text))
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
    # conver to lowercase
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
    # remove URLs
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
    # remove anything other than English letters or space
    
    # remove stopwords
    myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                     "use", "see", "used", "via", "amp", "retweet", "follow", "notifications", "rt")
    myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
    # remove extra whitespace
    myCorpus <- tm_map(myCorpus, stripWhitespace)
    # keep a copy for stem completion later
    myCorpusCopy <- myCorpus
    wordFreq <- function(corpus, word) {
      results <- lapply(corpus,
                        function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
      )
      sum(unlist(results))
    }
    replaceWord <- function(corpus, oldword, newword) {
      tm_map(corpus, content_transformer(gsub),
             pattern=oldword, replacement=newword)
    }
    tdm <- TermDocumentMatrix(myCorpus,
                              control = list(wordLengths = c(1, Inf)))
    tdm
    idx <- which(dimnames(tdm)$Terms %in% c("alienware", "dell"))
    idx
    (freq.terms <- findFreqTerms(tdm, lowfreq = 10))
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq >= 10)
    df <- data.frame(term = names(term.freq), freq = term.freq)
    df

    # sentiment analysis
    library(sentiment)
    sentiments <- sentiment(tweetsDF$text)
    output$sentimentResults <- renderText({
      paste("Negative: ", as.String(table(sentiments$polarity)[1]), "          Neutral: ",as.String(table(sentiments$polarity)[2]), "          Positive: ",as.String(table(sentiments$polarity)[3]))
    })
    ##
    ## neutral positive
    ## 428 sentiment20
    #  plot
    sentiments$score <- 0
    sentiments$score[sentiments$polarity == "positive"] <- 1
    sentiments$score[sentiments$polarity == "negative"] <- -1
    sentiments$date <- as.Date(tweetsDF$created)
    result <- aggregate(score ~ date, data = sentiments, sum)
    # plot(result, type = "l")
    # result
    
    library(RColorBrewer)
    m <- as.matrix(tdm)
    # calculate the frequency of words and sort it by frequency
    word.freq <- sort(rowSums(m), decreasing = T)
    # colors
    pal <- brewer.pal(9, "BuGn")[-(1:4)]
    # plot word cloud
    library(wordcloud)
    
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq >= 20)
    df <- data.frame(term = names(term.freq), freq = term.freq)
    14 / 40
    library(ggplot2)
    ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
      xlab("Terms") + ylab("Count") + coord_flip() +
      theme(axis.text=element_text(size=7))
    
  })
}

ui <- fluidPage(
  
  titlePanel("Twitter Sentiment Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = h3("Search Hashtags"), 
                value = "alienware"),
      submitButton("Submit"),
      br(),
      h4("Sentiment:"),
      textOutput("sentimentResults"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      p("This app was written in R, it will extract 500 Tweets based on your keyword and generate a word cloud and frequency histogram, and a simple sentiment analysis."),
  
      br(),
      strong("Takes a few seconds to load"),
      p("Sanidhya Singh, 2017")
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("Word Cloud and Frequency"),
      fluidRow(
        splitLayout(cellWidths = c("40%", "60%"), plotOutput("distPlot"), plotOutput("histPlot"))
      )
  )
))

shinyApp(ui = ui, server = server)