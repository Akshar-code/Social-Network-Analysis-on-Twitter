########################################################Libraries required ####################################################
library(shiny)
library(httr)
library(devtools)
library(base64enc)
library(twitteR)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tm)
library(wordcloud)
##########################################################Connection to Twitter API############################################
consumer_key ="0JgpQiHzLGoValA6EOh1Xp6Vp"
consumer_secret ="mBdajaSFvetDYLJgmH8O38vWjWFstQU95PjNVVyvTgnVLLDjx1"
access_token ="1168606699420356608-GCoTRb56H7Zn6twyALax3FDcddFqJA"
access_secret ="GWDUrew6ES8GiOvSk1GOpVqaqhJEYUGfJIFjbVoeiWUpW"
setup_twitter_oauth(consumer_key, consumer_secret,access_token,access_secret)
##########################################################Front End Code########################################################
ui <- fluidPage(theme = "lol.css",
  h1("Fandrum Ideathon Application"),
  wellPanel(
  fluidRow(
  column(3,textInput(inputId="topic",label="Topic")),
  column(3,numericInput(inputId="ntweet",label="Count",300))),
  radioButtons(inputId="citwit",label="Wanna see the tweets?",c("Yes"="one",
                                                                "No"="two")),
  selectInput(inputId="choice",label="What do you want today?",c("Histogram of node degree"="two",
                                                                 "Network Diagram"="three",
                                                                 "Community Detection"="four",
                                                                 "Wordcloud"="five",
                                                                 "Sentiment Analysis"="six")),
  checkboxInput(inputId="comalgo", "Enable Community detection"),
  conditionalPanel(
    condition = "input.comalgo == true",
    selectInput(inputId="choice1",label="Which Algorithm do you want to use?",c("Edge Betweeness"="one",
                                                                                "Label Propagation"="two",
                                                                                "Fast-greedy"="three")),
  )),

  tabsetPanel(              
    tabPanel(title = "Figure",
             plotOutput("mainstuff")
    ),
    tabPanel(title = "Tweets",
             textOutput("tweets")
    )
  )
)
########################################################Backend Code###########################################################
server <- function(input,output){
  output$mainstuff <- renderPlot({
    x<-reactive({input$choice})
    y<-reactive({input$comalgo})
    a<-reactive({input$choice1})
    tweets <- searchTwitter(input$topic,n=input$ntweet,lang='en')
    tweetsdf <- twListToDF(tweets)
    write.csv(tweetsdf,file="appexperiments.csv",row.names=F)
    data <- read.csv("appexperiments.csv", header = T)
    str(data)
    # Build corpus
    corpus <- iconv(data$text, to='UTF-8', sub = "byte")
    corpus <- Corpus(VectorSource(corpus))
    
    #####################################################Clean text##################################################
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    cleanset <- tm_map(corpus, removeWords, stopwords('english'))
    #removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
    #cleanset <- tm_map(cleanset, content_transformer(removeURL))
    cleanset <- tm_map(cleanset, stripWhitespace)
    
    ###############################################Term document matrix###############################################
    tdm <- TermDocumentMatrix(cleanset)
    tdm <- as.matrix(tdm)
    tdm <- tdm[rowSums(tdm)>10,]
    
    #################################################Network of terms#################################################
    library(igraph)
    tdm[tdm>1] <- 1
    termM <- tdm %*% t(tdm)
    termM[1:10,1:10]
    g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
    g
    g <- simplify(g)
    V(g)$label <- V(g)$name
    V(g)$degree <- degree(g)
    #show the tweets
    #############################################Histogram of node degree#############################################
    if(x()=="two"){
    hist(V(g)$degree,
         breaks = 100,
         col = 'red',
         main = 'Histogram of Node Degree',
         ylab = 'Frequency',
         xlab = 'Degree of Vertices')}
    
    ##################################################Network diagram#################################################
    if(x()=="three"){
    set.seed(222)
    plot(g)
    plot(g,
         vertex.color='green',
         vertex.size = 10,
         vertex.label.dist = 1.5,
         vertex.label=NA)}
    
    ################################################Community detection###############################################
    if(y()== TRUE){
      if(a()=="one"){
      #1ST ALGORITHM
      comm <- cluster_edge_betweenness(g)
      plot(comm, g)}
      else if(a()=="two"){
      #2ND ALGORITHM
      prop <- cluster_label_prop(g)
      plot(prop, g,vertex)}
      else if(a()=="three"){
      #3RD ALGORITHM
      greed <- cluster_fast_greedy(as.undirected(g))
      plot(greed, as.undirected(g))}
    }
    #######################################################wordcloud###################################################
    if(x()=="five"){
    w <- sort(rowSums(tdm), decreasing = TRUE)
    #set.seed for repeatability
    set.seed(222)
    wordcloud(words = names(w),
              freq = w,
              max.words = 150,
              random.order = F,
              min.freq = 5,
              colors = brewer.pal(8, 'Dark2'),
              scale = c(5, 0.3),
              rot.per = 0.3)}
    #####################################################sentiment analysis#############################################
    if(x()=="six"){
      data <- read.csv("appexperiments.csv", header = T)
      tweets <- iconv(data$text, to = 'UTF-8')
      # Obtain sentiment scores
      s <- get_nrc_sentiment(tweets)
      # Bar plot
      barplot(colSums(s),
              las = 2,
              col = rainbow(10),
              ylab = 'Count',
              main = 'Sentiment Scores for data Tweets')}
    })
  output$tweets <- renderPrint({
    z<-reactive({input$citwit})
    if(z()=="one"){
      tweets <- searchTwitter(input$topic,n=input$ntweet,lang='en')
      tweets
    }
  })
}

shinyApp(ui =ui,server= server)
