list.of.packages <- c("shiny","shinydashboard","DT","shinyjs","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)

contentWrapper<- "
.content-wrapper
{
background-color: #fff !imporatnt
}
"
divCSS <- "
#loading_indicator{
  background-image: url('loading.gif');
  background-repeat: no-repeat !important;
  background-size: cover !important;
  position: absolute;
  opacity: 0.7;
  z-index: 10000000 !important;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center
}
"

classTableBox <- "
.classTableBox{
margin-left: 45px;
margin-right: 45px;
border-top: solid #4d58ce;
border-top-width: medium;
border-top-width: 20px;
border-top-left-radius: 10px;
border-top-right-radius: 10px;
}
"

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Sentiment Analysis on Product Reviews", titleWidth = 400),
  dashboardSidebar(
    fileInput("file1", "Upload the text/csv file",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    ,
    sidebarMenu( id="sentimentAnalysisSideBar",
  menuItem("Original Table", tabName="originalTable"),
  menuItem("Normalized Table", tabName="normalizedTable"),
  menuItem("Word Frequency", tabName="wordFrequency"),
  menuItem("WordCloud", tabName = "wordCloud"),
  menuItem("Review Sentiment",tabName="reviewSentiment"),
  menuItem("Product Sentiment",tabName="productSentiment"),
  menuItem("Most Reviewed Products",tabName="mostReviewed"),
  menuItem("Scatter Plots",tabName="scatterPlots"),
  menuItem("Inference",tabName="inference")
  )),
  dashboardBody(
    useShinyjs(),
    inlineCSS(divCSS),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(tags$script(src = "lib.js")),
    tags$div(id="loading_indicator"),
    
    tabItems(
      
      tabItem(tabName = "originalTable",
              fluidRow(style="margin-left: 45px;margin-right: 45px;margin-top:50px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
                       wellPanel(tags$h3("Original Table",style="#fffmargin-top:0px;padding-bottom:20px"),dataTableOutput("originalTableOutput")))
      ),
    tabItem(tabName = "normalizedTable",
            fluidRow(style="margin-left: 45px;margin-right: 45px;margin-top:50px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
              wellPanel(tags$h3("Normalized Table",style="#fffmargin-top:0px;padding-bottom:20px"),dataTableOutput("normalizedTableOutput")))
            ),
    tabItem(tabName = "wordFrequency",
            fluidRow(fluidRow( style="margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
                            wellPanel(  tags$h3("Raw Word Frequency",style="#fffmargin-top:0px;padding-bottom:20px"),dataTableOutput("rawWordFrequencyOutput"))
                            ),
                     fluidRow(style="margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
                            wellPanel(tags$h3("Normalized Word Frequency",style="margin-top:0px;padding-bottom:20px"),dataTableOutput("normalizedWordFrequencyOutput"))
                            ))
            
            ),
    
    tabItem(tabName = "wordCloud",
            fluidRow(column(5,style="margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;background-color:white",
                            wellPanel(style="background-color:white",tags$h3("Original Word Cloud",style="#fffmargin-top:0px;padding-bottom:20px"),plotOutput("origReviewsWCPlot"))
            ),
            column(5,style="margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;;background-color:white",
                   wellPanel(style="background-color:white",tags$h3("Normalized Word Cloud",style="#fffmargin-top:0px;padding-bottom:20px;  "),plotOutput("normalizedReviewsWCPlot"))
            ))
            
    ),
    
    tabItem(tabName = "reviewSentiment",
            fluidRow(style="margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
              wellPanel(tags$h3("Review Sentiment",style="#fffmargin-top:0px;padding-bottom:20px"),dataTableOutput("reviewSentimentOutput")))
    ),
    tabItem(tabName = "productSentiment",
            fluidRow( style="margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
                      wellPanel(tags$h3("Product Sentiment",style="#fffmargin-top:0px;padding-bottom:20px"),dataTableOutput("productSentimentOutput")))
    ),
    tabItem(tabName = "mostReviewed",
            fluidRow( style=" margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
                      wellPanel(tags$h3("Most Reviewed Products",style="#fffmargin-top:0px;padding-bottom:20px"),dataTableOutput("mostReviewedtOutput")))
    ),
    tabItem(tabName = "scatterPlots",
            fluidRow( style="margin-top:90px;margin-left: 45px;margin-right: 45px;border-top: solid #4d58ce;border-top-width: medium;border-top-width: 20px;border-top-left-radius: 10px;border-top-right-radius: 10px;",
              wellPanel(tags$h3("Scatter Plots",style="#fffmargin-top:0px;padding-bottom:20px"),plotlyOutput("scatterPlotstOutput")))
    ),
    tabItem(tabName = "inference",
            fluidRow( htmlOutput("inferenceOutput"))
    )
    )
  )
)