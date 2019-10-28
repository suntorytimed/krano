# originally released under CC-BY-4.0 by Reynir S. Atlason and Davide Giacalone
# https://creativecommons.org/licenses/by/4.0/

# modified under EUPL v1.2 by adding data conversion functionality and shiny interface
# Stefan Weiberg, SUSE LLC. <sweiberg@suse.com>

# This is an R function designed to conduct a quantitative Kano analysis.
# Data should be imported as a n*2 dataset under the name "data.csv".
# The columns in the dataset should consist of the functional and dysfunctional answers.
# The numerical values correspond to the following answers: 1=Like, 2=Must-be, 3=Neutral, 4=Live with, 5=Dislike.
# Answers should be listed sequentially in the dataset.
# There should be no missing values in the dataset.

# The first time you use the function, you should run the whole code to save it. 
# After importing your data, use the function by running "kano(dataset,FR)", where dataset is the name of your dataset and FR is the number of functional requirements to be evaluated.
# The output will be printed in the console, exported to three .csv files, and graphed within R studio.

library(plyr)
library(shiny)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(tester)
source("kano.R")

# Define UI for data upload app ----
ui <- fluidPage(
  titlePanel("Evaluate Kano questionnaire"),
  sidebarLayout(
    sidebarPanel(
        h4("Upload CSV export of Kano questionnaire"),
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", FALSE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      tags$hr(),
      actionButton("action", "Evaluate")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("relevant features", 
          plotOutput("kanoCurves"),
          tableOutput("table")
          ),
        tabPanel("unimportant features",
          tableOutput("unimportant"),
        )
      )
    )
  )
)

server<-function(input,output){
  observeEvent(input$action, ignoreInit=TRUE, {

    req(input$file1)

    df <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)

    results<-kano(df)

    output$kanoCurves<-renderPlot({
      x=seq(from = 0.0, to = 1, by = 0.01)

      plot(5,
        5,
        type="n",
        axes=TRUE,
        ann=TRUE,
        xlim=c(0, 1),
        ylim = c(-1,1),
        xlab="Implementation",
        ylab="Customer satisfaction",
        main="Kano results")

      cols<-rainbow(results$FR, s=1,v=1,start=0,end=max(1,results$FR-1)/results$FR,alpha=1)

      for (h in 1:results$FR) {
        lines(x,(results$functions[h,]),col=cols[h])
        legend("topleft", legend=c(results$funcnames), title="Functional Requirements", 
              horiz = TRUE, fill=cols, col=cols[h])
      }
    })
    output$table<-renderTable(results$resultTable)
    output$unimportant<-renderTable(results$unimportant)
  })
}

# Create Shiny app ----
shinyApp(ui, server)

