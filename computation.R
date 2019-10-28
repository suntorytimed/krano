# Author: Stefan Weiberg, SUSE LLC. <sweiberg@suse.com>
# released under EUPL v1.2

# Shiny web interface for uploading Kano questionnaire results
# and evaluating the answers.
# Plots the results for must-have, attractive and one-dimensional
# features and prints a table with the results.
# All unimportant features are listed in a separate tab with their
# classification

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

