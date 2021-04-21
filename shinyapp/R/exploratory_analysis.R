library(shiny)
library(plotly)
library(plyr)
library(ggpubr)
library(DT)
library(shinycssloaders)
library(colourpicker)
library(R.utils)
library(shinyWidgets)

# Define UI for data upload app ----

exploratory_analysis_page  <- fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        fileInput("exploratory_data", "Select feature table file ('.csv' or .txt', 100MB limit)",
                         multiple = FALSE,
                         width="350px",
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".txt")),
              
        selectInput(inputId = "plot.type", 
                    label = "Select plot type", 
                    choices = c("histogram","boxplot","barplot","scatterplot","timeseriesplot","pca","heatmap","heatmap_correlation"), selected = "histogram"),
       # sliderInput(inputId = "bin", 
        #            label = "3. Select number of histogram bins", 
         #           min=1, max=25, value= c(10)),
        textInput(inputId = "text_x", 
                  label = "X.name", ""),
       textInput(inputId = "text_y", 
                 label = "Y.name", ""),
       textInput(inputId = "text_f1", 
                 label = "Group (or facet) by", ""),
       
       #textInput(inputId = "text_f2", 
        #         label = "Factor2 (optional)", ""),
       actionButton("exploratory.go","Plot data",icon=icon("play-circle"))
       
      ),
      
      mainPanel(
      #  plotOutput("myplot"),
        plotlyOutput("myplot",height = "450px"),
        #tableOutput("mytable"),
        #textOutput("mytext")
        height = "500px")
    )
  )
  
  
  
