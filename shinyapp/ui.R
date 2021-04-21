library(shiny)
library(shinyBS)
library(V8)
library(BiocManager)
options(repos = BiocManager::repositories())
library(xmsPANDA)
#source("R/introduction_page.R")
source("R/exploratory_analysis.R")
source("R/help_page.R")
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui<-fluidPage(
  shinyjs::useShinyjs(),
# shinyjs::extendShinyjs(script='js/css.js'),
#  shinyjs::extendShinyjs(text = jsResetCode),

    tags$head(
      tags$meta(charset="utf-8"),
      tags$meta(name="description",content="Free Web tutorials"),
      tags$title("Callisto - v0.1"),
      tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    ),
    column(12,
        tags$div(id="form",
          h3(tags$img(style="float:left;margin-right:15px;",src="images/callisto.png",height='80px',width="80px"),
             "Callisto (v0.1)- an interactive app for exploratory data analysis and visualization")),
        tabsetPanel(
         #https://photojournal.jpl.nasa.gov/jpegMod/PIA03456_modest.jpg
       #     tabPanel("Introduction", introduction_page), 
         # tabPanel("Statistical Analysis", statistical_analysis_page),
         
          #tabPanel("Additional Analysis", additional_analysis_page), 
          tabPanel("Analysis", exploratory_analysis_page), 
          tabPanel("Help and Support", help_page),
         
          type ="tabs"
        )
    ),
    column(style="padding-top:0px;padding-bottom:0px;",12,tags$hr(style="margin-top:0px;margin-bottom:15px;border-top: 0.5px solid #ccccb3;")),
column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:95%;color:black","Please ask questions or report any issues to ",
                                                                                                             tags$a(href='kul@ascendispharma.com',target="_blank","Karan Uppal"), 
                                                                                                                    ", Principal Scientist Bioinformatics, Biomarker Development (Oncology)")))
                                                       
                                                       
 #column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:95%;color:black","Please ask questions or report any issues on the ",
  #                                                      tags$a(href='https://github.com/kuppal2/xmsPANDA/issues',target="_blank","GitHub"), " page")))
)
