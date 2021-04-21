library(shiny)
library(shinyWidgets)

help_page <- fluidPage(
  
  #column(12,tags$h4("User Manual:")),
 # column(12,tags$p(style="font-size: 15px;","Click ",tags$a(href='xmsPANDA-manual.pdf',target="_blank","here")," to see the user manual.")),
  #column(12,tags$p(style="font-size: 15px;","Click ",tags$a(href='https://joneslabemory.github.io/March182019-metabolomics-analysis-workshop/',target="_blank","here")," to see the R code instruction.")),
  column(12,tags$h4("Input File Format:")),
  #column(12,tags$p(style="margin-top:10px;font-weight:bold; font-size: 17px;","feature table file")),
  column(12,tags$p(style="text-align:justify;font-size: 15px;","For input data table file, two formats are accepted. Format 1 for one-way or two-way factorial design: SampleID, Factor1, Factor2 (optional), The remaining columns should 
                   correspond to the variables and each row corresponds to a sample. Here is an example for format 1:",icon("hand-point-down", lib = "font-awesome", "fa-2x"))),
  column(12, div(style="display:block;margin-top:10px; margin-left: auto;margin-right: auto;width: 50%;",tableOutput('example_feat'))),

 column(12,tags$p(style="text-align:justify;font-size: 15px;","Here is an example for time series data format:",icon("hand-point-down", lib = "font-awesome", "fa-2x"))),
 column(12, div(style="display:block;margin-top:10px; margin-left: auto;margin-right: auto;width: 50%;",tableOutput('example_feat2'))
         #div(style="display: inline-block;vertical-align:top",tableOutput('example_feat'))
         #div(style=";margin-left:40px; width:40%;display: inline-block;vertical-align:top",tags$p(style="text-align:justify;font-size: 15px;","The feature table should include m/z, retention time, and measured intensity in each sample for each analyte. The first 2 columns should be the m/z and time. The remaining columns should correspond to the samples in the class labels file with each column including the intensity profile of a sample"))
         )

  
 # column(12,tags$h4("Download Resources:")),
  #column(12,tags$p(style="font-size: 15px;","Download ",tags$a(href='https://github.com/kuppal2/xmsPANDA',target="_blank","xmsPANDA")," from Github")),
  #column(12,tags$p(style="font-size: 15px;","Download ",tags$a(href='https://github.com/kuppal2/xmsPANDA/tree/master/examples_and_manual/Example_feature_table_and_classlabels',target="_blank","example data")," from Github."))
  
)
