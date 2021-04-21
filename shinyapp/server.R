options(shiny.maxRequestSize=100*1024^2)
options(shiny.sanitize.errors=FALSE)
library('xmsPANDA')
#library('lsmeans')
#library('car')
#library('KEGGREST')
#.libPaths("R/source_codes/library/")
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)

source("R/source_codes/xmsPANDA_v1.0.9.9.R")

# Server logic
server <- function(input, output, session) {
  
  ######Exploratory analysis
  
  #exploratory_analysis_1 <- reactive({
  library(ggpubr)
  
  #observeEvent(input$exploratory.go, 
               
               
  observeEvent(input$exploratory.go,{
  #exploratory_analysis_1 <- reactive({
   # if(input$exploratory.go!=0){
      
      showNotification(paste("Processing started"))
      if(input$exploratory_data$type=="text/plain"){
        metab_data <- read.delim(input$exploratory_data$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
      }else{
        if(input$exploratory_data$type=="text/csv"){
          metab_data <- read.csv(input$exploratory_data$datapath) #,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          
          metab_data <- NULL
        }
      }
                 
      if(input$text_f1!=""){
        facet_by_val=input$text_f1 
      }else{
        facet_by_val=NA
      }
                 
                 col_vec<-c("#0072B2", "#E69F00", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","#E64B35FF","#3C5488FF","#F39B7FFF",
                            "#8491B4FF","#91D1C2FF","#DC0000FF","#B09C85FF","#5F559BFF",
                            "#808180FF","#20854EFF","#FFDC91FF","#B24745FF",
                            
                            "#374E55FF","#8F7700FF","#5050FFFF","#6BD76BFF",
                            "#E64B3519","#4DBBD519","#631879E5","grey75")
      #metab_data
      if(length(grep(colnames(metab_data),pattern=input$text_x))>0 | length(grep((metab_data$Name),pattern=input$text_x))>0){
        
      if(input$plot.type!="timeseriesplot"){
        remove_col_1<-grep(colnames(metab_data),pattern="Factor1")
        remove_col_2<-grep(colnames(metab_data),pattern="Factor2")
        remove_col_3<-grep(colnames(metab_data),pattern="Name")
        
        remove_col<-c(remove_col_1,remove_col_2,remove_col_3)
        if(length(remove_col_2)>0){
          metab_data$Factor1<-factor(metab_data$Factor1,levels=unique(metab_data$Factor1))
          metab_data$Factor2<-factor(metab_data$Factor2,levels=unique(metab_data$Factor2))
          samplelabels<-metab_data$Factor1:metab_data$Factor2
        }else{
          samplelabels<-factor(metab_data$Factor1,levels=unique(metab_data$Factor1))
        }
       
        if(length(remove_col)>0){
          
          df=metab_data[,-c(1:max(remove_col))]
        }else{
          df<-metab_data
        }
        df<-apply(df,2,function(x){as.numeric(as.character(x))})
        metab_data[,-c(1:max(remove_col))]<-df
      }else{
        
        #df<-as.numeric(as.character(df))
      }
        
       # print(head(metab_data))
        
                 if(input$plot.type=="histogram"){
                   
                   if(input$text_f1=="Factor1:Factor2"){
                     
                     metab_data_temp<-metab_data
                     metab_data_temp$Factor1<-factor(metab_data$Factor1):factor(metab_data$Factor2)
                     metab_data_orig<-metab_data
                     metab_data<-metab_data_temp
                     facet_by_val="Factor1"
                   }
                   
                   if(is.na(facet_by_val)==TRUE){
                          output$myplot <- renderPlotly(gghistogram(
                            data=metab_data,
                            x=input$text_x,palette = "jco",fill=col_vec[1])+ggtitle(input$text_x)) #facet.by=facet_by_val,
                   }else{
                     
                     output$myplot <- renderPlotly(gghistogram(
                       data=metab_data,
                       x=input$text_x,palette = "jco",facet.by=input$text_f1,fill=col_vec[1])+ggtitle(input$text_x))
                   }
                   
                   if(input$text_f1=="Factor1:Factor2"){
                     
           
                     metab_data<-metab_data_orig
                   }
                          
                 }else{
                   
                   if(input$plot.type=="boxplot"){
                     
                     
                     if(input$text_f1=="Factor1:Factor2"){
                       
                       metab_data_temp<-metab_data
                       metab_data_temp$Factor1<-factor(metab_data$Factor1):factor(metab_data$Factor2)
                       metab_data_orig<-metab_data
                       metab_data<-metab_data_temp
                       facet_by_val="Factor1"
                     }
                     
                   #  print(head(metab_data))
                     if(is.na(facet_by_val)==TRUE){
                       
                       output$myplot <- renderPlotly(ggboxplot(
                         data=metab_data,
                         x=input$text_x,y = input$text_y,xlab = "",ylab=input$text_y,bxp.errorbar = TRUE,add = "jitter",
                         fill=input$text_x,palette = "jco",width=0.25,bxp.errorbar.width=0.6)+stat_compare_means())
                       #ggtitle(input$text_y)+
                       
                     }else{
                   output$myplot <- renderPlotly(ggboxplot(
                     data=metab_data,
                     x=input$text_x,y = input$text_y,facet.by=input$text_f1,xlab = "",ylab=input$text_y,bxp.errorbar = TRUE,
                     add = "jitter",fill=input$text_x,palette = "jco",width=0.25,bxp.errorbar.width=0.6,scales="fixed")+stat_compare_means())
                   #ggtitle(input$text_y)+
                     }
                     
                     if(input$text_f1=="Factor1:Factor2"){
                       
                       
                       metab_data<-metab_data_orig
                     }
                   
                   }else{
                     
                     if(input$plot.type=="pca"){
                       library(plotly)
                       
                       if(input$text_f1=="Factor1:Factor2"){
                         
                         metab_data_temp<-metab_data
                         metab_data_temp$Factor1<-factor(metab_data$Factor1):factor(metab_data$Factor2)
                         metab_data_orig<-metab_data
                         metab_data<-metab_data_temp
                         facet_by_val="Factor1"
                       }
                      
                       
                       pca_res <- mixOmics::pca(df, scale = TRUE,center = TRUE)
                       
                     
                       carsDf <- data.frame(pca_res$x, metab_data[,c(1:max(remove_col))]) #"cluster" = factor(carsClusters))
                       
                       rownames(carsDf)<-as.character(metab_data$SampleID)
                       library(plotly)
                       
                       if(input$text_f1=="Factor2"){
                       p1 <- ggplot(carsDf,aes(x=PC1, y=PC2)) +
                         theme_bw() +
                         geom_hline(yintercept = 0, color = "gray70") +
                         geom_vline(xintercept = 0, color = "gray70") +
                         geom_point(aes(color = Factor2), alpha = 0.55, size = 3) +
                         xlab("PC1") +
                         ylab("PC2") + 
                         xlim(-5, 6) + 
                         ggtitle("PCA") 
                       }else{
                         p1 <- ggplot(carsDf,aes(x=PC1, y=PC2)) +
                           theme_bw() +
                           geom_hline(yintercept = 0, color = "gray70") +
                           geom_vline(xintercept = 0, color = "gray70") +
                           geom_point(aes(color = Factor1), alpha = 0.55, size = 3) +
                           xlab("PC1") +
                           ylab("PC2") + 
                           xlim(-5, 6) + 
                           ggtitle("PCA") 
                         
                       }
                       p1=p1 + geom_text(aes(y =PC2 + 0.25, label = rownames(carsDf)))
                       
                       p1=ggplotly(p1)
                       
                       if(input$text_f1=="Factor1:Factor2"){
                         
                         
                         metab_data<-metab_data_orig
                       }
                       
                       output$myplot <- renderPlotly(p1) #renderPlot(p1)
                     }else{
                       
                       if(input$plot.type=="barplot"){
                         
                         if(input$text_f1=="Factor1:Factor2"){
                           
                           metab_data_temp<-metab_data
                           metab_data_temp$Factor1<-factor(metab_data$Factor1):factor(metab_data$Factor2)
                           metab_data_orig<-metab_data
                           metab_data<-metab_data_temp
                           facet_by_val="Factor1"
                         }
                         
                         if(is.na(facet_by_val)==TRUE){
                           
                           output$myplot <- renderPlotly(ggbarplot(
                             data=metab_data,
                             x=input$text_x,y = input$text_y,ylab = input$text_y,xlab="",add = "mean_se",fill=input$text_x,
                             palette = "jco",width=0.25)+stat_compare_means())
                           
                           #ggtitle(input$text_x)+
                           
                         }else{
                         output$myplot <- renderPlotly(ggbarplot(
                           data=metab_data,
                           x=input$text_x,y = input$text_y,facet.by=input$text_f1,ylab = input$text_y,xlab="",add = "mean_se",fill=input$text_x,
                           palette = "jco",width=0.25)+stat_compare_means())
                         
                         #ggtitle(input$text_x)+
                         
                         }
                         
                         if(input$text_f1=="Factor1:Factor2"){
                           
                           
                           metab_data<-metab_data_orig
                         }
                         
                       }else{
                         
                         if(input$plot.type=="scatterplot"){
                           
                        
                           if(input$text_f1=="Factor1:Factor2"){
                             
                             metab_data_temp<-metab_data
                             metab_data_temp$Factor1<-factor(metab_data$Factor1):factor(metab_data$Factor2)
                             metab_data_orig<-metab_data
                             metab_data<-metab_data_temp
                             facet_by_val="Factor1"
                           }
                           
                           if(is.na(facet_by_val)==TRUE){
                           output$myplot <- renderPlotly(ggscatter(
                             data=metab_data,
                             x=input$text_x,y = input$text_y,
                             add="reg.line",xlab = input$text_x,palette = "jco")+stat_cor(label.x = 3,method = "spearman",aes(label=paste(gsub(..rr.label..,pattern="~|`|italic",replacement=""),sep = ""))))
                           }else{
                             
                             output$myplot <- renderPlotly(ggscatter(
                               data=metab_data,
                               x=input$text_x,y = input$text_y,color=facet_by_val,
                               add="reg.line",xlab = input$text_x,palette = "jco")+stat_cor(label.x = 3,method = "spearman",aes(color = facet_by_val,label=paste(gsub(..rr.label..,pattern="~|`|italic",replacement=""), sep = ""))))
                           
                             if(input$text_f1=="Factor1:Factor2"){
                               
                               
                               metab_data<-metab_data_orig
                             }
                             
                             }
                         }else{
                           
                           if(input$plot.type=="timeseriesplot"){
                           # Dummy data
                           data <- data.frame(
                             Time = as.Date("2017-06-14") - 0:364,
                             Result = runif(365) + seq(-140, 224)^2 / 10000
                           )
                           metab_data<-as.data.frame(metab_data)
                           metab_data$Factor1<-factor(metab_data$Factor1,levels=unique(metab_data$Factor1)) 
                           metab_data$Factor1<-paste("Factor1.",metab_data$Factor1,sep="")
                           #
                           
                           if(length(grep(colnames(metab_data),pattern="Factor2"))>0){
                             
                             metab_data$Factor2<-factor(metab_data$Factor2,levels=unique(metab_data$Factor2))
                             metab_data$Factor2<-paste("Factor2.",metab_data$Factor2,sep="")
                             #metab_data$Factor2<-factor(metab_data$Factor2,levels=paste("Factor2.",unique(metab_data$Factor2),sep=""))
                           }
                           
                           #print(metab_data)
                           yval<-input$text_y
                           #print(xval)
                          metab_data_sub<-metab_data[which(metab_data$Name==input$text_y),]
                           # Most basic bubble plot
                           #subset(metab_data, Name %in% c(input$text_x))
                          
                           if(is.na(facet_by_val)==TRUE){
                           p <- ggplot(subset(metab_data, Name %in% c(input$text_y)),aes(x=Time, y=Result)) +
                             geom_line( color=col_vec[1]) + 
                            # geom_point() +
                             xlab("") +
                             theme_bw() +
                             theme(axis.text.x=element_text(angle=60, hjust=1)) #+
                            # scale_x_date(limit=c(as.Date("2017-01-01"),as.Date("2017-02-11"))) +
                             #ylim(0,1.5)
                           }else{
                             
                             if(input$text_f1=="Factor1"){
                             p <- ggplot(subset(metab_data, Name %in% c(input$text_y)),aes(x=Time, y=Result)) +
                               
                               
                               geom_line(aes(color = Factor1, linetype = Factor1)) +
                               
                               
                               xlab("") +
                               theme_bw() +
                               theme(axis.text.x=element_text(angle=60, hjust=1))
                             }else{
                               
                               if(input$text_f1=="Factor2"){
                                 p <- ggplot(subset(metab_data, Name %in% c(input$text_y)),aes(x=Time, y=Result)) +
                                   geom_line(aes(color = Factor2, linetype = Factor2)) +
                                   
                                   
                                   xlab("") +
                                   theme_bw() +
                                   theme(axis.text.x=element_text(angle=60, hjust=1))
                               }else{
                               
                                 if(input$text_f1=="Factor1:Factor2"){
                                   
                                   metab_data_temp<-metab_data
                                   metab_data_temp$Factor1<-factor(metab_data$Factor1):factor(metab_data$Factor2)
                                   p <- ggplot(subset(metab_data_temp, Name %in% c(input$text_y)),aes(x=Time, y=Result)) +
                                     geom_line(aes(color = Factor1, linetype = Factor1)) +
                                     xlab("") +
                                     theme_bw() +
                                     theme(axis.text.x=element_text(angle=60, hjust=1))
                                 }
                                 
                             }
                             }
                           }
                           p=p+scale_color_manual(values = col_vec)
                           p=p+ggtitle(input$text_y)+xlab("Time")
                           output$myplot <- renderPlotly(p)
                           }else{
                             
                            # library(pheatmap)
                             library(heatmaply)
                             if(input$plot.type=="heatmap" | input$plot.type=="heatmap_correlation"){
                               
                               if(length(grep(colnames(metab_data),pattern="Factor2"))>0){
                                 
                                 metab_data<-as.data.frame(metab_data)
                                 metab_data$Factor1<-factor(metab_data$Factor1,levels=unique(metab_data$Factor1)) 
                                 metab_data$Factor2<-factor(metab_data$Factor2,levels=unique(metab_data$Factor2)) 
                                        if(FALSE){ 
                                               ann_colors = list(
                                                 Factor1 = levels(metab_data$Factor1),
                                                 Factor2 =levels(metab_data$Factor2),
                                                 GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
                                               )
                                        }
                                 
                                 annotation_col<-as.data.frame(cbind(metab_data$Factor1,metab_data$Factor2))
                                 
                               }else{
                                 
                                 
                                 metab_data<-as.data.frame(metab_data)
                                 metab_data$Factor1<-factor(metab_data$Factor1,levels=unique(metab_data$Factor1)) 
                                 
                                 annotation_col<-as.data.frame(metab_data$Factor1)
                               }
                              
                               dft<-t(df)
                               rownames(dft)<-colnames(df)
                               colnames(dft)<-as.character(metab_data$SampleID)
                              # p1=heatmapr(df,scale="column",colors="RdBu") #pheatmap(dft,  main = "Heatmap") #,annotation_col=annotation_col)
                              # p1=ggheatmap(p1)
                               #p1=ggplotly(p1)
                               #print(dft)
                              if(FALSE){ 
                               mat <- dft
                               mat <- paste("This cell is", rownames(mat))
                               mat <- lapply(colnames(mat), function(colname) {
                                 paste0(mat[, colname], ", ", colname)
                               })
                              }
                             # dft_scale<-apply(dft,1,function(x){scale(x)})
                              
                              if(input$plot.type=="heatmap"){
                                
                                col_vec<-colorRampPalette(rev(brewer.pal(10, "RdBu")))
                                
                                
                              p1= heatmaply(dft,ylab="",xlab="",scale="row",colors = col_vec,row_dend_left = TRUE)
                              }else{
                                r <- WGCNA::cor(df,method="spearman")
                                ## We use this function to calculate a matrix of p-values from correlation tests
                                ## https://stackoverflow.com/a/13112337/4747043
                                cor.test.p <- function(x){
                                  FUN <- function(x, y){pres<-cor.test(x, y)[["p.value"]];if(pres==Inf){pres<-min(c(100,Inf))};return(pres)}
                                  z <- outer(
                                    colnames(x), 
                                    colnames(x), 
                                    Vectorize(function(i,j) FUN(x[,i], x[,j]))
                                  )
                                  dimnames(z) <- list(colnames(x), colnames(x))
                                  z
                                }
                                p <- cor.test.p(df)
                               
                                p1=heatmaply_cor(
                                  r,
                                  node_type = "scatter",
                                  point_size_mat = -log10(p+0.00001), 
                                  point_size_name = "-log10(p-value)",
                                  label_names = c("x", "y", "Correlation")
                                )
                               # p1= heatmaply_cor(dft)
                              }
                                
                                
                               
                                output$myplot <- renderPlotly(p1) #annotation_col = annotation_col,
                               
                               
                             }
                             
                           }
                         }
                       }
                     }
                     
                   }
                   
                 }
      }
    #output$mytext <- renderText(input$text_x)
    }
  )
  
  ##############################
  
  ##################################  Help Page #################################################  
 
  example_feat <- read.delim("example_data/sample_exploratory_data.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_feat <- example_feat[1:10,1:5]
 # colnames(example_feat) <- c(colnames(example_feat),"...")
  output$example_feat <- renderTable({ example_feat }, striped = TRUE)
  
  example_feat2 <- read.delim("example_data/sample_timeseries_data.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_feat2 <- example_feat2[1:25,]
  #colnames(example_feat2) <- c(colnames(example_feat2),"...")
  output$example_feat2 <- renderTable({ example_feat2 }, striped = TRUE)
  

  

  
  #################### interactive plot start
  
  
  #################### interactive plot end
  
} 

