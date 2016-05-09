library(shiny)
library(plyr)
#library(tidyr)
library(dplyr)
require(devtools)
#install_github('ramnathv/rCharts')
require(rCharts)


# Define server logic for slider examples
shinyServer(function(input, output,session) {
  
  
  DatasetInput<-reactive({
    inFile<-input$file1
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  #first tab test
  output$summary1<-renderPrint({
    dataset<-DatasetInput()
    if(is.null(dataset)) return ("For Infection,Movement,Stifness Summary by Windows")
    #summary(dataset)
    a<-plyr::count(dataset,"Window", "Infection")
    a<-plyr::rename(a,replace=c("freq"="Infection"))
    b<-plyr::count(dataset,"Window", "Movement")
    b<-plyr::rename(b,replace=c("freq"="Movement"))
    c<-plyr::count(dataset,"Window", "Stiffness")
    c<-plyr::rename(c,replace=c("freq"="Stiffness"))
    d<-plyr::count(dataset,"Window")
    # d<-plyr::rename(d,replace=c("x"="Window"))
    ab<-plyr::join(x=a,y=b,by="Window",type="left")
    abc<-plyr::join(x=ab,y=c,by="Window",type="left")
    abcd<-plyr::join(x=abc,y=d,by="Window",type="left")
    abcd
  })
  
  values<-reactiveValues(data = NULL)
  observeEvent(input$file1, {
    values$data <- read.csv(input$file1$datapath, header=input$header, sep=input$sep, 
                            quote=input$quote)
    write.csv(values$data,'updated.csv') #write out file will be under Rshiny folder
  })
  
  vals<-eventReactive(input$save,{
    
    #summary(dataset)
    if (input$x=="Yes") {xx=1} else {xx=0}
    if (input$y=="Yes") {yy=1} else {yy=0}
    if (input$z=="Yes") {zz=1} else {zz=0}
    
    values$data[nrow(values$data)+1,1]<-input$ID
    values$data[nrow(values$data),2]<-xx
    values$data[nrow(values$data),3]<-yy
    values$data[nrow(values$data),4]<-zz
    values$data[nrow(values$data),5]<-input$days
    temp <- values$data
    values$data <- temp
    write.csv(values$data,'updated.csv') #write out file will be under Rshiny folder
    values$data[,1:5]
    
  })
  
  
  
  output$summary2<-renderPrint({
    
    #vals()
    
    dataset<-vals()
    #if(is.null(dataset)) return ("For Infection,Movement,Stifness Summary by Windows")
    #summary(dataset)
    a<-plyr::count(dataset,"Window", "Infection")
    a<-plyr::rename(a,replace=c("freq"="Infection"))
    b<-plyr::count(dataset,"Window", "Movement")
    b<-plyr::rename(b,replace=c("freq"="Movement"))
    c<-plyr::count(dataset,"Window", "Stiffness")
    c<-plyr::rename(c,replace=c("freq"="Stiffness"))
    d<-plyr::count(dataset,"Window")
    # d<-plyr::rename(d,replace=c("x"="Window"))
    ab<-plyr::join(x=a,y=b,by="Window",type="left")
    abc<-plyr::join(x=ab,y=c,by="Window",type="left")
    abcd<-plyr::join(x=abc,y=d,by="Window",type="left")
    abcd
    
    
  })
  
  
  output$table1<-renderTable({
    head(DatasetInput(),n=input$obs)
  })
  
  # Reactive expression to compose a data frame containing all of
  # the values
  TableValues2 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("P", 
               "Q",
               "R"
      ),
      Value = as.character(c(input$x, 
                             input$y,
                             input$z)), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  #   output$values2 <- renderTable({
  #     TableValues2()
  #   })
  
  TableValues3 <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Stochastic walk"
      ),
      Window1 = as.character( stage2()[2]), 
      Window2=as.character( stage2()[1]),
      stringsAsFactors=FALSE)
  }) 
  
  
  #second tab contents
  output$values3 <- renderTable({
    TableValues3()
  })
  
  
  updates<-eventReactive(input$ShowPlot,{dataset<-read.csv("updated.csv", header=TRUE,  sep = ",",stringsAsFactors = FALSE) 
  
  #if(is.null(dataset)) return ("Waiting for dataset")
  #summary(dataset)
  a<-plyr::count(dataset,"Window", "Infection")
  a<-plyr::rename(a,replace=c("freq"="Infection"))
  b<-plyr::count(dataset,"Window", "Movement")
  b<-plyr::rename(b,replace=c("freq"="Movement"))
  c<-plyr::count(dataset,"Window", "Stiffness")
  c<-plyr::rename(c,replace=c("freq"="Stiffness"))
  d<-plyr::count(dataset,"Window")
  #d<-plyr::rename(d,replace=c("x"="Window"))
  ab<-plyr::join(x=a,y=b,by="Window",type="left")
  abc<-plyr::join(x=ab,y=c,by="Window",type="left")
  abcd<-plyr::join(x=abc,y=d,by="Window",type="left")
  library(tidyr)
  abcdLong<-as.data.frame(abcd%>%tidyr::gather(status,freq1, Infection:freq))
  })
  
  output$myChart <- renderChart({
    p1 <- nPlot(freq1 ~ Window, group = 'status', data = updates(), 
                type = input$type, dom = 'myChart', width = 800)
    #p6$chart(color = c('brown', 'blue', '#594c26', 'green'), stacked = input$stack)
    return(p1)
  })
  
  
  
  stage2<-eventReactive(input$stage2,{
    source("Stage2backendforMac.R", local=TRUE)
    #L1<-data.frame(c("1","2","3","4","5"),L,stringsAsFactors = FALSE)
    c(Windownext, Movestep,L)  #L has 5 value
    
  })
  
  output$summary3<-renderPrint({   
    stage2()
  })
  
  output$myChart1 <- renderChart2({
    #L has 5 value corresponding to [3:7] in vector stage2
    L1<-data.frame(c("1","2","3","4","5"),stage2()[3:7],stringsAsFactors = FALSE)
    
    colnames(L1)<-c("window","value")
    m1<-mPlot(x="window",y="value", data=L1,ymax=round(max(as.numeric(L1$value)),3),ymin=round(min(as.numeric(L1$value)),3),type="Line",parseTime=FALSE)
    m1$set( labels = "value" ) 
    return(m1)
  })
  
  #x    <- faithful[, 2]  # Old Faithful Geyser data
  #bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  #hist(x, breaks = bins, col = 'darkgray', border = 'white')
})
