library(plyr)
library(xts)
library(dygraphs)
library(dplyr)
library(AnomalyDetection)
source("helpers.R") # AnomalyDetectionTs -> AnomalyDetectionTs2


# get data from googlesheets, read as a csv file
url <- "https://docs.google.com/spreadsheets/d/1MjCVP5LhN2DimO0hfFxtBF-4s9v9C8l6bzs8K_PbzsY/pub?output=csv"
df_colClasses <- c("Date", "numeric","numeric", "numeric", "numeric", "character")
df <- read.csv(url, stringsAsFactors = FALSE, colClasses = df_colClasses)


# add columns
df <- mutate(df,
             Small = Small_Before - Small_After,
             Big = Big_Before - Big_After,
             Total = Small + Big)

# subset on complete cases, and columns of interest
df.two <- subset(df, complete.cases(df), select = c(Date, Small, Big, Total))
df.remark <- subset(df, complete.cases(df) & nchar(Remark)>0, select = c(Date, Remark)) # data frame with remarks only

# convert to xts object
xts.two <- xts(subset(df.two, select = -c(Date)),
               order.by = as.Date(df.two$Date))


shinyServer(function(input, output) {
  
  # a (dy)graph
  output$dygraph <- renderDygraph({
    main_title_text <- "Amount of Water per day"
    ylab_text <- "gram per day"
    
    # show lines per bowl or not ('Total' will always appear)
    if (input$showbowls) { p1 <- dygraph(xts.two, main = main_title_text, ylab = ylab_text) 
    } else { 
      p1 <- dygraph(xts.two$Total, main = main_title_text, ylab = ylab_text)
    }  
    
    # the graph 'meat'
    p1  %>% dyAxis("y", valueRange = c(0, max(df.two$Total, na.rm = TRUE)+30)) %>%
      dySeries(name = "Total", label = "Both bowls", fillGraph = TRUE, drawPoints = TRUE, pointSize = 4) %>%
      dyRangeSelector(height = 20) %>%
      dyOptions(drawGrid = input$showgrid) -> p1
    
    # show annotations
    if (input$showannot) {
      # add annotations
      for (i in 1:nrow(df.remark)) {
        p1 <- p1 %>% dyAnnotation(as.character(df.remark$Date[i]), text = df.remark$Remark[i], tooltip = df.remark$Remark[i], 
                                  width = max(nchar(strsplit(df.remark$Remark[i], " ")[[1]]))*8, height = nchar(df.remark$Remark[i])*3)
      }
    }
    
    # show anomalies
    if (input$showanoms == TRUE & nrow( anom() ) > 0) { # 
      # add anomalies annotations
      for (i in 1:nrow( anom() )) {
        p1 <- p1 %>% dyAnnotation(width = 25, height = 20, as.character(anom()$timestamp[i]), text = i, tooltip = paste0("Expected value: ", anom()$expected_value[i]))
       }
     }
    
    print(p1)
    
  })
  

  output$from <- renderText({
    if (!is.null(input$dygraph_date_window))
      strftime(as.Date(input$dygraph_date_window[[1]])+1, "%d %b %Y")      
  })
  
  output$to <- renderText({
    if (!is.null(input$dygraph_date_window))
      strftime(as.Date(input$dygraph_date_window[[2]])+1, "%d %b %Y")
  })


  # reactive anomalies data frame
  anom <- reactive({
    # convert to data.frame
    anom <- data.frame(Date=as.POSIXct(index(xts.two)), coredata(xts.two))
    
    # anomalies detection
    anom <- as.data.frame(
      AnomalyDetectionTs2(anom[, c(1,4)], max_anoms= input$max_anoms_adjust/100, alpha = input$alpha_adjust/100, direction='both', plot=FALSE,  e_value = TRUE)$anoms
    )
    
    if(nrow(anom) > 0) {
      
      # format timestamp to Date
      anom <- mutate(anom,
                     timestamp = as.Date(timestamp)+1)
      
    }
    
    return(anom)
    
  })
  
  
  # a data table
  output$mytable <- renderDataTable({
    
    if (input$showtable) {
      
      mytable <- df.two
      names(mytable) <- c("Date", "Small", "Big", "Both bowls")
      
      if(input$showbowls) {  } else { mytable <- mytable[,c(1,4)] }			
      
      print(mytable)
      
    } else {
      
      return(NULL)
      
    }
    
  },  options = list(paging = FALSE, 
                     searching = FALSE,
                     bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
  ))
  
  
})
