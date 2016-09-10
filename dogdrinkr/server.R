library(plyr)
library(xts)
library(dygraphs)
library(dplyr)
library(AnomalyDetection)
library(zoo)
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


df.two2 <- data.frame(Date = seq(min(df.two$Date), max(df.two$Date), by = "day"))
df.two <- merge(df.two2, df.two, by = "Date", all.x=TRUE)


# initial 'Total' Moving average calculation
ma <- 7
f3 <- rep(1/ma, ma)
df.two$Total_MA <- round(as.vector(stats::filter(df.two$Total, f3, method = "convolution", circular =FALSE, sides=2)), 2)
df.two <- subset(df.two, complete.cases(df), select = c(Date, Small, Big, Total, Total_MA))


# convert to xts object
xts.two <- xts(subset(df.two, select = -c(Date)),
               order.by = as.Date(df.two$Date))



shinyServer(function(input, output) {

  
  # reactive column selection
  col_selected <- reactive({
    if (input$showbowls) { cols1 <- c("Small", "Big") } else { cols1 <- NA }
    if (input$showma) { cols2 <- c("Total_MA") } else { cols2 <- NA }
    
    col_selected <- as.character(as.vector(na.omit(c("Total", cols1, cols2))))
    
    return(col_selected)
    
  })
  

 
  # reactive Moving Average calculation
  ma_selected <- reactive({
    ma <- input$ma_length
    
    df.two2 <- data.frame(Date = seq(min(df.two$Date), max(df.two$Date), by = "day"))
    df.two <- merge(df.two2, df.two, by = "Date", all.x=TRUE)
    
    f3 <- rep(1/ma, ma)
    df.two$Total_MA <- round(as.vector(stats::filter(df.two$Total, f3, method = "convolution", circular =FALSE, sides=2)), 2)
    

    if(input$showinterpol) { 
    
    df.two2 <- data.frame(index = 1:nrow(df.two), Total = df.two$Total)
    df.two_zoo <- zoo(df.two2)
    index(df.two_zoo) <-  df.two_zoo[,1]
    df.two_zoo_approx <- na.approx(df.two_zoo)

    df.two$Total_MA <- round(as.vector(stats::filter(as.data.frame(df.two_zoo_approx)$Total, f3, method = "convolution", circular =FALSE, sides=2)), 2)
    }
    
    
    df.two <- subset(df.two, complete.cases(df), select = c(Date, Small, Big, Total, Total_MA))
    
    
    # convert to xts object
    xts.two <- xts(subset(df.two, select = -c(Date)),
                   order.by = as.Date(df.two$Date))
    
    return(list(df.two, xts.two))
    
  })

  

  # a (dy)graph
  output$dygraph <- renderDygraph({
    main_title_text <- "Amount of Water per day"
    ylab_text <- "gram per day"
    
    col_selected <- col_selected()
    
    df.two <- ma_selected()[[1]]
    xts.two <- ma_selected()[[2]]
    
      p1 <- dygraph(xts.two[, col_selected], main = main_title_text, ylab = ylab_text)
      
      p1  %>% dyAxis("y", valueRange = c(0, max(df.two$Total, na.rm = TRUE)+30)) %>%
        dyRangeSelector(height = 20) %>%
        dyOptions(drawGrid = input$showgrid) -> p1
      
      if ("Total_MA" %in% col_selected) {	  
        p1 %>% dySeries(name = "Total_MA", label = "MA Both bowls", strokeWidth = 5, color = "red") -> p1	
      } else {
        
      }
      
      if ("Big" %in% col_selected) {	  
        p1 %>% dySeries(name = "Big", color = "#A248BD", strokePattern = "dotted") %>% 
          dySeries(name = "Small", color = "#5CC242", strokePattern = "dotdash") -> p1	 
      } else {
        
      }

      
    p1  %>% dySeries(name = "Total", label = "Both bowls", fillGraph = TRUE, drawPoints = TRUE, pointSize = 4, color = "#067C77") -> p1
      
      
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
    
    # NA Imputation
    df.imp <- data.frame(Date = seq(min(df.two$Date), max(df.two$Date), by = "day"))
    imp.dates <- as.Date(setdiff(df.imp$Date, df.two$Date))
    
    df.imp <- merge(df.imp, df.two, by = "Date", all.x=TRUE)
    
    mean.Total <-  mean(df.imp$Total, na.rm = TRUE) 
    df.imp$Total[is.na(df.imp$Total)] <- mean.Total # replace NA with mean value  
    
    # convert date to POSIXct
    df.imp <- mutate(df.imp,
                     Date = as.POSIXct(Date))
    
    # anomalies detection
    anom <- as.data.frame(
      AnomalyDetectionTs2(df.imp[, c(1,4)], max_anoms= input$max_anoms_adjust/100, alpha = input$alpha_adjust/100, direction='both', plot=FALSE,  e_value = TRUE)$anoms
    )
    
    
    if(nrow(anom) > 0) {
      
      # format timestamp to Date
      anom <- mutate(anom,
                     timestamp = as.Date(timestamp)+1)
      
    } else {
      
      # create a data frame with the same formatting but with zero rows
      anom <- data.frame(timestamp = as.Date("2010-01-01"), anoms = 300, expected_value = 300)
      #anom <- anom[0,]
      
    }
    
    # remove dates with imputated values
    anom <- subset(anom, !(as.Date(timestamp) %in% imp.dates))
    
    return(anom)
    
  })
  
  
  # a data table
  output$mytable <- renderDataTable({
    
    if (input$showtable) {
      
      df.two <- ma_selected()[[1]]
      
      mytable <- df.two[, c("Date", col_selected())]
      
      # colnames (same as in graph)
      df_names <- data.frame(varname = c("Total", "Small", "Big", "Total_MA"), 
                   colname = c("Both bowls", "Small", "Big", "MA Both bowls"))
      
      
      names(mytable) <- c("Date", as.character(subset(df_names, varname %in% names(mytable))$colname))
      
      
      mytable <- arrange(mytable, desc(Date))
      
      
      print(mytable)
      
    } else {
      
      return(NULL)
      
    }
    
  },  
  
  options = list(paging = FALSE, 
                 searching = FALSE,
                 bInfo = 0# information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
                 )
  #,rownames= FALSE) # doesn't work on remote Shiny Server, but works locally on the same RStudio server...
  )
})
