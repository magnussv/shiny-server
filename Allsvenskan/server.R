############################################################################################
# server.R for 'Allsvenskan' app
# 
# AUTHOR: MAGNUS SVENSSON
# DATE: 2015-07-01
# VERSION: 1
# NOTE 1: 
# NOTE 2: 
############################################################################################

# set language to English to easier create dates etc
Sys.setlocale(, "Swedish")

library(shiny)
library(DT)

### a place to put some code; run once when app is launched
source("helpers.R", encoding="utf-8")


### get data from past seasons

## data in "df3 format"
colclasses <- c("factor", "Date", "character", 
		"factor", "factor", "numeric",
		"factor", "numeric", "character",
		"factor", "factor", "character",
		"character", "character", "numeric",
		rep("numeric", 17), "integer", "integer")

df3_past <- read.csv(file = "Allsvenskan_past_seasons.csv", head=TRUE, sep=";", dec=".", encoding="UTF-8")					  

# creates POSIX date and time
library(plyr)
df3_past <- mutate(df3_past,
	     DATE_AND_TIME 	= as.POSIXct(as.character(DATE_AND_TIME), origin = "1970-01-01", tz = "CET", format="%Y-%m-%d %H:%M"),
	     DATE		= as.Date(as.character(DATE), origin = "1970-01-01", format="%Y-%m-%d")
	    )

##########################################################################################################	    
### note: the data from 'Allsvenskan_past_seasons.csv' is generated like this,
###   # create data frame with data from past seasons (2001-2015)
###   urls <- c( # seasons 2001-2015
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2001/tabell-resultat/?scr=fixturelist&ftid=54", # season 2001
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2002/tabell/?scr=fixturelist&ftid=3",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2003/spelprogram/",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2004/matcher/",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2005/matcher/",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2006/tabell/?scr=fixturelist&ftid=516",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2007/matcher/",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2008/tabell-resultat/?scr=fixturelist&ftid=14999",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/thylins-allsvenska-summering-2009/tabell-och-resultat/?scr=fixturelist&ftid=19440",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2010/tabell-och-resultat/?scr=fixturelist&ftid=27018",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2011/tabell-och-resultat/?scr=fixturelist&ftid=31625",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2012/tabell-och-resultat/?scr=fixturelist&ftid=35421",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2013/tabell-och-resultat/?scr=fixturelist&ftid=39682",
###   "http://svenskfotboll.se/allsvenskan/tidigare-ar/resultat-2014/tabell-och-resultat/?scr=fixturelist&ftid=50510",
###   "http://svenskfotboll.se/allsvenskan/tabell-och-resultat/?scr=fixturelist&ftid=57221") # season 2015
###   
###   # scrapes the web for the data
###   df <- webscraper_allsvenskan(url = urls, season = seq(from = 2001, to = 2015, by = 1))
###   
###   # cleans the data
###   df2 <- clean_allsvenskan(data = df)
###   
###   # creates a long data frame useful for creating a season table etc
###   df3 <- long_clean_allsvenskan(data = df2)
###   
###   write.table(df3, "Allsvenskan_past_seasons.csv", sep=";", dec=".", row.names= FALSE, fileEncoding="utf-8")
##########################################################################################################

### gets data via webscraping representing current season
urls <- c("http://svenskfotboll.se/allsvenskan/tabell-och-resultat/?scr=fixturelist&ftid=62068") # season 2016

# scrapes the web for the data
df <- webscraper_allsvenskan(url = urls, season = seq(from = 2016, to = 2016, by = 1))

# cleans the data
df2 <- clean_allsvenskan(data = df)

# creates a long data frame useful for creating a season table etc
df3_current <- long_clean_allsvenskan(data = df2)

# row binds the data frames
df3 <- rbind(df3_past, df3_current)

# data frame with unique combinations of seasons and team
df_picker <- ddply(df3, .(TEAM, SEASON), summarise, COUNT = 1)


shinyServer(function(input, output) {

# a place to put some code; run once each time a user visits the app

### render UI output

# "Tabell" panel: mytable1_show_season
output$mytable1_show_season <- renderUI({

#seasons <- sort(unique(as.character(df_picker$SEASON)), decreasing = TRUE) # last season first

selectInput(inputId = "mytable1_show_season",
                      label = 'Välj säsong(er):',
                      choices = 2001:2016, #faster solution instead of: choices = seasons,
                      selected = 2016, #faster solution instead of: selected = max( seasons ),
                      multiple = TRUE,
                      selectize = TRUE)
})

# "Position per omgång" panel: myplot1_show_season
output$myplot1_show_season <- renderUI({

seasons <- sort(unique(as.character(df_picker$SEASON)), decreasing = TRUE) # last season first

selectInput(inputId = "myplot1_show_season",
                      label = 'Välj säsong(er):',
                      choices = seasons,
                      selected = max( seasons ),
                      multiple = TRUE,
                      selectize = TRUE)
})

# "Position per omgång" panel: input$myplot1_show_team
output$myplot1_show_team <- renderUI({

teams <- sort(unique(as.character(df_picker$TEAM)))

selectInput(inputId = "myplot1_show_team",
                      label = 'Välj lag (ett eller flera):',
                      choices = teams,
                      selected = "Malmö FF",
                      multiple = TRUE,
                      selectize = TRUE)
})

# "Lag mot lag" panel: mytable4_show_season
output$mytable4_show_season <- renderUI({

seasons <- sort(unique(as.character(df_picker$SEASON)), decreasing = TRUE) # last season first

selectInput(inputId = "mytable4_show_season",
                      label = 'Välj säsong(er):',
                      choices = seasons,
                      selected = min(seasons):max(seasons),
                      multiple = TRUE,
                      selectize = FALSE)
})

# "Lag mot lag" panel: mytable4_show_team
output$mytable4_show_team <- renderUI({

teams <- sort(unique(as.character(df_picker$TEAM)))

selectInput(inputId = "mytable4_show_team",
                      label = 'Välj lag (ett eller flera):',
                      choices = teams,
                      selected = "Malmö FF",
                      multiple = TRUE,
                      selectize = FALSE)
})

# "Lag mot lag" panel: mytable4_show_opponent
output$mytable4_show_opponent <- renderUI({

teams <- sort(unique(as.character(df_picker$TEAM)))

selectInput(inputId = "mytable4_show_opponent",
                      label = 'Välj lag (ett eller flera):',
                      choices = teams,
                      selected = "Helsingborg",
                      multiple = TRUE,
                      selectize = FALSE)
})


  # a table, reactive to input$show_season
  output$mytable1 <- renderDataTable({
  
  
  mytable1 <- aggregate_long_clean_allsvenskan(data = subset(df3, HOME_OR_AWAY %in% mapvalues(as.character(input$mytable1_show_home_or_away), 
						c("Hemma", "Borta"), c("HOME", "AWAY"), warn_missing = FALSE)),
			aggregation_level = c("TEAM"),
			row_sort_vars = NA, #c("SEASON"), 
			row_sort_sign = NA, #c(1),
			subset_season = input$mytable1_show_season,
			subset_team = NA,
			subset_opponent = NA)
						
	print(mytable1)					
											
  },  options = list(paging = FALSE, 
			searching = FALSE,
			bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
		     ), colnames = c(" ", "Spelade", "V", "O", "F", "GM", "IM", "MS", "P"))

	
  # a table with latest games
  output$mytable2 <- renderDataTable({
  
  mytable2 <- arrange(subset(df3, HOME_OR_AWAY == "HOME" & DATE_AND_TIME < (Sys.time() - 60*(90+15+20))), desc(DATE_AND_TIME))[
					, c("DATE", "TIME", "WEEKDAY", "SEASON", "GAME", "RESULT", "ATTENDENCE", "STADIUM")]
							
	#names(mytable2) <- c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena")					
						
	print(mytable2)					
											
  },  options = list(lengthMenu = list(c(100, -1), c('100', 'All')),
			iDisplayLength = 100,
			paging = TRUE, 
			searching = TRUE,
			bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
			), 
		colnames = c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena"),
		rownames = FALSE)
					  
					  
					  
  # a table with forthcoming games
  output$mytable3 <- renderDataTable({
  
  mytable3 <- arrange(subset(df3, HOME_OR_AWAY == "HOME" & (DATE_AND_TIME > (Sys.time() - 60*(90+15+20)) | DATE > Sys.Date())), DATE_AND_TIME)[
					, c("DATE", "TIME", "WEEKDAY", "SEASON", "GAME", "RESULT", "ATTENDENCE", "STADIUM")] 

	print(mytable3)					
											
  },  options = list(#iDisplayLength = 15,
		paging = FALSE, 
		searching = TRUE,
		bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
		), 
		colnames = c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena"),
		rownames = FALSE)


	# a graph with development for a team across a season
	output$myplot1	<- renderPlot({	
	
library(ggplot2)
ggplot(subset(df3, TEAM %in% input$myplot1_show_team & SEASON %in% input$myplot1_show_season), 
	aes(x= as.integer(CUM_GAMES_PLAYED), y= CUM_POSITION, color = TEAM)) +	
			geom_step(size = 1) +
			geom_point(size = 1.5) +
			#geom_line(size = 1) +
			facet_wrap(~SEASON) +
			#scale_y_continuous(limits = c(0, 20))
			xlim(1,30) +
			scale_y_reverse(lim=c(16,1)) +
			ylab("Position") +
			xlab("Omgång") +
			theme(strip.text.x = element_text(size = 16),
				  axis.text.x  = element_text(size=16),
				  axis.text.y  = element_text(size=16),
				  legend.title=element_blank(),
				  axis.title.x = element_text(size=20),
				  axis.title.y = element_text(size=20),
				  legend.text = element_text(size = 16),
				  legend.position="top"
				  )
			#guides(fill=guide_legend(title=NULL))
			#guides(fill=guide_legend(title="Lag"))
})


  # a table, reactive to input$show_season
  output$mytable4 <- renderDataTable({

  mytable4 <- aggregate_long_clean_allsvenskan(data = subset(df3, HOME_OR_AWAY %in% mapvalues(as.character(input$mytable4_show_home_or_away), 
			c("Hemma", "Borta"), c("HOME", "AWAY"), warn_missing = FALSE)),
			aggregation_level = c("TEAM", "OPPONENT"),
			row_sort_vars = NA, #c("SEASON"), 
			row_sort_sign = NA, #c(1),
			subset_season = input$mytable4_show_season,
			subset_team = input$mytable4_show_team,
			subset_opponent = input$mytable4_show_opponent)					
						
	print(mytable4)					
											
  },  options = list(paging = FALSE, 
			searching = FALSE,
			 bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
			), colnames = c("Lag", "Motståndare", "Spelade", "V", "O", "F", "GM", "IM", "MS", "P"))
					  
					  
  # a table with games for team vs opponent
  output$mytable5 <- renderDataTable({
  
  mytable5 <- arrange(subset(df3, TEAM %in% input$mytable4_show_team & OPPONENT %in% input$mytable4_show_opponent & SEASON %in% input$mytable4_show_season
			), desc(DATE))[
			, c("DATE", "TIME", "WEEKDAY", "SEASON", "GAME", "RESULT", "ATTENDENCE", "STADIUM")]
						
	print(mytable5)					
											
  },  options = list(paging = FALSE, 
		searching = FALSE,
		bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
		), 
		 colnames = c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena"),
		rownames = FALSE)

})
