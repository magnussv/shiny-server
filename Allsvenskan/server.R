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


### gets data from local file
library(rdrop2)

## data in "df3 format"
	colclasses 	<- 	c("factor", "Date", "character", 
					  "factor", "factor", "numeric",
					  "factor", "numeric", "character",
					  "factor", "factor", "character",
					  "character", "character", "numeric",
					  rep("numeric", 17), "integer", "integer")

df3_past <- read.csv(file = "Allsvenskan_past_seasons.csv", head=TRUE, sep=";", dec=".", encoding="UTF-8")					  

# creates POSIX date and time
library(plyr)
df3_past <- mutate(df3_past,
			DATE_AND_TIME 	=		as.POSIXct(as.character(DATE_AND_TIME), origin = "1970-01-01", tz = "CET", format="%Y-%m-%d %H:%M"),
			DATE		=		as.Date(as.character(DATE), origin = "1970-01-01", format="%Y-%m-%d")
		  )


### gets data via webscraping representing current season
urls <- c("http://svenskfotboll.se/allsvenskan/tabell-och-resultat/?scr=fixturelist&ftid=57221", # season 2015
          "http://svenskfotboll.se/allsvenskan/tabell-och-resultat/?scr=fixturelist&ftid=62068") # season 2016

# scrapes the web for the data
df <- webscraper_allsvenskan(url = urls, season = seq(from = 2015, to = 2016, by = 1))

# cleans the data
df2 <- clean_allsvenskan(data = df)

# creates a long data frame useful for creating a season table etc
df3_current <- long_clean_allsvenskan(data = df2)

# row binds the data frames
df3 <- rbind(df3_past, df3_current)


shinyServer(function(input, output) {

# a place to put some code; run once each time a user visits the app

# checks if data for current season needs to be updated
	if (length(df3[is.na("ATTENDENCE"), "DATE_AND_TIME"]) > 0) { 
		min_date_time_with_NA <- min(df3[is.na("ATTENDENCE"), "DATE_AND_TIME"]) + (60*(90+15+30)) 
	} else { 
		min_date_time_with_NA <- as.POSIXct("2001-04-07 13:30:00 CEST")
	}


if ( min_date_time_with_NA > Sys.time() )   { 
						
### gets data via webscraping representing current season
urls <- c("http://svenskfotboll.se/allsvenskan/tabell-och-resultat/?scr=fixturelist&ftid=57221")

# scrapes the web for the data
df <- webscraper_allsvenskan(url = urls, season = seq(from = 2015, to = 2015, by = 1))

# cleans the data
df2 <- clean_allsvenskan(data = df)

# creates a long data frame useful for creating a season table etc
df3_current <- long_clean_allsvenskan(data = df2)

# row binds the data frames
df3 <- rbind(df3_past, df3_current)

 } else { 
	# don't update
 }

# render UI output
#output$Seasons <- renderUI({
#  seasons <- sort(unique(as.character(df3$SEASON)))
#  
#  checkboxGroupInput('mytable1_show_season', 'Välj säsong(er):',
#                     seasons, selected = max( seasons ))
#})

output$Seasons <- renderUI({

seasons <- sort(unique(as.character(df3$SEASON)))

selectInput('mytable1_show_season',
                      label = 'Välj säsong(er):',
                      choices = seasons,
                      selected = max( seasons ),
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
						
	#names(mytable1) <- c("", "Spelade", "V", "O", "F", "GM", "IM", "MS", "P")					
						
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
							
	#names(mytable3) <- c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena") # 			
						
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
	aes(x= CUM_GAMES_PLAYED, y= CUM_POSITION, color = TEAM)) +	
			geom_step(size = 1) +
			#geom_line(size = 1) +
			facet_wrap(~SEASON) +
			#scale_y_continuous(limits = c(0, 20))
			scale_y_reverse(lim=c(16,1)) +
			ylab("Position") +
			xlab("Omgång") +
			theme(strip.text.x = element_text(size = 16),
				  axis.text.x  = element_text(size=16),
				  axis.text.y  = element_text(size=16),
				  legend.title=element_blank(),
				  axis.title.x = element_text(size=20),
				  axis.title.y = element_text(size=20),
				  legend.text = element_text(size = 16)
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
					 ), desc(DATE_AND_TIME))[
					, c("DATE", "TIME", "WEEKDAY", "SEASON", "GAME", "RESULT", "ATTENDENCE", "STADIUM")]
							
	#names(mytable2) <- c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena")					
						
	print(mytable5)					
											
  },  options = list(paging = FALSE, 
					 searching = FALSE,
					 bInfo = 0 # information on/off (how many records filtered, etc) https://groups.google.com/forum/#!topic/shiny-discuss/YECf_dPip9M
					 ), 
					 colnames = c("Datum", "Tid", "Veckodag", "Säsong", "Match", "Resultat", "Publiksiffra", "Arena"),
					 rownames = FALSE)
					  				  
					  
					  
})
