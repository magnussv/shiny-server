############################################################################################
# helpers.R
# 
# AUTHOR: MAGNUS SVENSSON
# DATE: 2015-06-28
# VERSION: 1
# NOTE 1: Contains functions utilized in the "Allsvenskan" shiny app.
# 
############################################################################################

# set locale to Swedish
Sys.setlocale(, "Swedish")

#### webscraper_allsvenskan
# note: returns data frame with data per game from each season. Possible to fill in more than one season at a time

webscraper_allsvenskan <- function(url = NA, season = NA) {

# set locale to Swedish
Sys.setlocale(, "Swedish")
						
# creates a df where urls and season is described
df.urls <- data.frame(URL = url, SEASON = season)

# loop through the rows in df.urls and save each df in a list
library(RCurl)
library(XML)

list_of_dfs		<- 		list()	
for(i in 1:nrow(df.urls)){

url.doc <- htmlParse(df.urls$URL[i], encoding="UTF-8") # parse the document for R representation
html.table <- as.data.frame(readHTMLTable(url.doc))

				html.table$SEASON <- df.urls$SEASON[i]
				
list_of_dfs[[length(list_of_dfs)+1]]  <- html.table

}

# combines result into one data frame
library(data.table)
df.result <- as.data.frame(rbindlist(list_of_dfs)) # as df, instead of dt

# creates column names
names(df.result) <- c("DATE_AND_TIME", "GAME", "RESULT", "ATTENDENCE", "STADIUM", "SEASON")
		
# subsets on games only (there are some rows that are not games)
df.result.two <- subset(df.result, !is.na(GAME))

library(plyr)
df.result.two <- mutate(df.result.two,
			 DATE_AND_TIME  =  as.character(DATE_AND_TIME),
			 GAME =  as.character(GAME), 
			 RESULT =  as.character(RESULT), 
			 ATTENDENCE =  as.character(ATTENDENCE), 
			 STADIUM =  as.character(STADIUM), 
			 SEASON =  as.character(SEASON)
		)

return(df.result.two)

}


#### rename_team_allsvenskan
# note: renames team names in a consistant way 
# (a team might have different names between seasons some time etcetera)
# function is used within 'clean_allsvenskan' function

rename_team_allsvenskan <- function(data = NA) {
  
  data <- as.factor(data)
  
  library(plyr)
  data <- revalue(data,
                  replace = 
                    c(
                      "Assyriska Fören." = "Assyriska",
                      "Åtvidabergs FF" = "Åtvidaberg",
                      "BK Häcken" = "Häcken",
                      "Enköpings SK FK" = "Enköping",
                      "Falkenbergs FF" = "Falkenberg",
                      "Gefle IF" = "Gefle",
                      "Gefle IF FF" = "Gefle",
                      "Halmstads BK" = "Halmstad",
                      "Helsingborgs IF" = "Helsingborg",
                      "IF Brommapojkarna" = "Brommapojkarna",
                      "IF Elfsborg" = "Elfsborg",
                      "IFK Norrköping FK" = "IFK Norrköping",
                      "Jönköpings Södra IF" = "Jönköpings Södra",
                      "Ljungskile SK" = "Ljungskile",
                      "Mjällby AIF" = "Mjällby",
                      "Örebro SK" = "Örebro",
                      "Örgryte IS" = "Örgryte",
                      "Östers IF" = "Öster",
                      "Östersunds FK" = "Östersund",
                      "Syrianska FC" = "Syrianska",
                      "Trelleborgs FF" = "Trelleborg"
                    )
  )
  
  data <- as.character(data)
  
  return(data)
  
}


#### clean_allsvenskan
# note: cleans the data and creates some important features and returns a data frame

clean_allsvenskan <- function(data = NA) {

# set locale to Swedish
Sys.setlocale(, "Swedish")


# creates useful columns
library(plyr)
data <- mutate(data,
	DATE 		= as.Date(substring(as.character(DATE_AND_TIME), 1, 10), origin = "1970-01-01", tz = "CET", format="%Y-%m-%d"),
	DATE_AND_TIME 	= as.POSIXct(as.character(DATE_AND_TIME), origin = "1970-01-01", tz = "CET", format="%Y-%m-%d %H:%M"),
	TIME		= ifelse(nchar(as.character(DATE_AND_TIME)) < 11, NA, substring(DATE_AND_TIME, 12, 16)),
	WEEKDAY		= weekdays(DATE, abbreviate = FALSE),
	ATTENDENCE	= as.numeric(gsub(" ", "", as.character(ATTENDENCE)))
        )

# home and AWAY team								
TEXT <- as.vector(data$GAME)
 SPLIT <- strsplit(TEXT, " - ")
  data$HOME <- sapply(SPLIT, "[", 1)
  data$AWAY <- sapply(SPLIT, "[", 2)
		
   # clean/rename team names
   data$HOME <- rename_team_allsvenskan(data = as.character(data$HOME))
   data$AWAY <- rename_team_allsvenskan(data = as.character(data$AWAY))

   # but back the cleaned team names in the text string
    data$GAME  <- paste0(data$HOME, " - ", data$AWAY)
 
 # swedish written days... if run on server with problem understanding swedish locale	
 data$WEEKDAY <- as.character(data$WEEKDAY)
 data$WEEKDAY <- as.factor(data$WEEKDAY)
				
 levels(data$WEEKDAY)[levels(data$WEEKDAY)== "Monday"] <- "måndag"
 levels(data$WEEKDAY)[levels(data$WEEKDAY)== "Tuesday"] <- "tisdag"
 levels(data$WEEKDAY)[levels(data$WEEKDAY)==  "Wednesday"] <- "onsdag"
 levels(data$WEEKDAY)[levels(data$WEEKDAY)==  "Thursday"] <- "torsdag"
 levels(data$WEEKDAY)[levels(data$WEEKDAY)==  "Friday"] <- "fredag"
 levels(data$WEEKDAY)[levels(data$WEEKDAY)==  "Saturday"] <- "lördag"
 levels(data$WEEKDAY)[levels(data$WEEKDAY)==  "Sunday"] <- "söndag"
	
return(data)
		
}		
		

		
#### long_clean_allsvenskan
# note: input should be the output from 'clean_allsvenskan' function

long_clean_allsvenskan <- function(data = NA) {
library(plyr)

# reshapes table to long
library(reshape)
data.two <- melt(data, id.vars = c("DATE_AND_TIME", "DATE", "TIME", "GAME", "RESULT", 
                                  "ATTENDENCE", "STADIUM", "SEASON", "WEEKDAY"))
													
# renames variables		
data.two <- rename(data.two, 
		c(variable  	=		"HOME_OR_AWAY",
		value	        =		"TEAM"))

# creates OPPONENT team	column 						
TEXT <- as.vector(data.two$GAME)
 SPLIT <- strsplit(TEXT, " - ")
  data.two$HOME_TEAM <- sapply(SPLIT, "[", 1)
  data.two$AWAY_TEAM <- sapply(SPLIT, "[", 2)								

data.two <- mutate(data.two,						
		OPPONENT     =	ifelse(TEAM == HOME_TEAM, AWAY_TEAM,
				 ifelse(TEAM == AWAY_TEAM, HOME_TEAM, NA)))

# creates goals for and against						
TEXT <- as.vector(data.two$RESULT)
 SPLIT <- strsplit(TEXT, " - ")
  data.two$HOME_GOALS <- as.numeric(as.character(sapply(SPLIT, "[", 1)))
  data.two$AWAY_GOALS <- as.numeric(as.character(sapply(SPLIT, "[", 2)))

# note: two games have ATTENDENCE = NA in the past... how to fix
# subset(df.result.two, ATTENDENCE == "" & !is.na(HOME_GOALS))
data.two <- mutate(data.two,
		GOAL_FOR        =	     ifelse(TEAM == HOME_TEAM, HOME_GOALS,
				              ifelse(TEAM == AWAY_TEAM, AWAY_GOALS, NA)),
							
		GOAL_AGAINST	=            ifelse(TEAM == HOME_TEAM, AWAY_GOALS,
				              ifelse(TEAM == AWAY_TEAM, HOME_GOALS, NA)),
															
		GOAL_DIFF       =	      GOAL_FOR - GOAL_AGAINST								
	     )


# creates games placed, points and Win/Draw/Loss indicators															
data.two <- mutate(data.two,
		GAMES_PLAYED	=			ifelse(is.na(GOAL_FOR), NA, 1),
							
		POINTS		=			ifelse(GOAL_FOR > GOAL_AGAINST, 3,
							 ifelse(GOAL_FOR == GOAL_AGAINST, 1, 
							  ifelse(GOAL_FOR < GOAL_AGAINST, 0, NA))),
							
		W		=			ifelse(GOAL_FOR > GOAL_AGAINST, 1,
							 ifelse(GOAL_FOR == GOAL_AGAINST, 0, 
							  ifelse(GOAL_FOR < GOAL_AGAINST, 0, NA))),

		D		=			ifelse(GOAL_FOR > GOAL_AGAINST, 0,
							 ifelse(GOAL_FOR == GOAL_AGAINST, 1, 
							  ifelse(GOAL_FOR < GOAL_AGAINST, 0, NA))),

		L		=			ifelse(GOAL_FOR > GOAL_AGAINST, 0,
							 ifelse(GOAL_FOR == GOAL_AGAINST, 0, 
							  ifelse(GOAL_FOR < GOAL_AGAINST, 1, NA)))
	    )


# create cumulative values within each season

  # sorts data
  data.two <- arrange(data.two, TEAM, SEASON, DATE, TIME)											

	
    # cumsum variable within each season		
    data.two	<- ddply(data.two, .(TEAM, SEASON), mutate,
			CUM_GOAL_FOR		= cumsum(GOAL_FOR),
			CUM_GOAL_AGAINST	= cumsum(GOAL_AGAINST),
			CUM_GOAL_DIFF		= cumsum(GOAL_DIFF),
			CUM_GAMES_PLAYED	= cumsum(GAMES_PLAYED),
			CUM_POINTS			= cumsum(POINTS),
			CUM_W				= cumsum(W),
			CUM_D				= cumsum(D),
			CUM_L				= cumsum(L))

# create league position at each round within a season
data.two <- arrange(data.two, SEASON, CUM_GAMES_PLAYED, desc(CUM_POINTS), 
		     desc(CUM_GOAL_DIFF), desc(CUM_GOAL_FOR), desc(CUM_W))
			
 # order variable within each season		
 data.two <- ddply(data.two, .(SEASON, CUM_GAMES_PLAYED), mutate,
		 CUM_POSITION	=	1:NROW(piece))		


   # calculate last position per season
   data.two <- ddply(data.two, .(SEASON), mutate,
		LATEST_POSITION_PER_SEASON = CUM_POSITION[match(paste0(TEAM, SEASON, max(CUM_GAMES_PLAYED, na.rm=TRUE)), 
		                                          paste0(TEAM, SEASON, CUM_GAMES_PLAYED))]
		)
		
# returns a data frame with one row per team and game (i.e. twice as many as in the output from 'clean_allsvenskan'
# plus additional features and statistical measures (as columns)
return(data.two)

}
							

#### aggregate_season_long_clean_allsvenskan2
# note: input should be the output from 'long_clean_allsvenskan' function
#
# example: 
#aggregate_long_clean_allsvenskan2(data = df3, 
#	aggregation_level = c("TEAM"),
#	row_sort_vars = NA, 
#	row_sort_sign = NA,
#	subset_season = 2014,
#	subset_team = NA,
#	subset_opponent = NA)

aggregate_long_clean_allsvenskan <- function(data = NA, 
	aggregation_level = c("SEASON", "TEAM"),
	row_sort_vars = c("SEASON"), 
	row_sort_sign = c(1),
	subset_season = 2015,
	subset_team = "Malmö FF",
	subset_opponent = "AIK") {

# creates row sort vectors														
row_sort_vars <- if (length(row_sort_vars) == 1 && is.na(row_sort_vars)) {  c("POINTS", "W", "GOAL_DIFF", "GOAL_FOR") 
						} else { c(row_sort_vars, "POINTS", "W", "GOAL_DIFF", "GOAL_FOR") }

row_sort_sign <- if (length(row_sort_sign) == 1 && is.na(row_sort_sign)) {  c(-1,-1,-1,-1) 
						} else { c(row_sort_sign,-1,-1,-1,-1) }

# create subset condition
subset_season     <- if (is.na(subset_season))   { unique(data$SEASON) } else { subset_season }
subset_team   	  <- if (is.na(subset_team))   { unique(data$TEAM) } else { subset_team }
subset_opponent   <- if (is.na(subset_opponent))   { unique(data$OPPONENT) } else { subset_opponent }						
						
library(data.table)
library(plyr)

data <- data.table(data[!is.na(data$GAMES_PLAYED) & data$SEASON %in% subset_season & 
           data$TEAM %in% subset_team & data$OPPONENT %in% subset_opponent, ])

																
data <- as.data.frame(setorderv(
		data[, list(
			GAMES_PLAYED 		=		sum(GAMES_PLAYED, is.na = FALSE),
			W			=		sum(W, is.na = FALSE),
			D			=		sum(D, is.na = FALSE),
			L			=		sum(L, is.na = FALSE),
			GOAL_FOR		=		sum(GOAL_FOR, is.na = FALSE),
			GOAL_AGAINST		=		sum(GOAL_AGAINST, is.na = FALSE),
			GOAL_DIFF		=		sum(GOAL_DIFF, is.na = FALSE),
			POINTS			=		sum(POINTS, is.na = FALSE)
			),
		by = eval(aggregation_level)
		], 
			c(row_sort_vars), c(row_sort_sign))
	)
							
return(data)

}

