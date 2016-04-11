############################################################################################
# ui.R for 'Allsvenskan' app
# 
# AUTHOR: MAGNUS SVENSSON
# DATE: 2015-07-01
# VERSION: 1
# NOTE 1: 
# NOTE 2: 
############################################################################################

###############
# set locale to Swedish
Sys.setlocale(, "Swedish")

library(shiny)
library(DT)
library(jsonlite)
library(shinythemes)

shinyUI(navbarPage("ALLSVENSKAN",
  #theme = "flatly.css",
  theme = shinytheme("flatly"),
  title = 'ALLSVENSKAN',
  
  #tags$head(
  #    tags$link(rel = "stylesheet", type = "text/css", href = "shared/selectize/css/selectize.bootstrap3.css"),
  #    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/css/bootstrap-select.min.css"),
  #    tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css"),
  #    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
  #    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/js/bootstrap-select.min.js"),
  #    tags$script(src = "shared/selectize/js/selectize.min.js"),
  #    tags$script(src = "allsvenskan.js")
  #  ),

   tabPanel("Tabell",
    sidebarLayout(
      sidebarPanel(
			fluidRow(
				column(6,
		uiOutput("Seasons")),
				column(6,
		checkboxGroupInput('mytable1_show_home_or_away', 'Visa Hemma och/eller Borta:',
                           c("Hemma", "Borta"), selected = c("Hemma", "Borta")))				   
				)),
      mainPanel(
        dataTableOutput('mytable1')
      )
    )
  ),
  tabPanel("Spelade matcher", 
	dataTableOutput('mytable2')
  ),
  tabPanel("Kommande matcher", 
	dataTableOutput('mytable3')
  ),
  tabPanel("Position per omgång", 
	sidebarLayout(
      sidebarPanel(
        uiOutput("Seasons2"),
        uiOutput("teams")
        #checkboxGroupInput('myplot1_show_season', 'Välj säsong(er):',
        #                   2001:2016, selected = 2016),
	#	checkboxGroupInput('myplot1_show_team', 'Välj lag (ett eller flera):',
        #                   c("AIK", "Assyriska Fören.",  "BK Häcken", "Djurgården", "Enköpings SK FK",   "Falkenbergs FF",    "GAIS", "Gefle IF", "Gefle IF FF",      
	#						"GIF Sundsvall",     "Halmstads BK",      "Hammarby",          "Helsingborgs IF",   "IF Brommapojkarna", "IF Elfsborg",       "IFK Göteborg",      "IFK Norrköping",    "IFK Norrköping FK",
	#						"Kalmar FF",         "Landskrona BoIS",   "Ljungskile SK",     "Malmö FF",          "Mjällby AIF",       "Syrianska FC",      "Trelleborgs FF",    "Åtvidabergs FF",    "Örebro",           
	#						"Örebro SK",         "Örgryte",           "Örgryte IS",        "Östers IF")
	#					   , selected = "Malmö FF")				   	   
	),
      mainPanel(
        plotOutput('myplot1', height = "600")
      )
    )
  ),
  tabPanel("Lag mot Lag", 
	sidebarLayout(
      sidebarPanel(
        fluidRow(
        column(6,
		checkboxGroupInput('mytable4_show_season', 'Välj säsong(er):',
                           2001:2016, selected = 2001:2016)),
		column(6,
		checkboxGroupInput('mytable4_show_home_or_away', 'Visa Hemma och/eller Borta:',
                           c("Hemma", "Borta"), selected = c("Hemma", "Borta")))),			   
		fluidRow(
        column(6,
		checkboxGroupInput('mytable4_show_team', 'Välj lag (ett eller flera):',
                           c("AIK", "Assyriska Fören.",  "BK Häcken", "Djurgården", "Enköpings SK FK",   "Falkenbergs FF",    "GAIS", "Gefle IF", "Gefle IF FF",      
							"GIF Sundsvall",     "Halmstads BK",      "Hammarby",          "Helsingborgs IF",   "IF Brommapojkarna", "IF Elfsborg",       "IFK Göteborg",      "IFK Norrköping",    "IFK Norrköping FK",
							"Kalmar FF",         "Landskrona BoIS",   "Ljungskile SK",     "Malmö FF",          "Mjällby AIF",       "Syrianska FC",      "Trelleborgs FF",    "Åtvidabergs FF",    "Örebro",           
							"Örebro SK",         "Örgryte",           "Örgryte IS",        "Östers IF")
						   , selected = "Malmö FF")),
        column(6,
		checkboxGroupInput('mytable4_show_opponent', 'Välj motståndare (ett eller flera):',
                           c("AIK", "Assyriska Fören.",  "BK Häcken", "Djurgården", "Enköpings SK FK",   "Falkenbergs FF",    "GAIS", "Gefle IF", "Gefle IF FF",      
							"GIF Sundsvall",     "Halmstads BK",      "Hammarby",          "Helsingborgs IF",   "IF Brommapojkarna", "IF Elfsborg",       "IFK Göteborg",      "IFK Norrköping",    "IFK Norrköping FK",
							"Kalmar FF",         "Landskrona BoIS",   "Ljungskile SK",     "Malmö FF",          "Mjällby AIF",       "Syrianska FC",      "Trelleborgs FF",    "Åtvidabergs FF",    "Örebro",           
							"Örebro SK",         "Örgryte",           "Örgryte IS",        "Östers IF")
						   , selected = "Helsingborgs IF")))
				),
      mainPanel(
         dataTableOutput('mytable4'),
		 hr(),
		 dataTableOutput('mytable5')
      )
    )
  )
  
 )
)
