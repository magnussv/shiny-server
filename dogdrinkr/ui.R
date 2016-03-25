library(dygraphs)

shinyUI(fluidPage(
  
  titlePanel("DogDrinkR"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("showbowls", label = "Show per Bowl", value = FALSE),
      checkboxInput("showannot", label = "Show Annotation", value = FALSE),
      checkboxInput("showgrid", label = "Show Grid", value = TRUE),
      checkboxInput("showtable", label = "Show Table", value = FALSE),
      checkboxInput("showanoms", label = "Detect Anomalies", value = FALSE),
      
      # Display this only if showanoms = TRUE
      conditionalPanel(condition = "input.showanoms == true",
                       sliderInput(inputId = "max_anoms_adjust",
                                   label = "Maximum number of anomalies as a percentage of the data points",
                                   min = 1, max = 49, value = 10, step = 1, ticks = FALSE)
      ),
      # Display this only if showanoms = TRUE
      conditionalPanel(condition = "input.showanoms == true",
                       sliderInput(inputId = "alpha_adjust",
                                   label = "Significance level, as a percentage, with which to accept or reject anomalies",
                                   min = 1, max = 49, value = 5, step = 1, ticks = FALSE)
      ),
      hr(),
      div(strong("From: "), textOutput("from", inline = TRUE)),
      div(strong("To: "), textOutput("to", inline = TRUE)),
      br(),
      helpText("Click and drag to zoom in (double click to zoom back out)."),
      br(),
      # adding hyperlink           
      tags$div(class="header", checked=NA,
               tags$p("Fill in data at"), tags$a(href="http://tinyurl.com/DogDrinkR", "http://tinyurl.com/DogDrinkR")
      )
    ),
    mainPanel(
      dygraphOutput("dygraph"),
      dataTableOutput("mytable")
    )
  )
))
