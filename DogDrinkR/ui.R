library(dygraphs)

shinyUI(fluidPage(
  
  titlePanel("DogDrinkR"),
    mainPanel(
      dygraphOutput("dygraph"),
      dataTableOutput("mytable"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("showbowls", label = "Show per Bowl", value = FALSE),
      checkboxInput("showannot", label = "Show Annotation", value = FALSE),
      checkboxInput("showgrid", label = "Show Grid", value = TRUE),
      checkboxInput("showtable", label = "Show Table", value = FALSE),
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
    )
    )
  )
))
