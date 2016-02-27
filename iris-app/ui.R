library(shiny)


# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Iris dataset"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("what_species", "Species", 
                    choices=sort(unique(iris$Species))),
        hr(),
        helpText("Data from Edgar Anderson's Iris Data")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        h4("Boxplot"),
		plotOutput("Plot1"),
		
		h4("Summary Table"),
		tableOutput("view")
      )
      
    )
  )
)
