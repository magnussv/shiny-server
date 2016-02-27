library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  library(reshape)
  iris.two <- melt(iris, id.vars = "Species")

  library(plyr)

  
  # plot
  output$Plot1 <- renderPlot({	
	library(ggplot2)
	p <- ggplot(subset(iris.two, Species == input$what_species), aes(x= variable, y=value)) +
				geom_boxplot() +
				ggtitle(paste("Species: ", input$what_species, sep=""))
  
	print(p)
  })
  
  # table
  output$view <- renderTable({
    
	arrange(
					ddply(subset(iris.two, Species == input$what_species), .(Species, variable), summarise,
									
									Total 				= 		    sum(value, na.rm=TRUE),
									Mean 				= 			mean(value, na.rm=TRUE),
									Median 				= 			median(value, na.rm=TRUE),
									Percentile_10		=			quantile(value, 0.1, names = FALSE),
									Percentile_90		=			quantile(value, 0.9, names = FALSE))	
							,Species, variable)
	
  }, include.rownames=FALSE)
  
  
  
})
			