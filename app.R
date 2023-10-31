

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statistical Analysis Widget"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
          # solve for
          radioButtons(inputId = "solveFor",label = "Solve for?",
                       choices = c("Power","Significance Level","Sample Size","Effect Size"),
                       selected = "Sample Size"),
         
          #power
          conditionalPanel(condition = "input.solveFor != 'Power'",
                           sliderInput(inputId = "power",
                                       label = "Power",
                                       min = 0,
                                       max = 1,
                                       value = .8)),
          #significance level
          conditionalPanel(condition = "input.solveFor != 'Significance Level'",
                           sliderInput(inputId = "significanceLevel",
                                       label = "Significance Level",
                                       min = 0,
                                       max = 1,
                                       value = .05)),
          
          #sample size
          
          conditionalPanel(condition = "input.solveFor != 'Sample Size'",
                           sliderInput(inputId = "sampleSize",
                                       label = "Sample Size",
                                       min = 1,
                                       max =40,
                                       value = 20)),
          
          # effect size
          
          conditionalPanel(condition = "input.solveFor != 'Effect Size'",
                           sliderInput(inputId = "effectSize",
                                       label = "Effect Size",
                                       min = .5,
                                       max =10,
                                       value = 3)),
      
        ),# end sidebar

        # Show a plot of the generated distribution
        mainPanel(
           
        )# end main
    )# end layout
)# end ui

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
