

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Power Analysis Widget"),

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
                           sliderInput(inputId = "alpha",
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
          fluidRow(
            column(6,
                   plotOutput("nulldistribution")
            ),
            column(6,
                   plotOutput("altdistribution")
            )
          ) # end fluidRow
        )# end main
    )# end layout
)# end ui

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # critical value for the selected alpha
  zcrit <- reactive({
    qnorm(1-input$alpha/2, 0, 1/input$sampleSize)
  }) # assuming two-tailed
  
  # plot the distribution of the mean under the null, assuming mu=0 and sigma2=1
  output$nulldistribution <- renderPlot({
    #generate a normal distribution plot
    ggplot(data.frame(x = c(-1, 1)), aes(x = x)) +
      stat_function(fun = dnorm, args=c(0, 1/input$sampleSize), n=1000) +
      geom_vline(aes(xintercept=zcrit()))
  })
  
  # plot the distribution of the mean under the alternative, assuming mu=0+effectsize and sigma2=1
  output$altdistribution <- renderPlot({
    #generate a normal distribution plot
    ggplot(data.frame(x = c(0, 10)), aes(x = x)) +
      stat_function(fun = dnorm, args=c(input$effectSize, 1/input$sampleSize), n=1000) 
  })
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
