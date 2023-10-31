

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Power Analysis Widget"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
          # Travis I'm removing this because I'd rather keep it more simple
          # radioButtons(inputId = "solveFor",label = "Solve for?",
          #              choices = c("Power","Significance Level","Sample Size","Effect Size"),
          #              selected = "Power"),
          
         shiny::helpText("Use this app to explore the power of a one-tailed test for a difference in means between 
                          a test population and a reference population. Select various combinations of
                          Type I error rate (alpha), effect size (delta - the true difference in population means),
                          and sample size (n). Variance is assumed to be known and equal between the two populations."),

         radioButtons(inputId = "sigma2",
                      label = "Variance",
                      choices = c(5, 10, 15, 20, 25),
                      selected = 15, inline=TRUE),
         
          # power 
          # conditionalPanel(condition = "input.solveFor != 'Power'",
          #  sliderInput(inputId = "power",
          #              label = "Power",
          #              min = 0,
          #              max = 1,
          #              value = .8)),
          
          # significance level
          sliderInput(inputId = "alpha",
                       label = "Significance Level",
                       min = 0,
                       max = 1,
                       value = .05),
          
          # sample size
          sliderInput(inputId = "sampleSize",
                       label = "Sample Size",
                       min = 1,
                       max =40,
                       value = 20),
          
          # effect size
          sliderInput(inputId = "effectSize",
                       label = "Effect Size",
                       min = .5,
                       max = 4,
                       value = 3),
      
        ),# end sidebar

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            column(12,
                   plotOutput("distributionPlot")
            )
          ), # end fluidRow
          fluidRow(
            #Bonus Question
            shiny::helpText("Bonus: Calculate the sample size if the effect\n
            size is 10, the variance for both populations is 5, and the desired\n
            power is 0.8"),
            actionButton("go", label = "Calculate Sample Size"),
            br(),
            textOutput("printN"),
            actionButton("reveal", label = "Reveal Exercise"),
            textOutput("printExercise")
          )
        )# end main
    )# end layout
)# end ui

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # critical value for the selected alpha
  zcrit <- reactive({
    qnorm(1-input$alpha, 0, as.integer(input$sigma2)/input$sampleSize)
  }) # assuming two-tailed
  
  # min and max of the plot, reactive to selection
  xlims <- reactive({
    var_mean <- as.integer(input$sigma2)/input$sampleSize
    c(-4*var_mean, input$effectSize+4*var_mean)
  })
  
  # plot the distribution of the mean under the reference and test distributions
  output$distributionPlot <- renderPlot({
    #generate a normal distribution plot
    ggplot(data.frame(x = xlims()), aes(x = x)) +
      stat_function(fun = dnorm, args=c(0, as.integer(input$sigma2)/input$sampleSize), n=1000) +
      geom_vline(aes(xintercept=zcrit())) +
      stat_function(fun = dnorm, args=c(input$effectSize, as.integer(input$sigma2)/input$sampleSize), n=1000)
  })
  
  calculatedN <- eventReactive(input$go, {
    # sample size calculation
    power <- 0.8
    alpha <- 0.05
    effect_size <- 2
    z_beta <- qnorm(1-power, 0, 1)
    z_alpha <- qnorm(alpha, 0, 1)
    sigma <- 5
    # sample size - round up
    ceiling(2*((z_alpha*sigma)^2 - (z_beta*sigma)^2) / effect_size)
  })
  
  output$printN <- renderText({
    paste("The minimum sample size is:", calculatedN())
  })
  
  exerciseText <- eventReactive(input$reveal, {
    "Design a widget that will calculate sample size for any desired power input by the user. Assume alpha=0.05, effect size=5, and equal variance = 1"
  })
  
  output$printExercise <- renderText({ 
    exerciseText()
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
