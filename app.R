

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Power Analysis Widget"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
         shiny::helpText("Use this app to explore the power of a one-tailed test for testing whether the mean of a population is greater than 0. 
                          Select various combinations of Type I error rate (alpha), effect size (delta - the true difference from 0),
                          and sample size (n). Variance is assumed to be known.\n The green shaded area on the plot is the calculated power."),

         radioButtons(inputId = "sigma2",
                      label = "Variance",
                      choices = c(1, 5, 10, 15, 20, 25),
                      selected = 15, inline=TRUE),
          
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
                       max = 6,
                       value = 3),
          #export a report 
          downloadButton("Report", "Generate Report")
         
      
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
                             size is 4, the variance is 25, the alpha level\n
                             level is 0.05, and the desired power is 0.8."),
            actionButton("go", label = "Calculate Sample Size",icon= icon("calculator"),
            style="color: #fff; background-color: #0B3188; border-color: #000000;
                               border-radius: 10px; 
                               border-width: 2px"),
            br(),
            textOutput("printN"),
            actionButton("reveal", label = "Reveal Exercise",icon = icon("eye"),
            style="color: #fff; background-color: #087A27; border-color: #000000;
                               border-radius: 10px; 
                               border-width: 2px"),
            textOutput("printExercise1"),
            textOutput("printExercise2")
          )
        )# end main
    )# end layout
)# end ui

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # critical value for the selected alpha
  zcrit <- reactive({
    qnorm(1-input$alpha, 0, sd=sqrt(as.integer(input$sigma2)/input$sampleSize))
  }) # assuming two-tailed
  
  # min and max of the plot, reactive to selection
  xlims <- reactive({
    se_mean <- sqrt(as.integer(input$sigma2)/input$sampleSize)
    c(-4*se_mean, input$effectSize+4*se_mean)
  })
  
  power <- reactive({
    round(1-pnorm(zcrit(), input$effectSize, sd=sqrt(as.integer(input$sigma2)/input$sampleSize)), digits=3)
  })
  
  #plot 
  stat_plot <- reactive({
   ggplot(data.frame(x = xlims()),
                 aes(x = x)) +
      stat_function(fun = dnorm,
                    args=c(0, sqrt(as.integer(input$sigma2)/input$sampleSize)),
                    n=1000) +
      geom_text(x = 0, y = -0.05, label = "H0")+
      geom_vline(aes(xintercept = zcrit()),
                 linetype = c("dashed"))+
      annotate(geom = "text",
              label = "Zcrit",
              x = zcrit(),
              y = -0.05,
              angle = 90, 
              vjust = 1.4)+
      stat_function(fun = dnorm, 
                    args=c(input$effectSize, sqrt(as.integer(input$sigma2)/input$sampleSize)), 
                    xlim = c(zcrit(), xlims()[2]),
                    geom = "area", fill = "#84CA72", alpha = .2) +
      stat_function(fun = dnorm, 
                    args=c(input$effectSize, sqrt(as.integer(input$sigma2)/input$sampleSize)), 
                    n=1000) +
      geom_text(x = input$effectSize, y = -.05, label = "HA") +
      ggtitle(paste("Power =", power())) +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  })
  
  # plot the distribution of the mean under the reference and test distributions
  output$distributionPlot <- renderPlot({
    stat_plot()
  })
  
  calculatedN <- eventReactive(input$go, {
    
    # Sample Size Calculation (1tailed)
    power <- 0.8
    alpha <- 0.05
    effect_size <- 4
    variance <- 25
    sigma = sqrt(variance)
    z_beta <- qnorm(1-power, 0, 1)
    z_alpha <- qnorm(alpha, 0, 1)
    # sample size - round up
    ceiling((z_alpha + z_beta)^2*variance / effect_size^2)
    
    
  })
  
  output$printN <- renderText({
    paste("The minimum sample size is:", calculatedN())
  })
  
  exerciseText1 <- eventReactive(input$reveal, {
    "Option 1. Design a widget that will calculate sample size for any desired power input by the user.\n 
    Assume alpha=0.05, effect size=0.6, and a known variance of 1." 
  })
  
  exerciseText2 <- eventReactive(input$reveal, {
    "Option 2. Create a timeseries plot of the specific conductivity data in the included file StMarysRiverOH_SondeData_25Sep-03Oct2023.xlsx that appears after pressing a 'Plot' button."
  })
  
  output$printExercise1 <- renderText({ 
    exerciseText1()
  })
  
  output$printExercise2 <- renderText({ 
    exerciseText2()
  })
  
  
  #report generator
  output$Report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Report.html",
    content = function(file) {
      # Set up parameters to pass to Rmd document
      par_list <- list("sigma2"=input$sigma2,
                         "alpha" = input$alpha,
                         "sampleSize"=input$sampleSize,
                         "effectSize"=input$effectSize,
                         "stat_plot"=stat_plot())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document rom the code in this app).
      print(str(par_list))
      
      saveRDS(par_list, file = 'diagnosis.RDS')
      rmarkdown::render("report_output.Rmd", output_file = file,
                        params = par_list,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
