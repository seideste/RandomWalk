library(shiny)

ui <- fluidPage(
  # title of shiny app
  titlePanel("Gambler's Ruin: Exercise 5.2"),
  
  # creat layout with sliders
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("i_a", "Money player A:",
                  min = 0, max = 500,
                  value = 50),
      
      sliderInput("i_b", "Money player B:",
                  min = 0, max = 500,
                  value = 50),
      
      sliderInput("p", "Winning probability A:",
                  min = 0, max = 1,
                  value = 0.4929, step = 0.001),
      actionButton("go", "recalcualate")

    ),
    
    mainPanel(
      verbatimTextOutput("Text"),
      plotOutput("Plot", height = 800)
    )
  )
)

server <- function(input, output) {
  
  sliderValues <- reactive({
      data.frame(
        Name = c("i_a","i_b","p"),
        Value = as.numeric(c(input$i_a,input$i_b,input$p)),
        stringsAsFactors = FALSE)
      })
  
  
  # perform a random walk until player has no money or all money.
  solutions <- eventReactive(input$go, {
    ja_init=input$i_a
    jb_init=input$i_b
    j=ja_init
    solution = matrix(j)
    while((j>=0) && (j<=(ja_init+jb_init))){
      r=runif(1)
      if(r<input$p){
        j=j+1
      }else{
        j=j-1
      }
      solution <- c(solution, j)
    }
    return(solution)
  })
  
  
  # plot random walk of the money from  player A
  output$Plot <- renderPlot({
    plot(seq(1:length(solutions())),solutions(),'l',xlab = 'games [-]',
         ylab = 'money player A [$]',cex.lab=1.5, cex.axis=1.2, cex.main=1.5,
         ylim = c(0,(input$i_a+input$i_b)),
         main = "Random walk with given parameters")
  })
  
  # calculates the probability that player A goes bankrupt 
  # before he wins all the money.
  
    output$Text <- renderText({
    paste("Pr{X_n = 0 before X_n = N | X_0 = ",as.character(input$i_a),"} = ",
          as.character((((1-input$p)/input$p)^input$i_a-((1-input$p)/
                              input$p)^(input$i_a+input$i_b))/(1-((1-input$p)/
                                              input$p)^(input$i_a+input$i_b))))
  })
  
}


shinyApp(ui, server)