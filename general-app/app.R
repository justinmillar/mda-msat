# Shiny app for cost tool graph

library(shiny)
library(tidyverse)
library(ggpubr)
library(plotly)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparison of mass drug administration (MDA) and mass-screen-than-treat (MSAT) across range of prevalence rates"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      sliderInput("sn",
                  "RDT Sensitivity",
                  min = 0, 
                  max = 1,
                  step = 0.01,
                  value = c(0.75,0.9)),
      
      sliderInput("sp",
                  "RDT Specificity",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = c(0.7, 0.85)),
      
      # Input for treatment
      numericInput("treat", "Cost of treatment:", min = 0.00, value = 2.55),
      
      # Input for treatment
      numericInput("rdt", "Cost of RDT:", min = 0.00, value = 0.60),
      
      # Input for false-positive
      numericInput("falsePos", "Cost of false positive:", min = 0.00, value = 0.50),
      
      # Input for false-negative
      numericInput("falseNeg", "Cost of false negative:", min = 0.00, value = 10.00), 
      p("Note: this application assumes the same unit of value (i.e. U.S. dollar) is used for each input")
    ),
    
    # Show the plot
    mainPanel(
      plotlyOutput("costPlot"), 
      p("This graph compares MDA (red) and MSAT (blue) 
        intervention strategies as a function of rapid diagonstic test (RDT) performance 
        (sensitivity and specificity) and related costs (treatment, RDT, misdiagnosis)."),
      p("The interactive interface allows the user to selected a range for sensitivity and specificiity, which for create a range possible costs for screen-and-treat. 
        Lower RDT performance will increase expected cost."),
      p("Whichever line is lowest for a particular prevalence value is estimated to be 
        the most economically efficient. A specified prevalence range can be selected by 
        dragging a box around the desired region of the plot (Double click image to reset)."),
      p("A separate application for comparing these intervention strategies using observed 
        data on prevalence, sensitivity and specificity from five west African countries 
        is availible here:", 
        a("https://jjmillar.shinyapps.io/msat-example/",
          href = "https://jjmillar.shinyapps.io/msat-example/"))
      )
)
  )

# Define server logic 
server <- function(input, output) {
  
  output$costPlot <- renderPlotly({
    prev <- seq(0,1,0.01)
    
    # Calculate costs
    costPresump = input$treat + input$falsePos*(1 - prev)
    
    costTNT_a = input$rdt + 
      input$treat*(input$sn[1]*prev + (1 - input$sp[1])*(1 - prev)) + 
      input$falsePos*((1 - prev)*(1 - input$sp[1])) + 
      input$falseNeg*(prev * (1 - input$sn[1]))
    
    costTNT_b = input$rdt + 
      input$treat*(input$sn[1]*prev + (1 - input$sp[2])*(1 - prev)) + 
      input$falsePos*((1 - prev)*(1 - input$sp[2])) + 
      input$falseNeg*(prev * (1 - input$sn[1]))
    
    costTNT_c = input$rdt + 
      input$treat*(input$sn[2]*prev + (1 - input$sp[1])*(1 - prev)) + 
      input$falsePos*((1 - prev)*(1 - input$sp[1])) + 
      input$falseNeg*(prev * (1 - input$sn[2]))
    
    costTNT_d = input$rdt +
      input$treat*(input$sn[2]*prev + (1 - input$sp[2])*(1 - prev)) +
      input$falsePos*((1 - prev)*(1 - input$sp[2])) +
      input$falseNeg*(prev * (1 - input$sn[2]))
    
    df <- data.frame(prev, costPresump, costTNT_a, 
                     costTNT_b, costTNT_c, costTNT_d)

    df$costTNT_low <- pmin(costTNT_a, costTNT_b, costTNT_c, costTNT_d)
    df$costTNT_high <- pmax(costTNT_a, costTNT_b, costTNT_c, costTNT_d)
    
    # Find breakpoints
    bp <- df %>% 
      gather(CostType, CostTNT, c(costTNT_low, costTNT_high)) %>% 
      filter(costPresump <= CostTNT) %>% 
      group_by(CostType) %>% 
      filter(CostTNT == min(CostTNT))
    
    lbp <- min(bp$prev)
    rbp <- ifelse(nrow(bp) == 1, 1, max(bp$prev))
    
    # Plot
    p <- ggplot(data = df) +
      geom_rect(aes(NULL, NULL, xmin = lbp, xmax = rbp, ymin = -Inf, ymax = Inf)) +
      geom_ribbon(aes(x = prev, ymin = costTNT_low, ymax = costTNT_high, color = "MSAT", fill = "MSAT"),
                  alpha = 0.5) +
      geom_ribbon(aes(x = prev, ymin = costPresump, ymax = costPresump, 
                      color = "MDA", fill = "MDA"), size = 1) +
      # geom_rect(aes(xmin = lbp, xmax = rbp, ymin = -Inf, ymax = Inf)) +
      scale_color_manual(name = "",
                         values = c("red", "blue"),
                         breaks = c("MDA", "MSAT")) +
      scale_fill_manual(name = "",
                        values = c("red", "blue"),
                        breaks = c("MDA", "MSAT")) +
      ylab("") +
      ylim(c(0,7.5)) +
      xlab("") +
      theme_classic() +
      theme(axis.line = element_line("black"), 
            text = element_text(size = 8), 
            legend.position = "bottom")
    
    # p <- ggplot(data = df) +
    #   geom_rect(aes(xmin = lbp, xmax = rbp, ymin = -Inf, ymax = Inf),
    #             fill = "gray") +
    #   geom_ribbon(aes(x = prev, ymin = costTNT_low, ymax = costTNT_high), 
    #                  color = "blue", fill = "blue", alpha = 0.5) +
    #   geom_line(aes(x = prev,y = costPresump), color = "red") +
    #   ylab("Estimated Cost") + 
    #   xlab("Malaria Prevalence") +
    #   theme_minimal() +
    #   theme(axis.line = element_line("black"))
    
    
    # p <- ggplot(data = dt, aes(x = Prevalence, y = Cost, color = Type)) +
    #   geom_line(size = 1) +
    #   scale_color_manual(values = c("red", "blue")) +
    #   ylab("Estimated Cost") +
    #   theme_minimal() +
    #   theme(axis.line = element_line("black"))
    
    ggplotly(p)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)