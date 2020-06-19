

# Loading libraries ----
library(leaflet)
library(plotly)
library(shiny)
library(shinydashboard)
library(sp)
library(tidyverse)

# Load static files ----
dt_org <- read_csv("data/post-samps-all.csv") %>% 
  mutate(region2 = region, 
         country = survey)

reg <- dt_org %>% 
  dplyr::select(uniq_reg_code, Region = region2) %>% 
  distinct()

rdt_cost = seq(0,4,0.01)

source("functions.R")

# UI ----

ui <- ui <- dashboardPage(
  dashboardHeader(title ="To screen or not to screen?", titleWidth = 275),
  dashboardSidebar(width = 275, 
                   sidebarMenu(
                     selectInput(inputId = "country", 
                                 label = "Country",
                                 choices = c("Burkina Faso" = "BF6", 
                                             "Cote d'Ivoire" = "CI6", 
                                             "Ghana" = "GH6", 
                                             "Guinea" = "GN6", 
                                             "Nigeria" = "NG6",
                                             "Togo" = "TG6"), 
                                 selected = "GH6"),
                     
                     # Input for urbanicity
                     radioButtons(inputId = "urban", 
                                  label = "Urbanicity:", 
                                  choices = c("Urban", "Rural"), 
                                  selected = "Rural"), 
                     
                     # Input for age range
                     sliderInput(inputId = "age", 
                                 label = "Age range (in months):",
                                 min = 6, max = 59, value = c(6,59)),
                     h4("Select cost values:"),
                     # Input for treatment
                     numericInput("treat", "Cost of treatment:", 
                                  min = 0.00, value = 2.55),
                     
                     # Input for treatment
                     numericInput("rdt", "Cost of RDT:", 
                                  min = 0.00, value = 0.60),
                     
                     # Input for false-positive
                     numericInput("falsePos", "Cost of false positive:", 
                                  min = 0.00, value = 0.00),
                     
                     # Input for false-negative
                     numericInput("falseNeg", "Cost of false negative:", 
                                  min = 0.00, value = 0.00), 
                     # Input for confidence interval range
                     
                     menuItem("Cost Comparison", tabName = "cost"),
                     menuItem("About", tabName = "about")
                   )),
  dashboardBody(tabItems(
    tabItem(tabName = "cost",
            fluidRow(
              box(leafletOutput("costPlot"), width = 6),
              box(plotlyOutput("costGraph"), width = 6),
              box(plotlyOutput("priceComp"), width = 12)
            )
    ),
    
    tabItem(tabName = "about",
            p("This map depicts an interactive cost-effectiveness comparison between 
  two malaria intervention strategies: 
              mass drug administration (MDA) and mass-screen-and-treat (MSAT)."),
            p("MDA is a malaria control strategy which involves presumptively treating 
              an entire population or subpopulation with antimalarial drugs regardless 
              of symptoms and without a confirmed diagnosis."),
            p("MSAT is a related malaria intervention strategy involving mass distribution 
              of antimalarial drugs regardless of symptoms, but on the basis of a confirmed 
              diagnosis from a rapid diagnostic test (RDT)."),
            p("The cost-effectiveness comparison is based on malaria prevalence, RDT performance, 
              and the direct and indirect costs associated with each intervention."),
            p("Prevalence, sensitivity, and specificity are modelled from Demographic and Health Surveys (DHS) 
              and Malaria Indicator Survey (MIS) data. Cost values are selected by the user."),
            p("Regions that are shaded in",
              span("blue", style = "color:blue; font-weight: bold"),
              "indicate screening with RDT (i.e. MSAT) would be economically beneficial."),
            p("Regions shaded in ", 
              span("red", style = "color:red; font-weight: bold"),
              "suggest that presumptive treatment (i.e. MDA) may be more economically efficient."),
            p("A separate application for comparing these intervention strategies based 
              on theoretical values for RDT sensitivity and specificity is available 
              here:", 
              a("https://jjmillar.shinyapps.io/msat-cost-graph/",
                href = "https://jjmillar.shinyapps.io/msat-cost-graph/"))
            )
            ))
)


# Server ----

server <- function(input, output) { 
  
  output$costPlot <- renderLeaflet({
    
    # dt <- dt_org %>% 
    #   filter(country == input$country,  
    #          urban == input$urban, 
    #          age >= min(input$age) & age <= max(input$age)) %>% 
    #   group_by(uniq_reg_code, age) %>%
    #   summarise_at(c("prev", "sens", "spec"), mean) %>% 
    #   mutate(costComp = cost(input$treat, input$rdt, 
    #                          input$falsePos, input$falseNeg, 
    #                          prev, sens, spec)) # Value added from screening
    
    # Get mean cost for map
    dt_m <- dt_org %>% 
      filter(country == input$country,  
             urban == input$urban, 
             age >= min(input$age) & age <= max(input$age)) %>% 
      group_by(uniq_reg_code) %>%
      summarise_at(c("prev", "sens", "spec"), mean) %>% 
      mutate(costComp = cost(input$treat, input$rdt, 
                           input$falsePos, input$falseNeg, 
                           prev, sens, spec)) # Value added from screening
      
    # Read shape file
    map_file <- paste("shapefiles/", input$country, ".rds", sep = "")
    shp <- readRDS(map_file)
    map_dt <- left_join(dt_m, shp@data)
    
    # Format leaflet inputs
    
    cr <- if(abs(min(map_dt$costComp)) > abs(max(map_dt$costComp))){
      c(-1*abs((min(map_dt$costComp))), abs((min(map_dt$costComp)))) 
    } else {
      c(-1*abs((max(map_dt$costComp))), abs((max(map_dt$costComp))))
    }
    
    cost_pal <- colorNumeric(palette = "RdYlBu", domain = cr)
    cost_label <- sprintf("<strong>Region: %s</strong><br/>Value added: %g",
                          map_dt$DHSREGEN, round(map_dt$costComp, 3)) %>% 
      lapply(htmltools::HTML)
    
    # Construct map
    map <- leaflet(shp) %>% 
      addPolygons(color = "black", 
                  weight = 1, 
                  label = cost_label, 
                  highlight = highlightOptions(
                    weight = 3,
                    color = "black",
                    fillOpacity = 0.9),
                  fillColor = ~cost_pal(map_dt$costComp), 
                  fillOpacity = 0.5) %>%
      addLegend(pal = cost_pal, values = ~cr, 
                title = "Value added:") %>% 
      addProviderTiles(providers$Stamen) %>% 
      addMiniMap(tiles = providers$Stamen, width = 100, height = 100)
  })
  
 
  output$costGraph <- renderPlotly({
    
    dt <- dt_org %>% 
      filter(country == input$country,  
             urban == input$urban, 
             age >= min(input$age) & age <= max(input$age)) %>% 
      mutate(Value = cost(input$treat, input$rdt, 
                          input$falsePos, input$falseNeg, 
                          prev, sens, spec)) %>% 
      left_join(reg) 
    
    d1 <- dt_org %>% 
      filter(country == input$country,  
             urban == input$urban, 
             age >= min(input$age) & age <= max(input$age)) %>%
      mutate(value = cost(input$treat, input$rdt, 
                          input$falsePos, input$falseNeg, 
                          prev, sens, spec)) %>% 
      group_by(uniq_reg_code) %>%
      summarise("value.lwr" = quantile(value, 0.025),
                "value.upr" = quantile(value, 0.975)) %>% 
      left_join(reg)
    
    d1$favor = "No Diff."
    d1[d1$value.lwr > 0, "favor"] = "Favors MSAT"
    d1[d1$value.upr < 0, "favor"] = "Favors MDA"
    
    d1 <- d1 %>% 
      select(uniq_reg_code, favor)
    
    gg <- dt  %>%  
      left_join(d1) %>% 
      ggplot(aes(Region, Value, fill = favor)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, lty = 2) +
      scale_fill_manual(name = str_wrap("Preferred strategy", 10),
                        values = c("Favors MDA" = "red",
                                   "Favors MSAT" = "blue",
                                   "No Diff." = "gray")) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
      ylab("Value added from screening") + xlab("") +
      theme_classic() +
      theme(
        # axis.text.x = element_blank(), 
        # axis.ticks.x = element_blank(),
        legend.position = "top", legend.title = element_blank()
            )
    
    ggplotly(gg, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(legend = list(orientation = "h"))
  })
  
  output$priceComp <- renderPlotly({
    
    dt_sum <- dt_org %>% 
      filter(country == input$country, urban == input$urban,
             age >= min(input$age) & age <= max(input$age)) %>% 
      group_by(region) %>% 
      summarize_at(c("prev", "sens", "spec"), mean) %>% 
      inner_join(tidyr::expand(., nesting(region), rdt_cost)) %>% 
      mutate(trt_cost = cost_t(rdt_cost, input$falsePos, input$falseNeg, 
                               prev, sens, spec))
    
    # Plot
    p <- ggplot(dt_sum, aes(x = rdt_cost, y = trt_cost, color = region)) +
      geom_line() +
      geom_hline(yintercept = input$treat, lty = 3, alpha = 0.5) +
      geom_vline(xintercept = input$rdt,   lty = 3, alpha = 0.5) +
      xlab("Cost of RDT") + ylab("Cost of Treatment") + 
      ggtitle("Price Comparison")
    
    ggplotly(p)
  })
  
  
}

# Run app ---
shinyApp(ui, server)