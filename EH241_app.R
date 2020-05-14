# load libraries
library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(tidyverse)
library(scales) 
# injury count data - manual addition of regex results
injury <- read.csv(url('https://raw.githubusercontent.com/luchenyue95/EH241_ScarletPhoenix/master/injury_cost.csv'), sep=',',check.names = F,header = TRUE)
injury$injury_type
injury$year <- 2019

#---UI---
ui <- fluidPage(
    titlePanel('Budget projections after implementing our proposed EHS interventions'),
    fluidRow(
        column(12, sidebarPanel(
            checkboxGroupInput("intervention", "Select intervention(s)",
                            c("Hire a ergonomist - $130000" = "hire_ergo",
                                 "Hire a certified industrial hygienist (CIH) - $70000" = "hire_IH",
                                 "Buy 2 torque multipliers - $24456" = "torque_multi",
                                 "Buy employees slip resistant shoes - $49000"= "shoes"
                                 )),
            sliderInput("ergo_effect", "Estimated % reduction in ergonomic injuries",
                        min = 0, max = 1,
                        value = 0.2),
            sliderInput("IH_effect", "Estimated % reduction in sharps injuries - CIH",
                        min = 0, max = 1,
                        value = 0.2),
            sliderInput("lifting_effect", "Estimated % reduction in lifting injuries - torque multipliers",
                        min = 0, max = 1,
                        value = 0.2),
            sliderInput("fall_effect", "Estimated% reduction in falls injuries - slip-resistant shoes",
                        min = 0, max = 1,
                        value = 0.2)
        ),
        mainPanel(plotlyOutput('histogram')))
    )
    ,
    DT::dataTableOutput("table"),
    fluidRow(verbatimTextOutput("NPV"))
)

# Define server logic
server <- function(input, output, session) {
    projections <- reactive({
      intervention_cost <- 0
      injury_projections <- injury %>% 
          group_by(injury_type) %>% 
          complete(year = 2019:2025, 
                 cost_per_injury = injury[which(injury$injury_type == injury_type),]$cost_per_injury) %>%
          mutate(intervention_cost = rep(0, length(2019:2025))) %>%
          mutate(NPV = rep(0,length(2019:2025)))%>%
          mutate(ROI = rep(0,length(2019:2025)))
      
    ergo_effect=0
    IH_effect=0
    lifting_effect=0
    fall_effect=0
    chemical_effect <- 0.8
    
    if ('hire_ergo' %in% input$intervention){
      ergo_effect <- input$ergo_effect
      type <- 'Repetitive motion ergonomic injuries'
      ergo_num <- injury_projections[which(injury_projections$injury_type == type),]$number
      ergo_num <- replace(ergo_num, is.na(ergo_num), ceiling(ergo_num[1] * (1-ergo_effect)^(seq(1:6))))
      injury_projections[which(injury_projections$injury_type == type),]$number <- ergo_num
      injury_projections[which(injury_projections$injury_type == type),]$intervention_cost <- c(0, rep(130000,6)) 
      injury_projections[which(injury_projections$injury_type == type),]$NPV <- round(lag(injury_projections[which(injury_projections$injury_type == type),]$number) * injury_projections[which(injury_projections$injury_type == type),]$cost_per_injury* ergo_effect - injury_projections[which(injury_projections$injury_type == type),]$intervention_cost)
      injury_projections[which(injury_projections$injury_type == type),]$ROI <- round(injury_projections[which(injury_projections$injury_type == type),]$NPV / injury_projections[which(injury_projections$injury_type == type),]$intervention_cost ,2)
      remove(type)
      }
    
    if ('hire_IH' %in% input$intervention){
      IH_effect <- input$IH_effect
      type <- 'Sharps injuries'
      IH_num <- injury_projections[which(injury_projections$injury_type == type),]$number
      IH_num = replace(IH_num, is.na(IH_num), IH_num[1] * (1-IH_effect)^(seq(1:6)))
      injury_projections[which(injury_projections$injury_type == type),]$number <- as.integer(IH_num)
      injury_projections[which(injury_projections$injury_type == type),]$intervention_cost <- c(0, rep(70000,6))
      injury_projections[which(injury_projections$injury_type == type),]$NPV <- round(lag(injury_projections[which(injury_projections$injury_type == type),]$number) * injury_projections[which(injury_projections$injury_type ==type),]$cost_per_injury* IH_effect - injury_projections[which(injury_projections$injury_type == type),]$intervention_cost)
      injury_projections[which(injury_projections$injury_type == type),]$ROI <- round(injury_projections[which(injury_projections$injury_type == type),]$NPV / injury_projections[which(injury_projections$injury_type == type),]$intervention_cost, 2)
      remove(type)
      }
    if('torque_multi' %in% input$intervention){
      lifting_effect <- input$lifting_effect
      type <- unique(injury_projections$injury_type)[2]
      lifting_num <- injury_projections[which(injury_projections$injury_type == type),]$number
      lifting_num = replace(lifting_num, is.na(lifting_num), lifting_num[1] * (1-lifting_effect)^(seq(1:6)))
      injury_projections[which(injury_projections$injury_type == type),]$number <- as.integer(lifting_num)
      injury_projections[which(injury_projections$injury_type == type),]$intervention_cost <- c(0, 24456, rep(0,5))
      injury_projections[which(injury_projections$injury_type == type),]$NPV <- round(lag(injury_projections[which(injury_projections$injury_type == type),]$number) * injury_projections[which(injury_projections$injury_type ==type),]$cost_per_injury* lifting_effect - injury_projections[which(injury_projections$injury_type == type),]$intervention_cost)
      injury_projections[which(injury_projections$injury_type == type),]$ROI <- round(injury_projections[which(injury_projections$injury_type == type),]$NPV / injury_projections[which(injury_projections$injury_type == type),]$intervention_cost, 2)
      remove(type)
    }
    if('shoes' %in% input$intervention){
      fall_effect <- input$fall_effect
      type <- 'Slips, trips, or falls on the same level'
      fall_num <- injury_projections[which(injury_projections$injury_type == type),]$number
      fall_num <- replace(fall_num, is.na(fall_num), fall_num[1] * (1-fall_effect)^(seq(1:6)))
      injury_projections[which(injury_projections$injury_type == type),]$number <- as.integer(fall_num)
      injury_projections[which(injury_projections$injury_type == type),]$intervention_cost <- c(0, 49000, rep(0,5))
      injury_projections[which(injury_projections$injury_type == type),]$NPV <- injury_projections[which(injury_projections$injury_type == type),]$number * injury_projections[which(injury_projections$injury_type ==type),]$cost_per_injury* fall_effect - injury_projections[which(injury_projections$injury_type == type),]$intervention_cost
      injury_projections[which(injury_projections$injury_type == type),]$ROI <- round(injury_projections[which(injury_projections$injury_type == type),]$NPV / injury_projections[which(injury_projections$injury_type == type),]$intervention_cost, 2)
      remove(type)
    }

    ergo_num <- injury_projections[which(injury_projections$injury_type == 'Repetitive motion ergonomic injuries'),]$number
    ergo_num = replace(ergo_num, is.na(ergo_num), ergo_num[1] * (1-ergo_effect)^(seq(1:6)))
    injury_projections[which(injury_projections$injury_type == 'Repetitive motion ergonomic injuries'),]$number <- as.integer(ergo_num)
    
    IH_num <- injury_projections[which(injury_projections$injury_type == 'Sharps injuries'),]$number
    IH_num = replace(IH_num, is.na(IH_num), IH_num[1] * (1-IH_effect)^(seq(1:6)))
    injury_projections[which(injury_projections$injury_type == 'Sharps injuries'),]$number <- as.integer(IH_num)
    
    chemical_num <- injury_projections[which(injury_projections$injury_type == 'Chemical injuries'),]$number
    chemical_num = replace(chemical_num, is.na(chemical_num), chemical_num[1] * chemical_effect^(seq(1:6)))
    injury_projections[which(injury_projections$injury_type == 'Chemical injuries'),]$number <- as.integer(chemical_num)
    
    lifting_num <- injury_projections[which(injury_projections$injury_type == unique(injury_projections$injury_type)[2]),]$number
    lifting_num = replace(lifting_num, is.na(lifting_num), lifting_num[1] * (1-lifting_effect)^(seq(1:6)))
    injury_projections[which(injury_projections$injury_type == unique(injury_projections$injury_type)[2]),]$number <- as.integer(lifting_num)
    
    fall_num <- injury_projections[which(injury_projections$injury_type == 'Slips, trips, or falls on the same level'),]$number
    fall_num = replace(fall_num, is.na(fall_num), fall_num[1] * (1-fall_effect)^(seq(1:6)))
    injury_projections[which(injury_projections$injury_type == 'Slips, trips, or falls on the same level'),]$number <- as.integer(fall_num)

    injury_projections$injury_cost <- injury_projections$cost_per_injury * injury_projections$number 
    injury_projections$total_cost <- injury_projections$cost_per_injury * injury_projections$number + injury_projections$intervention_cost
    return(injury_projections)
    })

    output$table <- DT::renderDataTable(DT::datatable({
        injury_projections <- projections()
        data <- injury_projections
        data
    }))
    output$histogram <- renderPlotly(

        ggplot(data = projections(), aes(x = year, y= total_cost/1000000, fill = injury_type)) +
            geom_bar(position="stack", stat="identity") +
            labs(title = 'projection of total costs', y = 'total cost (in million)') 
        )
}

#--Run the application--
shinyApp(ui = ui, server = server)

