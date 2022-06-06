library (dplyr)
library (ggplot2)
library (plotly)
library(shiny)
library(stringr)
library (purrr)
library(shiny)
library(rsconnect)
options(shiny.maxRequestSize=30*1024^2) # data max 30 mb 
source("function.R")
#Deploy
# rsconnect::deployApp('/app.R')



ui <- fluidPage(
  fileInput("file", "File input:"),
  selectInput("id_covs", "ID Columns",NA),
  selectInput("cat_covs", "Categorical Columns",NA,multiple = TRUE),
  selectInput("conti_covs", "Continuous Columns",NA,multiple = TRUE),
  uiOutput("multi_plt_covs")
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session ) {
  
  multi_plot_covs <- function (num){
    output[[paste0("plot_covs_",num)]] <- renderPlotly(li()[[paste0("fig", num)]])
  }
  
  df_unfiltered <- eventReactive(input$file,{
    req (input$file)
    
    return (reading_df(input$file$datapath))
  })
  
  observeEvent(df_unfiltered(),{
    
    req(df_unfiltered())
    
    ### Covariates Tab ###
    updateSelectInput(session, "id_covs", label = "ID Columns",
                      selected = "NMID",
                      choices = names(df_unfiltered()))
    updateSelectInput(session, "cat_covs", label = "Categorical Columns",
                      selected = c("SEX", "RACE"),
                      choices = names(df_unfiltered()))
    updateSelectInput(session, "conti_covs", label = "Continuous Columns", 
                      selected = c('AGEBL','BSABL'),
                      choices = names(df_unfiltered()))
  })
  
  df <- eventReactive(input$id_covs,{
    df <- df_unfiltered() %>% filter (MDV == 0)
    df <- df %>% filter (!duplicated(get(input$id_covs)))
    return (df)
  })
  
  n_plot_covs <- reactive(length(input$cat_covs) * length (input$conti_covs))
  
  
  li <- reactive (covariates_plot(df(), input$cat_covs, input$conti_covs))
  
  output$multi_plt_covs <- renderUI({
    map(paste0 ("plot_covs_",1:n_plot_covs()), ~ plotlyOutput(.x, height = "300px"))
    
    map (1:n_plot_covs(),~multi_plot_covs(.x))
  })
  
  
}


shinyApp(ui = ui, server = server)
