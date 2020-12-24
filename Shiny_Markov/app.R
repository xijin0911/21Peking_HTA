library(truncnorm)
library(shiny)       
source("functions/function.R")


ui <- fluidPage(   
  titlePanel("Sick Sicker Model in Shiny"),   
  sidebarLayout(    
    sidebarPanel( 
      numericInput(inputId = "SI_c_Trt",      
                   label = "Treatment Cost", 
                   value = 200,              
                   min = 0,                   
                   max = 400),               
      numericInput(inputId = "SI_n_sim",     
                   label = "PSA runs",       
                   value = 1000,              
                   min = 0,                   
                   max = 400),               
      sliderInput(inputId = "SI_n_age_init",  
                  label = "Initial Age",      
                  value = 25,                
                  min = 10,                   
                  max = 80),                  
      actionButton(inputId = "run_model",     
                   label   = "Run model")    
    ),  
    mainPanel(                                
      h3("Results Table"),                                 
      tableOutput(outputId = "SO_icer_table"),   
      h3("Cost-effectiveness Plane"),         
      plotOutput(outputId = "SO_CE_plane")       
    )   
  ) 
) 

server <- function(input, output){   
  observeEvent(input$run_model,       
               ignoreNULL = F, {
                 df_model_res = f_wrapper(
                   c_Trt = input$SI_c_Trt,
                   n_age_init = input$SI_n_age_init,
                   n_sim = input$SI_n_sim)
                 output$SO_icer_table <- renderTable({ 
                   df_res_table <- data.frame( 
                     Option =  c("Treatment","No Treatment"), 
                     QALYs  =  c(mean(df_model_res$QALY_Trt),
                                 mean(df_model_res$QALY_NoTrt)),
                     Costs  =  c(mean(df_model_res$Cost_Trt),
                                 mean(df_model_res$Cost_NoTrt)),
                     Inc.QALYs = c(mean(df_model_res$QALY_Trt) - 
                                     mean(df_model_res$QALY_NoTrt),
                                   NA),
                     Inc.Costs = c(mean(df_model_res$Cost_Trt) -
                                     mean(df_model_res$Cost_NoTrt),
                                   NA),
                     ICER = c(mean(df_model_res$ICER), NA)
                   ) 
                   df_res_table[,2:6] = round(
                     df_res_table[,2:6],digits = 2) 
                   df_res_table
                 }) 
                 output$SO_CE_plane <- renderPlot({ 
                   df_model_res$inc_C <- df_model_res$Cost_Trt - 
                     df_model_res$Cost_NoTrt
                   df_model_res$inc_Q <- df_model_res$QALY_Trt - 
                     df_model_res$QALY_NoTrt
                   plot(
                     x = df_model_res$inc_Q, 
                     y = df_model_res$inc_C,
                     xlab = "Incremental QALYs", 
                     ylab = "Incremental Costs", 
                     xlim = c( min(df_model_res$inc_Q,
                                   df_model_res$inc_Q*-1),
                               max(df_model_res$inc_Q,
                                   df_model_res$inc_Q*-1)),
                     ylim = c( min(df_model_res$inc_C,
                                   df_model_res$inc_C*-1),
                               max(df_model_res$inc_C,
                                   df_model_res$inc_C*-1)),
                     abline(h = 0,v=0)
                   ) 
                 })
               }) 
}

shinyApp(ui, server)
