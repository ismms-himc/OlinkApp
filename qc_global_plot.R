
# Function for module UI
qc_global_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(6, selectInput(ns("qc_select"), "Choose Identifier", choices = "", selected = NULL)),
    column(12, plotOutput(ns("qc_global_plot"),width = "100%")))
  
}


# Function for module server logic
qc_global_plot <- function(input, output, session, values) {
  
  observe({
    updateSelectInput(session, "qc_select", choices = strsplit(input$ref_idf_str, split = ",")%>%unlist(), selected = NULL)
  })
  
  output$qc_global_plot <- renderPlot({
    req(values$combined_data)
    req(input$qc_select)
    req(values$n_analyte)
    p <- plot_npx_norm_qc(values$combined_data, bridge_pattern = input$qc_select)
    print(p$all)
  }, height = function(){20*values$n_analyte})
}