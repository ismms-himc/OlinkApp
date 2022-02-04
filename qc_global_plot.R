
# Function for module UI
qc_global_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(6, selectInput(ns("qc_select"), "Choose Bridging Sample", choices = "", selected = NULL)),
    column(12, plotOutput(ns("qc_global_plot"),width = "100%", height = "1800px")))
  
}


# Function for module server logic
qc_global_plot <- function(input, output, session, values) {
  
  observe({
    updateSelectInput(session, "qc_select", choices = strsplit(input$ref_idf_str, split = ",")%>%unlist(), selected = NULL)
  })
  
  output$qc_global_plot <- renderPlot({
    req(values$combined_data)
    req(input$qc_select)
    p <- plot_npx_norm_qc(values$combined_data, bridge_pattern = input$qc_select)
    print(p$all)
  })
}