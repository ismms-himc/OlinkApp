# Function for module UI
reset_dataset_UI <- function(id) {
  
  ns <- NS(id)
  
  actionButton(ns("reset_dataset"), "Clear Dataset, Restart Upload")
  
}


# Function for module server logic
reset_dataset <- function(input, output, session, values) {
  
  observeEvent(input$reset_dataset,{
    
    values$upload_data = NULL
    values$norm_method = NULL
    values$normed_data = NULL
    values$combined_data = NULL
    values$combined_meta = NULL
    values$select_observe = NULL
    values$select_observe_data = NULL
    values$range_summary = NULL
    values$pca_fit = NULL
    values$n_analyte = NULL
    
    return(values)
    
    shinyCatch(message("Dataset has been cleared..."))
  })
  
}