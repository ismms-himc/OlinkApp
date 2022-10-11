# Function for module UI
# shared by 
user_select_list_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, textInput(ns("user_select_list"),
                         "User defined batch print list.",
                         placeholder = NULL)),
    column(12, verbatimTextOutput(ns("validate_list")))
  )
  
}


# Function for module server logic
# type defineds analyte or sample_id
user_select_list <- function(input, output, session, values, type) {
  
  observe({
    req(input$user_select_list)
    if(type == "analyte"){
      values$batch_plot_analyte_ls <- strsplit(input$user_select_list, split = ",")%>%unlist()%>%unique()
    }
    if(type == "sample_id"){
      # sample_id 
      values$batch_plot_sample_ls <- strsplit(input$user_select_list, split = ",")%>%unlist()%>%unique()
    }
  })
  
  output$validate_list <- renderText({
    validate(need(input$user_select_list, message = "No user selected list defined."))
    
    if(type == "analyte"){
      test <- values$batch_plot_analyte_ls[!(values$batch_plot_analyte_ls %in% rownames(values$combined_data))]
    }else{
      # sample_id 
      test <- values$batch_plot_sample_ls[!(values$batch_plot_sample_ls %in% values$combined_meta$sample_id)]
    }
    if(length(test) > 0){
      paste(test, "is not a validate input.")
    }else{
      paste("All user inputs are validate.")
    }
    
  })
}