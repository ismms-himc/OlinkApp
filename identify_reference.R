# Function for module UI
identify_reference_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, textInput(ns("ref_idf_str"),
                         "Input common text string to identify Reference/Common Sample among plates.",
                         placeholder = NULL)),
    column(12, DT::DTOutput(ns("ref_sample_summary")))
  )
}


# Function for module server logic
identify_reference <- function(input, output, session, values) {
  
  #observeEvent(input$ref_idf_str,{
  #  req(input$ref_idf_str)
  #  values$ref_sample_identifier <- input$ref_idf_str
  #})
  
  output$ref_sample_summary <- DT::renderDT({
    req(input$ref_idf_str)
    lapply(strsplit(input$ref_idf_str, split = ",")%>%unlist(), function(x){
      bdg_ls <- pull_bdg(values$upload_data, pattern = x)
      data.frame(filename = names(bdg_ls),
                 identifier = x,
                 total_samples = sapply(bdg_ls, function(x) ncol(x)) %>% unlist(),
                 details = sapply(bdg_ls, function(x) paste(x$Assay, collapse = ",")) %>% unlist())
    })%>%
      do.call(what = "rbind")%>%
      DT::datatable(rownames = NULL)
  })
  
}


