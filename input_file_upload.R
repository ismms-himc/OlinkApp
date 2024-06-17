# Function for module UI
input_file_upload_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, radioButtons(ns("data_type"), "Data Type", choices = c("NPX", "Quantative"), inline = TRUE)),
    column(12, fileInput(ns("raw_file"), label = "Choose files",
                         multiple = TRUE,
                         buttonLabel = "Browse or Drop...",
                         placeholder = "(multiple) xls")),
    column(12, actionButton(ns("add_upload"), label = "UPLOAD"))
  )
}


# Function for module server logic
input_file_upload <- function(input, output, session, values) {
  
  observeEvent(input$add_upload, {
    req(input$raw_file)
    temp_ls <- list()
    
    if(input$data_type == "NPX"){
      for(x in input$raw_file$name){
        #temp_ls[[x]] <- shinyCatch(read_npx(input$raw_file$datapath[input$raw_file$name == x], startrow = 8), 
        #                           blocking_level = "error")
        temp_ls[[x]] <- shinyCatch(read_npx_v2(input$raw_file$datapath[input$raw_file$name == x]) %>% npx_long2se(), 
                                   blocking_level = "error")
        temp_ls[[x]][["file_name"]] <- x
        #colnames(temp_ls[[x]]) <- temp_ls[[x]]$Assay
        colnames(temp_ls[[x]]) <- temp_ls[[x]]$SampleID
      }
    } else{
      for(x in input$raw_file$name){ # not tested yet
        temp_ls[[x]] <- shinyCatch(read_npx_v2(input$raw_file$datapath[input$raw_file$name == x]) %>% npx_long2se(),
                                   blocking_level = "error")
        temp_ls[[x]][["file_name"]] <- x
        colnames(temp_ls[[x]]) <- temp_ls[[x]]$SampleID
      }
    }
    
    values$upload_data <- c(values$upload_data, temp_ls)
    values$n_analyte <- max(sapply(values$upload_data, nrow))
  })
  
}

