# Function for module UI
creat_data_obj_UI <- function(id) {
  ns <- NS(id)
  actionButton(ns("creat_data_obj"), "Creat Data Object", class = "btn-warning")
}


# Function for module server logic
creat_data_obj <- function(input, output, session, values) {
  
  #creat data object combine files
  observeEvent(input$creat_data_obj, {
    if(length(values$upload_data) == 1){
      values$combined_data <- values$upload_data[[1]]
    }
    else if(!is.null(values$normed_data)){
      values$combined_data <- cmb_npx_se(values$normed_data) #?? clean
    }else{
      values$combined_data <- cmb_npx_se(values$upload_data)
    }
    values$combined_meta <- data.frame(values$combined_data@colData)
    #colnames(values$combined_meta)[1] <- "sample_id"
    values$combined_meta$unique_id <- NULL
    colnames(values$combined_meta)[which(colnames(values$combined_meta) == "Assay")] <- "sample_id"
    
    values$range_summary <- data.frame(Plate.ID = values$combined_meta$Plate.ID, 
                                       t(values$combined_data@assays@data$npx))%>%
      gather(-Plate.ID, key = "analyte", value = "npx")%>%
      group_by(analyte)%>%
      summarize(low = quantile(npx, probs = 0.1, na.rm = T),
                hi = quantile(npx, probs = 0.9, na.rm = T))%>%
      left_join(
        data.frame(analyte = make.names(values$combined_data@elementMetadata$Analyt),
                   LOD = values$combined_data@elementMetadata$LOD))%>%
      mutate(analyte = factor(analyte, levels = make.names(values$combined_data@elementMetadata$Analyt)))
    
    ref_pattern <- paste(strsplit(input$ref_idf_str, split = ",")%>%unlist(), collapse = "|")
    ref_pattern <- paste0("(", ref_pattern, ")")
    values$combined_meta$ref_sample <- ifelse(grepl(ref_pattern, ignore.case = T, values$combined_meta$sample_id), "ref_sample", "study_sample")
    values$ref_sample_identifier <- input$ref_idf_str # update ref_sample sotred, might be different from Normalization 
    shinyCatch(message("Data Object is Created"))
  })
}