
# Function for module UI
show_file_level_meta_UI <- function(id) {
  
  ns <- NS(id)
  
  DT::DTOutput(ns("plate_summary"))
  
}


# Function for module server logic
show_file_level_meta <- function(input, output, session, values) {
  
  output$plate_summary <- DT::renderDT(
    DT::datatable(
      data.frame(software_version = sapply(values$upload_data, function(x) x@metadata$software_version) %>% unlist(),
                 filename = sapply(values$upload_data, function(x) x$file_name[1]) %>% unlist(),
                 panel = sapply(values$upload_data, function(x) x@metadata$panel)%>% unlist(),
                 total_samples = sapply(values$upload_data, function(x) ncol(x[ , !grepl("^NC$|^IPC$|^Randox", ignore.case = T, x$SampleID)])) %>% unlist(),
                 total_analyte = sapply(values$upload_data, function(x) nrow(x[ , !grepl("^NC$|^IPC$|^Randox", ignore.case = T, x$SampleID)])) %>% unlist(),
                 warning_count = sapply(values$upload_data, function(x) sum(x$QC.Warning[!grepl("^NC$|^IPC$|^Randox", ignore.case = T, x$SampleID)] == "Warning")) %>% unlist(),
                 protein_detected = sapply(values$upload_data, function(x){
                   sapply(rownames(x), function(y){
                     temp <- x@assays@data$npx  
                     temp[is.na(temp)] <- -99 
                     ((sum(temp[rownames(x) == y, ] >  x@elementMetadata$LOD[rownames(x) == y]) / (ncol(x) - sum(grepl("^NC$|^IPC$", x$SampleID)))) >= 0.75) 
                     #((sum(temp[rownames(x) == y, ] <  2.5) / 92) < 0.25) 
                     })%>%sum()
                 }) %>% unlist(),
                 na_count = sapply(values$upload_data, function(x) sum(is.na(colSums(x@assays@data$npx[ , !grepl("^NC$|^IPC$|^Randox", ignore.case = T, x$SampleID)])))) %>% unlist()),
      options = list(scrollX = TRUE), rownames = NULL)
  )
}

