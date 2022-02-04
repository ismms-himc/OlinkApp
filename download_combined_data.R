# Function for module UI
download_combined_data_UI <- function(id) {
  
  ns <- NS(id)
  
  downloadButton(ns("export_cmb"), "Download Combined Data")
  
}


# Function for module server logic
download_combined_data <- function(input, output, session, values) {
  
  # download combined files
  output$export_cmb <- downloadHandler(
    filename = function() {
      paste0("Exported_", gsub("-", "_", Sys.Date()), ".zip")
    },
    content = function(file){
      
      tmpdir <- tempdir()
      on.exit(setwd(tmpdir))
      print(tmpdir)
      
      # for clustergrammer
      
      clustergm <- lapply(names(values$combined_meta), function(x){
        paste0(gsub("(\\.|-)", "_", x), ": ",values$combined_meta[[x]])
      })%>%
        do.call(what = "rbind")%>%
        rbind(t(scale(t(values$combined_data@assays@data$npx))))%>%
        set_colnames(value = paste0("id: c", 1 : ncol(values$combined_data)))
      
      rownames(clustergm) <- ifelse(rownames(clustergm) %in% rownames(values$combined_data@assays@data$npx), 
                                    paste0("analyte: ", rownames(clustergm)), "")
      
      if(!is.null(values$normed_data)){
        files <- c("Meta.csv", "Feature.csv", "Raw_NPX.csv", "clustergm.csv", "Normed_NPX.csv")
        
        write.csv(data.frame(values$combined_meta), 
                  file = "Meta.csv", row.names = F)
        write.csv(data.frame(values$combined_data@elementMetadata@listData), 
                  file = "Feature.csv", row.names = F)
        write.csv(values$combined_data@assays@data$npx%>%
                    set_colnames(values$combined_data$Assay), 
                  file = "Raw_NPX.csv", row.names = T)
        write.csv(clustergm, 
                  file = "clustergm.csv", row.names = T)
        write.csv(values$combined_data@assays@data$normed%>%
                    set_colnames(values$combined_data$Assay), 
                  file = "Normed_NPX.csv", row.names = T)
        zip(file,files)
      }
      else{
        files <- c("Meta.csv", "Feature.csv", "Raw_NPX.csv", "clustergm.csv")
        
        write.csv(data.frame(values$combined_meta), 
                  file = "Meta.csv", row.names = F)
        write.csv(data.frame(values$combined_data@elementMetadata@listData), 
                  file = "Feature.csv", row.names = F)
        write.csv(values$combined_data@assays@data$npx%>%
                    set_colnames(values$combined_data$Assay), 
                  file = "Raw_NPX.csv", row.names = T)
        write.csv(clustergm, 
                  file = "clustergm.csv", row.names = T)
        zip(file,files)
      }
    }
  )
}