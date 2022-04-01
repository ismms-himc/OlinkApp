

# Function for module UI
download_pca_report_UI <- function(id) {
  
  ns <- NS(id)
  
  downloadButton(ns("download_pca_report"), "Batch Download PCA Report")
}


# Function for module server logic
download_pca_report <- function(input, output, session, values) {
  
  output$download_pca_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      #tmpdir <- tempdir()
      #on.exit(setwd(tmpdir))
      #print(tmpdir)
      tempReport <- file.path(tempdir(), "pca_report.Rmd")
      file.copy("pca_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        rp_upload_data = values$upload_data,
        rp_combined_meta = values$combined_meta,
        rp_combined_data = values$combined_data,
        rp_choice_c0 = input$choice_c0,
        rp_choice_c1 = input$choice_c1,
        rp_choice_c2 = input$choice_c2,
        rp_pca_fit = values$pca_fit,
        rp_range_summary = values$range_summary,
        rp_batch_plot_pca = values$batch_plot_sample_ls
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}