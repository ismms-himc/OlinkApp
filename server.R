server <- function(input, output, session) {
  
  values <- reactiveValues(upload_data = NULL,
                           norm_method = NULL,
                           normed_data = NULL,
                           combined_data = NULL,
                           combined_meta = NULL,
                           pca_fit = NULL)
  
  callModule(module = analyte_panel_querry, id = "id_1", olink_all_analyte_panel)
  
  callModule(module = reset_dataset, id = "id_1", values)
  
  callModule(module = input_file_upload, id = "id_1", values)
  #observeEvent(input$add_upload, {
  #  req(input$raw_file)
  #  message(input$raw_file$name)
  #  
  #  temp_ls <- list()
  #  if(input$data_type == "NPX"){
  #    for(x in input$raw_file$name){
  #      temp_ls[[x]] <- read_npx(input$raw_file$datapath[input$raw_file$name == x], startrow = 8)
  #      temp_ls[[x]][["file_name"]] <- x
  #      colnames(temp_ls[[x]]) <- temp_ls[[x]]$Assay
  #    }
  #  } else{
  #    for(x in input$raw_file$name){
  #      temp_ls[[x]] <- read_npx(input$raw_file$datapath[input$raw_file$name == x], startrow = 9)
  #      temp_ls[[x]][["file_name"]] <- x
  #      colnames(temp_ls[[x]]) <- temp_ls[[x]]$Assay
  #    }
  #  }
  #  values$upload_data <- c(values$upload_data, temp_ls)
  #  
  #})
 
  callModule(module = show_file_level_meta, id = "id_1", values)
  #--plate metadata
  #output$plate_summary <- DT::renderDT(
  #  DT::datatable(
  #    data.frame(software_version = sapply(values$upload_data, function(x) x@metadata$software_version) %>% unlist(),
  #               filename = sapply(values$upload_data, function(x) x$file_name[1]) %>% unlist(),
  #               panel = sapply(values$upload_data, function(x) x@metadata$panel)%>% unlist(),
  #               total_samples = sapply(values$upload_data, function(x) ncol(x)) %>% unlist(),
  #               total_analyte = sapply(values$upload_data, function(x) nrow(x)) %>% unlist()),
  #    rownames = NULL)
  #  )
  
  #-bridge sample summary 
  #output$bridge_summary <- DT::renderDT({
  #  req(input$bridging_str)
  #  bdg_ls <- pull_bdg(values$upload_data, pattern = input$bridging_str)
  #  DT::datatable(
  #    data.frame(filename = names(bdg_ls),
  #               total_samples = sapply(bdg_ls, function(x) ncol(x)) %>% unlist(),
  #               total_analyte = sapply(bdg_ls, function(x) paste(x$Assay, collapse = ",")) %>% unlist()),
  #    rownames = NULL)
  #  })
  
  callModule(module = identify_reference, id = "id_1", values)
  
  callModule(module = ref_normalization, id = "id_1", values)
  
  #output$bridge_summary <- DT::renderDT({
  #  req(input$raw_file)
  #  req(input$bridging_str)
  #  lapply(strsplit(input$bridging_str, split = ",")%>%unlist(), function(x){
  #    bdg_ls <- pull_bdg(values$upload_data, pattern = x)
  #    data.frame(filename = names(bdg_ls),
  #               identifier = x,
  #               total_samples = sapply(bdg_ls, function(x) ncol(x)) %>% unlist(),
  #               total_analyte = sapply(bdg_ls, function(x) paste(x$Assay, collapse = ",")) %>% unlist())
  #  })%>%
  #    do.call(what = "rbind")%>%
  #    DT::datatable(rownames = NULL)
  #})
  
  # normalization 
  #observeEvent(input$normalize,{
  #  req(input$bridging_str)
  #  req(input$norm_method)
  #  #bdg_ls <- pull_bdg(values$upload_data, pattern = input$bridging_str)
  #  #names(bdg_ls) <- names(values$upload_data) <- unlist(input$raw_file$name)
  #  #temp <- bdg_norm(bridge.ls = bdg_ls, data.ls = values$upload_data, between.plate.method = input$norm_method)#%>%cmb_npx_se()
  #  temp <- bdg_norm_multi(bridge.str =  strsplit(input$bridging_str, split = ",")%>%unlist(), 
  #                         data.ls = values$upload_data, between.plate.method = input$norm_method)
  #  #colnames(temp) <- temp$Assay
  #  values$norm_method <- input$norm_method
  #  values$normed_data <- temp
  #})
  
  # download normalization report
  #output$download_norm_report <- downloadHandler(
  #  # For PDF output, change this to "report.pdf"
  #  filename = "report.html",
  #  content = function(file) {
  #    # Copy the report file to a temporary directory before processing it, in
  #    # case we don't have write permissions to the current working dir (which
  #    # can happen when deployed).
  #    
  #    #tmpdir <- tempdir()
  #    #on.exit(setwd(tmpdir))
  #    #print(tmpdir)
  #    tempReport <- file.path(tempdir(), "norm_report.Rmd")
  #    file.copy("norm_report.Rmd", tempReport, overwrite = TRUE)
  #    
  #    # Set up parameters to pass to Rmd document
  #    params <- list(
  #      rp_upload_data = values$upload_data,
  #      rp_bridging_str = input$bridging_str,
  #      rp_combined_data = values$combined_data,
  #      rp_combined_meta = values$combined_meta
  #    )
  #    
  #    # Knit the document, passing in the `params` list, and eval it in a
  #    # child of the global environment (this isolates the code in the document
  #    # from the code in this app).
  #    rmarkdown::render(tempReport, output_file = file,
  #                      params = params,
  #                      envir = new.env(parent = globalenv())
  #    )
  #  }
  #)
  
  #creat data object combine files
  #observeEvent(input$combine_files, {
  #  if(length(values$upload_data) == 1){
  #    values$combined_data <- values$upload_data[[1]]
  #  }
  #  else if(!is.null(values$normed_data)){
  #    values$combined_data <- cmb_npx_se(values$normed_data) #?? clean
  #  }else{
  #    values$combined_data <- cmb_npx_se(values$upload_data)
  #  }
  #  values$combined_meta <- data.frame(values$combined_data@colData)
  #  #colnames(values$combined_meta)[1] <- "sample_id"
  #  values$combined_meta$unique_id <- NULL
  #  colnames(values$combined_meta)[which(colnames(values$combined_meta) == "Assay")] <- "sample_id"
  #  ref_pattern <- paste(strsplit(input$bridging_str, split = ",")%>%unlist(), collapse = "|")
  #  ref_pattern <- paste0("(", ref_pattern, ")")
  #  values$combined_meta$ref_sample <- ifelse(grepl(ref_pattern, ignore.case = T, values$combined_meta$sample_id), "ref_sample", "study_sample")
  #})
  #
  # qc plot for normalization
  #observeEvent(input$combine_files, {
  #  updateSelectInput(session, "qc_view", choices = strsplit(input$bridging_str, split = ",")%>%unlist())
  #})
  #
  #output$plot_norm_qc <- renderPlot({
  #  req(values$combined_data)
  #  req(input$qc_view)
  #  p <- plot_npx_norm_qc(values$combined_data, bridge_pattern = input$qc_view)
  #  print(p$all)
  #})
  
  
  # download qa report
  #output$download_qa_report <- downloadHandler(
  #  # For PDF output, change this to "report.pdf"
  #  filename = "report.html",
  #  content = function(file) {
  #    # Copy the report file to a temporary directory before processing it, in
  #    # case we don't have write permissions to the current working dir (which
  #    # can happen when deployed).
  #    
  #    #tmpdir <- tempdir()
  #    #on.exit(setwd(tmpdir))
  #    #print(tmpdir)
  #    tempReport <- file.path(tempdir(), "report.Rmd")
  #    file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #    
  #    # Set up parameters to pass to Rmd document
  #    params <- list(
  #      rp_upload_data = values$upload_data,
  #      rp_bridging_str = input$bridging_str,
  #      rp_combined_data = values$combined_data,
  #      rp_combined_meta = values$combined_meta
  #                   )
  #    
  #    # Knit the document, passing in the `params` list, and eval it in a
  #    # child of the global environment (this isolates the code in the document
  #    # from the code in this app).
  #    rmarkdown::render(tempReport, output_file = file,
  #                      params = params,
  #                      envir = new.env(parent = globalenv())
  #    )
  #  }
  #)
  callModule(module = creat_data_obj, id = "id_1", values)
  
  callModule(module = download_qa_report, id = "id_1", values)
  
  callModule(module = download_norm_report, id = "id_1", values)
  
  callModule(module = download_combined_data, id = "id_1", values)
  
  callModule(module = meta_view_replace, id = "id_1", values)
  
  callModule(module = qc_global_plot, id = "id_1", values)
  # download combined files
  #output$export_cmb <- downloadHandler(
  #  filename = function() {
  #    paste0("Exported_", gsub("-", "_", Sys.Date()), ".zip")
  #  },
  #  content = function(file){
  #    
  #    tmpdir <- tempdir()
  #    on.exit(setwd(tmpdir))
  #    print(tmpdir)
  #    
  #    # for clustergrammer
  #    
  #    clustergm <- lapply(names(values$combined_meta), function(x){
  #      paste0(gsub("(\\.|-)", "_", x), ": ",values$combined_meta[[x]])
  #    })%>%
  #      do.call(what = "rbind")%>%
  #      rbind(t(scale(t(values$combined_data@assays@data$npx))))%>%
  #      set_colnames(value = paste0("id: c", 1 : ncol(values$combined_data)))
  #    
  #    rownames(clustergm) <- ifelse(rownames(clustergm) %in% rownames(values$combined_data@assays@data$npx), 
  #                                  paste0("analyte: ", rownames(clustergm)), "")
  #    
  #    if(!is.null(values$normed_data)){
  #      files <- c("Meta.csv", "Feature.csv", "Raw_NPX.csv", "clustergm.csv", "Normed_NPX.csv")
  #      
  #      write.csv(data.frame(values$combined_meta), 
  #                file = "Meta.csv", row.names = F)
  #      write.csv(data.frame(values$combined_data@elementMetadata@listData), 
  #                file = "Feature.csv", row.names = F)
  #      write.csv(values$combined_data@assays@data$npx%>%
  #                  set_colnames(values$combined_data$Assay), 
  #                file = "Raw_NPX.csv", row.names = T)
  #      write.csv(clustergm, 
  #                file = "clustergm.csv", row.names = T)
  #      write.csv(values$combined_data@assays@data$normed%>%
  #                  set_colnames(values$combined_data$Assay), 
  #                file = "Normed_NPX.csv", row.names = T)
  #      zip(file,files)
  #    }
  #    else{
  #      files <- c("Meta.csv", "Feature.csv", "Raw_NPX.csv", "clustergm.csv")
  #      
  #      write.csv(data.frame(values$combined_meta), 
  #                file = "Meta.csv", row.names = F)
  #      write.csv(data.frame(values$combined_data@elementMetadata@listData), 
  #                file = "Feature.csv", row.names = F)
  #      write.csv(values$combined_data@assays@data$npx%>%
  #                  set_colnames(values$combined_data$Assay), 
  #                file = "Raw_NPX.csv", row.names = T)
  #      write.csv(clustergm, 
  #                file = "clustergm.csv", row.names = T)
  #      zip(file,files)
  #    }
  #  }
  #)
  
  #download metadata for user modification 
  #output$export_meta <- downloadHandler(
  #  filename = function() {
  #    paste0("Exported_meta", gsub("-", "_", Sys.Date()), ".csv")
  #  },
  #  content = function(file){
  #    
  #    tmpdir <- tempdir()
  #    on.exit(setwd(tmpdir))
  #    print(tmpdir)
  #    
  #    write.csv(data.frame(
  #      #values$combined_data@colData@listData
  #      values$combined_meta
  #      ), 
  #              file, row.names = F)
  #    
  #  }
  #)  
  #
  ## render and update update metadata
  #
  #proxy <- DT::dataTableProxy("meta_table")
  #
  #observeEvent(input$add_meta, {
  #  req(input$import_meta)
  #  message(input$import_meta$name)
  #  
  #  values$combined_meta <- read.csv(input$import_meta$datapath)
  #  DT::replaceData(proxy, values$combined_meta, resetPaging = FALSE)
  #})
  #
  #
  #output$meta_table <- DT::renderDT({
  #  DT::datatable(values$combined_meta, editable = TRUE, options = list(scrollX = TRUE))
  #  
  #})
  
  
  #b plot functions
  
  # scatter plot
  #observe({
  #  updateSelectInput(session, "choice_a0", choices = rownames(values$combined_data)) # analyte
  #})
  #observe({
  #  updateSelectInput(session, "choice_a1", choices = names(values$combined_meta), selected = "Plate.ID") # x var
  #  })
  #observe({
  #  updateSelectInput(session, "choice_a2", choices = names(values$combined_meta), selected = "Plate.ID") # color var
  #})
  #observe({
  #  updateSelectInput(session, "choice_a3", choices = names(values$combined_meta)) # label var
  #})
  #
  #output$scatter_plot <- renderPlotly({
  #  req(values$combined_data)
  #  
  #  lod_df <- data.frame(values$combined_data@elementMetadata@listData)
  #  if(sum(grep("(LOD_)", colnames(lod_df))) == 0){
  #    lod_df <- cbind(Analyt = lod_df[, 1],
  #                    LOD = lod_df[ ,grep("(LOD)", colnames(lod_df))])
  #  }else{
  #    lod_df <- cbind(Analyt = lod_df[, 1], 
  #                    lapply(grep("(LOD_)", colnames(lod_df)), function(x){
  #                      lod_df[ ,x]
  #                    })%>% do.call(what = "cbind")%>%
  #                      data.frame()%>%
  #                      set_colnames(value = colnames(lod_df)[grep("(LOD_)", colnames(lod_df))]))%>%
  #      gather(-Analyt, key = "Plate.ID", value = "LOD")%>%
  #      mutate(Plate.ID = gsub("LOD_", "", Plate.ID))
  #  }
  #  
  #  plot_ly(cbind.data.frame(
  #    values$combined_meta,
  #    NPX = values$combined_data@assays@data$npx[rownames(values$combined_data) == input$choice_a0, ]%>%as.numeric()
  #    )%>% left_join(
  #      data.frame(Plate.ID = unique(values$combined_meta$Plate.ID))%>%
  #        merge.data.frame(lod_df)%>%
  #        filter(Analyt == input$choice_a0)%>%
  #        dplyr::select(-Analyt),
  #        by = "Plate.ID"), 
  #    x = ~get(input$choice_a1), 
  #    y = ~NPX, 
  #    type = 'box', 
  #    color = ~get(input$choice_a2), 
  #    text = ~get(input$choice_a3), 
  #    boxpoints = "all", jitter = 0.7, pointpos = 0, hoverinfo = "text")%>%
  #    add_text(x = ~get(input$choice_a1), y = ~LOD, text = "LOD", textposition = "left",marker = list(color = 'rgb(0, 0, 0)', symbol = "arrow-up"))%>% 
  #    layout(boxmode = "group", xaxis = list(title = ""))
  #})
  
  callModule(module = scatter_plot, id = "id_1", values)
  callModule(module = download_scatter_report, id = "id_1", values)
  # download scatter report
  #output$download_scat_report <- downloadHandler(
  #  # For PDF output, change this to "report.pdf"
  #  filename = "report.html",
  #  content = function(file) {
  #    # Copy the report file to a temporary directory before processing it, in
  #    # case we don't have write permissions to the current working dir (which
  #    # can happen when deployed).
  #    
  #    #tmpdir <- tempdir()
  #    #on.exit(setwd(tmpdir))
  #    #print(tmpdir)
  #    tempReport <- file.path(tempdir(), "scat_report.Rmd")
  #    file.copy("scat_report.Rmd", tempReport, overwrite = TRUE)
  #    
  #    # Set up parameters to pass to Rmd document
  #    params <- list(
  #      rp_upload_data = values$upload_data,
  #      rp_bridging_str = input$bridging_str,
  #      rp_combined_data = values$combined_data,
  #      rp_combined_meta = values$combined_meta,
  #      rp_choice_a1 = input$choice_a1,
  #      rp_choice_a2 = input$choice_a2
  #    )
  #    
  #    # Knit the document, passing in the `params` list, and eval it in a
  #    # child of the global environment (this isolates the code in the document
  #    # from the code in this app).
  #    rmarkdown::render(tempReport, output_file = file,
  #                      params = params,
  #                      envir = new.env(parent = globalenv())
  #    )
  #  }
  #)  
  
  callModule(module = corr_plot, id = "id_1", values)
  # corr plot
  #observe({
  #  updateSelectInput(session, "choice_b0", choices = names(values$upload_data))
  #})
  #observe({
  #  updateSelectInput(session, "choice_b1", choices = names(values$upload_data))
  #})
  #observe({
  #  updateSelectInput(session, "choice_b2", 
  #                    choices = intersect(rownames(values$upload_data[[input$choice_b0]]), rownames(values$upload_data[[input$choice_b1]])))
  #})
  #
  #cor_df <- reactive({
  #  req(values$upload_data)
  #  #common_sample <- intersect(values$upload_data[[input$choice_b0]]$Assay, values$upload_data[[input$choice_b1]]$Assay)
  #  common_sample <- intersect(values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0], 
  #                             values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])
  #  
  #  data.frame(A = values$upload_data[[input$choice_b0]]@assays@data$npx[
  #    rownames(values$upload_data[[input$choice_b0]]) == input$choice_b2, 
  #    #match(common_sample, values$upload_data[[input$choice_b0]]$Assay)
  #    match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0])
  #    ]%>%as.numeric(),
  #             B = values$upload_data[[input$choice_b1]]@assays@data$npx[
  #               rownames(values$upload_data[[input$choice_b1]]) == input$choice_b2, 
  #               #match(common_sample, values$upload_data[[input$choice_b1]]$Assay)]%>%as.numeric(),
  #               match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])
  #               ]%>%as.numeric(),
  #             Sample = common_sample)%>%
  #    filter((!is.na(A) & !is.na(B)))
  #})
  #
  #output$corr_plot <- renderPlot({
  #   cor_df()%>%
  #   ggscatter(x = "A", y = "B", palette = "jco",
  #              add = "reg.line", conf.int = F)+
  #    stat_cor(label.x.npc = 0.2, size = 6)+
  #    labs(title = input$choice_b2)
  #})
  
  callModule(module = download_cor_report, id = "id_1", values)
  # download cor report
  #output$download_cor_report <- downloadHandler(
  #  # For PDF output, change this to "report.pdf"
  #  filename = "report.html",
  #  content = function(file) {
  #    # Copy the report file to a temporary directory before processing it, in
  #    # case we don't have write permissions to the current working dir (which
  #    # can happen when deployed).
  #    
  #    #tmpdir <- tempdir()
  #    #on.exit(setwd(tmpdir))
  #    #print(tmpdir)
  #    tempReport <- file.path(tempdir(), "cor_report.Rmd")
  #    file.copy("cor_report.Rmd", tempReport, overwrite = TRUE)
  #    
  #    # Set up parameters to pass to Rmd document
  #    params <- list(
  #      rp_upload_data = values$upload_data,
  #      rp_combined_meta = values$combined_meta,
  #      rp_choice_b0 = input$choice_b0,
  #      rp_choice_b1 = input$choice_b1
  #    )
  #    
  #    # Knit the document, passing in the `params` list, and eval it in a
  #    # child of the global environment (this isolates the code in the document
  #    # from the code in this app).
  #    rmarkdown::render(tempReport, output_file = file,
  #                      params = params,
  #                      envir = new.env(parent = globalenv())
  #    )
  #  }
  #)
  
  callModule(module = pca_plot, id = "id_1", values)
  # pca plot 
  #observeEvent(input$combine_files, {
  #  updateSelectInput(session, "choice_c0", choices = names(values$combined_data@assays))
  #})
  #observe({
  #  updateSelectInput(session, "choice_c1", choices = names(values$combined_meta))
  #})
  #observe({
  #  updateSelectInput(session, "choice_c2", choices = names(values$combined_meta))
  #})
  #
  #
  #observeEvent(input$run_pca, {
  #  req(values$combined_data)
  #  # simple mean impute for na
  #  mat <- values$combined_data@assays@data[[input$choice_c0]]
  #  mat <- apply(mat, 2, function(x){
  #    temp <- x
  #    temp[is.na(temp)] <- mean(temp, na.rm = T)
  #    temp
  #  })
  #  set.seed(1234)
  #  values$pca_fit <- prcomp(t(mat))
  #  values$pca_fit$plot_df <- data.frame(values$combined_meta,
  #                                       pc_1 = values$pca_fit$x[ ,1],
  #                                       pc_2 = values$pca_fit$x[ ,2])
  #})
  #
  #
  #output$pca_plot <- renderPlot({
  #  req(values$pca_fit)
  #  
  #  values$pca_fit$plot_df%>%
  #    filter(!grepl("(^NC|IPC)", sample_id))%>%
  #    ggplot(aes_string("pc_1", "pc_2", color = input$choice_c1), alpha = 0.7, shape = 21)+
  #    geom_point()+
  #    labs(x     = paste0("1st dimension (",
  #                        round((values$pca_fit$sdev^2/sum(values$pca_fit$sdev^2))[1] * 100),
  #                        "%)"),
  #         y     = paste0("2nd dimension (",
  #                        round((values$pca_fit$sdev^2/sum(values$pca_fit$sdev^2))[2] * 100),
  #                        "%)"))+
  #    stat_ellipse()+
  #    ggrepel::geom_text_repel(aes_string(label = input$choice_c2))+
  #    theme_bw()+
  #    guides(color = "none")
  #  
  #  })
    
  
  
}

  





#-----
