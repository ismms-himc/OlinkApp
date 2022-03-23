
# Function for module UI
pca_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(12, selectInput(ns("choice_c0"), "Choose Dataset Type", choices = "", selected = NULL)),
    column(6, actionButton(ns("run_pca"), "Run PCA")),
    column(6, radioButtons(ns("shape_missing"), "Highlight Sample with NA value(blank cell in file uploaded)", choices = c("No", "Yes"), inline = TRUE)),
    column(6, selectInput(ns("choice_c1"), "Choose Color Variable", choices = "", selected = "File")),
    column(6, selectInput(ns("choice_c2"), "Choose label var", choices = "", selected = "File")),
    column(12, plotOutput(ns("pca_plot"), click = ns("plot_click"), width = "100%", height = "500px")),
    column(12, h4(p("Click Sample Point on", span("PCA Plot", style = "color:blue"), "to view the sample's Analyte value
                    in", span("10%-90% quantile Range Plot(left panel),", style = "color:blue"), "and ", 
                    span("QC.Wanring-Deviation Plot (right panel),", style = "color:blue")))),
    column(12, h4(p("Range bar in", span("Grey,", style = "color:grey"),
                    "LOD in", span("Blue,", style = "color:blue"),
                    "Clicked Point NPX values in", span("Red.", style = "color:red")))),
    column(8, panel(
      style = "overflow-x:scroll",
      plotOutput(ns("range_plot"), height = "400px")
    )),
    column(4, plotOutput(ns("deviation_plot"), width = "100%", height = "400px")),
    column(12, DT::DTOutput(ns("click_df")))
  )
  
}


# Function for module server logic
pca_plot <- function(input, output, session, values) {
  
  # pca plot 
  observe({
    req(values$combined_data)
    updateSelectInput(session, "choice_c0", choices = names(values$combined_data@assays@data))
  })
  observe({
    updateSelectInput(session, "choice_c1", choices = names(values$combined_meta))
  })
  observe({
    updateSelectInput(session, "choice_c2", choices = names(values$combined_meta))
  })
  
  
  observeEvent(input$run_pca, {
    req(values$combined_data)
    # simple mean impute for na
    mat <- values$combined_data@assays@data[[input$choice_c0]]
    
    values$combined_meta$sample_with_na <- NA
    for (i in 1:ncol(mat)) {
      if(sum(is.na(mat[ , i])) != 0){
        values$combined_meta$sample_with_na[i] <- paste(rownames(mat)[is.na(mat[ , i])], collapse = ",")
      }
    }
    
    mat <- apply(mat, 2, function(x){
      temp <- x
      temp[is.na(temp)] <- ifelse(is.na(mean(temp, na.rm = T)), 0, mean(temp, na.rm = T)) # ifelse for all sample == NA case
      temp
    })
    set.seed(1234)
    values$pca_fit <- prcomp(t(mat))
    values$pca_fit$plot_df <- data.frame(values$combined_meta,
                                         pc_1 = values$pca_fit$x[ ,1],
                                         pc_2 = values$pca_fit$x[ ,2])
  })
  
  observe({
    req(input$plot_click)
      values$select_observe <- nearPoints(values$pca_fit$plot_df, input$plot_click, maxpoints = 1, threshold = 20,
                                             xvar = "pc_1", yvar = "pc_2")%>%
        select(sample_id, Plate.ID, QC.Warning, 
               QC.Deviation.from.median.Inc.Ctrl, 
               QC.Deviation.from.median.Det.Ctrl,
               pc_1, pc_2)
      
      values$select_observe_data <- data.frame(
        npx = values$combined_data@assays@data$npx[ , (values$combined_meta$sample_id == values$select_observe$sample_id) &
                                                      (values$combined_meta$Plate.ID == values$select_observe$Plate.ID)],
        analyte = factor(make.names(values$combined_data@elementMetadata$Analyt), 
                         levels = make.names(values$combined_data@elementMetadata$Analyt))
      )
  })
  
  
  output$click_df <- DT::renderDT(
    DT::datatable(
      values$select_observe,
      rownames = NULL,
      options = list(scrollX = TRUE))
  )
  
  output$pca_plot <- renderPlot({
    req(values$pca_fit)
    
    p <- values$pca_fit$plot_df%>%
      filter(!grepl("(^NC|IPC|Randox)", ignore.case = T, sample_id))%>%
      ggplot(aes_string(x = "pc_1", y = "pc_2", color = input$choice_c1))+
      geom_point(shape = 21, size = 4, alpha = 0.7)+
      stat_ellipse()+
      ggrepel::geom_text_repel(aes_string(label = input$choice_c2))+
      labs(x     = paste0("1st dimension (",
                          round((values$pca_fit$sdev^2/sum(values$pca_fit$sdev^2))[1] * 100),
                          "%)"),
           y     = paste0("2nd dimension (",
                          round((values$pca_fit$sdev^2/sum(values$pca_fit$sdev^2))[2] * 100),
                          "%)"))+
      theme_bw(base_size = 15)
    
    
    if(length(unique(values$pca_fit$plot_df[[input$choice_c1]])) > 10){
      p <- p+
        guides(color = "none")
    }
    
    
    if(input$shape_missing == "No"){
      p <- p
    }else{
      p <- p + geom_point(data = values$pca_fit$plot_df%>%
                      filter(!is.na(sample_with_na)),
                    shape = 8, size = 4, aes_string("pc_1", "pc_2"))
    }
    
    
    if(is.null(values$select_observe)){
      p
    }else{
      p + geom_point(data = values$select_observe, 
                     size = 4, aes_string("pc_1", "pc_2"))
    }
    })
  
  output$deviation_plot <- renderPlot({
    req(values$combined_meta)
    p <- values$combined_meta%>%
      filter(!grepl("(^NC|IPC|Randox)", ignore.case = T, sample_id))%>%
      ggplot(aes(QC.Deviation.from.median.Inc.Ctrl, QC.Deviation.from.median.Det.Ctrl))+
      geom_point(aes(shape = QC.Warning), shape = 21, size = 4)+
      scale_shape_manual(values = c(21, 16))+
      theme_bw(base_size = 15)+
      theme(legend.position="bottom")+
      guides(shape = "none")
    
    if(is.null(values$select_observe)){
      p
    }else{
      p + geom_point(data = values$select_observe, shape = 8,
                     size = 4, color = "red")
    }
    
  })
  
  output$range_plot <- renderPlot({
    req(values$range_summary)
    p <- ggplot(values$range_summary)+
      geom_errorbar(aes(analyte, ymin = low, ymax = hi), color = "grey")+
      geom_point(aes(analyte, LOD), color = "blue", shape = 13)+
      theme_bw(base_size = 15)+
      labs(x = "", y = "NPX")+
      theme(legend.position="bottom", legend.box="vertical",
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
    
    if(is.null(values$select_observe_data)){
      p
    }else if(nrow(values$select_observe) != 0){
      p + geom_point(data = values$select_observe_data,
                     aes(analyte, npx), color = "red")
    }else{
      p
    }
  }, width = function(){
    if(is.null(values$n_analyte)){
      return(200)
    }else{
      20*values$n_analyte
    }
  })
}

