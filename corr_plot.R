# Function for module UI
corr_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(3, selectInput(ns("choice_b0"), "Choose Dataset A", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_b1"), "Choose Dataset B", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_b2"), "Choose Analyte", choices = "", selected = NULL)),
    column(12, plotOutput(ns("corr_plot"),width = "100%", height = "500px")))
  
}


# Function for module server logic
corr_plot <- function(input, output, session, values) {
  
  # corr plot
  observe({
    updateSelectInput(session, "choice_b0", choices = names(values$upload_data))
  })
  observe({
    updateSelectInput(session, "choice_b1", choices = names(values$upload_data))
  })
  observe({
    updateSelectInput(session, "choice_b2", 
                      choices = intersect(rownames(values$upload_data[[input$choice_b0]]), rownames(values$upload_data[[input$choice_b1]])))
  })
  
  cor_df <- reactive({
    req(values$upload_data)
    common_sample <- intersect(values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0], 
                               values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])
    
    data.frame(A = values$upload_data[[input$choice_b0]]@assays@data$npx[
      rownames(values$upload_data[[input$choice_b0]]) == input$choice_b2, 
      match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0])
    ]%>%as.numeric(),
    B = values$upload_data[[input$choice_b1]]@assays@data$npx[
      rownames(values$upload_data[[input$choice_b1]]) == input$choice_b2, 
      match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])
    ]%>%as.numeric(),
    Sample = common_sample,
    Warning = ifelse((values$upload_data[[input$choice_b0]]$QC.Warning[match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0])] == "Pass") & (values$upload_data[[input$choice_b1]]$QC.Warning[match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])] == "Pass"), "Pass", "Warning")
      )%>%
      filter((!is.na(A) & !is.na(B)))
  })
  
  output$corr_plot <- renderPlot({
    req(cor_df())
    if((nrow(cor_df()) != 0) & !is.null(cor_df())){
      range_min <- min(c(as.numeric(cor_df()$A), as.numeric(cor_df()$B)), na.rm = T)-0.1
      range_max <- max(c(as.numeric(cor_df()$A), as.numeric(cor_df()$B)), na.rm = T)+0.1
      
      fit = lm(B ~ A, data = cor_df())
      fit <- abs(cor_df()$B - predict(fit))
      
      cor_df()%>%
        ggscatter(x = "A", y = "B", shape = 21, fill = "Warning",
                  add = "reg.line", conf.int = F)+
        stat_cor(label.x.npc = 0.2, size = 6)+
        lims(x = c(range_min, range_max),
             y = c(range_min, range_max))+
        geom_text_repel(aes(A, B, label = ifelse((fit >= quantile(fit, probs = 0.975)), Sample, NA)))+
        scale_fill_manual(values = c(NA, "red"))+
        labs(title = input$choice_b2)
      }
    })
}