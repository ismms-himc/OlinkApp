
# Function for module UI
cor_summary_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  column(12, panel(
    style = "overflow-x:scroll",
    plotOutput(ns("cor_summary_plot"), height = "500px")
  ))
}


# Function for module server logic
cor_summary_plot <- function(input, output, session, values) {
  
  cv_cor_mat <- reactive({
    req(values$upload_data)
    validate(need(input$choice_b0 !=  input$choice_b1, message = "Panel avaliable when comparing different files. 
                  Use 'Choose Dataset A' and 'Choose Dataset B' to select 2 different files."))
    
    common_sample <- intersect(values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0], 
                               values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])
    common_analyte <- intersect(rownames(values$upload_data[[input$choice_b0]]), 
                                rownames(values$upload_data[[input$choice_b1]]))
    
    mat_A <- values$upload_data[[input$choice_b0]]@assays@data$npx[match(common_analyte, rownames(values$upload_data[[input$choice_b0]])), 
                                                                   match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b0])]
    mat_B <- values$upload_data[[input$choice_b1]]@assays@data$npx[match(common_analyte, rownames(values$upload_data[[input$choice_b1]])),
                                                                   match(common_sample, values$combined_meta$sample_id[values$combined_meta$file_name == input$choice_b1])]
    
    cv <- matrix(nrow = nrow(mat_A), ncol = ncol(mat_A))
    for (i in 1:nrow(cv)) {
      for (j in 1:ncol(cv)) {
        cv[i, j] <- sd(c(mat_A[i, j], mat_B[i, j]), na.rm = F)/mean(c(mat_A[i, j], mat_B[i, j]), na.rm = F) %>%
          abs()
      }
    }
    
    rownames(cv) <- common_analyte
    colnames(cv) <- common_sample
    
    # cor
    cor <- data.frame(Analyte = common_analyte,
                      cor = sapply(1:length(common_analyte), function(x) 
                        cor(unlist(mat_A[x, ]), unlist(mat_B[x, ]), use = "complete.obs")))
    re <- list("cv" = cv,
               "cor" = cor)
    
    return(re)
  })

  
  output$cor_summary_plot <- renderPlot({
    req(cv_cor_mat())
    p1 <- cv_cor_mat()$cv%>%
      data.frame()%>%
      rownames_to_column(var = "Analyte")%>%
      gather(-Analyte, key = "sample", value = "cv")%>%
      ggplot()+
      geom_point(aes(Analyte, log10(cv)), position = "jitter", shape = 21, alpha = 0.3)+
      geom_text(data = data.frame(Analyte = rownames(cv_cor_mat()$cv),
                                                 median_cv = apply(cv_cor_mat()$cv, 1, median, na.rm = T)),
                               aes(Analyte, log10(median_cv) + 3, label = round(median_cv * 100, 1), color = median_cv), size = 6)+
      scale_color_gradient(low = "grey", high = "red")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 15), 
            axis.text.y = element_text(size = 15),
            legend.position = "none")
    
    p2 <- cv_cor_mat()$cor%>%
      ggplot(aes(Analyte, cor))+
      geom_bar(aes(fill = cor), stat = "identity", alpha = 0.7)+
      geom_text(aes(label = round(cor, 2)))+
      labs(x = "")+
      theme_bw()+
      theme(axis.ticks.x = element_blank())
    
    p2 + p1 + plot_layout(heights = c(1, 3), ncol = 1, guides = "collect") & theme(legend.position = "top") 
    
  }, width = function(){
    if(is.null(values$n_analyte)){
      return(200)
    }else{
      20*values$n_analyte
    }
  })
}


