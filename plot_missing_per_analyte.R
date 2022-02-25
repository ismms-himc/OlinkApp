
# Function for module UI
plot_missing_per_analyte_UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot_missing_per_analyte"))
}


# Function for module server logic
plot_missing_per_analyte <- function(input, output, session, values) {
  
  output$plot_missing_per_analyte <- renderPlot({
    req(values$upload_data)
    lapply(values$upload_data, function(x){
      apply(x@assays@data$npx, 1, function(y) sum(is.na(y)))
      })%>%
      do.call(what = "rbind")%>%
      data.frame()%>%
      rownames_to_column(var = "Plate.ID")%>%
      mutate(Plate.ID = trim_string_bycommon(Plate.ID))%>%
      gather(-Plate.ID, key = "Analyte", value = "is_na")%>%
      ggplot(aes(Analyte, Plate.ID))+
      geom_point(aes(color = is_na), alpha = 0.5, size = 6, shape = 15)+
      scale_color_gradient(low = "grey", high = "blue")+
      geom_text(aes(label = ifelse(is_na == 0, NA, is_na)))+
      labs(x = "", y = "")+
      theme_bw()+
      theme(legend.position="bottom", legend.box="vertical",
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
    
  })
}