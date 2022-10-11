
# Function for module UI
plot_belowLOD_per_analyte_UI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot_belowLOD_per_analyte"),width = "100%")
}


# Function for module server logic
plot_belowLOD_per_analyte <- function(input, output, session, values) {
  
  output$plot_belowLOD_per_analyte <- renderPlot({
    req(values$upload_data)
    lapply(values$upload_data, function(x){
      data.frame(number_of_sample_below_lod = apply(x@assays@data$npx, 2, function(y){
        y_1 <- y
        y_1[is.na(y_1)] <- -99
        y_1 < x@elementMetadata$LOD
        })%>%rowSums(),
                 Plate.ID = x$Plate.ID[1])%>%
        rownames_to_column(var = "Analyte")
    })%>%
      do.call(what = "rbind")%>%
      data.frame()%>%
      mutate(Plate.ID = trim_string_bycommon(Plate.ID))%>%
      ggplot(aes(Analyte, Plate.ID))+
      geom_point(aes(color = number_of_sample_below_lod), alpha = 0.4, size = 8, shape = 15)+
      scale_color_gradient(low = "grey", high = "blue")+
      geom_text(aes(label = ifelse(number_of_sample_below_lod == 0, NA, number_of_sample_below_lod)), size = 6)+
      #facet_wrap(~Plate.ID, ncol = 1, scales = "free")+
      labs(x = "", y = "")+
      theme_bw(base_size = 18)+
      theme(legend.position="bottom", legend.box="vertical",
            axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 15),
            axis.text.y = element_text(size = 15))
    
  }, width = function(){
    if(is.null(values$n_analyte)){
      return(200)
    }else{
      20*values$n_analyte
    }
  })
}