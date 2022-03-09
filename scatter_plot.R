
# Function for module UI
scatter_plot_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(3, selectInput(ns("choice_a0"), "Choose Analyte", choices = "", selected = NULL)),
    column(3, selectInput(ns("choice_a1"), "Choose x axis var", choices = "", selected = "File")),
    column(3, selectInput(ns("choice_a2"), "Choose color var", choices = "", selected = "File")),
    column(3, selectInput(ns("choice_a3"), "Choose label var", choices = "", selected = NULL)),
    column(12, plotlyOutput(ns("scatter_plot"),width = "100%", height = "800px"))
  )
  
}


# Function for module server logic
scatter_plot <- function(input, output, session, values) {
  
  observe({
    updateSelectInput(session, "choice_a0", choices = rownames(values$combined_data)) # analyte
  })
  observe({
    updateSelectInput(session, "choice_a1", choices = names(values$combined_meta), selected = "Plate.ID") # x var
  })
  observe({
    updateSelectInput(session, "choice_a2", choices = names(values$combined_meta), selected = "Plate.ID") # color var
  })
  observe({
    updateSelectInput(session, "choice_a3", choices = names(values$combined_meta)) # label var
  })
  
  output$scatter_plot <- renderPlotly({
    req(values$combined_data)
    
    lod_df <- data.frame(values$combined_data@elementMetadata@listData)
    if(sum(grep("(LOD_)", colnames(lod_df))) == 0){
      lod_df <- cbind(Analyt = lod_df[, 1],
                      LOD = lod_df[ ,grep("(LOD)", colnames(lod_df))])
    }else{
      lod_df <- cbind(Analyt = lod_df[, 1], 
                      lapply(grep("(LOD_)", colnames(lod_df)), function(x){
                        lod_df[ ,x]
                      })%>% do.call(what = "cbind")%>%
                        data.frame()%>%
                        set_colnames(value = colnames(lod_df)[grep("(LOD_)", colnames(lod_df))]))%>%
        gather(-Analyt, key = "Plate.ID", value = "LOD")%>%
        mutate(Plate.ID = gsub("LOD_", "", Plate.ID))
    }
    
    plot_ly(cbind.data.frame(
      values$combined_meta,
      NPX = values$combined_data@assays@data$npx[rownames(values$combined_data) == input$choice_a0, ]%>%as.numeric()
    )%>% left_join(
      data.frame(Plate.ID = unique(values$combined_meta$Plate.ID))%>%
        merge.data.frame(lod_df)%>%
        filter(Analyt == input$choice_a0)%>%
        dplyr::select(-Analyt),
      by = "Plate.ID"), 
    x = ~get(input$choice_a1), 
    y = ~NPX, 
    type = 'box', 
    color = ~get(input$choice_a2), 
    text = ~get(input$choice_a3), 
    boxpoints = "all", jitter = 0.7, pointpos = 0, hoverinfo = "text")%>%
      add_text(x = ~get(input$choice_a1), y = ~LOD, text = "LOD", textposition = "left",marker = list(color = 'rgb(0, 0, 0)', symbol = "arrow-up"))%>% 
      layout(boxmode = "group", xaxis = list(title = ""))
  })
}


