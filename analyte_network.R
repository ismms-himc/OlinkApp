
# Function for module UI
analyte_network_UI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(4, selectInput(ns("sel_panel"), "Select Olink Panel A", panel_query)),
    column(8, selectInput(ns("sel_analytes"), "Select one or more analyte", multiple = T, "")),
    column(12, visNetworkOutput(ns("query_network_vis"))),
    column(6, selectInput(ns("source_type"), "Select one or more source tissue type", 
                          multiple = T, selected = unique(cell2cell$Source_tissue),
                          choices = unique(cell2cell$Source_tissue))),
    column(6, selectInput(ns("target_type"), "Select one or more target tissue type", 
                          multiple = T, selected = unique(cell2cell$Source_tissue),
                          choices = unique(cell2cell$Target_tissue))),
    column(12, DT::DTOutput(ns("analyte_network_df")))
    )

}


# Function for module server logic
analyte_network <- function(input, output, session, olink_all_analyte_panel, cell2cell, analyte_type_db) {
  
  observe({
    panel_uniport <- olink_all_analyte_panel$Uniprot_ID[grepl(input$sel_panel, olink_all_analyte_panel$Query)]
    panel_analyte <- olink_all_analyte_panel$Analyte[grepl(input$sel_panel, olink_all_analyte_panel$Query)]
    intersect_uniport <- c(intersect(panel_uniport, cell2cell$cytokine_uniprot), intersect(panel_uniport, cell2cell$Receptor_uniprot))
    panel_analyte <- panel_analyte[match(unique(intersect_uniport), panel_uniport)]
    updateSelectInput(session, "sel_analytes", "Select one or more analyte", 
                      choices = panel_analyte)
  })
  
  query_return <- reactive({
    req(input$sel_analytes)
    query <- olink_all_analyte_panel$Uniprot_ID[match(input$sel_analytes, olink_all_analyte_panel$Analyte)]
    idx <- sapply(query, function(x){
      c(grep(x, cell2cell$cytokine_uniprot, ignore.case = T), grep(x, cell2cell$Receptor_uniprot, ignore.case = T))%>%unique()
    })
    
    idx <- idx[!sapply(idx, function(x) identical(x, integer(0)))]%>% names()%>% unique()
    
    query_return <- cell2cell%>%
      filter((cytokine_uniprot %in% idx) | (Receptor_uniprot %in% idx))%>%
      merge.data.frame(olink_all_analyte_panel%>%
                         distinct(Analyte, Uniprot_ID), by.x = "cytokine_uniprot", by.y = "Uniprot_ID", all.x = T)%>%
      merge.data.frame(olink_all_analyte_panel%>%
                         distinct(Analyte, Uniprot_ID), by.x = "Receptor_uniprot", by.y = "Uniprot_ID", all.x = T)%>%
      mutate(source = gsub("NA", "--", cytokine_genename),
             target = gsub("NA", "--", cytokine_genename),
             label = paste(cytokine_genename, Receptor_genename, sep = ">>>"))#%>%
      #filter((Source_tissue %in% input$source_type), (Target_tissue %in% input$target_type))
    return(query_return)
  })
  
  
  output$analyte_network_df <- DT::renderDT({
    DT::datatable(
      query_return(),
      options = list(scrollX = TRUE), rownames = NULL)
  })
  
  
  output$query_network_vis <- renderVisNetwork({
    validate(
      need(length(input$sel_analytes) > 1, "Requires at least 2 validate proteins to plot")
    )
    validate(
      need(!is.null(input$source_type)|(!is.null(input$target_type)), "Requires at least 1 source or target tissue type")
    )
    
    qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) #max = 74
    
    nodes <- data.frame(label = unique(c(query_return()$Source_tissue, query_return()$Target_tissue)))%>%
      rowid_to_column(var = "id")%>%
      mutate(title = label,
             value = 1)
    
    set.seed(1234)
    nodes$color = sample(col_vector, nrow(nodes), replace = F)
    
    
    edges <- query_return()%>%
      merge.data.frame(analyte_type_db, by.x = "cytokine_genename", by.y = "gene", all.x = T)%>%
      replace_na(list(type = "other"))%>%
      rename(group = type)%>%
      select(label, Source_tissue, Target_tissue, group)%>%
      merge.data.frame(nodes, by.x = "Source_tissue", by.y = "label", all.x = T)%>%
      rename(from = id)%>%
      merge.data.frame(nodes, by.x = "Target_tissue", by.y = "label", all.x = T)%>%
      rename(to = id)%>%
      select(label, from, to, group)
    
    
    set.seed(1234)
    edges$color <- sample(col_vector, length(unique(edges$group)))[as.numeric(factor(edges$group))]
    
    ledges <- edges%>%
      distinct(color, group)%>%
      rename(label = group)
    
    edges$group <- NULL
    edges$value <- 1
    
    visNetwork(nodes, edges)%>% 
      visIgraphLayout(layout = "layout_nicely") %>% 
      visEdges(arrows = "middle") %>% 
      visLegend(addEdges = ledges)
  })
  
  #observe({
  #  nodes_selection <- input$source_type
  #  visNetworkProxy(("query_network_vis")) %>%
  #    visSelectNodes(id = nodes_selection)
  #})
  
}
