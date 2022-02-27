
# Function for module UI
analyte_panel_querry_UI <- function(id) {

  ns <- NS(id)

  fluidRow(
    column(6, selectInput(ns("panel_1"), "Select Olink Panel A.", c("*", panel_query))),
    column(6, selectInput(ns("panel_2"), "Select Olink Panel B.", c("*", panel_query))),
    column(12, DT::DTOutput(ns("analyte_panel_querry_df"))))

}


# Function for module server logic
analyte_panel_querry <- function(input, output, session, olink_all_analyte_panel) {

  output$analyte_panel_querry_df <- DT::renderDT(
    DT::datatable(
      olink_all_analyte_panel%>%
        filter(Protein %in% intersect(olink_all_analyte_panel$Protein[grepl(input$panel_1, olink_all_analyte_panel$Query)],
                                      olink_all_analyte_panel$Protein[grepl(input$panel_2, olink_all_analyte_panel$Query)]))%>%
        select(-Query)%>%
        distinct(),
      rownames = NULL)
  )
}
