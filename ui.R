
header <- dashboardHeader(title = "OlinkApp",
                          tags$li(class = "dropdown", actionButton("browser", "browser"),
                                  tags$script("$('#browser').hide();")),
                          dropdownMenuOutput("messageMenu"))
# sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem( "Upload Files", tabName = 'upload_files', icon = icon('import', lib = 'glyphicon')),
    menuItem( "Visulization", tabName = 'data_vis', icon = icon('th-list', lib = 'glyphicon')),
    menuItem( "Olink Pub", tabName = 'olink_pub', icon = icon('th-list', lib = 'glyphicon'))
  )
)

#p0 olink_pub
olink_pub_box <- box(title = "Olink-immuno-oncology-validation-data--Different Speciment Types",
                  status = "info", solidHeader = TRUE, width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  fluidRow(
                    column(12, h4(icon("circle"), "Data source from https://www.olink.com/content/uploads/2021/09/olink-immuno-oncology-validation-data-v2.1.pdf")),
                    column(12, "NPX level of different sample types")),
                  fluidRow(
                    column(12, img(src='olink_pub.png', align = "left", width="100%")))
                  )

#p0 analyte_panel_querry
analyte_panel_querry_box <- box(title = "Overlapping Analyte Panel Query",
                     status = "warning", solidHeader = TRUE, width = 12,
                     collapsible = TRUE,
                     # collapsed = TRUE,
                     fluidRow(
                       column(12, h4(icon("circle"), "Analyte Panel Querry data version Complete Biomarker List_20201201")),
                       column(12, h4("Select Panel to view Analytes/Common Analytes"))),
                     analyte_panel_querry_UI(id = "id_1")
)

# p1.a upload ----
upload_box <- box(title = "Upload Data",
                  status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(12, h4(icon("upload"), "Accept Files of different Olink Panel.")),
                    column(12, h4(icon("circle"), "Select one or multiple Olink files then use the 'UPLOAD' button to start file upload. Addational files can be added by repeating the process.")),
                  ),
                  input_file_upload_UI(id = "id_1"),
                  br(),
                  fluidRow(
                    column(6, reset_dataset_UI(id = "id_1")))
                  )



# p1.b summary plate metadata of upload----
upload_summary_box <- box(title = "Uploaded plate summary stat",
                       collapsible = TRUE,
                       # collapsed = TRUE,
                       status = "primary", solidHeader = TRUE, width = 12,
                       fluidRow(
                         column(12, h4(icon("database"), "Uploaded plate summary stat"))),
                       show_file_level_meta_UI(id = "id_1")
                       )

# p1.c normalization setting ----
reference_normalization_box <- box(title = "Reference Sample Setup and Optional Reference-sample-based Normalization",
                         collapsible = TRUE,
                         #collapsed = TRUE,
                         status = "warning", solidHeader = TRUE, width = 12,
                         fluidRow(
                           column(12, h4(icon("circle"), "Input Identifier to Setup Reference/Common Sample")),
                           column(12, h4(p("If more than one set of reference are used, seperate identifier strings by ','. For example: 'HD1017,HD1018'.", 
                                           span("Do not put empty space after ','", style = "color:red"))))),
                         identify_reference_UI(id = "id_1"),
                        
                         fluidRow(column(12, h4(icon("star"), "Optional Reference-sample-based Normalization--Only within SAME Olink Panel")),
                                  column(12, offset = 1, h4("Method: Reference-sample-based normalization The plate-to-plate variation was adjusted based on 
                                                adjust-factor(plate specific) which makes equal NPX value among reference samples (HD_reference run on each plate)."))),
                         ref_normalization_UI(id = "id_1")
                        )

# p1.c normalization setting ----
creat_obj_box <- box(title = "Creat Data Object",
                     collapsible = TRUE,
                     # collapsed = TRUE,
                     status = "info", solidHeader = TRUE, width = 12,
                     fluidRow(
                       column(12, h4(icon("circle"), "Creat Data Object for Visulization and Download.")),
                       column(12, offset = 1, h4(icon("star"), "If normalization is done, Normed_NPX data will be generated.")),
                       column(12, offset = 1, h4(icon("star"), "Raw_NPX, Normed_NPX, Feature, Meta data, and Clustergrammer input file will be produced in download.")),
                       column(12, offset = 1, h4(icon("star"), "If combining Files of Different Olink Panels, only the COMMON analyte will be remained."))),
                     fluidRow(
                       column(4, creat_data_obj_UI(id = "id_1"))),
                     br(),
                     fluidRow(
                       column(12, h4(div("Download ONLY AVALIABLE after 'Creat Data Object'!!!", style = "color:red"))),
                       column(4, download_qa_report_UI(id = "id_1")),
                       column(4, download_norm_report_UI(id = "id_1")),
                       column(4, download_combined_data_UI(id = "id_1")))
)

# p1.d rename setup ----
meta_view_replace_box <- box(title = "Metadata Setup",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(12, h4(icon("circle"), "Download metadata for optional user input column, Tiempoint or Sample_Type for example.")),
                    column(12, h4(icon("circle"), "Upload modified metadata to replace the current data in display")),
                    column(12, h4(icon("circle"), "***DO NOT change Row Orders!!!"))),
                  meta_view_replace_UI(id = "id_1")
                  )

# p1.e qc plot of normalization ----
qc_plot_box <- box(title = "Global Point Distribution Plot",
                   collapsible = TRUE,
                   collapsed = TRUE,
                   status = "warning", solidHeader = TRUE, width = 12,
                   fluidRow(
                     column(12, h4(div("Use 'Reference Sample Setup Box' to change samples in view. To view all samples, user '*' as identifier.", style = "color:blue"))),
                     column(12, h4(icon("circle"), "If viewing multiple set of identifiers, use the dropdown list.")),
                     column(12, h4(icon("star"), "This panel can also be used to check Reference Sample pre/post Normalization."))),
                   fluidRow(
                     column(12, h4(icon("star"), "Plot shows samples of user's choice (in circle) and analyte's LOD (in black dot)"))),
                   qc_global_plot_UI(id = "id_1")
                        )



# p2.a
scatter_plot_box <- box(title = "Scatter Plot",
                        collapsible = T,
                        collapsed = T,
                        status = "warning", solidHeader = T, width = 12,
                        fluidRow(
                          column(12, download_scatter_report_UI(id = "id_1"))),
                        scatter_plot_UI(id = "id_1")
                        )


# p2.b
corr_plot_box <- box(title = "Correlation Plot--Based on NoneNormalized Data",
                     collapsible = T,
                     collapsed = T,
                     status = "primary", solidHeader = T, width = 12,
                     fluidRow(
                       column(12, download_cor_report_UI(id = "id_1"))),
                     corr_plot_UI(id = "id_1")
                     )


# p2.c
pca_plot_box <- box(title = "PCA Plot",
                    collapsible = T,
                    collapsed = T,
                    status = "info", solidHeader = T, width = 12,
                    fluidRow(
                      column(12, h4(icon("circle"), "NC, IPC, Randox were removed before compute PCA.")),
                      column(12, h4(icon("circle"), "Select dataset and click 'Run PCA' to generate the PCA plot."))),
                    pca_plot_UI(id = "id_1")
                    )


## p2.d
#heatmap_plot_box <- box(title = "Heatmap Plot",
#                    collapsible = T,
#                    collapsed = T,
#                    status = "warning", solidHeader = T, width = 12,
#                    fluidRow(
#                      column(3, selectInput("choice_d0", "Choose Dataset Type", choices = "", selected = NULL)),
#                      column(3, radioButtons("heatmap_scale", "Scale Data",
#                                             choices = c("Z-Score", "Raw"), inline = TRUE))),
#                    fluidRow(
#                      column(12, InteractiveComplexHeatmapOutput("ht_output"))
#                    ))
#


# body ----
body <- dashboardBody(

  add_busy_bar(color = "blue", height = "8px"),
  
  tabItems(tabItem(tabName = "upload_files",
                   fluidRow(upload_box),
                   fluidRow(upload_summary_box),
                   fluidRow(reference_normalization_box),
                   fluidRow(creat_obj_box),
                   fluidRow(meta_view_replace_box),
                   fluidRow(qc_plot_box)),
           
           tabItem(tabName = "data_vis",
                   fluidRow(scatter_plot_box),
                   fluidRow(pca_plot_box),
                   fluidRow(corr_plot_box)#,
                   #fluidRow(heatmap_plot_box)
                   ),
           
           tabItem(tabName = "olink_pub",
                   fluidRow(olink_pub_box),
                   fluidRow(analyte_panel_querry_box)
           )
           )

)
# assemble UI
ui <- dashboardPage(header, sidebar, body,skin = "green")