
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
upload_summary_box <- box(title = "Uploaded file/s summary stat",
                       collapsible = TRUE,
                       # collapsed = TRUE,
                       status = "primary", solidHeader = TRUE, width = 12,
                       fluidRow(
                         column(12, h4(icon("database"), "Uploaded file/s summary stat"))),
                       show_file_level_meta_UI(id = "id_1")
                       )
#p1.b-1 summary of plate analyte-wise na count below LOD count ----
upload_na_below_lod_box <- box(title = "File level analyte-wise count of samples with analyte value NA(blank cell in file uploaded) or below LOD",
                               collapsible = TRUE,
                               collapsed = TRUE,
                               status = "info", solidHeader = TRUE, width = 12,
                               style='overflow-x: scroll;overflow-y: scroll;',
                               fluidRow(
                                 column(12, h4("File level analyte-wise NA(blank cell in file uploaded) sample count"))),
                               plot_missing_per_analyte_UI(id = "id_1"),
                               fluidRow(
                                 column(12, h4("File level analyte-wise below LOD sample count"))),
                               plot_belowLOD_per_analyte_UI(id = "id_1")
                               )



# p1.c normalization setting ----
reference_normalization_box <- box(title = "Optional Reference Sample Setup and Reference-sample-based Normalization",
                         collapsible = TRUE,
                         #collapsed = TRUE,
                         status = "warning", solidHeader = TRUE, width = 12,
                         fluidRow(
                           column(12, h4(icon("circle"), "Input Identifier to Setup Reference/Common Sample")),
                           column(12, h4(p("If more than one set of reference are used, seperate identifier strings by ", span("Comma", style = "color:red"), "For example: 'HD1017,HD1018'"))),
                           column(12, h4(p("Do not put empty space after ", span("Comma", style = "color:red"))))),
                         identify_reference_UI(id = "id_1"),
                        
                         fluidRow(column(12, h4(icon("star"), "Optional Reference-sample-based Normalization--Only within SAME Olink Panel")),
                                  column(12, offset = 1, h4("Method: The file-to-file variation will be adjusted based on 
                                                adjust-factor(file specific) which makes equal NPX value among reference samples (HD_reference run on each file)."))),
                         ref_normalization_UI(id = "id_1")
                        )

# p1.c normalization setting ----
creat_obj_box <- box(title = "Creat Data Object",
                     collapsible = TRUE,
                     # collapsed = TRUE,
                     status = "info", solidHeader = TRUE, width = 12,
                     fluidRow(
                       column(12, h4(icon("circle"), "Creat Data Object for Visulization and Download.")),
                       column(12, offset = 1, h4(icon("star"), "If user defined reference-sample-based normalization is done, Normed_NPX data will be generated.")),
                       column(12, offset = 1, h4(icon("star"), "Raw_NPX, Normed_NPX, Feature, Meta data, and Clustergrammer input file will be produced in download.")),
                       column(12, offset = 1, h4(icon("star"), "If combining Files of Different Olink Panels, only the COMMON analyte will remain."))),
                     fluidRow(
                       column(4, creat_data_obj_UI(id = "id_1"))),
                     br(),
                     fluidRow(
                       column(12, h4(div("Download and Visualization ONLY AVAILABLE after 'Create Data Object'!!!", style = "color:red"))),
                       br(),
                       column(4, download_qa_report_UI(id = "id_1")),
                       column(4, download_norm_report_UI(id = "id_1")),
                       column(4, download_combined_data_UI(id = "id_1")))
)

# p1.d rename setup ----
meta_view_replace_box <- box(title = "Optional Metadata Setup",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(12, h4(icon("circle"), "After 'Create Data Object', File metadata will be shown below.")),
                    column(12, h4(icon("circle"), "It is optional to 'Download assay Metadata'  to add user input column,  such as Tiempoint, Sample_Type for example.")),
                    column(12, h4(icon("circle"), "Use 'Upload Modified Metadata' then 'Replace existing Metadata' to update the information. Edited metadata columns can be used in Visualization.")),
                    column(12, h4(div("DO NOT change Row Orders!!!", style = "color:red")))),
                  br(),
                  meta_view_replace_UI(id = "id_1")
                  )

# p1.e qc plot of normalization ----
qc_plot_box <- box(title = "Global Point Distribution Plot",
                   collapsible = TRUE,
                   collapsed = TRUE,
                   status = "warning", solidHeader = TRUE, width = 12,
                   style='overflow-x: height:900px;scroll;overflow-y: scroll;',
                   fluidRow(
                     column(12, h4(div("Plot shows samples identified from 'Reference Sample Setup section'  (in circle) and analyte's LOD (in black dot). To view all samples, use '*' (asterisk) as identifier.", style = "color:blue"))),
                     column(12, h4(icon("circle"), "If multiple sets of identifiers were defined, use the dropdown list.")),
                     column(12, h4(icon("star"), "If 'Optional Reference-sample-based Normalization' is used, this panel can also be used to check Reference Sample pre/post Normalization."))),
                   qc_global_plot_UI(id = "id_1")
                        )



# p2.a
scatter_plot_box <- box(title = "Scatter Plot",
                        collapsible = T,
                        collapsed = T,
                        status = "warning", solidHeader = T, width = 12,
                        fluidRow(
                          column(12, h4(div("This section plots NPX data from uploaded file, to view assay metadata or user updated additional metadata.", style = "color:blue"))),
                          column(12, h4(icon("circle"), "Use dropdown option to query a single analyte, and set a preview template for the download report."))),
                        scatter_plot_UI(id = "id_1"),
                        fluidRow(column(12, h4(icon("star"), "User defined analyte for batch plot downloads. If more than one analytes are listed, seperate analytes by Comma.")),
                                 column(12, h4(div("If no user input is made, use 'Batch Download Scatter Plots' to download correlation plots for all analytes", style = "color:blue")))),
                        user_select_list_UI(id = "id_3"),
                        fluidRow(
                          column(12, download_scatter_report_UI(id = "id_1")))
                        )


# p2.b
corr_plot_box <- box(title = "Correlation Plot--Based on NoneNormalized Data",
                     collapsible = T,
                     collapsed = T,
                     status = "primary", solidHeader = T, width = 12,
                     fluidRow(column(12, h4(icon("star"), "User defined analyte for batch plot downloads. If more than one analytes are listed, seperate analytes by Comma.")),
                              column(12, h4(div("If no user input is made, use 'Batch Download Correlation Plots' to download correlation plots for all analytes", style = "color:blue")))),
                     user_select_list_UI(id = "id_2"),
                     fluidRow(
                       column(12, h4(div("This section plots NPX data from uploaded files, only common samples and common analytes will be used for plotting.", style = "color:blue"))),
                       column(12, h4(icon("circle"), "'Batch Download Correlation Plots' will create a html report for all analytes.  Use dropdown option to query a single analyte.")),
                       column(12, download_cor_report_UI(id = "id_1"))),
                     corr_plot_UI(id = "id_1"),
                     fluidRow(
                       column(12, h4("When viewing different files, TOP panel showes pearson correlation barplot, BOTTOM pannel shows CV plots in log10(scale) with median CV(in percentage) labeled")),
                       cor_summary_plot_UI(id = "id_1"))
                     )


# p2.c
pca_plot_box <- box(title = "PCA Plot",
                    collapsible = T,
                    collapsed = T,
                    status = "info", solidHeader = T, width = 12,
                    fluidRow(
                      column(12, h4(icon("circle"), "NC, IPC, Randox were removed before computing PCA.")),
                      column(12, h4(icon("circle"), "For visulization purpose, NA value(blank cell in file uploaded) is imputated by analyte-wise mean")),
                      column(12, h4(icon("circle"), "Select dataset and click 'Run PCA' to generate the PCA plot.")),
                      column(12, h4(icon("circle"), "If 'Optional Reference-sample-based Normalization' is done, 'normed_NPX' will be available in the 'Choose Dataset Type' dropdown list."))),
                    pca_plot_UI(id = "id_1"),
                    fluidRow(
                      column(12, h4(icon("circle"), "'Batch Download PCA NA Checking Report' will create a html report for all samples with blank value, showing sample’s NPX value 10%-90% quantile Range Plot,  sample highlighted in red Warning-Deviation Plot, sample highlighted with the name of missing analyte displayed in PCA plot.")),
                      column(12, h4(icon("circle"), "PCA color code in this section is a  preview for the download report’s PCA plot.")),
                      column(12, h4(icon("star"), "User defined sample_id for batch plot downloads. If more than one samplie_id are listed, seperate samplie_ids by Comma.")),
                      column(12, h4(div("If no user input is made, use 'Batch Download PCA Report' to download sample with NA values(blank cell in file uploaded)", style = "color:blue")))),
                    user_select_list_UI(id = "id_1"),
                    fluidRow(
                      column(12, download_pca_report_UI(id = "id_1")))
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
                   fluidRow(upload_na_below_lod_box),
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