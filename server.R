server <- function(input, output, session) {

  values <- reactiveValues(upload_data = NULL,
                           norm_method = NULL,
                           normed_data = NULL,
                           combined_data = NULL,
                           combined_meta = NULL,
                           select_observe = NULL, # clicked point in pca plot
                           select_observe_data = NULL, # npx values of select_observe (clicked point in pca plot)
                           range_summary = NULL, # 10-90 quantile range summary by analyte (part of PCA box)
                           pca_fit = NULL,
                           batch_plot_sample_ls = NULL, # samples to batch plot in pca.report
                           batch_plot_analyte_ls = NULL, # analyte to batch plot in cor.report
                           n_analyte = NULL)
  

  callModule(module = analyte_panel_querry, id = "id_1", olink_all_analyte_panel)

  callModule(module = reset_dataset, id = "id_1", values)

  callModule(module = input_file_upload, id = "id_1", values)

  callModule(module = show_file_level_meta, id = "id_1", values)
  
  callModule(module = plot_missing_per_analyte, id = "id_1", values)
  
  callModule(module = plot_belowLOD_per_analyte, id = "id_1", values)

  callModule(module = plot_missing_per_analyte, id = "id_1", values)

  callModule(module = plot_belowLOD_per_analyte, id = "id_1", values)

  callModule(module = identify_reference, id = "id_1", values)

  callModule(module = ref_normalization, id = "id_1", values)

  callModule(module = creat_data_obj, id = "id_1", values)

  callModule(module = download_qa_report, id = "id_1", values)

  callModule(module = download_norm_report, id = "id_1", values)

  callModule(module = download_combined_data, id = "id_1", values)

  callModule(module = meta_view_replace, id = "id_1", values)

  callModule(module = qc_global_plot, id = "id_1", values)

  callModule(module = scatter_plot, id = "id_1", values)

  callModule(module = download_scatter_report, id = "id_1", values)
  
  callModule(module = user_select_list, id = "id_3", values, type = "analyte")

  callModule(module = cor_summary_plot, id = "id_1", values)
  
  callModule(module = corr_plot, id = "id_1", values)
  
  callModule(module = user_select_list, id = "id_2", values, type = "analyte")

  callModule(module = download_cor_report, id = "id_1", values)

  callModule(module = pca_plot, id = "id_1", values)
  
  callModule(module = user_select_list, id = "id_1", values, type = "sample_id")
  
  callModule(module = download_pca_report, id = "id_1", values)

}
