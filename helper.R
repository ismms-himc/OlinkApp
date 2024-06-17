read_npx_v2 <- function(file_name, type = "NPX"){
  
  if(grepl("xlsx$", file_name)){
    npx <- readxl::read_xlsx(file_name, sheet = 1, col_names = F, .name_repair = "unique_quiet")
  }else if(grepl("csv$", file_name)){
    npx <- read.delim(file_name, header = F, sep = ",")
  }else{
    stop("input a valid raw olink data!")
  }
  
  #long/short format check
  sector_gap <- which(is.na(npx[ , 1]))
  
  if(length(sector_gap) != 0){ #short format
    olink_version <- as.character(unlist(npx[1, 2]))
    
    row_df <- npx[c(3:6, (sector_gap[2]+1):nrow(npx)), ]%>%
      as.matrix()
    
    npx <- npx[8 : (sector_gap[2]-1), ]%>%
      as.matrix()
    
    col_df <- data.frame(SampleID = as.character(unlist(npx[ ,1])))
    
    col_df <- col_df%>%
      cbind(npx[ ,98:101])%>%
      data.frame()%>%
      set_colnames(value = c("SampleID", "PlateID", "QC_Warning", "QC Deviation Inc Ctrl", "QC Deviation Det Ctrl"))%>%
      mutate("Olink NPX Signature Version" = olink_version,
             "Index" = row_number())
    
    npx <- apply(npx[ , 2:97], 2, function(x) suppressWarnings(as.numeric(x)))
    colnames(npx) <- row_df[2, 2:97]
    
    row_df[1:2 ,98] <- NA
    row_df[ ,1] <- ifelse(is.na(row_df[ ,98]), row_df[ ,1], row_df[ ,98])
    row_df <- t(row_df[, 2:97])%>%
      data.frame()%>%
      set_colnames(value = row_df[, 1])%>%
      mutate(Panel_Version = gsub("(.*\\(|\\))", "", Panel),
             Panel = gsub("\\(.*\\)", "", Panel))%>%
      dplyr::rename("UniProt" = "Uniprot ID",
                    "MissingFreq" = "Missing Data freq.")%>%
      gather(-Panel, -Assay, -UniProt, -OlinkID, -Panel_Version, -MaxLOD, -MissingFreq, -Normalization, key = "PlateID", value = "PlateLOD")
    
    cmb <- col_df%>%
      cbind(npx)%>%
      gather(-SampleID, -PlateID, -QC_Warning, -Index, -"QC Deviation Inc Ctrl", -"QC Deviation Det Ctrl", -"Olink NPX Signature Version",
             key = "Assay", value = "NPX")%>%
      left_join(row_df)
    
    col_order <- c('SampleID', "Index", 'OlinkID','UniProt','Assay','MissingFreq',
                   'Panel','Panel_Version','PlateID','QC_Warning','MaxLOD','PlateLOD',
                   'NPX','Normalization','QC Deviation Inc Ctrl','QC Deviation Det Ctrl',
                   'Olink NPX Signature Version')
    cmb <- cmb[ , match(col_order, colnames(cmb))]
    
  }else{#long format
    cmb <- npx[-1, ]%>%
      data.frame()%>%
      set_colnames(value = npx[1, ])
  }
  cmb <- cmb %>%
    mutate(NPX = as.numeric(NPX),
           MaxLOD = as.numeric(MaxLOD),
           PlateLOD = as.numeric(PlateLOD),
           `QC Deviation Inc Ctrl` = as.numeric(`QC Deviation Inc Ctrl`),
           `QC Deviation Det Ctrl` = as.numeric(`QC Deviation Det Ctrl`))
  
  return(cmb)
}


npx_long2se <- function(npx_long){
  colData <- npx_long[ ,colnames(npx_long) %in% c("SampleID", "Index", "Panel", "PlateID", "QC_Warning","QC Deviation Inc Ctrl", "QC Deviation Det Ctrl")]%>%
    distinct()%>%
    arrange(PlateID, as.numeric(Index), SampleID)%>%
    set_rownames(value = NULL)
  
  assay_order <- npx_long$Assay[!duplicated(npx_long$Assay)]
  # "Inc Ctrl 1", "Ext Ctrl" , "Inc Ctrl 2" ,  "Det Ctrl"
  #assay_order <- assay_order[!(assay_order %in% c("Inc Ctrl 1", "Ext Ctrl" , "Inc Ctrl 2" ,  "Det Ctrl" ))]
  
  rowData <- npx_long%>%
    dplyr::select(Assay, OlinkID, UniProt,MissingFreq, Panel_Version, MaxLOD, Normalization, "Olink NPX Signature Version")%>%
    distinct()
  rowData <- rowData[match(assay_order, rowData$Assay), ]
  
  lod_df <- npx_long%>%
    dplyr::select(Assay, PlateID, PlateLOD)%>%
    distinct()%>%
    spread(key = "PlateID", value = "PlateLOD")
  lod_df <- lod_df[match(assay_order, lod_df$Assay), ]%>%
    dplyr::select(-Assay)
  
  colnames(lod_df) <- ifelse(ncol(lod_df) == 1, "PlateLOD", paste0("LOD_", colnames(lod_df)))
  
  rowData <- cbind(rowData, lod_df)%>%
    set_rownames(value = NULL)
  
  npx <- npx_long%>%
    dplyr::filter(Assay %in% assay_order)%>%
    dplyr::select(SampleID, Index, PlateID, Assay, NPX)%>%
    spread(key = "Assay", value = "NPX")%>%
    arrange(PlateID, as.numeric(Index), SampleID)
  npx <- npx[ ,c("SampleID", "Index", "PlateID", assay_order)]
  
  re <- SummarizedExperiment(colData = colData,
                             rowData = rowData,
                             assays = list(npx = t(npx[ ,-c(1:3)])),
                             metadata = list("software_version" = rowData$`Olink NPX Signature Version`[1],
                                             "panel" = colData$Panel[1]))
  
  
  re<- re[!(rownames(re) %in% c("Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl")), ]
  
  re$Plate.ID <- re$PlateID
  re$Assay <- re$SampleID
  re$QC.Warning <- re$QC_Warning
  re$QC.Deviation.from.median <- re$`QC Deviation Inc Ctrl`
  re$QC.Deviation.from.median.1 <- re$`QC Deviation Det Ctrl`
  re@elementMetadata$LOD <- re@elementMetadata$PlateLOD
  return(re)
}

read_npx <- function(f, lot = "default", startrow = 8, type = "NPX"){
  
  if(type != "NPX"){
    if(grepl("xlsx", f)){
      npx <- readxl::read_xlsx(f, sheet = 1, col_names = F, .name_repair = "unique_quiet")
    }else{
      npx <- read.csv(f, header = F)
    }
    n_col <- length(npx[which(npx[, 1] == "Assay warning"), ])
    npx[npx == ""] <- NA
  }
  else{
    npx <- readxl::read_xlsx(f, sheet = 1, col_names = F, .name_repair = "unique_quiet")
    n_col <- length(npx[which(npx[, 1] == "LOD"), ])
  }
  
  
  npx <- npx[, 1:n_col]
  sw_version <- npx[1, 2]
  npx_panel <- npx[3, 2]
  ctrl_col_idx <- grep("(Ctrl|Plate ID|QC)", ignore.case = T, npx[4, ])
  
  
  npx_ctrl <- npx[startrow : nrow(npx), ctrl_col_idx]
  
  colnames(npx_ctrl) <- npx[4, ctrl_col_idx]
  
  row_feat <- rbind(npx[4 : 5, -ctrl_col_idx])
  
  npx <- npx[startrow : nrow(npx), -ctrl_col_idx]
  
  row_feat <- rbind(row_feat, npx[(min(which(is.na(npx[ , 1]))) + 1): nrow(npx), ])
  
  npx <- npx[1 : (min(which(is.na(npx[, 1]))) - 1), ]
  npx_ctrl <- npx_ctrl[1 : nrow(npx), ]
  
  colnames(npx) <- row_feat[1, ]
  unique_id <- paste("idx", 1:nrow(npx), sep = "_")
  
  rowData <- t(row_feat[, -1])%>%
    data.frame()%>%
    set_colnames(unlist(row_feat[, 1]))%>%
    set_rownames(.$Assay)
  lod_idx <- grep("LOD", colnames(rowData))
  rowData <- cbind(rowData[ ,-grep("LOD", colnames(rowData))],
                   LOD  = rowData[ ,lod_idx[length(lod_idx)]])
  rowData$LOD <- as.numeric(rowData$LOD)
  
  colData <- cbind(unique_id, Assay = npx$Assay, f_name = toString(f), npx_ctrl)%>%
    setNames(make.names(names(.), unique = TRUE))%>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("Ctrl")), .funs = as.numeric)%>%
    dplyr::mutate_at(.vars = dplyr::vars(dplyr::matches("QC.Deviation")), .funs = as.numeric)%>%
    data.frame(row.names = unique_id)
  
  if(type != "NPX"){
    npx <- cbind(unique_id, npx)%>%
      data.frame(row.names = unique_id) %>%
      dplyr::select(-unique_id, -Assay)%>%
      t()
  }else{
    npx <- cbind(unique_id, npx)%>%
      dplyr::mutate_at(.vars = dplyr::vars(!dplyr::matches("(unique_id|Assay)")),
                       .funs = as.numeric) %>%
      data.frame(row.names = unique_id) %>%
      dplyr::select(-unique_id, -Assay)%>%
      t()
  }
  
  rownames(npx) <- rownames(rowData)
  format(npx, digits = 5)
  re <- SummarizedExperiment(colData = colData,
                             rowData = rowData,
                             assays = list(npx = npx),
                             metadata = list("software_version" = sw_version,
                                             "panel" = npx_panel,
                                             "file_name" = toString(f)))
  
  return(re)
}

pull_bdg <- function(f_list, pattern = "hd", fields = "Assay"){
  lapply(f_list, function(x){
    x[, grep(pattern, ignore.case = T, x[[fields]])]
  })
}

bdg_norm_multi <- function(bridge.str, data.ls, between.plate.method = "median",
                           from_assay = "npx", save_assay = "normed"){
  if(
    sapply(bridge.str, function(x){
      lapply(data.ls, function(y){
        sum(is.na(grepl(x, y$Assay)))
      })
    })%>%unlist()%>%sum() != 0
  ){
    stop("not all bridging samples exist in each plate!")
  }
  
  names(bridge.str) <- bridge.str
  adj.ls <- lapply(bridge.str, function(x){
    bridge <- pull_bdg(data.ls, pattern = x, fields = "Assay")%>%
      cmb_npx_se()
    #  count
    bridge.plate.mean <- cbind.data.frame(file_name = bridge$file_name,
                                          t(bridge@assays@data[[from_assay]]))%>%
      group_by(file_name)%>%
      summarize_all(.fun = mean, na.rm = T)
    
    
    bridge.median <- bridge.plate.mean%>%
      dplyr::select(-file_name)%>%
      summarize_all(.funs = between.plate.method, na.rm = T)
    
    
    # update adjust factor
    query <- bridge.plate.mean$file_name
    names(query) <- query
    
    # adjust factor count
    bridge.adj <- lapply(query, function(x){
      bridge.plate.mean[bridge.plate.mean$file_name == x ,-1] - unlist(bridge.median)
    })
    
    return(bridge.adj)
  })
  
  adj.mean <- adj.ls[[names(adj.ls)[1]]]
  if(length(adj.ls) > 1){
    for (i in names(adj.mean)) {
      for (j in 2 : length(adj.ls)) {
        #adj.mean[[i]] <- adj.mean[[i]] + adj.ls[[j]][[i]]
        adj.mean[[i]] <- mapply(function(x, y) sum(x, y, na.rm = T), x = adj.mean[[i]], y = adj.ls[[j]][[i]]) 
      }
      adj.mean[[i]] <- adj.mean[[i]]/length(adj.ls)
    }
  }
  
  query <- names(adj.mean)
  names(query) <- query
  
  data.ls <- lapply(query, function(x){
    temp <- data.ls[[x]]
    temp@assays@data[[save_assay]] <- (temp@assays@data[[from_assay]] - unlist(adj.mean[[x]]))%>%round(5)
    temp@elementMetadata$LOD <- as.numeric(temp@elementMetadata$LOD) - unlist(adj.mean[[x]])
    temp
  })
  
  data.ls
  
}



cmb_npx_se <- function(se_list){
  # get common columns
  com_col <- unlist(lapply(se_list, function(x) {x@colData%>%colnames()}))%>%table()
  com_col <- names(com_col)[com_col == length(se_list)]
  
  com_row <- rownames(se_list[[1]])
  for (i in 2 : length(se_list)) {
    com_row <- intersect(com_row, rownames(se_list[[i]]))
  }
  
  se_list <- lapply(se_list, function(x){
    x[match(com_row, rownames(x)), ]
  })
  
  rowdata_list <- lapply(se_list, function(x){
    temp <- data.frame(x@elementMetadata@listData)
    colnames(temp) <- paste(colnames(temp), x$Plate.ID[1], sep = "_")
    temp
  })
  row_data <- rowdata_list[[1]]
  colnames(row_data)[1] <- "Analyt"
  
  for (i in 2 : length(rowdata_list)) {
    row_data <- cbind(row_data, rowdata_list[[i]][ ,-c(1,2)]) #Analyt Assay
  }
  
  colnames(row_data) <- make.names(colnames(row_data), unique = T)
  row_data$LOD <- rowMeans(row_data[ , grepl("LOD", colnames(row_data))], na.rm = T)
  
  se_list <- lapply(se_list, function(x){
    x@colData <- x@colData[match(com_col, names(x@colData@listData))]
    x@elementMetadata@listData <- data.frame()
    x
  })
  temp <- se_list[[1]]
  for (i in 2 : length(se_list)) {
    temp <- cbind(temp, se_list[[i]])
  }
  rowData(temp) <- row_data
  return(temp)
}


trim_string_bycommon <- function(strings, split = "_"){
  plid <- unique(strings)
  str_parts <- strsplit(plid, split = split)
  str_parts <- lapply(str_parts, function(x){
    temp <- x
    temp[1] <- paste0("^", temp[1])
    for (i in 2 : length(temp)) {
      temp[i] <- paste0(split, temp[i])
    }
    temp[length(temp)] <- paste0(temp[length(temp)], "$")
    temp
  })
  idf <- str_parts%>%
    unlist()%>%
    unique()
  
  idf_ct <- sapply(idf, function(x){
    sum(sapply(plid, function(y){
      grepl(x, y)
    }))
  })
  idf <- idf[which(idf_ct == length(plid))]
  idf <- paste(idf, collapse = "|")
  idf <- paste0("(", idf, ")")
  return(gsub(idf, "", strings))
}


plot_npx_norm_qc <- function(normed_se, bridge_pattern= "OAAFKW", fields = "Assay"){
  
  bridge <- normed_se[, grep(bridge_pattern, ignore.case = T, normed_se[[fields]])]
  
  assay_ls <- names(bridge@assays@data)
  bridge_data_ls <- lapply(assay_ls, function(x){
    bridge@assays@data[[x]] %>%
      data.frame()%>%
      set_colnames(value = bridge$file_name)%>%
      rownames_to_column(var = "Analyt") %>%
      gather(-Analyt, key = "file_name", value = "value")%>%
      mutate(Assay = x,
             file_name = gsub("^.*/", "", file_name))
  })
  bridge_data_ls <- do.call(rbind, bridge_data_ls)%>%
    mutate(file_name = gsub("\\.[0-9]+$", "", file_name))
  
  p1 <- bridge_data_ls %>%
    mutate(Assay = factor(Assay, levels = c("npx", "normed")),
           Analyt = factor(Analyt, levels = make.names(rownames(normed_se))))%>%
    ggplot()+
    geom_point(aes(value, Analyt, color = file_name), shape = 21, size = 4)+
    geom_point(aes(LOD, Analyt), 
               data = data.frame(Analyt = factor(make.names(normed_se@elementMetadata$Analyt), levels = make.names(rownames(normed_se))),
                                 LOD = as.numeric(normed_se@elementMetadata$LOD)))+
    labs(#title = "Bridge QC Reference", 
         y = "Analyte", x = "NPX")+
    facet_grid( ~ Assay)+
    theme_bw(base_size = 15)+
    theme(legend.position = "bottom")+
    guides(color = guide_legend(ncol = 2,byrow = TRUE))
  
  re <- list("all" = p1)
  
  return(re)
}

