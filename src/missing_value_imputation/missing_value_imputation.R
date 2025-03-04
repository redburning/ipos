source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")

mvi_missingnessmap_before_width <- NULL
mvi_missingnessmap_before_height <- NULL
mvi_missingnessmap_after_width <- NULL
mvi_missingnessmap_after_height <- NULL

mvi_missingnesshistogram_before_width <- 650
mvi_missingnesshistogram_before_height <- 800
mvi_missingnesshistogram_after_width <- 650
mvi_missingnesshistogram_after_height <- 800

missingValueImputationUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useToastr(),
    useShinyjs(),
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$style(type="text/css", ".checkbox {margin-top: 20px;"),
              tags$base(target = "_blank")
    ),
    
    box(
      id = "box-mvi",
      title = "Missing value imputation",
      solidHeader = TRUE,
      collapsible = FALSE,
      collapsed = FALSE,
      style = "min-height: calc(100vh - 120px); height:auto;",
      width = 12,
      tags$div(
        style = "display:flex; gap:12px; height:100%;",
        tags$div(
          id="drawer-mvi", class="drawer",
          tags$div(style = "padding:15px 20px 20px 20px;",
                   tags$div(
                     style = "padding:10px 10px 30px; border-radius:10px; border:1px solid #dfe1e5; margin-bottom:20px;",
                     fileInput(
                       inputId = ns("dataloader"),
                       label = "",
                       buttonLabel = div(icon("folder-open"), " Upload data... "),
                       placeholder = "Click button to select, or drag file here.",
                       accept = c(".xlsx", ".csv")
                     ),
                     uiOutput(ns("data_summary_matrix")),
                     tags$div(
                       id = 'heatmap_help_matrix',
                       class = 'annotation-area',
                       tags$h5("What data formats do we support?"),
                       tags$p("The file content format is as shown in the following figure. 
                               Both .csv and .xlsx formats are supported."),
                       tags$img(src = 'missingvalue_imputation/help_mvi.svg', style = 'width:100%;')
                     )
                   )
          )
        ),
        tags$div(id="overlay-mvi", class="overlay",
                 onclick = onOverlayClick('drawer-mvi', 'overlay-mvi')),
        tags$div(
          style = 'width:250px; height:100%; background-color:white;',
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-radius: 5px 5px 0px 0px;',
            buildAccordionItem('mvi-settings-mydata', 'My Data', collapsed = FALSE),
            tags$div(id = 'mvi-settings-mydata', class = 'collapse-item-body', style = 'display:block;',
                     # view data
                     tags$button(class = "action-button-primary", 
                                 style = "height:26px; font-size:13px; margin-bottom:8px;",
                                 tags$div(tags$i(class="fas fa-cloud-arrow-up"),
                                          tags$span("Upload data"), 
                                 ),
                                 onclick = onInspectDataBtnClick('drawer-mvi', 'overlay-mvi')
                     ),
            )
          ),
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
            buildAccordionItem('mvi-settings-method-imputation', 'Method', collapsed = FALSE),
            tags$div(id = 'mvi-settings-method-imputation', class = 'collapse-item-body', style = 'display:block;',
                     numericInput(inputId = ns('mvi_settings_missingtolerance_qc'), label = 'Tolerance for missing value(QC)',
                                  min = 0, max = 1, value = 1),
                     numericInput(inputId = ns('mvi_settings_missingtolerance_all'), label = 'Tolerance for missing value(All)',
                                  min = 0, max = 1, value = 1),
                     selectInput(inputId = ns('mvi_settings_imputation_method'), label = 'Imputation method', selected = 'Half Minimum',
                                 choices = c("Zero", "Half Minimum", "Median", "Mean", "Minimum", "KNN", "RF", "QRILC", "SVD", "PPCA")),
                     hidden(numericInput(inputId = ns('mvi_settings_method_knn_kvalue'), label = 'k-value of KNN', min = 1, value = 5)),
                     hidden(numericInput(inputId = ns('mvi_settings_method_svd_ksvd'), label = 'k-svd of SVD', min = 1, value = 5)),
                     hidden(numericInput(inputId = ns('mvi_settings_method_ndim_ppca'), label = 'n-dim of PPCA', min = 1, value = 2))
            )
          ),
          tags$div(
            class = 'collapse-item-body',
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none; border-radius: 0px 0px 5px 5px; padding-top:12px; padding-bottom:12px;',
            actionButton(ns("execute"),
                         label = "Execute",
                         icon = icon("play"),
                         class = "action-button-primary")
          )
        ),
        tags$div(
          style= "width:calc(100% - 250px)",
          tabsetPanel(
            tabPanel(
              title = "Imputation Result",
              icon = icon("table"),
              tags$div(
                # style = "width:100%;",
                hidden(div(id = ns("mvi-result-data"),
                           tags$div(
                             style = "max-height:calc(100vh - 220px); overflow-y:scroll; overflow-x:scroll;",
                             withSpinner(DT::DTOutput(outputId = ns("mvi_imputation_data"))),
                           ),
                           tags$div(
                             style = 'width:120px; float:right; margin-top:10px;',
                             downloadButton(outputId = ns("export_mvi_result_table"),
                                            class = "action-button-primary",
                                            label = "Download",
                                            icon = icon("download"))
                           )
                ))
              )
            ),
            tabPanel(
              title = "Missingness Map",
              style = "width:100%;",
              hidden(div(id = ns("mvi-result-missingnessmap"),
                         tags$div(
                           withSpinner(
                             plotOutput(outputId = ns("mvi_result_visualization_missingnessmap_before"),
                                        width = "100%",
                                        height = "auto")
                           ),
                           tags$div(
                             style = 'width:100%; display:flex; justify-content:flex-end;',
                             tags$div(
                               style = "width:120px;",
                               actionButton(inputId = ns("export_mvi_missingnessmap_before"),
                                            class = "action-button-primary",
                                            label = "Download",
                                            icon = icon("download"))
                             )
                           )
                         ),
                         tags$div(
                           withSpinner(
                             plotOutput(outputId = ns("mvi_result_visualization_missingnessmap_after"),
                                        width = "100%",
                                        height = "auto")
                           ),
                           tags$div(
                             style = 'width:100%; display:flex; justify-content:flex-end;',
                             tags$div(
                               style = "width:120px;",
                               actionButton(inputId = ns("export_mvi_missingnessmap_after"),
                                            class = "action-button-primary",
                                            label = "Download",
                                            icon = icon("download"))
                             )
                           )
                         )
              ))
            ),
            tabPanel(
              title = "Missingness Histogram",
              style = "width:100%;",
              hidden(div(id = ns("mvi-result-missingnesshistogram"),
                         tags$div(
                           style = "display:flex; width:100%",
                           tags$div(
                             style = "width:50%; padding-right:30px;",
                             tags$div(
                               style = "font-size:13px; font-weight:bold; display:flex; justify-content:center; padding-top:10px; padding-bottom:10px;",
                               "Before Delete Missing Value"
                             ),
                             withSpinner(
                               plotOutput(outputId = ns("mvi_result_visualization_missingnesshistogram_before"),
                                          width = "100%",
                                          height = "800px")
                             ),
                             tags$div(
                               style = 'width:100%; display:flex; justify-content:flex-end;',
                               tags$div(
                                 style = "width:120px;",
                                 actionButton(inputId = ns("export_mvi_missingnesshistogram_before"),
                                              class = "action-button-primary",
                                              label = "Download",
                                              icon = icon("download"))
                               )
                             )
                           ),
                           tags$div(
                             style = "width:50%; padding-left:30px;",
                             tags$div(
                               style = "font-size:13px; font-weight:bold; display:flex; justify-content:center; padding-top:10px; padding-bottom:10px;",
                               "After Delete Missing Value"
                             ),
                             withSpinner(
                               plotOutput(outputId = ns("mvi_result_visualization_missingnesshistogram_after"),
                                          width = "100%",
                                          height = "800px")
                             ),
                             tags$div(
                               style = 'width:100%; display:flex; justify-content:flex-end;',
                               tags$div(
                                 style = "width:120px;",
                                 actionButton(inputId = ns("export_mvi_missingnesshistogram_after"),
                                              class = "action-button-primary",
                                              label = "Download",
                                              icon = icon("download"))
                               )
                             )
                           )
                         )
              ))
            )
          )
        )
      )
      
    )
    
  )
  
}


missingValueImputationServer <- function(input, output, session) {
  ns <- session$ns
  dataset <- NULL
  mvi_result_data <- NULL
  mvi_result_histogram_before <- NULL
  mvi_result_histogram_after <- NULL
  
  # 上传样本x变量矩阵数据
  observeEvent(input$dataloader$datapath, {
    tryCatch({
      if (str_ends(input$dataloader$datapath, ".xlsx")) {
        dataset <<- xlsx::read.xlsx(input$dataloader$datapath, sheetIndex = 1, row.names = 1, check.names = F)
      } else if (str_ends(input$dataloader$datapath, ".csv")) {
        dataset <<- read.csv(input$dataloader$datapath, check.names = FALSE, row.names = 1)
      }
      
      output$data_summary_matrix <- renderUI({
        tags$div(
          style = 'margin-top:-15px; margin-bottom:8px;',
          tags$span("Sample size: ", style = 'background-color:#5c5c5c; color:white; padding:2px 5px; font-size:12px; border-radius: 4px 0 0 4px;'),
          tags$span(nrow(dataset), style = 'background-color:#f37f40; color:white; padding:2px 5px; font-size:12px; border-radius: 0 4px 4px 0;'),
          tags$span("Number of variables: ", style = 'margin-left:10px; background-color:#5c5c5c; color:white; padding:2px 5px; font-size:12px; border-radius: 4px 0 0 4px;'),
          tags$span(ncol(dataset) - 2, style = 'background-color: #55b599; color:white; padding:2px 5px; font-size:12px; border-radius: 0 4px 4px 0;')
        )
      })
    }, error = function (e) {
      print(paste(e))
    })
  })
  
  
  # 点击Execute时运行计算
  observeEvent(input$execute, {
    tryCatch({
      if (is.null(dataset)) {
        toastr_warning(title = "Please upload data!", message = '')
      } else {
        shinyjs::show("mvi-result-data")
        shinyjs::show("mvi-result-missingnessmap")
        shinyjs::show("mvi-result-missingnesshistogram")
        
        # 缺失值填充-prepare data
        time <- dataset[, 1]
        data0 <- dataset[, -1]
        
        group <- cbind(rownames(data0), data0[, 1]) %>% `colnames<-`(c("Sample", "group"))
        group_unique <- unique(data0$group) # 包含的分组信息查看
        data0_split_by_group <- split(data0[, -1], data0$group) # 按分组拆分原始数据后存储的data
        #计算缺失比例
        prop_missing <- data.frame(matrix(ncol = length(group_unique), nrow = ncol(data0[, -1]))) %>% 
          `colnames<-`(c(group_unique)) %>% `rownames<-`(colnames(data0[, -1]))
        for (i in 1:length(group_unique)) {
          prop_missing[, group_unique[i]] <- apply(data0_split_by_group[[group_unique[i]]], 2, function(x) { sum(is.na(x)) / length(x) } ) 
        }
        
        # 删除QC 缺失超过一定阈值的数据
        data_delet_miss_qc <- data0[, -1][, which(prop_missing$QC < input$mvi_settings_missingtolerance_qc)]
        
        # 设定删除阈值
        # cut_missing_value = 1 # 设定的阈值,可更改
        # 删除大于设定阈值的
        data_delet_miss_qc_cut_value <- data_delet_miss_qc[, (colSums(is.na(data_delet_miss_qc)) / nrow(data_delet_miss_qc)) < input$mvi_settings_missingtolerance_all]
        data_delet_miss_qc_cut_value <- merge(x = group, y = data_delet_miss_qc_cut_value %>% mutate("Sample" = rownames(data_delet_miss_qc_cut_value)), sort = F, all.y = T) %>% 
          `rownames<-`(rownames(data_delet_miss_qc_cut_value)) %>% dplyr::select(-"Sample")
        
        # 删除后的分组信息提取
        data0_split_by_group_delet <- split(data_delet_miss_qc_cut_value[, -1], data_delet_miss_qc_cut_value$group)
        # 删除后缺失比例计算
        prop_missing_delet <- data.frame(matrix(ncol = length(group_unique), nrow = ncol(data_delet_miss_qc_cut_value[, -1]))) %>% 
          `colnames<-`(c(group_unique)) %>% `rownames<-`(colnames(data_delet_miss_qc_cut_value[, -1]))
        for (i in 1:length(group_unique)) {
          prop_missing_delet[, group_unique[i]] <- apply(data0_split_by_group_delet[[group_unique[i]]], 2, function(x) { sum(is.na(x)) / length(x) }) 
        }
        
        impute_result <<- IMPUTE_NA(data_delet_miss_qc_cut_value, 
                                    Method = input$mvi_settings_imputation_method,
                                    k_KNN = input$mvi_settings_method_knn_kvalue, 
                                    K_SVD = input$mvi_settings_method_svd_ksvd, 
                                    ndim = input$mvi_settings_method_ndim_ppca) %>% as.data.frame()
        
        colnames(impute_result)[1] <- "group"
        
        # for QC 格式整理
        n <- nrow(impute_result) + 2
        m <- ncol(impute_result) + 3
        impute_result_for_QC <- data.frame(matrix(ncol = n, nrow = m))
        impute_result_for_QC[1, 2:n] <- c("batch", rep(1, n - 2))
        impute_result_for_QC[2, 2:n] <- c("type", ifelse(impute_result[, 1] != "QC", "Sample", "QC"))
        impute_result_for_QC[3, 2:n] <- c("time", time)
        impute_result_for_QC[4, 1:n] <- c("No.", "label", rownames(impute_result))
        impute_result_for_QC[5:m, 1] <- paste(1:(m - 4))
        impute_result_for_QC[5:m, 2] <- colnames(impute_result)[-1]
        impute_result_for_QC[5:m, 3:n] <- impute_result[, -1] %>% t()
        
        # 找到填充内容的所有坐标
        coord <- which(is.na(data_delet_miss_qc_cut_value) & !is.na(impute_result), arr.ind = TRUE)
        # 将坐标转换为JS数组格式
        coord_all <- c()
        num_rows <- nrow(coord)
        for (i in 1:num_rows) {
          row_index <- coord[i, 1]  # 获取行索引
          col_index <- coord[i, 2]  # 获取列索引
          # 因为上文有转置操作，所以行列坐标对调
          # 并且注意补充了2行+3列header信息
          # 并且注意R的下标是从1开始，JavaScript的下标是从0开始
          coord_current <- paste0('[', col_index + 2, ', ', row_index + 1, ']')
          coord_all <- c(coord_all, coord_current)
        }
        coord_all_str <- paste0('[', paste(coord_all, collapse = ", "), '];')
        
        mvi_result_data <<- impute_result_for_QC
        
        output$mvi_imputation_data <- DT::renderDT({
          DT::datatable({
            mvi_result_data
          },
          options = list(
            bLengthChange = FALSE,
            pageLength = 26,
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().body()).css({'font-size': '12px'});",
              "  $(this.api().table().header()).css({'font-size': '12px'});",
              "}"),
            columnDefs = list(list(className='dt-left', targets="_all",
                                   createdCell = JS(
                                     "function(td, cellData, rowData, row, col) {",
                                        paste0("var coords = ", coord_all_str),
                                     "  for (var i = 0; i < coords.length; i++) {",
                                     "    var row_index = coords[i][0];",
                                     "    var col_index = coords[i][1];",
                                     "    if (row === row_index && col === col_index) {",
                                     "      $(td).css('color', 'white').css('background-color', '#ffb456');",
                                     "    }",
                                     "  }",
                                     "}"
                                   )
                                   )
                              ),
            scrollX = TRUE,
            searching = FALSE,
            paging = FALSE,
            bInfo = FALSE,
            language = list(paginate = list("next" = "<i class='fa fa-chevron-right'></i>",
                                            "previous" = "<i class='fa fa-chevron-left'></i>")
            )
          ),
          selection = list(mode = 'single', target = 'cell'),
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        output$mvi_result_visualization_missingnessmap_before <- renderPlot({
          # 统计变量名长度的最大值，以便设置plot margin
          label_max_length <- max(sapply(colnames(data0), nchar))
          return(missmap(data0[, -1], legend = F, 
                         main = 'Missingness Map Before Delete Missing Value',
                         margins = c(label_max_length / 2, 3)))
        }, height = function() {
          # 统计变量名长度的最大值，以便设置总体高度
          label_max_length <- max(sapply(colnames(data0), nchar))
          height <- nrow(data0) * 8 + label_max_length * 5
          mvi_missingnessmap_before_height <<- height
          return(height)
        })
        
        output$mvi_result_visualization_missingnessmap_after <- renderPlot({
          # 统计变量名长度的最大值，以便设置plot margin
          label_max_length <- max(sapply(colnames(data_delet_miss_qc_cut_value), nchar))
          return(missmap(data_delet_miss_qc_cut_value[, -1], legend = F, 
                         main = 'Missingness Map After Delete Missing Value',
                         margins = c(label_max_length / 2, 3)))
        }, height = function() {
          # 统计变量名长度的最大值，以便图形总高度
          label_max_length <- max(sapply(colnames(data_delet_miss_qc_cut_value), nchar))
          height <- nrow(data_delet_miss_qc_cut_value) * 8 + label_max_length * 5
          mvi_missingnessmap_after_height <<- height
          return(height)
        })
        
        output$mvi_result_visualization_missingnesshistogram_before <- renderPlot({
          mvi_result_histogram_before <<- plot_hist(prop_missing)
          return(mvi_result_histogram_before)
        })
        
        output$mvi_result_visualization_missingnesshistogram_after <- renderPlot({
          mvi_result_histogram_after <<- plot_hist(prop_missing_delet)
          return(mvi_result_histogram_after)
        })
        
      }
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "Runtime error", 
                   message = 'Possibly due to improper parameter settings or the method not being suitable for the current dataset! You can try other methods.')
    })
  })
  
  # Method区域参数根据选择方法变化
  observeEvent(input$mvi_settings_imputation_method, {
    if (input$mvi_settings_imputation_method == 'KNN') {
      shinyjs::show('mvi_settings_method_knn_kvalue')
    } else {
      shinyjs::hide('mvi_settings_method_knn_kvalue')
    }
    if (input$mvi_settings_imputation_method == 'SVD') {
      shinyjs::show('mvi_settings_method_svd_ksvd')
    } else {
      shinyjs::hide('mvi_settings_method_svd_ksvd')
    }
    if (input$mvi_settings_imputation_method == 'PPCA') {
      shinyjs::show('mvi_settings_method_ndim_ppca')
    } else {
      shinyjs::hide('mvi_settings_method_ndim_ppca')
    }
  })
  
  #############################################
  # Export & Download
  #############################################
  
  # 导出缺失值补偿结果Table
  output$export_mvi_result_table <- downloadHandler(
    filename = function() {
      paste("imputation-result-", input$mvi_settings_imputation_method, "-", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep="")
    },
    content = function(file) {
      openxlsx::write.xlsx(mvi_result_data, file, asTable = TRUE)
    }
  )
  
  # 导出 missing map - before delete missing value
  observeEvent(input$export_mvi_missingnessmap_before, {
    if (is.null(mvi_missingnessmap_before_width)) {
      mvi_missingnessmap_before_width <<- ncol(data0) * 2
    }
    showModal(modalDialog(
      title = "Download Missingness Map",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_mvi_missingnessmap_before_format"), label = "Choose format", 
                    choices = c("jpeg", "png", "tiff"), selected = 'png'),
        numericInput(inputId = ns('export_mvi_missingnessmap_before_width'), label = 'Width', 
                     value = mvi_missingnessmap_before_width, min = 1),
        numericInput(inputId = ns('export_mvi_missingnessmap_before_height'), label = 'Height', 
                     value = mvi_missingnessmap_before_height, min = 1)
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_mvi_missingnessmap_before_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  
  output$export_mvi_missingnessmap_before_ok <- downloadHandler(
    filename = function() {
      paste("missing-map-before-delete-missing-value-", format(Sys.time(), "%Y%m%d%H%M%S"), ".", 
            input$export_mvi_missingnessmap_before_format, sep="")
    },
    content = function(file) {
      # 重新绘图
      label_max_length <- max(sapply(colnames(data0), nchar))
      
      if (input$export_mvi_missingnessmap_before_format == 'png') {
        png(file, 
            width = input$export_mvi_missingnessmap_before_width, 
            height = input$export_mvi_missingnessmap_before_height)
      } else if (input$export_mvi_missingnessmap_before_format == 'jpeg') {
        jpeg(file, 
             width = input$export_mvi_missingnessmap_before_width, 
             height = input$export_mvi_missingnessmap_before_height)
      } else if (input$export_mvi_missingnessmap_before_format == 'tiff') {
        tiff(file, 
             width = input$export_mvi_missingnessmap_before_width, 
             height = input$export_mvi_missingnessmap_before_height)
      }
      
      missmap(data0[, -1], legend = F, 
              main = 'Missingness Map Before Delete Missing Value',
              margins = c(label_max_length / 2, 3))
      dev.off()
      
      removeModal()
    }
  )
  
  
  # 导出 missing map - after delete missing value
  observeEvent(input$export_mvi_missingnessmap_after, {
    if (is.null(mvi_missingnessmap_after_width)) {
      mvi_missingnessmap_after_width <<- ncol(data_delet_miss_qc_cut_value) * 2
    }
    showModal(modalDialog(
      title = "Download Missingness Map",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_mvi_missingnessmap_after_format"), label = "Choose format", 
                    choices = c("jpeg", "png", "tiff"), selected = 'png'),
        numericInput(inputId = ns('export_mvi_missingnessmap_after_width'), label = 'Width', 
                     value = mvi_missingnessmap_after_width, min = 1),
        numericInput(inputId = ns('export_mvi_missingnessmap_after_height'), label = 'Height', 
                     value = mvi_missingnessmap_after_height, min = 1)
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_mvi_missingnessmap_after_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  
  output$export_mvi_missingnessmap_after_ok <- downloadHandler(
    filename = function() {
      paste("missing-map-after-delete-missing-value-", format(Sys.time(), "%Y%m%d%H%M%S"), ".", 
            input$export_mvi_missingnessmap_after_format, sep="")
    },
    content = function(file) {
      # 重新绘图
      label_max_length <- max(sapply(colnames(data_delet_miss_qc_cut_value), nchar))
      
      if (input$export_mvi_missingnessmap_after_format == 'png') {
        png(file, 
            width = input$export_mvi_missingnessmap_after_width, 
            height = input$export_mvi_missingnessmap_after_height)
      } else if (input$export_mvi_missingnessmap_after_format == 'jpeg') {
        jpeg(file, 
             width = input$export_mvi_missingnessmap_after_width, 
             height = input$export_mvi_missingnessmap_after_height)
      } else if (input$export_mvi_missingnessmap_after_format == 'tiff') {
        tiff(file, 
             width = input$export_mvi_missingnessmap_after_width, 
             height = input$export_mvi_missingnessmap_after_height)
      }
      
      missmap(data_delet_miss_qc_cut_value[, -1], legend = F, 
              main = 'Missingness Map After Delete Missing Value',
              margins = c(label_max_length / 2, 3))
      
      dev.off()
      
      removeModal()
    }
  )
  
  
  # 导出 missing histogram - before delete missing value
  observeEvent(input$export_mvi_missingnesshistogram_before, {
    showModal(modalDialog(
      title = "Download Missingness Histogram",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_mvi_missingnesshistogram_before_format"), label = "Choose format", 
                    choices = c("jpg", "png", "tiff", "pdf"), selected = 'png'),
        numericInput(inputId = ns('export_mvi_missingnesshistogram_before_width'), label = 'Width', 
                     value = mvi_missingnesshistogram_before_width, min = 1),
        numericInput(inputId = ns('export_mvi_missingnesshistogram_before_height'), label = 'Height', 
                     value = mvi_missingnesshistogram_before_height, min = 1)
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_mvi_missingnesshistogram_before_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  
  output$export_mvi_missingnesshistogram_before_ok <- downloadHandler(
    filename = function() {
      paste("missing-histogram-before-delete-missing-value-", format(Sys.time(), "%Y%m%d%H%M%S"), ".", 
            input$export_mvi_missingnesshistogram_before_format, sep="")
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = mvi_result_histogram_before,
             width = input$export_mvi_missingnesshistogram_before_width / dpi,
             height = input$export_mvi_missingnesshistogram_before_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 导出 missing histogram - after delete missing value
  observeEvent(input$export_mvi_missingnesshistogram_after, {
    showModal(modalDialog(
      title = "Download Missingness Histogram",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_mvi_missingnesshistogram_after_format"), label = "Choose format", 
                    choices = c("jpg", "png", "tiff", "pdf"), selected = 'png'),
        numericInput(inputId = ns('export_mvi_missingnesshistogram_after_width'), label = 'Width', 
                     value = mvi_missingnesshistogram_after_width, min = 1),
        numericInput(inputId = ns('export_mvi_missingnesshistogram_after_height'), label = 'Height', 
                     value = mvi_missingnesshistogram_after_height, min = 1)
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_mvi_missingnesshistogram_after_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  
  output$export_mvi_missingnesshistogram_after_ok <- downloadHandler(
    filename = function() {
      paste("missing-histogram-after-delete-missing-value-", format(Sys.time(), "%Y%m%d%H%M%S"), ".", 
            input$export_mvi_missingnesshistogram_after_format, sep="")
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = mvi_result_histogram_after,
             width = input$export_mvi_missingnesshistogram_after_width / dpi,
             height = input$export_mvi_missingnesshistogram_after_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
}





IMPUTE_NA <- function(x, Method = "Half Minimum", k_KNN = 5, K_SVD = 5, ndim = 2) {
  library(impute)
  library(missForest)
  library(imputeLCMD)
  library(Rdimtools)
  
  x1 <- x[-which(x$group == "QC"), -1]
  x2 <- x[which(x$group == "QC"), -1]
  imputed_data_x1 <- 
    if (Method == "Zero") {
      apply(x1, 2, function(x){ replace(x, is.na(x), 0)})
    } else if (Method == "Half Minimum") {
      apply(x1, 2, function(x) {
        result = 0
        for (i in 1:sum(is.na(x))) {
          e <- runif(1, 0.991, 0.999) #可更改为0.91~0.99
          na <- min(x, na.rm = TRUE)/2*e
          result <- c(result, na)
        }
        replace(x, is.na(x), result[-1])
      })
    } else if (Method == "Median") {
      apply(x1, 2, function(x) {
        replace(x, is.na(x), median(x, na.rm = TRUE))
      })
    } else if (Method == "Mean") {
      apply(x1, 2, function(x) {
        replace(x, is.na(x), mean(x, na.rm = TRUE))
      })
    } else if (Method == "Minimum") {
      apply(x1, 2, function(x) {
        replace(x, is.na(x), min(x, na.rm = TRUE))
      })
    } else if (Method == "KNN") {
      x1 %>% as.matrix %>% impute.knn(., k = k_KNN) %>% extract2(1)
    } else if (Method == "RF") {
      missForest(x1)[[1]]
    } else if (Method == "QRILC") {
      x1 %>% log %>% impute.QRILC() %>% extract2(1) %>% exp
    } else if (Method == "SVD") {
      x1 %>% impute.wrapper.SVD(., K = K_SVD)
    } else if (Method == "PPCA") {
      x1 %>% do.ppca(., ndim = ndim)
    }
  imputed_data_x2 <- apply(x2, 2, function(x) { replace(x, is.na(x), mean(x, na.rm = TRUE))})
  imputed_data_x2 <- round(imputed_data_x2, 4)
  imputed_data_x1 <- round(imputed_data_x1, 4)
  result_res <- x$group %>% cbind(rbind(imputed_data_x1, imputed_data_x2)) 
  return(result_res)
}


plot_hist <- function(data, facet = "facet", facet_num = "all", show_value = 0) {
  data <- data 
  data_long <- gather(data, key = "variable", value = "value", everything())
  if (facet == "facet"){
    if(facet_num == "all") {
      plots <- list()
      for (column in names(data)) {
        # 获取大于0的数据
        filtered_data <- data[which(data[[column]] > show_value), column]
        filtered_data <- data.frame(value = filtered_data)  # 将筛选后的数据存储在数据框中
        
        # 创建频率分布直方图
        hist_plot <- ggplot(filtered_data, aes(x = value)) +
          geom_histogram(fill = "skyblue", color = "black", bins = 20) +
          labs(title = paste(column, "Histogram"))
        
        # 创建频率分布折线图
        line_plot <- ggplot(filtered_data, aes(x = value)) +
          geom_freqpoly(color = "red") +
          labs(title = paste(column, "Line"))
        
        # 将直方图和折线图添加到列表
        plots[[paste(column, "Histogram")]] <- hist_plot
        plots[[paste(column, "Line")]] <- line_plot
      }
      
      # 将图形对象组合成多面板图
      multiplot <- plot_grid(plotlist = plots, ncol = 2)
      # 显示图形
      # print(multiplot)
      return(multiplot)
    } else if (facet_num == "2") {
      # 创建频数分布直方图
      hist_plot <- ggplot(data_long[data_long$value > show_value, ], aes(x = value, fill = variable)) +
        geom_histogram(binwidth = 0.2, position = "dodge") +
        scale_fill_aaas()+
        labs(fill = "group", y = NULL) +
        guides(fill = guide_legend(title = "group")) +
        theme_minimal()
      
      # 创建频数分布折线图
      line_plot <- ggplot(data_long[data_long$value > show_value, ], aes(x = value, y = ..count.. * 1, color = variable, group = variable)) +
        stat_bin(geom = "line") +
        scale_color_aaas()+
        labs(color = "group", y = NULL) +
        guides(color = guide_legend(title = "group")) +
        theme_minimal()
      
      # 分面显示直方图和折线图
      plot <- grid.arrange(hist_plot, line_plot, ncol = 2)
      return(plot)
    } else {
      stop("please set facet_num to 'all' or '2'")
    }
  } else if (facet == "none"){
    # 频数分布直方图和折线图
    plot <- ggplot(data_long[data_long$value > show_value, ], aes(x = value, fill = variable)) +
      geom_histogram(binwidth = 0.2, position = "dodge") +
      geom_line(stat = "bin", aes(y = ..count.. * 2, color = variable, group = variable)) +
      scale_color_igv() +
      scale_fill_igv() +
      labs(color = "group", fill = "group", y = NULL) +
      guides(color = guide_legend(title = "group"), fill = guide_legend(title = "group")) +
      theme_minimal()
    return(plot)
  } else if(facet == "facet") {
    stop("please set facet to 'facet' or 'none'")
  }
}