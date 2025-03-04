source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")

# loading plot尺寸
opls_result_loadingplot_width <- 800
opls_result_loadingplot_height <- 600

dimensionReductionOPLSLoadingPlotUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useToastr(),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("execute")),
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$style(type="text/css", ".checkbox {margin-top: 20px;"),
              tags$base(target = "_blank")
    ),
    
    box(
      id = "box-dr-opls",
      title = "OPLS",
      solidHeader = TRUE,
      collapsible = FALSE,
      collapsed = FALSE,
      style = "min-height: calc(100vh - 120px); height: calc(100vh - 120px);",
      width = 12,
      tags$div(
        style = "display:flex; gap:12px; height:100%;",
        tags$div(
          id="drawer-opls-gallery-loadingplot", class="drawer",
          tags$div(style = "padding:15px 20px 20px 20px;",
                   tabsetPanel(
                     tabPanel(
                       title = "Metabolite Matrix",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("opls_gallery_loadingplot_matrix")))
                       )
                     ),
                     tabPanel(
                       title = "Sample Grouping",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("opls_gallery_loadingplot_sample_grouping")))
                       )
                     ),
                     tabPanel(
                       title = "Metabolite Class",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("opls_gallery_loadingplot_metabolite_class")))
                       )
                     )
                   )
          )
        ),
        tags$div(id="overlay-opls-gallery-loadingplot", class="overlay",
                 onclick = onOverlayClick('drawer-opls-gallery-loadingplot', 'overlay-opls-gallery-loadingplot')),
        tags$div(
          style = 'width:250px; height:100%; background-color:white;',
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-radius: 5px 5px 0px 0px;',
            buildAccordionItem('dimension-reduction-opls-settings-mydata', 'My Data', collapsed = FALSE),
            tags$div(id = 'dimension-reduction-opls-settings-mydata', class = 'collapse-item-body', style = 'display:block;',
                     # view data
                     tags$button(class = "action-button-primary", 
                                 style = "height:26px; font-size:13px; margin-bottom:8px;",
                                 tags$div(tags$i(class="fas fa-cloud-arrow-up"),
                                          tags$span("Upload data"), 
                                 ),
                                 onclick = onInspectDataBtnClick('drawer-opls-gallery-loadingplot', 'overlay-opls-gallery-loadingplot')
                     ),
            )
          ),
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
            buildAccordionItem('settings-group-selection', 'Grouping Selection'),
            tags$div(id = 'settings-group-selection', class = 'collapse-item-body', style = 'display:block;',
                     selectInput(inputId = ns('settings_groupcol_selection'), label = "", 
                                 choices = c('Type', 'Group'), selected = 'Type')
            )
          ),
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
            buildAccordionItem('settings-group-comparison', 'Grouping Comparison'),
            uiOutput(outputId = ns("groups_selection_ui"))
          ),
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
            buildAccordionItem('settings-data-preparation', 'Data Preparation'),
            tags$div(id = 'settings-data-preparation', class = 'collapse-item-body', style = 'display:block;',
                     selectInput(inputId = ns('settings_data_transformation'), label = 'Transformation', choices = c('None', 'Log2', 'Log10'), selected = 'Log2'),
                     selectInput(inputId = ns('settings_data_scaling'), label = 'Scaling', choices = c('None', 'Centering', 'Unit Variance scaling', 'Pareto scaling', 'Range scaling', 'Vast scaling', 'Level scaling'), selected = 'Pareto scaling')
            )
          ),
          tags$div(
            class = 'collapse-item-body',
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none; border-radius: 0px 0px 5px 5px; padding-top:12px; padding-bottom:12px;',
            actionButton(ns("execute"),
                         label = "Execute",
                         icon = icon("play"),
                         class = "action-button-primary"),
          )
        ),
        tags$div(
          style = "width:calc(100% - 520px); max-width:calc(100% - 520px); height:100%; overflow-y:scroll; overflow-x:scroll; padding:20px; background-color:white;",
          withSpinner(plotlyOutput(outputId = ns("loading_plot"),
                                 width = "100%",
                                 height = "100%")
          )
        ),
        tags$div(
          style = "width:270px; height:100%; background-color:white;",
          tags$div(
            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
            # Datapoint settings
            tags$div(
              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(id = 'datapoint-settings-opls-loadingplot', title = 'Data points', collapsed = TRUE),
              tags$div(id = 'datapoint-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       # inspect data
                       tags$button(class = "action-button-primary", 
                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                   tags$div(tags$i(class="far fa-eye"),
                                            tags$span("Inspect data"), 
                                   ), 
                                   onclick = onInspectDataBtnClick('drawer-opls-loadingplot', 'overlay-opls-loadingplot')
                       ),
                       
                       # point
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'datapoint-point-settings-opls-loadingplot', title = 'Point'),
                         tags$div(
                           id = 'datapoint-point-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                           selectInput(inputId = ns('opls_loadingplot_datapoint_fillpalette'), label = 'Fill color', selected = 'd3', 
                                       choices = c('d3(10)' = 'd3', 'aaas(10)' = 'aaas', 'jama(7)' = 'jama', 'jco(10)' = 'jco', 'lancet(9)' = 'lancet', 'locuszoom(7)' = 'locuszoom', 'nejm(8)' = 'nejm', 'npg(10)' = 'npg', 'simpsons(16)' = 'simpsons')),
                           numericInput(inputId = ns('opls_loadingplot_datapoint_fillalpha'), label = 'Fill transparency', min = 0.01, max = 1, step = 0.1, value = 1),
                           selectInput(inputId = ns('opls_loadingplot_datapoint_shape'), label = 'Point shape', selected = '21', 
                                       choices = c('Circle' = '21', 'Square' = '22', 'rhomboid' = '23', 'triangle' = '24')),
                           numericInput(inputId = ns('opls_loadingplot_datapoint_size'), label = 'Size', min = 1, value = 2.5, step = 1)
                         )
                       ),
                       # h-line
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'hline-settings-opls-loadingplot', title = 'h-line', collapsed = TRUE),
                         tags$div(
                           id = 'hline-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                           checkboxInput(inputId = ns('opls_loadingplot_showhline'), value = TRUE, label = 'Show h-line'),
                           numericInput(inputId = ns('opls_loadingplot_hline_value'), label = 'Reference value', step = 0.5, value = 0),
                           numericInput(inputId = ns('opls_loadingplot_hline_size'), label = 'Line width', step = 0.5, value = 0.5),
                           selectInput(inputId = ns('opls_loadingplot_hline_shape'), label = 'Line shape', 
                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                           colourInput(inputId = ns("opls_loadingplot_hline_color"), label = "Line color", value = "gray33",
                                       allowTransparent = TRUE, closeOnClick = TRUE)
                         )
                       ),
                       # v-line
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'vline-settings-opls-loadingplot', title = 'v-line', collapsed = TRUE),
                         tags$div(
                           id = 'vline-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                           checkboxInput(inputId = ns('opls_loadingplot_showvline'), value = TRUE, label = 'Show v-line'),
                           numericInput(inputId = ns('opls_loadingplot_vline_value'), label = 'Reference value', step = 0.5, value = 0),
                           numericInput(inputId = ns('opls_loadingplot_vline_size'), label = 'Line width', step = 0.5, value = 0.5),
                           selectInput(inputId = ns('opls_loadingplot_vline_shape'), label = 'Line shape', 
                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                           colourInput(inputId = ns("opls_loadingplot_vline_color"), label = "Line color", value = "gray33",
                                       allowTransparent = TRUE, closeOnClick = TRUE)
                         )
                       )
              )
            ),
            
            # Title settings
            tags$div(
              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(id = 'title-settings-opls-loadingplot', title = 'Title', collapsed = TRUE),
              tags$div(id = 'title-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       textInput(inputId = ns('opls_loadingplot_title_text'), label = 'Text'),
                       selectInput(inputId = ns('opls_loadingplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                       numericInput(inputId = ns('opls_loadingplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                       selectInput(inputId = ns('opls_loadingplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
              )
            ),
            
            # Panel settings
            tags$div(
              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(id = 'panel-settings-opls-loadingplot', title = 'Panel', collapsed = TRUE),
              tags$div(id = 'panel-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'panel-background-settings-opls-loadingplot', title = 'Background', collapsed = FALSE),
                         tags$div(
                           id = 'panel-background-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                           colourInput(inputId = ns("opls_loadingplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                       allowTransparent = TRUE, closeOnClick = TRUE)
                         )
                       ),
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'panel-grid-settings-opls-loadingplot', title = 'Grid', collapsed = FALSE),
                         tags$div(
                           id = 'panel-grid-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                           checkboxInput(inputId = ns('opls_loadingplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                         )
                       ),
                       
              )
            ),
            
            # Axis settings
            tags$div(
              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(id = 'axis-settings-opls-loadingplot', title = 'Axis', collapsed = TRUE),
              tags$div(id = 'axis-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'xaxis-settings-opls-loadingplot', title = 'xAxis', collapsed = TRUE),
                         tags$div(
                           id = 'xaxis-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                           textInput(inputId = ns('opls_loadingplot_xaxis_label'), label = 'Label', value = 't[1]'),
                           selectInput(inputId = ns('opls_loadingplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                           numericInput(inputId = ns('opls_loadingplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                         )
                       ),
                       tags$div(
                         class = 'collapse-subitem',
                         buildAccordionItem(id = 'yaxis-settings-opls-loadingplot', title = 'yAxis', collapsed = TRUE),
                         tags$div(
                           id = 'yaxis-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                           textInput(inputId = ns('opls_loadingplot_yaxis_label'), label = 'Label', value = 'o[1]'),
                           selectInput(inputId = ns('opls_loadingplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                           numericInput(inputId = ns('opls_loadingplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                         )
                       ),
                       
              )
            ),
            
            # Legend settings
            tags$div(
              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(id = 'legend-settings-opls-loadingplot', title = 'Legend', collapsed = TRUE),
              tags$div(id = 'legend-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       # legend显示在内部、外部、不显示
                       radioButtons(inputId = ns('opls_loadingplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                       selectInput(inputId = ns('opls_loadingplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                       tags$div(id = 'opls_loadingplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                tags$div(
                                  style = 'width:50%',
                                  numericInput(inputId = ns('opls_loadingplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                ),
                                tags$div(
                                  style = 'width:50%',
                                  numericInput(inputId = ns('opls_loadingplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                )
                       )
              )
            ),
            
            # Size
            tags$div(
              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(id = 'size-settings-opls-loadingplot', title = 'Size', collapsed = TRUE),
              tags$div(id = 'size-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                tags$div(
                                  style = 'width:50%',
                                  numericInput(inputId = ns('opls_result_loadingplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_loadingplot_width)
                                ),
                                tags$div(
                                  style = 'width:50%',
                                  numericInput(inputId = ns('opls_result_loadingplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_loadingplot_height)
                                )
                       )
              )
            ),
            
            # Export
            tags$div(
              buildAccordionItem(id = 'export-settings-opls-loadingplot', title = 'Export', collapsed = TRUE),
              tags$div(id = 'export-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                       # export plot
                       tags$div(
                         # selectInput(inputId = ns('export_loadingplot_format'), label = 'Format', choices = c('html'), selected = 'html'),
                         actionButton(inputId = ns("export_loadingplot"), 
                                        label = "Export plot",
                                        icon = icon("download"),
                                        class = "action-button-primary"),
                         style = "padding:6px 12px 20px 12px;"
                       )
              )
            ),
            
          )
        )
      )
      
    )
    
    
  )
  
}


dimensionReductionOPLSLoadingPlotServer <- function(input, output, session) {
  ns <- session$ns
  classes <- c()              # 所有的数据分组
  preprocessed_data <- NULL   # 预处理(log transform + scaling)之后的数据
  
  opls_result_loadingplot <- NULL
  opls_result_loadingplotly <- NULL
  opls_result_loadingplot_data <- NULL
  
  origdataset <<- read.csv("help/data/dimension_reduction_opls_matrix.csv", 
                          stringsAsFactors = TRUE, check.names = FALSE)
  dataset <<- read.csv("help/data/dimension_reduction_opls_matrix.csv", stringsAsFactors = TRUE)
  
  mapping <<- data.frame(matrix(ncol = ncol(dataset), nrow = 0), check.names = FALSE)
  mapping <<- rbind(mapping, colnames(origdataset))
  colnames(mapping) <<- colnames(dataset)
  
  dataset_attachment_sample_group <- read.csv("help/data/dimension_reduction_opls_sample_group.csv", 
                                              stringsAsFactors = TRUE, check.names = FALSE)
  dataset_attachment_var_class <- read.csv("help/data/dimension_reduction_opls_metabolite_class.csv", 
                                           stringsAsFactors = TRUE, check.names = FALSE)
  
  # 样本分组列名称选择, 样例数据不必自动生成
  # output$groupcol_selection_ui <- renderUI({
  #   tags$div(id='settings-group-selection', class = 'collapse-item-body', style = 'display:block;',
  #            selectInput(inputId = ns('settings_groupcol_selection'), label = "", 
  #                        choices = colnames(dataset_attachment_sample_group)[2: length(colnames(dataset_attachment_sample_group))])
  #   )
  # })
  
  # 样本分组列改变，所有的分类也相应改变
  observeEvent(input$settings_groupcol_selection, {
    classes <- unique(dataset_attachment_sample_group[[input$settings_groupcol_selection]])
    # 数据分组选择, 默认选择 first two
    output$groups_selection_ui <- renderUI({
      tags$div(id = 'settings-group-comparison', class = 'collapse-item-body', style = 'display:block;',
               checkboxGroupInput(inputId = ns("select_data_group"), label = "",
                                  choices = classes,
                                  selected = classes[1:2]
               )
      )
    })
    if (is.null(opls_result_loadingplot)) {
      js$execute("dimension_reduction_opls_loadingplot-execute")
    }
  })
  
  output$opls_gallery_loadingplot_matrix <- DT::renderDT({
    DT::datatable({
      origdataset
    },
    options = dataTableOptions_pageLength30,
    selection = 'none',
    style = 'bootstrap4',
    class = 'cell-border stripe compact datatable',
    rownames = FALSE
    )
  })
  
  output$opls_gallery_loadingplot_sample_grouping <- DT::renderDT({
    DT::datatable({
      dataset_attachment_sample_group
    },
    options = dataTableOptions_pageLength30,
    selection = 'none',
    style = 'bootstrap4',
    class = 'cell-border stripe compact datatable',
    rownames = FALSE
    )
  })
  
  output$opls_gallery_loadingplot_metabolite_class <- DT::renderDT({
    DT::datatable({
      dataset_attachment_var_class
    },
    options = dataTableOptions_pageLength30,
    selection = 'none',
    style = 'bootstrap4',
    class = 'cell-border stripe compact datatable',
    rownames = FALSE
    )
  })
  
  # 点击Execute时运行计算
  observeEvent(input$execute, {
    tryCatch({
      # 设置默认的分组选择, 此处为hard coding
      select_data_group <- c()
      if (is.null(opls_result_loadingplot)) {
        select_data_group <- c("Normal", "Early Abortion")
      } else {
        select_data_group <- input$select_data_group
      }
      
      
      if (is.null(dataset)) {
        toastr_warning(title = "Please upload metabolite matrix data!", message = '')
      } else if (is.null(dataset_attachment_sample_group)) {
        toastr_warning(title = "Please upload sample grouping data!", message = '')
      } else if (length(select_data_group) != 2) {
        toastr_warning(title = "OPLS-DA only available for binary classification (use PLS-DA for multiple classes)!", message = '')
      } else {
        preprocessed_data <<- dataset
        # 1. data transformation, 注意第一列是样本名称
        if (input$settings_data_transformation == 'Log2') {
          df_log2 <- dataset
          df_log2[, -c(1)] <- lapply(df_log2[, -c(1)], function(x) {
            if (is.numeric(x)) {
              return(round(log2(x), 6))
            } else {
              return(x)
            }
          })
          preprocessed_data <<- df_log2
        } else if (input$settings_data_transformation == 'Log10') {
          df_log10 <- dataset
          df_log10[, -c(1)] <- lapply(df_log10[, -c(1)], function(x) {
            if (is.numeric(x)) {
              return(round(log10(x), 6))
            } else {
              return(x)
            }
          })
          preprocessed_data <<- df_log10
        }
        
        # 2. data scaling
        if (input$settings_data_scaling == 'Centering') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              return(x - mean_val)
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Unit Variance scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              sd_val <- sd(x)
              return(round((x - mean_val) / sd_val, 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Pareto scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              sd_val <- sd(x)
              return(round((x - mean_val) / sqrt(sd_val), 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Range scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              max_val <- max(x)
              min_val <- min(x)
              return(round((x - mean_val) / (max_val - min_val), 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Vast scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              sd_val <- sd(x)
              return(round(((x - mean_val) / sd_val) * (mean_val / sd_val), 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Level scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              return(round((x - mean_val) / mean_val, 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        }
        
        # 替换为原始的变量名称
        header <- c()
        for (i in 1:length(colnames(preprocessed_data))) {
          header <- append(header, as.character(mapping[[colnames(preprocessed_data)[i]]][1]))
        }
        preprocessed_data[[1]] <- as.factor(preprocessed_data[[1]])
        colnames(preprocessed_data) <- header
        rownames(preprocessed_data) <- preprocessed_data[, 1]
        
        # result table of preprocessed data
        # output$preprocessed_data <- DT::renderDT({
        #   DT::datatable({
        #     preprocessed_data
        #   },
        #   options = dataTableOptions_pageLength25,
        #   selection = 'none',
        #   style = 'bootstrap4',
        #   class = 'cell-border stripe compact datatable',
        #   rownames = FALSE
        #   )
        # })
        
        # 3. 根据选定的分组列名，新增一列group补充到matrix数据中
        dataset_withgroup <- preprocessed_data
        dataset_withgroup['group'] <- dataset_attachment_sample_group[[input$settings_groupcol_selection]]
        
        # 4. 根据选择的数据分组进行数据筛选
        selected_group_data <- dataset_withgroup[dataset_withgroup$group %in% select_data_group, ]
        
        # 去掉第一列的样本名称和group列，数据矩阵作为OPLS的分析对象
        data_matrix <- selected_group_data[, -(1)]
        data_matrix <- subset(data_matrix, select = -group)
        
        # 选择数据对应的样本名称
        sample_name_of_selected_data <- selected_group_data[[1]]
        
        # OPLS
        model <- opls(x = data_matrix,
                      y = as.character(selected_group_data$group),
                      scaleC = "none",# 不选择默认'UV',默认选择“none”，不可更改
                      predI = 1, 
                      orthoI = 1,# 输入框，正交主成分数量，【若不填写数值默认NA】。
                      # algoC = c("default", "nipals", "svd")[1], # 建模方法，单选，选项：default，svd，nipls
                      crossvalI = 7,# 输入框，交叉验证段数，整数，默认7，必须比x行数小”；
                      permI = 9) # 输入框，置换检验次数
        
        # loading plot
        output$loading_plot <- renderPlotly({
          if (!is.null(dataset_attachment_var_class)) {
            if (ncol(dataset_attachment_var_class) == 2 && nrow(dataset_attachment_var_class) > 0) {
              variable_class_mapping <- data.frame(matrix(ncol = nrow(origdataset), nrow = 0), check.names = FALSE)
              variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment_var_class[[2]]))
              colnames(variable_class_mapping) <- dataset_attachment_var_class[[1]]
              variable_class <- c()
              # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
              for (v in rownames(model@loadingMN)) {
                if (v %in% colnames(variable_class_mapping)) {
                  variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
                } else {
                  toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
                  variable_class <- append(variable_class, "unknown")
                }
              }
              
              # prepare loading data
              loading_data = data.frame(
                'metabolite' = rownames(model@loadingMN),
                "class" = variable_class,
                'p1' = round(model@loadingMN, digits = 6),
                'o1' = round(model@orthoLoadingMN, digits = 6)
              )
              
              loading_data$class = as.factor(loading_data$class)
              
              opls_result_loadingplot_data <<- loading_data
              # inspect data
              # output$opls_loadingplot_data <- DT::renderDT({
              #   DT::datatable({
              #     loading_data
              #   },
              #   options = list(initComplete = JS(
              #     "function(settings, json) {",
              #     "$(this.api().table().body()).css({'font-size': '12px'});",
              #     "$(this.api().table().header()).css({'font-size': '12px'});",
              #     "}"),
              #     columnDefs = list(list(className='dt-left', targets="_all")),
              #     scrollX = FALSE,
              #     searching = FALSE,
              #     paging = FALSE,
              #     bInfo = TRUE),
              #   selection = 'none',
              #   style = 'bootstrap4',
              #   class = 'cell-border stripe compact datatable',
              #   rownames = FALSE
              #   )
              # })
              
              legend_position <- 'none'
              if (input$opls_loadingplot_legend_position == 'outside') {
                legend_position <- input$opls_loadingplot_legend_position_outside
              } else if (input$opls_loadingplot_legend_position == 'none') {
                legend_position <- 'none'
              } else if (input$opls_loadingplot_legend_position == 'inside') {
                legend_position <- c(input$opls_loadingplot_legend_position_inside_x, input$opls_loadingplot_legend_position_inside_y)
              }
              
              theme_settings <- theme_bw() +
                theme(legend.position = legend_position,
                      legend.justification = "center",
                      legend.box = "vertical",
                      legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      panel.background = element_rect(fill = input$opls_loadingplot_panel_background_fill_color),
                      plot.title = element_text(family = input$opls_loadingplot_title_fontfamily, size = input$opls_loadingplot_title_fontsize, vjust = 1, hjust = input$opls_loadingplot_title_position),
                      axis.title.x = element_text(family = input$opls_loadingplot_xaxis_fontfamily, size = input$opls_loadingplot_xaxis_fontsize),
                      axis.title.y = element_text(family = input$opls_loadingplot_yaxis_fontfamily, size = input$opls_loadingplot_yaxis_fontsize),
                      axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      axis.ticks = element_line(color = 'black')
                )
              
              if (input$opls_loadingplot_panel_settings_showgrid == FALSE) {
                theme_settings <- theme_settings + theme(panel.grid = element_blank())
              }
              
              # plot
              opls_result_loadingplot <<- ggplot(loading_data, aes(p1, o1, label = metabolite))
              
              # h-line
              if (input$opls_loadingplot_showhline == TRUE) {
                opls_result_loadingplot <<- opls_result_loadingplot + geom_hline(yintercept = input$opls_loadingplot_hline_value, 
                                                                                 linetype = input$opls_loadingplot_hline_shape, 
                                                                                 size = input$opls_loadingplot_hline_size, 
                                                                                 color = input$opls_loadingplot_hline_color)
              }
              # v-line
              if (input$opls_loadingplot_showvline == TRUE) {
                opls_result_loadingplot <<- opls_result_loadingplot + geom_vline(xintercept = input$opls_loadingplot_vline_value, 
                                                                                 linetype = input$opls_loadingplot_vline_shape, 
                                                                                 size = input$opls_loadingplot_vline_size, 
                                                                                 color = input$opls_loadingplot_vline_color)
              }
              
              color_values <- c()
              if (input$opls_loadingplot_datapoint_fillpalette == 'd3') {
                color_values <- pal_d3()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'aaas') {
                color_values <- pal_aaas()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'jama') {
                color_values <- pal_jama()(7)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'jco') {
                color_values <- pal_jco()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'lancet') {
                color_values <- pal_lancet()(9)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'locuszoom') {
                color_values <- pal_locuszoom()(7)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'nejm') {
                color_values <- pal_nejm()(8)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'npg') {
                color_values <- pal_npg()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'simpsons') {
                color_values <- pal_simpsons()(16)
              }
              # if color is not enough
              if (length(color_values) < length(levels(loading_data$class))) {
                color_values <- append(color_values, sample(pal_igv()(50), size = (length(levels(loading_data$class)) - length(color_values))))
              }
              
              opls_result_loadingplot <<- opls_result_loadingplot +
                geom_point(aes(fill = class), 
                           size = input$opls_loadingplot_datapoint_size, 
                           shape = as.numeric(input$opls_loadingplot_datapoint_shape), 
                           stroke = 0.1) +
                scale_fill_manual(values = color_values) +
                ggtitle(input$opls_loadingplot_title_text) + 
                xlab(input$opls_loadingplot_xaxis_label) + ylab(input$opls_loadingplot_yaxis_label) +
                theme_settings
              
              # return(opls_result_loadingplot)
              
              opls_result_loadingplotly <<- config(ggplotly(opls_result_loadingplot), displayModeBar = FALSE) %>% layout(width = input$opls_result_loadingplot_width, height = input$opls_result_loadingplot_height)
              
              return(opls_result_loadingplotly)
            } else {
              toastr_warning(message = "", title = "Please check the format of metabolite classification data")
              return(NULL)
            }
          } else {
            toastr_warning(message = "", title = "Please upload the metabolite classification data")
            return(NULL)
          }
          
        })
      }
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "运行时遇到错误", message = '')
    })
  })
  
  # loadingplot legend显示位置
  observeEvent(input$opls_loadingplot_legend_position, {
    if (input$opls_loadingplot_legend_position == 'none') {
      shinyjs::hide('opls_loadingplot_legend_position_outside')
      shinyjs::hide('opls_loadingplot_legend_position_inside_x')
      shinyjs::hide('opls_loadingplot_legend_position_inside_y')
    } else if (input$opls_loadingplot_legend_position == 'inside') {
      shinyjs::hide('opls_loadingplot_legend_position_outside')
      shinyjs::show('opls_loadingplot_legend_position_inside_x')
      shinyjs::show('opls_loadingplot_legend_position_inside_y')
    } else if (input$opls_loadingplot_legend_position == 'outside') {
      shinyjs::show('opls_loadingplot_legend_position_outside')
      shinyjs::hide('opls_loadingplot_legend_position_inside_x')
      shinyjs::hide('opls_loadingplot_legend_position_inside_y')
    }
  })
  
  # 导出数据预处理结果
  observeEvent(input$export_preprocessed_data, {
    showModal(modalDialog(
      title = "Download data",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_opls_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_opls_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_opls_result_table_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  output$export_opls_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_table_name) || input$export_opls_result_table_name == "") {
        paste("preprocessed-data-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_table_format, sep="")
      } else {
        paste(input$export_opls_result_table_name, input$export_opls_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_opls_result_table_format == ".csv") {
        write.csv(preprocessed_data, file, row.names = FALSE)
      } else if (input$export_opls_result_table_format == ".xlsx") {
        write.xlsx(preprocessed_data, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  # 导出loading plot
  observeEvent(input$export_loadingplot, {
    showModal(modalDialog(
      title = "Download plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_loadingplot_format"), label = "Choose format", 
                    choices = c("html" = ".html"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_loadingplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  
  output$export_loadingplot_ok <- downloadHandler(
    filename = function() {
      paste("loadingplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_loadingplot_format, sep="")
    },
    content = function(file) {
      saveWidget(opls_result_loadingplotly, file = file)
      removeModal()
    }
  )
  
  # 下载 loadingplot 对应的数据
  output$download_opls_loadingplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-loadingplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_loadingplot_data, file, asTable = TRUE)
    }
  )
}