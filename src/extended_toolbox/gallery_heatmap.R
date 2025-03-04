source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")

toolbox_heatmap_width <- 800
toolbox_heatmap_height <- 600


heatmapUI <- function(id) {
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
      id = "box-toolbox-heatmap",
      title = "HeatMap",
      solidHeader = TRUE,
      collapsible = FALSE,
      collapsed = FALSE,
      style = "min-height: calc(100vh - 120px); height:auto;",
      width = 12,
      tags$div(
        style = "display:flex; gap:12px; height:100%;",
        tags$div(
          id="drawer-toolbox-heatmap", class="drawer",
          tags$div(style = "padding:15px 20px 20px 20px;",
                   tabsetPanel(
                     tabPanel(
                       title = "Metabolite Matrix",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("toolbox_gallery_matrix")))
                       )
                     ),
                     tabPanel(
                       title = "Sample Grouping",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("toolbox_gallery_sample_grouping")))
                       )
                     ),
                     tabPanel(
                       title = "Metabolite Class",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("toolbox_gallery_metabolite_class")))
                       )
                     )
                   )
          )
        ),
        tags$div(id="overlay-toolbox-heatmap", class="overlay",
                 onclick = onOverlayClick('drawer-toolbox-heatmap', 'overlay-toolbox-heatmap')),
        tags$div(
          style = 'width:250px; height:100%; max-height:calc(100vh - 150px); overflow-y:scroll; background-color:white;',
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-radius: 5px 5px 0px 0px;',
            buildAccordionItem('toolbox-heatmap-settings-mydata', 'My Data', collapsed = FALSE),
            tags$div(id = 'toolbox-heatmap-settings-mydata', class = 'collapse-item-body', style = 'display:block;',
                     # view data
                     tags$button(class = "action-button-primary", 
                                 style = "height:26px; font-size:13px; margin-bottom:8px;",
                                 tags$div(tags$i(class="fas fa-cloud-arrow-up"),
                                          tags$span("Upload data"), 
                                 ),
                                 onclick = onInspectDataBtnClick('drawer-toolbox-heatmap', 'overlay-toolbox-heatmap')
                     ),
            )
          ),
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
            buildAccordionItem('toolbox-heatmap-settings-scale-method', 'Scaling method', collapsed = TRUE),
            tags$div(id = 'toolbox-heatmap-settings-scale-method', class = 'collapse-item-body', style = 'display:none;',
                     radioButtons(inputId = ns('settings_scale_method'), label = '', inline = TRUE, selected = 'row', 
                                  choices = c('row', 'column', 'none')),
            )
          ),
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
            buildAccordionItem('toolbox-heatmap-settings-cluster-method', 'Clustering method', collapsed = TRUE),
            tags$div(id = 'toolbox-heatmap-settings-cluster-method', class = 'collapse-item-body', style = 'display:none;',
                     radioButtons(inputId = ns('settings_cluster_method'), label = '', selected = 'none', 
                                  choices = c('none', 'row', 'column', 'both')),
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
          style = "width:calc(100% - 520px); max-width:calc(100% - 520px); height:calc(100vh - 210px); max-height:calc(100vh - 210px); overflow-y:scroll; overflow-x:scroll;'",
          withSpinner(plotOutput(outputId = ns("toolbox_heatmap"),
                                 width = "100%",
                                 height = "auto")
                      )
        ),
        tags$div(
          style = "width:270px; height:100%; background-color:white; max-height:calc(100vh - 150px); overflow-y:scroll;",
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
            # Title settings
            tags$div(
              class = 'accordion-item-first',
              buildAccordionItem(title = 'Title', collapsed = TRUE),
              tags$div(class = 'collapse-item-body', style = 'display:none;',
                       textInput(inputId = ns('toolbox_heatmap_title_text'), label = 'Text')
              )
            ),
            
            # Panel settings
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Panel', collapsed = TRUE),
              tags$div(class = 'collapse-item-body', style = 'display:none;',
                       selectInput(inputId = ns('toolbox_heatmap_panel_colorpalette'), label = 'Color palette', selected = 'Palette 1', 
                                   choices = paste0('Palette ', 1:11)),
                       numericInput(inputId = ns('toolbox_heatmap_panel_base_fontsize'), label = 'Base fontsize', value = 12, min = 1),
                       numericInput(inputId = ns('toolbox_heatmap_panel_row_fontsize'), label = 'Fontsize for rownames', value = 10, min = 1),
                       numericInput(inputId = ns('toolbox_heatmap_panel_col_fontsize'), label = 'Fontsize colnames', value = 10, min = 1),
                       selectInput(inputId = ns('toolbox_heatmap_panel_col_angle'), label = 'Angle for colnames', selected = 90, choices = c(0, 45, 90, 270, 315))
              )
            ),
            
            # Annotation settings
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Annotation', collapsed = TRUE),
              tags$div(class = 'collapse-item-body', style = 'display:none;',
                       selectInput(inputId = ns('toolbox_heatmap_annotation_rowcolor'), label = 'Color for row annotation', selected = 'd3',
                                   choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light', 'random')),
                       selectInput(inputId = ns('toolbox_heatmap_annotation_colcolor'), label = 'Color for col annotation', selected = 'simpson',
                                   choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light', 'random')),
                       selectInput(inputId = ns('toolbox_heatmap_annotation_showgap'), label = 'Gap', selected = 'both', 
                                   choices = c('none', 'row', 'column', 'both')),
                       radioButtons(inputId = ns('toolbox_heatmap_annotation_showrowname'), label = 'Rowname', inline = TRUE, 
                                    selected = 'show', choices = c('show', 'hide')),
                       radioButtons(inputId = ns('toolbox_heatmap_annotation_showcolname'), label = 'Colname', inline = TRUE, 
                                    selected = 'show', choices = c('show', 'hide')),
                       radioButtons(inputId = ns('toolbox_heatmap_annotation_showcolannotationname'), label = 'Row annotation name', 
                                    inline = TRUE, selected = 'show', choices = c('show', 'hide')),
                       radioButtons(inputId = ns('toolbox_heatmap_annotation_showrowannotationname'), label = 'Col annotation name', 
                                    inline = TRUE, selected = 'show', choices = c('show', 'hide'))
              )
            ),
            
            # Size
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Size', collapsed = TRUE),
              tags$div(class = 'collapse-item-body', style = 'display:none;',
                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                tags$div(
                                  style = 'width:50%',
                                  numericInput(inputId = ns('toolbox_heatmap_width'), label = 'Width', min = 100, step = 10, value = toolbox_heatmap_width)
                                ),
                                tags$div(
                                  style = 'width:50%',
                                  numericInput(inputId = ns('toolbox_heatmap_height'), label = 'Height', min = 100, step = 10, value = toolbox_heatmap_height)
                                )
                       )
              )
            ),
            
            # Export
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Export', collapsed = FALSE),
              tags$div(class = 'collapse-item-body', style = 'display:block;',
                       # export plot
                       tags$div(
                         actionButton(inputId = ns("export_heatmap"), 
                                      label = "Export plot",
                                      icon = icon("download"),
                                      class = "action-button-primary"),
                         style = "padding:6px 0px 20px 0px;"
                       )
              )
            ),
            
          )
        )
      )
      
    )
  )
}


heatmapServer <- function(input, output, session) {
  ns <- session$ns
  result_heatmap <- NULL
  
  # 加载默认矩阵数据
  dataset <- read.csv("help/data/toolbox_heatmap_matrix.csv", 
                       stringsAsFactors = TRUE, check.names = FALSE, row.names = 1)
  
  # 加载默认样本分组数据
  dataset_attachment_sample_group <- read.csv("help/data/toolbox_heatmap_sample_group.csv", 
                                              stringsAsFactors = TRUE, check.names = FALSE, row.names = 1)
  
  # 加载默认变量分组数据
  dataset_attachment_var_class <- read.csv("help/data/toolbox_heatmap_metabolite_class.csv", 
                                           stringsAsFactors = TRUE, check.names = FALSE, row.names = 1)
  # 根据变量数更新图的默认高度
  default_height = nrow(dataset_attachment_var_class) * 20
  updateNumericInput(inputId = "toolbox_heatmap_height", value = default_height)
  toolbox_heatmap_height <<- default_height
  
  # 自动触发execute
  js$execute("extended_toolbox_heatmap-execute")
  
  output$toolbox_gallery_matrix <- DT::renderDT({
    DT::datatable({
      dataset
    },
    options = dataTableOptions_pageLength30,
    selection = 'none',
    style = 'bootstrap4',
    class = 'cell-border stripe compact datatable',
    rownames = TRUE
    )
  })
  
  output$toolbox_gallery_sample_grouping <- DT::renderDT({
    DT::datatable({
      dataset_attachment_sample_group
    },
    options = dataTableOptions_pageLength30,
    selection = 'none',
    style = 'bootstrap4',
    class = 'cell-border stripe compact datatable',
    rownames = TRUE
    )
  })
  
  output$toolbox_gallery_metabolite_class <- DT::renderDT({
    DT::datatable({
      dataset_attachment_var_class
    },
    options = dataTableOptions_pageLength30,
    selection = 'none',
    style = 'bootstrap4',
    class = 'cell-border stripe compact datatable',
    rownames = TRUE
    )
  })
  
  
  # 点击Execute时运行计算
  observeEvent(input$execute, {
    tryCatch({
      if (is.null(dataset)) {
        toastr_warning(title = "Please upload metabolite matrix data!", message = '')
      } else if (is.null(dataset_attachment_sample_group)) {
        toastr_warning(title = "Please upload sample grouping data!", message = '')
      } else if (is.null(dataset_attachment_var_class)) {
        toastr_warning(title = "Please upload metabolite classification data!", message = '')
      } else {
        # heatmap
        gap_group <- which(!duplicated(dataset_attachment_sample_group[[1]], fromLast = T))
        gap_class <- which(!duplicated(dataset_attachment_var_class[[1]], fromLast = T))
        
        m <- length(levels(factor(dataset_attachment_sample_group[[1]])))
        n <- length(levels(factor(dataset_attachment_var_class[[1]])))
        
        output$toolbox_heatmap <- renderPlot({
          # color palette
          mycolor <- getGradientColorPalette(input$toolbox_heatmap_panel_colorpalette)
          
          # annotation color
          annotation_colors <- list(
            group = ggsciColorPalette(palettename = input$toolbox_heatmap_annotation_rowcolor, size = m) %>% setNames(., levels(factor(dataset_attachment_sample_group[[1]]))),
            class = ggsciColorPalette(palettename = input$toolbox_heatmap_annotation_colcolor, size = n) %>% setNames(., levels(factor(dataset_attachment_var_class[[1]])))
          )
          names(annotation_colors) <- c(colnames(dataset_attachment_sample_group)[1], 
                                        colnames(dataset_attachment_var_class)[1])
          
          # cluster method
          cluster_rows = FALSE
          cluster_cols = FALSE
          if (input$settings_cluster_method == 'none') {
            cluster_rows = FALSE
            cluster_cols = FALSE
          } else if (input$settings_cluster_method == 'row') {
            cluster_rows = TRUE
            cluster_cols = FALSE
          } else if (input$settings_cluster_method == 'column') {
            cluster_rows = FALSE
            cluster_cols = TRUE
          } else if (input$settings_cluster_method == 'both') {
            cluster_rows = TRUE
            cluster_cols = TRUE
          }
          
          # gap
          gaps_col = gap_group
          gaps_row = gap_class
          if (input$toolbox_heatmap_annotation_showgap == 'both') {
            gaps_col = gap_group
            gaps_row = gap_class
          } else if (input$toolbox_heatmap_annotation_showgap == 'row') {
            gaps_col = NULL
            gaps_row = gap_class
          } else if (input$toolbox_heatmap_annotation_showgap == 'column') {
            gaps_col = gap_group
            gaps_row = NULL
          } else if (input$toolbox_heatmap_annotation_showgap == 'none') {
            gaps_col = NULL
            gaps_row = NULL
          }
          
          result_heatmap <<- pheatmap(mat = dataset,
                                      main = input$toolbox_heatmap_title_text,
                                      display_numbers = F,# 是否在框内展示数值；
                                      scale = input$settings_scale_method, # 归一化方式
                                      cluster_rows = cluster_rows, # 按行聚类，若选择both或row则为T，其余为F;
                                      cluster_cols = cluster_cols, # 按列聚类，若选择both或column则为T,其余为F;
                                      annotation_col = dataset_attachment_sample_group, # 行注释，用读取的group文件；
                                      annotation_row = dataset_attachment_var_class, # 列注释，用读取的Class文件；
                                      gaps_col = gaps_col, # 行间隙，样本分组间间隙；
                                      gaps_row = gaps_row, # 列间隙，代谢物分类间间隙；
                                      fontsize_row = input$toolbox_heatmap_panel_row_fontsize, # 列注释字体大小；
                                      fontsize_col = input$toolbox_heatmap_panel_col_fontsize,# 行注释字体大小；
                                      fontsize = input$toolbox_heatmap_panel_base_fontsize, # 注释标题等字体大小；
                                      color = mycolor, # 主图颜色
                                      annotation_colors = annotation_colors,# 注释颜色
                                      angle_col = input$toolbox_heatmap_panel_col_angle, # 横坐标倾斜角度
                                      annotation_names_row = ifelse(input$toolbox_heatmap_annotation_showrowannotationname == 'show', TRUE, FALSE),
                                      annotation_names_col = ifelse(input$toolbox_heatmap_annotation_showcolannotationname == 'show', TRUE, FALSE),
                                      show_rownames = ifelse(input$toolbox_heatmap_annotation_showrowname == 'show', TRUE, FALSE),
                                      show_colnames = ifelse(input$toolbox_heatmap_annotation_showcolname == 'show', TRUE, FALSE)
          )
          return(result_heatmap)
        }, height = function() {
          toolbox_heatmap_height <<- input$toolbox_heatmap_height
          return(toolbox_heatmap_height)
        }, width = function() {
          toolbox_heatmap_width <<- input$toolbox_heatmap_width
          return(toolbox_heatmap_width)
        })
      }
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "运行时遇到错误", message = '')
    })
  })
  
  observeEvent(input$export_heatmap, {
    showModal(modalDialog(
        title = "Download heatmap",
        size = "m",
        fluidPage(
          selectInput(inputId = ns("export_toolbox_heatmap_format"), label = "Choose format", 
                      choices = c("jpg", "png", "tiff", "pdf"), selected = 'png'),
          numericInput(inputId = ns('export_toolbox_heatmap_width'), label = 'Width', value = toolbox_heatmap_width, min = 1),
          numericInput(inputId = ns('export_toolbox_heatmap_height'), label = 'Height', value = toolbox_heatmap_height, min = 1)
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_heatmap_ok"), label = "OK", icon = NULL),
          modalButton(label = "Cancel")
        )
      )
    )
  })
  
  # 导出heatmap
  output$export_heatmap_ok <- downloadHandler(
    filename = function() {
      paste("heatmap-", format(Sys.time(), "%Y%m%d%H%M%S"), ".", input$export_toolbox_heatmap_format, sep="")
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = result_heatmap,
             width = input$export_toolbox_heatmap_width / dpi,
             height = input$export_toolbox_heatmap_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
}