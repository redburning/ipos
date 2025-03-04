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
      title = "Difference Analysis - Rose Plot",
      solidHeader = TRUE,
      collapsible = FALSE,
      collapsed = FALSE,
      style = "min-height: calc(100vh - 120px); height: calc(100vh - 120px);",
      width = 12,
      tags$div(
        style = "display:flex; gap:12px; height:100%;",

        # -----------------------------------
        # 数据预览抽屉层
        # -----------------------------------
        tags$div(
          id="drawer-da-gallery-roseplot", class="drawer",
          tags$div(style = "padding:15px 20px 20px 20px;",
                   tabsetPanel(
                     tabPanel(
                       title = "Metabolite Matrix",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("da_gallery_roseplot_matrix")))
                       )
                     ),
                     tabPanel(
                       title = "Sample Grouping",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("da_gallery_roseplot_sample_grouping")))
                       )
                     ),
                     tabPanel(
                       title = "Metabolite Class",
                       icon = icon("table"),
                       tags$div(
                         style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                         withSpinner(DT::DTOutput(outputId = ns("da_gallery_loadingplot_metabolite_class")))
                       )
                     )
                   )
          )
        ),
        tags$div(id="overlay-da-gallery-roseplot", class="overlay",
                 onclick = onOverlayClick('drawer-da-gallery-roseplot', 'overlay-da-gallery-roseplot')),


        # ------------------------------------------------------
        # 方法参数设置区
        # ------------------------------------------------------
        tags$div(
          style = 'width:250px; height:100%; background-color:white;',
          tags$div(
            style = 'border:1px solid rgba(36, 41, 46, 0.12); border-radius: 5px 5px 0px 0px;',
            buildAccordionItem('My Data', collapsed = FALSE),
            tags$div(class = 'collapse-item-body', style = 'display:block;',
                     # view data
                     tags$button(class = "action-button-primary", 
                                 style = "height:26px; font-size:13px; margin-bottom:8px;",
                                 tags$div(tags$i(class="fas fa-cloud-arrow-up"),
                                          tags$span("Upload data"), 
                                 ),
                                 onclick = onInspectDataBtnClick('drawer-da-gallery-roseplot', 'overlay-da-gallery-roseplot')
                     ),
            )
          ),
          # -----------------------------------------------------
          # 补充方法的更多customize参数
          # -----------------------------------------------------
          
        ),

        # -------------------------------------------------------
        # 绘图区
        # -------------------------------------------------------
        tags$div(
          style = "width:calc(100% - 520px); max-width:calc(100% - 520px); height:100%; overflow-y:scroll; overflow-x:scroll; padding:20px; background-color:white;",
          withSpinner(plotlyOutput(outputId = ns("loading_plot"), width = "100%", height = "100%"))
        ),

        # ------------------------------------------------------
        # 绘图数据下钻抽屉层
        # ------------------------------------------------------
        tags$div(
          id="drawer-opls-loadingplot", class="drawer",
          tags$div(style = "padding:15px 20px 20px 20px;",
                   downloadButton(outputId = ns("download_opls_splot_data"),
                                  class = "action-button-primary",
                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                  label = "Download",
                                  icon = icon("download")),
                   tags$div(
                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                     DT::DTOutput(outputId = ns("opls_gallery_loadingplot_inspectdata"))
                   )
          )
        ),
        tags$div(id="overlay-opls-loadingplot", class="overlay", onclick = onOverlayClick('drawer-opls-loadingplot', 'overlay-opls-loadingplot')),

        # ------------------------------------------------------
        # 绘图参数设置区
        # ------------------------------------------------------
        tags$div(
          style = "width:270px; height:100%; background-color:white;",
          tags$div(
            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
            # Data settings
            tags$div(
              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
              buildAccordionItem(title = 'Data', collapsed = TRUE),
              tags$div(class = 'collapse-item-body', style = 'display:none;',
                       # inspect data
                       tags$button(class = "action-button-primary", 
                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                   tags$div(tags$i(class="far fa-eye"),
                                            tags$span("Inspect data"), 
                                   ), 
                                   onclick = onInspectDataBtnClick('drawer-opls-loadingplot', 'overlay-opls-loadingplot')
                       ),
                       # ------------------------------------------
                       # 更多关于Plot Data的参数设置
                       # ------------------------------------------

              )
            ),
            # --------------------------------------------------
            # 补充Plot的更多customize参数
            # --------------------------------------------------
            
            
            
            
          )
        )
      )
    )
  )
}


dimensionReductionOPLSLoadingPlotServer <- function(input, output, session) {
  ns <- session$ns
  # --------------------------------------------
  # 数据区
  # --------------------------------------------

  classes <- c()              # 所有的数据分组
  preprocessed_data <- NULL   # 预处理(log transform + scaling)之后的数据
  
  opls_result_loadingplot <- NULL
  opls_result_loadingplotly <- NULL
  opls_result_loadingplot_data <- NULL
  
  # --------------------------------------------
  # 加载样例数据
  # --------------------------------------------
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
  
  
  # ----------------------------------------------
  # 内置数据集
  # ----------------------------------------------
  output$da_gallery_roseplot_matrix <- DT::renderDT({
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
  
  output$da_gallery_roseplot_sample_grouping <- DT::renderDT({
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
  
  output$da_gallery_loadingplot_metabolite_class <- DT::renderDT({
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
      # ---------------------------------------------
      # 1. 数据校验
      # ---------------------------------------------

      # ---------------------------------------------
      # 2. 方法参数校验
      # ---------------------------------------------

      # ---------------------------------------------
      # 3. 方法执行
      # ---------------------------------------------

      # ---------------------------------------------
      # 4. 方法执行结果转换为绘图数据格式
      # ---------------------------------------------

      # Plot渲染
      output$loading_plot <- renderPlotly({
        return(NULL)
      })
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "运行时遇到错误", message = '')
    })
  })
  
  # ----------------------------------------------
  # Reactive Settings
  # ----------------------------------------------
  

  
  # -----------------------------------------------
  # 绘图导出区
  # -----------------------------------------------
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
  
  # ------------------------------------------------
  # 数据导出区
  # ------------------------------------------------
  output$download_opls_loadingplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-loadingplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_loadingplot_data, file, asTable = TRUE)
    }
  )
}