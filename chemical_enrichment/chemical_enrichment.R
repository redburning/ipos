source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")
source('chemical_enrichment/chemrich.R', encoding = "UTF-8")

chemicalencirh_plot_width <- 900
chemicalencirh_plot_height <- 600


chemicalEnrichmentUI <- function(id) {
  ns <- NS(id)
  
  useToastr()
  fluidPage(
    fluidRow(
      tags$div(
        class = "chemical_enrichment",
        fluidRow(
          column(width = 12,
                 box(title = "上传数据", collapsible = TRUE, collapsed = FALSE, width = 12,
                     solidHeader = TRUE,
                     fluidRow(column(width = 2, offset = 10, actionButton(inputId = ns("help_upload"),
                                                                          label = "帮助文档",
                                                                          icon = icon("book"),
                                                                          class = "help-button")
                                     )
                              ),
                     fluidRow(
                       column(width = 12,
                              fileInput(
                                inputId = ns("dataloader"),
                                label = "",
                                buttonLabel = div(icon("folder-open"), " 上传数据... "),
                                placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                                accept = c(".xlsx", ".csv"))
                              ),
                       ),
                     ),
                 )
        ),
        fluidRow(
          column(width = 12,
                 box(id = "box-ce", title = "化学富集", collapsible = TRUE, collapsed = TRUE, width = 12,
                     solidHeader = TRUE,
                     fluidRow(column(width = 2, offset = 10, helpButton(ns("help_analysis")))),
                     fluidRow(
                       sidebarPanel(
                         width = 2,
                         fluid  = TRUE,
                         actionButton(ns("setting"),
                                      label = "参数设置",
                                      icon = icon("sliders"),
                                      class = "setting-button"),
                         actionButton(ns("execute"), 
                                      # label = "Execute", 
                                      label = "执行", 
                                      icon = icon("play"),
                                      class = "setting-button"),
                       ),
                       mainPanel(
                         width = 10,
                         tabsetPanel(
                           tabPanel(title = "数据", 
                                    icon = icon("table"),
                                    hidden(div(id = ns("chemicalrichResultTable"), 
                                               fluidRow(
                                                 column(width = 3, selectInput(inputId = ns("select_chemicalenrich_table_sheet"),
                                                                               label = "",
                                                                               choices = c("ChemRICH_Results", "Compound_ChemRICH")))
                                               ),
                                               withSpinner(DT::DTOutput(outputId = ns("chemicalenrich_result_table"))),
                                               fluidRow(
                                                 column(width = 2, offset = 10, 
                                                        downloadButton(outputId = ns("export_chemicalrich_result_table"), 
                                                                       label = "下载数据", 
                                                                       class = "download-button",
                                                                       icon = icon("download")
                                                                       )
                                                        )
                                                 )
                                               )
                                           )
                           ),
                           tabPanel(title = "富集图",
                                    icon = tags$i(class = "iconfont icon-scatter", role="presentation"),
                                    hidden(div(id = ns("chemicalrichPlot"), 
                                               fluidRow(
                                                 br(),
                                                 column(width = 10,
                                                        withSpinner(plotlyOutput(outputId = ns("chemicalenrich_plot"),
                                                                                 width = "100%",
                                                                                 height = "600px")
                                                                    ),
                                                        # withSpinner(plotOutput(outputId = ns("chemicalenrich_plot"),
                                                        #                        width = "100%", 
                                                        #                        height = "600px")
                                                        #             ),
                                                        ),
                                                 # column(width = 3,
                                                 #        class = "plot-setting-column",
                                                 #        tags$div(
                                                 #          class = "plot-setting",
                                                 #          box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 #              fluidRow(
                                                 #                
                                                 #              )
                                                 #              )
                                                 #          )
                                                 #        )
                                               ),
                                               fluidRow(
                                                 column(width = 2, offset = 10, actionButton(inputId = ns("export_chemicalenrich_plot"),
                                                                                             class = "download-button",
                                                                                             label = "下载图表",
                                                                                             icon = icon("download"))
                                                        )
                                               )
                                    )
                                    )
                                    )
                         )
                       )
                     )
                 )
                 )
        )
      )
      
    )
  )
}


chemicalEnrichmentServer <- function(input, output, session) {
  
  ns <- session$ns
  dataset <- NULL
  result <- NULL
  
  observeEvent(input$dataloader$datapath, {
    if (str_ends(input$dataloader$datapath, ".xlsx")) {
      dataset <<- data.frame(readxl::read_xlsx(path = input$dataloader$datapath, sheet = 1), stringsAsFactors = F)
    } else if (str_ends(input$dataloader$datapath, ".csv")) {
      dataset <<- read.csv(input$dataloader$datapath)
    }
    
    
    # 自动展开
    js$collapse("box-ce")
  })
  
  # 执行
  observeEvent(input$execute, {
    if (!is.null(dataset)) {
      shinyjs::show("chemicalrichResultTable")
      shinyjs::show("chemicalrichPlot")
      
      result <<- run_chemrich_basic(data = dataset)
      
      # plotly
      output$chemicalenrich_plot <- renderPlotly({
        return(result$plotly)
      })
      
      # plot
      # output$chemicalenrich_plot <- renderPlot({
      #   return(result$plot)
      # })
      
    }
  })
  
  observeEvent(input$select_chemicalenrich_table_sheet, {
    # table
    if (input$select_chemicalenrich_table_sheet == "ChemRICH_Results") {
      output$chemicalenrich_result_table <- DT::renderDT({
        DT::datatable({
          result$data$ChemRICH_Results
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_chemicalenrich_table_sheet == "Compound_ChemRICH") {
      output$chemicalenrich_result_table <- DT::renderDT({
        DT::datatable({
          result$data$Compound_ChemRICH
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    }
  })
  
  
  # 下载数据
  output$export_chemicalrich_result_table <- downloadHandler(
    filename = function() {
      paste0("chemRICH_class_results.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(result$data, file, asTable = TRUE)
    }
  )
  
  # 下载图表
  observeEvent(input$export_chemicalenrich_plot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_chemicalenrichplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_chemicalenrichplot_format"), label = "Choose format", choices = c(".jpg", ".png", ".tiff", ".pdf", ".pptx", ".html"))
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_chemicalenrichplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_chemicalenrichplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_chemicalenrichplot_name) || input$export_chemicalenrichplot_name == "") {
        paste("chemrichplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_chemicalenrichplot_format, sep="")
      } else {
        paste(input$export_chemicalenrichplot_name, input$export_chemicalenrichplot_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_chemicalenrichplot_format == ".pptx") {
        read_pptx() %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(dml(ggobj = result$plot), location = ph_location(type = "body", width=10, height=6.18, left = 0, top = 0)) %>%
          print(target = file) %>%
          invisible()
      } else if (input$export_chemicalenrichplot_format == ".html") {
        saveWidget(result$plotly, file = file, selfcontained = T)
      } else {
        ggsave(filename = file, 
               plot = result$plot, 
               width = chemicalencirh_plot_width / plot_size_fold,
               height = chemicalencirh_plot_height / plot_size_fold,
               units = "mm")
      }
      removeModal()
    }
  )
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "化学富集",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/ChemicalEnrich_UploadData.md")),
        fluidRow(column(width = 2, 
                        downloadButton(outputId = ns("download_sampledata_chemicalenrich"), 
                                       label = "下载样例数据", icon = icon("download"), 
                                       style = STYLES$help_download_sampledata_button)),
        ),
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        modalButton(label = "确认")
      ),
      style="max-height:500px; overflow:auto;"
    ))
  })
  
  output$download_sampledata_chemicalenrich <- downloadHandler(
    filename = "chemrich_sample.xlsx",
    content = function(file) {
      sampledata <- readxl::read_xlsx(path = "help/data/chemrich_sample.xlsx", sheet = 1)
      write.xlsx(sampledata, file)
      # write.csv(sampledata, file, row.names = FALSE)
    }
  )
}