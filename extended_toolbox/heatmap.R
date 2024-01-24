source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")

heatmap_width <- 600
heatmap_height <- 600


heatmapUI <- function(id) {
  ns <- NS(id)
  
  useToastr()
  fluidPage(
    fluidRow(
      tags$div(
        class = "extended_tool_box",
        fluidRow(
          column(width = 12,
                 box(title = "相关性热图", collapsible = TRUE, collapsed = FALSE, width = 12,
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
                                inputId = ns("dataloader_heatmap"),
                                label = "",
                                buttonLabel = div(icon("folder-open"), " 上传数据... "),
                                placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                                accept = ".csv"
                                )
                              ),
                       ),
                     hidden(div(id = ns("div_heatmap"),
                                fluidRow(
                                  column(width = 8,
                                         withSpinner(plotOutput(outputId = ns("plotoutput_heatmap"), 
                                                                width = paste0(heatmap_width, "px"), 
                                                                height = paste0(heatmap_height, "px"))
                                                     ),
                                         ),
                                  column(width = 4,
                                         class = "plot-setting-column",
                                         tags$div(
                                           class = "plot-setting",
                                           box(title = "基础设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                               fluidRow(
                                                 column(width = 6, selectInput(inputId = ns("method_of_heatmap"),
                                                                               label = "method",
                                                                               choices = c("circle", "square", "ellipse", "number", "shade", "color", "pie"))
                                                        ),
                                                 column(width = 6, selectInput(inputId = ns("type_of_heatmap"),
                                                                               label = "type",
                                                                               choices = c("full", "lower", "upper"))
                                                        )
                                               ),
                                               fluidRow(
                                                 column(width = 6, selectInput(inputId = ns("order_of_heatmap"),
                                                                               label = "order",
                                                                               choices = c("original", "AOE", "FPC", "hclust", "alphabet"))
                                                        ),
                                                 column(width = 6, selectInput(inputId = ns("diag_of_heatmap"),
                                                                               label = "diag",
                                                                               choices = c("是" = "TRUE", "否" = "FALSE"))
                                                        )
                                                 )
                                               )
                                           ),
                                         )
                                  ),
                                fluidRow(
                                  column(width = 2, offset = 10, actionButton(inputId = ns("export_heatmap"),
                                                                              class = "download-button",
                                                                              label = "下载图表",
                                                                              icon = icon("download"))
                                         ),
                                  )
                                )
                            ),
                     
                     
                     
                     
                     ),
                 ),
        )
      )
      
    )
  )
}


heatmapServer <- function(input, output, session) {
  
  ns <- session$ns
  data <- NULL
  heatmap <- NULL
  
  observeEvent(input$dataloader_heatmap$datapath, {
    data <<- read.csv(input$dataloader_heatmap$datapath, row.names = 1)
    
    shinyjs::show("div_heatmap")
    
    output$plotoutput_heatmap <- renderPlot({
      heatmap <<- corrplot(corr = as.matrix(data),
                           method = input$method_of_heatmap,
                           type = input$type_of_heatmap,
                           order = input$order_of_heatmap,
                           diag = as.logical(input$diag_of_heatmap))
      return(heatmap)
    })
  })
  
  
  # 导出heatmap
  observeEvent(input$export_heatmap, {
    showModal(tags$div(
      modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_heatmap_name"), label = "文件名称",
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_heatmap_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_heatmap_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })


  output$export_heatmap_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_heatmap_name) || input$export_heatmap_name == "") {
        paste("corrheatmap-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_heatmap_format, sep="")
      } else {
        paste(input$export_heatmap_name, input$export_heatmap_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_heatmap_format == ".png") {
        png(file, width = heatmap_width, height = heatmap_height)
      } else if (input$export_heatmap_format == ".jpg") {
        jpeg(file, width = heatmap_width, height = heatmap_height)
      } else if (input$export_heatmap_format == ".tiff") {
        tiff(file, width = heatmap_width, height = heatmap_height)
      } else if (input$export_heatmap_format == ".pdf") {
        pdf(file)
      }
      corrplot(corr = as.matrix(data),
               method = input$method_of_heatmap,
               type = input$type_of_heatmap,
               order = input$order_of_heatmap,
               diag = as.logical(input$diag_of_heatmap))
      dev.off()
      removeModal()
    }
  )
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "相关性热图",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/extendedtoolbox_corrheatmap.md")),
        fluidRow(column(width = 2, 
                        downloadButton(outputId = ns("download_sampledata_corrheatmap"), 
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
  
  output$download_sampledata_corrheatmap <- downloadHandler(
    filename = "corrheatmap.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/corrheatmap.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
}