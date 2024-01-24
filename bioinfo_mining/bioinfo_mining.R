source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")

heatmap_width <- 600
heatmap_height <- 600


bioinfoMiningUI <- function(id) {
  ns <- NS(id)
  
  useToastr()
  fluidPage(
    fluidRow(
      tags$div(
        class = "bioinfo_mining",
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
                                inputId = ns("dataloader_heatmap"),
                                label = "",
                                buttonLabel = div(icon("folder-open"), " 上传数据... "),
                                placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                                accept = ".csv"
                              )
                       ),
                     ),
                 ),
                 ),
        ),
        fluidRow(
          column(width = 12,
                 box(title = "生信挖掘", collapsible = TRUE, collapsed = FALSE, width = 12,
                     solidHeader = TRUE,
                 )
                 )
        )
      )
      
    )
  )
}


bioinfoMiningServer <- function(input, output, session) {
  
  ns <- session$ns
  
  observeEvent(input$dataloader_heatmap$datapath, {
    data <- read.csv(input$dataloader_heatmap$datapath, row.names = 1)
  })
  
  
  
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "生信挖掘",
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