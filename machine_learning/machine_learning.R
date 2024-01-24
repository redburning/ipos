source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")
source('machine_learning/msvmRFE.R', encoding = "UTF-8")

# 渐变色柱状图
rf_result_sequential_barplot_width <- 450
rf_result_sequential_barplot_height <- 500
# 离散色柱状图
rf_result_qualitative_barplot_width <- 550
rf_result_qualitative_barplot_height <- 500
# 渐变色气泡图
rf_result_sequential_bubbleplot_width <- 450
rf_result_sequential_bubbleplot_height <- 500
# 离散色气泡图
rf_result_qualitative_bubbleplot_width <- 550
rf_result_qualitative_bubbleplot_height <- 500

drawFeatureImportanceSequentialBarplot <- function(dataset, topnum, xaxis, yaxis, palette) {
  top_features <- head(dataset, topnum)
  # adjust order
  top_features <- top_features[order(top_features[yaxis]), ]
  top_features[[xaxis]] <- factor(top_features[[xaxis]], levels = as.character(top_features[[xaxis]]))
  cols <- brewer.pal(3, palette)
  pal <- colorRampPalette(cols)
  mycolors <- pal(nrow(top_features))
  plot <- ggplot(top_features, aes(x = .data[[xaxis]], y = .data[[yaxis]], fill = .data[[xaxis]])) +
    geom_bar(stat = "identity") + 
    coord_flip() +
    scale_fill_manual(values = mycolors) +
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    xlab("") +
    theme(legend.position = 'none')
  return(plot)
}


drawFeatureImportanceQualitativeBarplot <- function(dataset, topnum, xaxis, yaxis, palette) {
  top_features <- head(dataset, topnum)
  # adjust order
  top_features <- top_features[order(top_features[yaxis]), ]
  top_features[[xaxis]] <- factor(top_features[[xaxis]], levels = as.character(top_features[[xaxis]]))
  plot <- ggplot(top_features, aes(x = .data[[xaxis]], y = .data[[yaxis]], fill = .data[[xaxis]])) +
    geom_bar(stat = "identity", aes(fill = .data[["class"]])) + 
    coord_flip() +
    scale_fill_manual("", values = getColorPalette(palette, classnum = length(unique(dataset[[3]])))) +
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_rect(fill=NA, color = "black", linetype = "solid")) +
    xlab("")
  return(plot)
}


drawFeatureImportanceSequentialBubbleplot <- function(dataset, topnum, xaxis, yaxis, palette) {
  top_features <- head(dataset, topnum)
  # adjust order
  top_features <- top_features[order(top_features[yaxis]), ]
  top_features[[xaxis]] <- factor(top_features[[xaxis]], levels = as.character(top_features[[xaxis]]))
  cols <- brewer.pal(3, palette)
  pal <- colorRampPalette(cols)
  mycolors <- pal(nrow(top_features))
  plot <- ggplot(top_features, aes(x = .data[[xaxis]], y = .data[[yaxis]])) +
    geom_point(stat = "identity", aes(size = 20, color = .data[[xaxis]])) +
    coord_flip() +
    scale_colour_manual(values = mycolors) +
    scale_fill_manual(values = mycolors) +
    xlab("") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(legend.position = 'none')
  
  return(plot)
}


drawFeatureImportanceQualitativeBubbleplot <- function(dataset, topnum, xaxis, yaxis, palette) {  
  top_features <- head(dataset, topnum)
  # adjust order
  top_features <- top_features[order(top_features[yaxis]), ]
  top_features[[xaxis]] <- factor(top_features[[xaxis]], levels = as.character(top_features[[xaxis]]))
  plot <- ggplot(top_features, aes(x = .data[[xaxis]], y = .data[[yaxis]], fill = .data[[xaxis]])) +
    geom_point(stat = "identity", aes(size = 20, color = .data[["class"]], fill = .data[["class"]])) +
    coord_flip() +
    scale_colour_manual(values = getColorPalette(palette, classnum = length(unique(dataset[[3]])))) +
    scale_fill_manual(values = getColorPalette(palette, classnum = length(unique(dataset[[3]])))) +
    xlab("") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(legend.position = "right") +
    guides(size = "none", fill = "none")
  
  return(plot)
}


machineLearningUI <- function(id) {
  ns <- NS(id)
  
  useToastr()
  fluidPage(
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$base(target = "_blank")
              ),
    
    fluidRow(
      box(
        title = "上传数据",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        # fluidRow(column(width = 2, offset = 10, helpButton(ns("help_upload")))),
        fluidRow(column(width = 2, offset = 10, 
                        actionButton(inputId = ns("help_upload"), 
                                     label = "帮助文档", 
                                     icon = icon("book"),
                                     class = "help-button"
                                     )
                        )
                 ),
        fluidRow(
          column(width = 7,
                 fileInput(
                   inputId = ns("dataloader"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " 上传数据... "),
                   placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                   accept = ".csv"
                 )
          ),
          column(width = 5,
                 fileInput(
                   inputId = ns("attachmentloader"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " 上传附表数据... "),
                   # placeholder = "Click the button to select a file, or directly drag and drop the file here.",
                   placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                   accept = ".csv"
                 )
          )
        ),
        fluidRow(
          column(width = 12, uiOutput(ns("data_summary")))
        ),
      ),
      box(
        id = "box-ml",
        title = "特征选择",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        fluidRow(column(width = 2, offset = 10, helpButton(ns("help_fs")))),
        fluidRow(
          sidebarPanel(
            width = 2,
            tags$head(
              tags$style(type="text/css", "select { min-width: 140px; }"),
              tags$style(type="text/css", ".span4 { min-width: 190px; }"),
              tags$style(type="text/css", ".well { min-width: 180px; }")
            ),
            
            fluid  = TRUE,
            selectInput(inputId = ns("classification_or_regression"),
                        # label = "classification/regression",
                        label = "分类/回归",
                        choices = c("classification" = "classification", "regression" = "regression")),
            conditionalPanel(
              condition = "input.classification_or_regression == 'classification'",
              selectInput(inputId = ns("select_classification_model"),
                          # label = "classification model",
                          label = "分类模型",
                          choices = c("Random Forest" = "rf", "SVMRFE" = "svmrfe")),
              ns = ns
            ),
            conditionalPanel(
              condition = "input.classification_or_regression == 'regression'",
              selectInput(inputId = ns("select_regression_model"),
                          # label = "regression model",
                          label = "回归模型",
                          choices = c("Random Forest" = "rf")),
              ns = ns
            ),
            br(),
            actionButton(ns("params_setting"), 
                         # label = "parameter setting", 
                         label = "参数设置", 
                         icon = icon("sliders"),
                         style = STYLES$execute_button),
            actionButton(ns("train"), 
                         # label = "train model", 
                         label = "训练模型", 
                         icon = icon("play"),
                         style = STYLES$execute_button)
          ),
          conditionalPanel(
            condition = "(input.classification_or_regression == 'classification' & input.select_classification_model == 'rf') | 
                         (input.classification_or_regression == 'regression' & input.select_regression_model == 'rf')",
            mainPanel(
              useShinyjs(),
              width = 10,
              tabsetPanel(
                tabPanel(title = "模型参数",
                         icon = icon("sliders"),
                         hidden(div(id = ns("rfResultParams"),
                                    withSpinner(DT::DTOutput(outputId = ns("rf_result_params"))),
                         )),
                         hidden(div(id = ns("rfResultConfusionplot"),
                                    br(),
                                    withSpinner(plotOutput(outputId = ns("rf_result_confusionplot"))),
                                    )),
                         hidden(div(id = ns("rfResultConfusionplotDownload"),
                                    fluidRow(column(width = 2, offset = 10,
                                                    actionButton(inputId = ns("export_rf_result_confusionplot"), 
                                                                 label = "下载图表", 
                                                                 icon = icon("download"),
                                                                 class = "download-button",
                                                                 )
                                                    )
                                             )
                                    ))
                ),
                tabPanel(title = "数据",
                         icon = icon("table"),
                         hidden(div(id = ns("rfResultTable"), 
                                    withSpinner(DT::DTOutput(outputId = ns("rf_result_table"))),
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_rf_result_table"),
                                                                                class = "download-button",
                                                                                label = "下载数据",
                                                                                icon = icon("download")
                                                                                )
                                           )
                         ))
                ),
                tabPanel(title ="渐变色柱状图", 
                         icon = tags$i(class = "iconfont icon-shuipingzhuzhuangtu", role="presentation"),
                         hidden(div(id = ns("rfResultSequentialBarplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("rf_result_sequential_barplot"), 
                                                                    height = "auto", 
                                                                    width = "auto")
                                                         )
                                             ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 6, selectInput(inputId = ns("feature_number_of_rf_sequential_barplot"),
                                                                                   label = "特征数量",
                                                                                   selected = "20",
                                                                                   choices = c("top 10" = "10",
                                                                                               "top 20" = "20",
                                                                                               "top 30" = "30",
                                                                                               "top 40" = "40",
                                                                                               "top 50" = "50"))
                                                     ),
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, numericInput(inputId = ns("rf_result_sequential_barplot_width"),
                                                                                    label = "宽度",
                                                                                    value = rf_result_sequential_barplot_width)
                                                            ),
                                                     column(width = 6, numericInput(inputId = ns("rf_result_sequential_barplot_height"),
                                                                                    label = "高度",
                                                                                    value = rf_result_sequential_barplot_height)
                                                            )
                                                   )
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 8, selectInput(inputId = ns("palette_of_rf_sequential_barplot"),
                                                                                   label = "调色板",
                                                                                   choices = ColorBrewr$seq)
                                                     ),
                                                   )
                                               )
                                             )
                                             )
                                      
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_rf_result_sequential_barplot"),
                                                                                  class = "download-button",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")
                                                                                  )
                                      ),
                                    )
                                    )
                                ),
                         
                ),
                tabPanel(title ="离散色柱状图", 
                         icon = tags$i(class = "iconfont icon-shuipingzhuzhuangtu", role="presentation"),
                         hidden(div(id = ns("rfResultQualitativeBarplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("rf_result_qualitative_barplot"), 
                                                                    height = "auto", 
                                                                    width = "auto")
                                                         ),
                                             ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 6,
                                                            selectInput(inputId = ns("feature_number_of_rf_qualitative_barplot"),
                                                                        label = "特征数量",
                                                                        selected = "20",
                                                                        choices = c("top 10" = "10",
                                                                                    "top 20" = "20",
                                                                                    "top 30" = "30",
                                                                                    "top 40" = "40",
                                                                                    "top 50" = "50"))
                                                            )
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, 
                                                            numericInput(inputId = ns("rf_result_qualitative_barplot_width"),
                                                                         label = "宽度",
                                                                         value = rf_result_qualitative_barplot_width)
                                                            ),
                                                     column(width = 6, numericInput(inputId = ns("rf_result_qualitative_barplot_height"),
                                                                                    label = "高度",
                                                                                    value = rf_result_qualitative_barplot_height)
                                                            )
                                                   )
                                                   )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 8,
                                                            selectInput(inputId = ns("palette_of_rf_qualitative_barplot"),
                                                                        label = "调色板",
                                                                        choices = c("Palette 1", "Palette 2", "Palette 3", "Palette 4", "Palette 5", 
                                                                                    "Palette 6", "Palette 7", "Palette 8", "Palette 9"),
                                                                        selected = "Palette 2")
                                                            )
                                                   )
                                                   )
                                             )
                                             )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_rf_result_qualitative_barplot"),
                                                                                  class = "download-button",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")
                                                                                  )
                                             )
                                    ))
                         ),
                         
                ),
                tabPanel(title ="渐变色气泡图",
                         icon = tags$i(class = "iconfont icon-scatter", role="presentation"),
                         hidden(div(id = ns("rfResultSequentialBubbleplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(
                                               plotOutput(
                                                 outputId = ns("rf_result_sequential_bubbleplot"), 
                                                 width = "auto", 
                                                 height = "auto"
                                                 )
                                               ),
                                             ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 6,
                                                            selectInput(inputId = ns("feature_number_of_rf_sequential_bubbleplot"),
                                                                        label = "特征数量",
                                                                        selected = "20",
                                                                        choices = c("top 10" = "10",
                                                                                    "top 20" = "20",
                                                                                    "top 30" = "30",
                                                                                    "top 40" = "40",
                                                                                    "top 50" = "50"))
                                                            )
                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId = ns("rf_result_sequential_bubbleplot_width"),
                                                                         label = "宽度",
                                                                         value = rf_result_sequential_bubbleplot_width)
                                                            ),
                                                     column(width = 6,
                                                            numericInput(inputId = ns("rf_result_sequential_bubbleplot_height"),
                                                                         label = "高度", 
                                                                         value = rf_result_sequential_bubbleplot_height)
                                                            )
                                                   )
                                                   ),
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 8,
                                                            selectInput(inputId = ns("palette_of_rf_sequential_bubbleplot"),
                                                                        label = "调色板",
                                                                        choices = ColorBrewr$seq)
                                                            )
                                                   )
                                                   )
                                             )
                                             )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_rf_result_sequential_bubbleplot"),
                                                                                  class = "download-button",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")
                                                                                  )
                                             )
                                    ))
                                ),
                         
                ),
                tabPanel(title ="离散色气泡图",
                         icon = tags$i(class = "iconfont icon-scatter", role="presentation"),
                         hidden(div(id = ns("rfResultQualitativeBubbleplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(
                                               plotOutput(outputId = ns("rf_result_qualitative_bubbleplot"), 
                                                          height = "auto", 
                                                          width = "auto")
                                               )
                                             ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 6,
                                                            selectInput(inputId = ns("feature_number_of_rf_qualitative_bubbleplot"),
                                                                        label = "特征数量",
                                                                        selected = "20",
                                                                        choices = c("top 10" = "10",
                                                                                    "top 20" = "20",
                                                                                    "top 30" = "30",
                                                                                    "top 40" = "40",
                                                                                    "top 50" = "50"))
                                                            )
                                                     ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId = ns("rf_result_qualitative_bubbleplot_width"),
                                                                         label = "宽度",
                                                                         value = rf_result_qualitative_bubbleplot_width)
                                                            ),
                                                     column(width = 6,
                                                            numericInput(inputId = ns("rf_result_qualitative_bubbleplot_height"),
                                                                         label = "高度",
                                                                         value = rf_result_qualitative_bubbleplot_height)
                                                            )
                                                   )
                                                   )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 8,
                                                            selectInput(inputId = ns("palette_of_rf_qualitative_bubbleplot"),
                                                                        label = "调色板",
                                                                        choices = c("Palette 1", "Palette 2", "Palette 3", "Palette 4", "Palette 5", 
                                                                                    "Palette 6", "Palette 7", "Palette 8", "Palette 9"),
                                                                        selected = "Palette 2")
                                                            )
                                                   )
                                                   )
                                             )
                                             )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_rf_result_qualitative_bubbleplot"),
                                                                                  class = "download-button",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")
                                                                                  )
                                      )
                                    ))
                                ),
                         
                )
              )
            ),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.classification_or_regression == 'classification' & input.select_classification_model == 'svmrfe'",
            mainPanel(
              useShinyjs(),
              width = 10,
              tabsetPanel(
                tabPanel(title = "模型参数",
                         icon = icon("sliders"),
                         hidden(div(id = ns("svmrfeResultParams"),
                                    withSpinner(DT::DTOutput(outputId = ns("svmrfe_result_params")))
                         ))
                ),
                tabPanel(title = "数据",
                         icon = icon("table"),
                         hidden(div(id = ns("svmrfeResultTable"), 
                                    withSpinner(DT::DTOutput(outputId = ns("svmrfe_result_table"))),
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_svmrfe_result_table"),
                                                                                class = "download-button",
                                                                                label = "下载数据",
                                                                                icon = icon("download")
                                                                                )
                                           )
                         ))
                )
              )
            ),
            ns = ns
          )
        )
      )
    )
  )
}


machineLearningServer <- function(input, output, session) {
  
  ns <- session$ns
  dataset <- NULL
  dataset_attachment <- NULL
  mapping <- NULL
  rf_result_params <- NULL
  rf_result_confusionplot <- NULL
  rf_result_table <- NULL
  rf_result_sequential_barplot <- NULL
  rf_result_qualitative_barplot <- NULL
  
  rf_result_sequential_bubbleplot <- NULL
  rf_result_qualitative_bubbleplot <- NULL
  rf_params_ntree <- 500
  svmrfe_result_params <- NULL
  svmrfe_result_table <- NULL
  svmrfe_params_nfold <- 5
  svmrfe_params_kernel <- "linear"
  svmrfe_params_cost <- 10
  
  observeEvent(input$dataloader$datapath, {
    # dataset with original column names
    origdataset <- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
    dataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE)
    
    mapping <<- data.frame(matrix(ncol = ncol(dataset), nrow = 0), check.names = FALSE)
    mapping <<- rbind(mapping, colnames(origdataset))
    colnames(mapping) <<- colnames(dataset)
    
    output$data_summary <- renderUI({
      p("样本数: ", nrow(origdataset),
        "变量数: ", ncol(origdataset) - 1,
      )
    })
    
    # 自动展开
    js$collapse("box-ml")
    
  })
  
  observeEvent(input$attachmentloader$datapath, {
    # dataset with original column names
    dataset_attachment <<- read.csv(input$attachmentloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
  })
  
  observeEvent(input$params_setting, {
    if ((input$classification_or_regression == "regression" && input$select_regression_model == "rf") || 
        (input$classification_or_regression == "classification" && input$select_classification_model == "rf")) {
      showModal(modalDialog(
        title = "参数设定",
        size = "m",
        fluidPage(
          numericInput(inputId = ns("rf_params_ntree"), label = "Number of trees to grow", value = rf_params_ntree, min = 1),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          actionButton(inputId = ns("rf_params_setting_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    } else if (input$classification_or_regression == "classification" && input$select_classification_model == "svmrfe") {
      showModal(modalDialog(
        title = "参数设定",
        size = "m",
        fluidPage(
          numericInput(inputId = ns("svmrfe_params_nfold"), label = "Fold of cross-validation", value = svmrfe_params_nfold, min = 2, max = 10),
          selectInput(inputId = ns("svmrfe_params_kernel"), label = "Kernel", selected = svmrfe_params_kernel, choices = c("linear")),
          numericInput(inputId = ns("svmrfe_params_cost"), label = "Cost", value = svmrfe_params_cost)
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          actionButton(inputId = ns("svmrfe_params_setting_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    }
  })
  
  # 根据选择的特征数量自动更新渐变色柱状图的高度
  observeEvent(input$feature_number_of_rf_sequential_barplot, {
    rf_result_sequential_barplot_height <<- 20 * as.numeric(input$feature_number_of_rf_sequential_barplot) + 100
    updateNumericInput(inputId = "rf_result_sequential_barplot_height", value = rf_result_sequential_barplot_height)
  })
  
  # 根据选择的特征数量自动更新离散色柱状图的高度
  observeEvent(input$feature_number_of_rf_qualitative_barplot, {
    rf_result_qualitative_barplot_height <<- 20 * as.numeric(input$feature_number_of_rf_qualitative_barplot) + 100
    updateNumericInput(inputId = "rf_result_qualitative_barplot_height", value = rf_result_qualitative_barplot_height)
  })
  
  # 根据选择的特征数量自动更新渐变色气泡图的高度
  observeEvent(input$feature_number_of_rf_sequential_bubbleplot, {
    rf_result_sequential_bubbleplot_height <<- 20 * as.numeric(input$feature_number_of_rf_sequential_bubbleplot) + 100
    updateNumericInput(inputId = "rf_result_sequential_bubbleplot_height", value = rf_result_sequential_bubbleplot_height)
  })
  
  # 根据选择的特征数量自动更新离散色气泡图的高度
  observeEvent(input$feature_number_of_rf_qualitative_bubbleplot, {
    rf_result_qualitative_bubbleplot_height <<- 20 * as.numeric(input$feature_number_of_rf_qualitative_bubbleplot) + 100
    updateNumericInput(inputId = "rf_result_qualitative_bubbleplot_height", value = rf_result_qualitative_bubbleplot_height)
  })
  
  observeEvent(input$train, {
    if (!is.null(dataset)) {
      if ((input$classification_or_regression == "regression" && input$select_regression_model == "rf") || 
          (input$classification_or_regression == "classification" && input$select_classification_model == "rf")) {
        shinyjs::show("rfResultParams")
        shinyjs::show("rfResultTable")
        shinyjs::show("rfResultSequentialBarplot")
        shinyjs::show("rfResultSequentialBubbleplot")
        if (!is.null((dataset_attachment))) {
          shinyjs::show("rfResultQualitativeBarplot")
          shinyjs::show("rfResultQualitativeBubbleplot")
        } else {
          toastr_warning(message = "离散色柱状图需要上传附表数据", title = "请上传附表")
          toastr_warning(message = "离散色气泡图需要上传附表数据", title = "请上传附表")
        }
        if (input$classification_or_regression == "classification" && input$select_classification_model == "rf") {
          shinyjs::show("rfResultConfusionplot")
          shinyjs::show("rfResultConfusionplotDownload")
        } else {
          shinyjs::hide("rfResultConfusionplot")
          shinyjs::hide("rfResultConfusionplotDownload")
        }
        
        # train
        set.seed(10)
        rf <- randomForest(dataset[, -1], dataset[, 1], ntree = rf_params_ntree, importance = TRUE, proximity = TRUE)
        
        ## calculate feature importance
        feature_col <- COLUMNS$feature
        importance_col <- COLUMNS$meanDecreaseInAccuracy
        importance_matrix <- round(importance(rf, type = 1), 2)
        feature_importance_df <- data.frame(importance_matrix, row.names = NULL)
        feature_importance_df[feature_col] <- row.names(importance_matrix)
        colnames(feature_importance_df) <- c(importance_col, feature_col)
        
        # adjust column order
        feature_importance_df <- feature_importance_df[, c(feature_col, importance_col)]
        # select feature of which importance is greater than threshold.
        feature_importance_df <- subset(feature_importance_df, feature_importance_df[importance_col] > 0)
        # sort in desc order
        feature_importance_df <- feature_importance_df[order(-feature_importance_df[importance_col]),]
        
        # replace with original feature name
        for (i in 1:length(feature_importance_df[[feature_col]])) {
          feature_importance_df[[feature_col]][i] <- as.character(mapping[[feature_importance_df[[feature_col]][i]]][1])
        }
        
        # feature name column as factor to keep order
        feature_importance_df[[feature_col]] <- factor(feature_importance_df[[feature_col]],
                                                       levels = as.character(feature_importance_df[[feature_col]]))
        # params for randomforest regression
        if (input$classification_or_regression == "regression") {
          rf_result_params <<- data.frame(param = c("number of trees grown",
                                                    "mean square errors"), 
                                          value = c(rf$ntree,
                                                    round(mean(rf$mse), 4)),
                                          stringsAsFactors = FALSE)
        }
        # params for randomforest classification
        if (input$classification_or_regression == "classification") {
          rf_result_params <<- data.frame(param = c("number of trees grown", "oob.err.rate"), 
                                          value = c(rf$ntree, round(mean(as.data.frame(rf$err.rate)[["OOB"]]), digits = 4)),
                                          stringsAsFactors = FALSE)
          
          confusion <- rf$confusion
          confusion <- as.table(confusion)
          confusion <- as.data.frame(confusion)
          confusion <- subset(confusion, confusion["Var2"] != "class.error")
          rf_result_confusionplot <<- ggplot(confusion, aes(Var1, Var2, fill = Freq)) +
            geom_tile() +
            geom_text(aes(label = Freq)) +
            scale_fill_gradient(low = "white", high = "#3575b5") +
            labs(x = "", y = "", title = "Confusion matrix", fill = "") +
            theme(legend.position = 'none') +
            theme(plot.margin = unit(c(1, 1, 1, 0), "cm"))
        }
        
        rf_result_table <<- feature_importance_df
        
        # rf reuslt params
        output$rf_result_params <- DT::renderDT({
          DT::datatable({
            rf_result_params
          },
          options = dataTableOptions,
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        # rf confusion matrix [classification]
        output$rf_result_confusionplot <- renderPlot({
          return(rf_result_confusionplot)
        }, height = function() {length(unique(dataset[[1]])) * 100}, width = function() {length(unique(dataset[[1]])) * 100})
        
        # rf result table
        output$rf_result_table <- DT::renderDT({
          DT::datatable({
            rf_result_table
          },
          options = dataTableOptions,
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        # rf sequential bar plot
        output$rf_result_sequential_barplot <- renderPlot({
          rf_result_sequential_barplot <<- drawFeatureImportanceSequentialBarplot(dataset = rf_result_table,
                                                                                  topnum = as.numeric(input$feature_number_of_rf_sequential_barplot),
                                                                                  xaxis = COLUMNS$feature,
                                                                                  yaxis = COLUMNS$meanDecreaseInAccuracy,
                                                                                  palette = input$palette_of_rf_sequential_barplot)
          return(rf_result_sequential_barplot)
        }, height = function() { 
          rf_result_sequential_barplot_height <<- input$rf_result_sequential_barplot_height
          return(rf_result_sequential_barplot_height)
        }, width = function() {
          rf_result_sequential_barplot_width <<- input$rf_result_sequential_barplot_width
          return(rf_result_sequential_barplot_width)
        })
        
        # rf qualitative bar plot
        output$rf_result_qualitative_barplot <- renderPlot({
          
          ## 添加变量类别信息
          variable_class_mapping <- data.frame(matrix(ncol = nrow(rf_result_table), nrow = 0), check.names = FALSE)
          variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment[[2]]))
          colnames(variable_class_mapping) <- dataset_attachment[[1]]
          variable_class <- c()
          # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
          for (v in rf_result_table[["feature"]]) {
            if (v %in% colnames(variable_class_mapping)) {
              variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
            } else {
              toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
              variable_class <- append(variable_class, "unknown")
            }
          }
          rf_result_table["class"] = variable_class
          
          rf_result_qualitative_barplot <<- drawFeatureImportanceQualitativeBarplot(dataset = rf_result_table,
                                                                                    topnum = as.numeric(input$feature_number_of_rf_qualitative_barplot),
                                                                                    xaxis = COLUMNS$feature,
                                                                                    yaxis = COLUMNS$meanDecreaseInAccuracy,
                                                                                    palette = input$palette_of_rf_qualitative_barplot)
          return(rf_result_qualitative_barplot)
        }, height = function(){ 
            rf_result_qualitative_barplot_height <<- input$rf_result_qualitative_barplot_height
            return(rf_result_qualitative_barplot_height)
        }, width = function() {
          rf_result_qualitative_barplot_width <<- input$rf_result_qualitative_barplot_width
          return(rf_result_qualitative_barplot_width)
          }
        )
        
        # sequential bubble plot
        output$rf_result_sequential_bubbleplot <- renderPlot({
          rf_result_sequential_bubbleplot <<- drawFeatureImportanceSequentialBubbleplot(dataset = rf_result_table,
                                                                   topnum = as.numeric(input$feature_number_of_rf_sequential_bubbleplot),
                                                                   xaxis = COLUMNS$feature,
                                                                   yaxis = COLUMNS$meanDecreaseInAccuracy,
                                                                   palette = input$palette_of_rf_sequential_bubbleplot)
          return(rf_result_sequential_bubbleplot)
        }, height = function() {
          rf_result_sequential_bubbleplot_height <<- input$rf_result_sequential_bubbleplot_height
          return(rf_result_sequential_bubbleplot_height)
        }, width = function() {
          rf_result_sequential_bubbleplot_width <<- input$rf_result_sequential_bubbleplot_width
          return(rf_result_sequential_bubbleplot_width)
          }
        )

        # qualitative bubble plot
        output$rf_result_qualitative_bubbleplot <- renderPlot({
          ## 添加变量类别信息
          variable_class_mapping <- data.frame(matrix(ncol = nrow(rf_result_table), nrow = 0), check.names = FALSE)
          variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment[[2]]))
          colnames(variable_class_mapping) <- dataset_attachment[[1]]
          variable_class <- c()
          # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
          for (v in rf_result_table[["feature"]]) {
            if (v %in% colnames(variable_class_mapping)) {
              variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
            } else {
              toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
              variable_class <- append(variable_class, "unknown")
            }
          }
          rf_result_table["class"] = variable_class

          rf_result_qualitative_bubbleplot <<- drawFeatureImportanceQualitativeBubbleplot(dataset = rf_result_table, 
                                                                   topnum = as.numeric(input$feature_number_of_rf_qualitative_bubbleplot),
                                                                   xaxis = COLUMNS$feature,
                                                                   yaxis = COLUMNS$meanDecreaseInAccuracy,
                                                                   palette = input$palette_of_rf_qualitative_bubbleplot)
          return(rf_result_qualitative_bubbleplot)
        }, height = function() {
          rf_result_qualitative_bubbleplot_height <<- input$rf_result_qualitative_bubbleplot_height
          return(rf_result_qualitative_bubbleplot_height)
        }, width = function() {
          rf_result_qualitative_bubbleplot_width <<- input$rf_result_qualitative_bubbleplot_width
          return(rf_result_qualitative_bubbleplot_width)
        })
        
      } else if (input$classification_or_regression == "classification" && input$select_classification_model == "svmrfe") {
        shinyjs::show("svmrfeResultParams")
        shinyjs::show("svmrfeResultTable")
        
        # Set up cross validation
        nfold = svmrfe_params_nfold
        nrows = nrow(dataset)
        folds = rep(1:nfold, len=nrows)[sample(nrows)]
        folds = lapply(1:nfold, function(x) which(folds == x))
        # Perform feature ranking on all training sets
        # results = lapply(folds, svmRFE.wrap, dataset, k=10, halve.above=100)
        results = c()
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                       index = 1
                       for (fold in folds) {
                         results[[index]] = svmRFE.wrap(fold, dataset, k=10, halve.above=100, kernel = svmrfe_params_kernel, cost = svmrfe_params_cost)
                         incProgress(1/length(folds))
                         index = index + 1
                       }
                     })
        
        # Obtain top features across ALL folds
        top.features = WriteFeatures(results, dataset, save=F)
        top.features[["FeatureName"]] <- factor(top.features[["FeatureName"]], levels = as.character(top.features[["FeatureName"]]))
        svmrfe_result_table <<- head(top.features, 100)
        svmrfe_result_table <<- data.frame(svmrfe_result_table, stringsAsFactors = FALSE)
        
        # replace with original feature name
        originalFeatureName <- c()
        for (i in 1:length(svmrfe_result_table[["FeatureName"]])) {
          originalFeatureName <- append(originalFeatureName, as.character(mapping[[svmrfe_result_table[["FeatureName"]][i]]][1]))
        }
        svmrfe_result_table["FeatureName"] <- originalFeatureName
        
        svmrfe_result_params <<- data.frame(param = c("Fold of cross-validation", "Kernel", "Cost"),
                                            value = c(svmrfe_params_nfold, svmrfe_params_kernel, svmrfe_params_cost),
                                            stringsAsFactors = FALSE)
        
        # svmrfe reuslt params
        output$svmrfe_result_params <- DT::renderDT({
          DT::datatable({
            svmrfe_result_params
          },
          options = list(bLengthChange = FALSE,
                         pageLength = 10,
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().body()).css({'font-size': '10px'});",
                           "$(this.api().table().header()).css({'font-size': '10px'});",
                           "}"),
                         columnDefs = list(list(className='dt-left', targets="_all")),
                         scrollX = TRUE,
                         searching = FALSE,
                         paging = FALSE,
                         bInfo = FALSE
          ),
          selection = 'multiple',
          style = 'bootstrap',
          class = 'cell-border stripe compact',
          rownames = FALSE
          )
        })
        
        # svmrfe result table
        output$svmrfe_result_table <- DT::renderDT({
          DT::datatable({
            svmrfe_result_table
          },
          options = list(bLengthChange = FALSE,
                         pageLength = 10,
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().body()).css({'font-size': '10px'});",
                           "$(this.api().table().header()).css({'font-size': '10px'});",
                           "}"),
                         columnDefs = list(list(className='dt-left', targets="_all")),
                         scrollX = TRUE,
                         scrollY = "400px",
                         searching = FALSE,
                         paging = FALSE
          ),
          selection = 'multiple',
          style = 'bootstrap',
          class = 'cell-border stripe compact',
          rownames = FALSE
          )
        })
      }
    }
  })
  
  observeEvent(input$rf_params_setting_ok, {
    rf_params_ntree <<- input$rf_params_ntree
    removeModal()
  })
  
  observeEvent(input$svmrfe_params_setting_ok, {
    svmrfe_params_nfold <<- input$svmrfe_params_nfold
    svmrfe_params_kernel <<- input$svmrfe_params_kernel
    svmrfe_params_cost <<- input$svmrfe_params_cost
    removeModal()
  })
  
  # 混淆矩阵图导出
  observeEvent(input$export_rf_result_confusionplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_rf_result_confusionplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_rf_result_confusionplot_format"), label = "Choose format", choices = c(".jpg", ".png", ".tiff", ".pdf"))
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_rf_result_confusionplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_rf_result_confusionplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_confusionplot_name) || input$export_rf_result_confusionplot_name == "") {
        paste("confusionmatrix-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_confusionplot_format, sep="")
      } else {
        paste(input$export_rf_result_confusionplot_name, input$export_rf_result_confusionplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = rf_result_confusionplot)
      removeModal()
    }
  )
  
  # 渐变色柱状图-导出
  observeEvent(input$export_rf_result_sequential_barplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_rf_result_sequential_barplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_rf_result_sequential_barplot_format"), label = "Choose format", choices = c(".jpg", ".png", ".tiff", ".pdf"))
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_rf_result_sequential_barplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_rf_result_sequential_barplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_sequential_barplot_name) || input$export_rf_result_sequential_barplot_name == "") {
        paste("barplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_sequential_barplot_format, sep="")
      } else {
        paste(input$export_rf_result_sequential_barplot_name, input$export_rf_result_sequential_barplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = rf_result_sequential_barplot,
             width = rf_result_sequential_barplot_width / plot_size_fold,
             height = rf_result_sequential_barplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  # 离散色柱状图-导出
  observeEvent(input$export_rf_result_qualitative_barplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_rf_result_qualitative_barplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_rf_result_qualitative_barplot_format"), label = "Choose format", choices = c(".jpg", ".png", ".tiff", ".pdf"))
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_rf_result_qualitative_barplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_rf_result_qualitative_barplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_qualitative_barplot_name) || input$export_rf_result_qualitative_barplot_name == "") {
        paste("barplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_qualitative_barplot_format, sep="")
      } else {
        paste(input$export_rf_result_qualitative_barplot_name, input$export_rf_result_qualitative_barplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = rf_result_qualitative_barplot,
             width = rf_result_qualitative_barplot_width / plot_size_fold,
             height = rf_result_qualitative_barplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  # 渐变色气泡图-导出
  observeEvent(input$export_rf_result_sequential_bubbleplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_rf_result_sequential_bubbleplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_rf_result_sequential_bubbleplot_format"), label = "Choose format", choices = c(".jpg", ".png", ".tiff", ".pdf"))
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_rf_result_sequential_bubbleplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_rf_result_sequential_bubbleplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_sequential_bubbleplot_name) || input$export_rf_result_sequential_bubbleplot_name == "") {
        paste("bubbleplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_sequential_bubbleplot_format, sep="")
      } else {
        paste(input$export_rf_result_sequential_bubbleplot_name, input$export_rf_result_sequential_bubbleplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = rf_result_sequential_bubbleplot,
             width = rf_result_sequential_bubbleplot_width / plot_size_fold,
             height = rf_result_sequential_bubbleplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )

   # 离散色气泡图-导出
  observeEvent(input$export_rf_result_qualitative_bubbleplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_rf_result_qualitative_bubbleplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_rf_result_qualitative_bubbleplot_format"), label = "Choose format", choices = c(".jpg", ".png", ".tiff", ".pdf"))
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_rf_result_qualitative_bubbleplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_rf_result_qualitative_bubbleplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_qualitative_bubbleplot_name) || input$export_rf_result_qualitative_bubbleplot_name == "") {
        paste("bubbleplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_qualitative_bubbleplot_format, sep="")
      } else {
        paste(input$export_rf_result_qualitative_bubbleplot_name, input$export_rf_result_qualitative_bubbleplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = rf_result_qualitative_bubbleplot,
             width = rf_result_qualitative_bubbleplot_width / plot_size_fold,
             height = rf_result_qualitative_bubbleplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  observeEvent(input$export_rf_result_table, {
    showModal(modalDialog(
      title = "下载数据",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_rf_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_rf_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_rf_result_table_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    ))
  })
  
  output$export_rf_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_table_name) || input$export_rf_result_table_name == "") {
        paste("fs-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_table_format, sep="")
      } else {
        paste(input$export_rf_result_table_name, input$export_rf_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_rf_result_table_format == ".csv") {
        write.csv(rf_result_table, file, row.names = FALSE)
      } else if (input$export_rf_result_table_format == ".xlsx") {
        write.xlsx(rf_result_table, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  observeEvent(input$export_svmrfe_result_table, {
    showModal(modalDialog(
      title = "下载数据",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_svmrfe_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_svmrfe_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_svmrfe_result_table_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    ))
  })
  
  output$export_svmrfe_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_rf_result_table_name) || input$export_rf_result_table_name == "") {
        paste("fs-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_rf_result_table_format, sep="")
      } else {
        paste(input$export_rf_result_table_name, input$export_rf_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_rf_result_table_format == ".csv") {
        write.csv(svmrfe_result_table, file, row.names = FALSE)
      } else if (input$export_rf_result_table_format == ".xlsx") {
        write.xlsx(svmrfe_result_table, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "上传数据",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/MachineLearning_UploadData.md")),
        fluidRow(column(width = 2, 
                        downloadButton(outputId = ns("download_classification_sampledata"), label = "下载分类数据", icon = icon("download"), style = STYLES$help_download_sampledata_button)),
                 column(width = 2,
                        downloadButton(outputId = ns("download_regression_sampledata"), label = "下载回归数据", icon = icon("download"), style = STYLES$help_download_sampledata_button)),
                 column(width = 2,
                        downloadButton(outputId = ns("download_attachment_sampledata"), label = "下载附表数据", icon = icon("download"), style = STYLES$help_download_sampledata_button))
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
  
  output$download_classification_sampledata <- downloadHandler(
    filename = "ml-classification-datasample.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_main.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  output$download_regression_sampledata <- downloadHandler(
    filename = "muscle_lipid.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/muscle_lipid.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  output$download_attachment_sampledata <- downloadHandler(
    filename = "ml_attachment.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/ml_attachment.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  callModule(helpServer, "help_fs", title = "特征选择", size = "l", file = "help/MachineLearning.md")
  
}