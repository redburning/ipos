source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")
source("quality_control/normalizations.R")
source("quality_control/utils.R")
source("quality_control/evaluationMethods.R")
source("quality_control/MetNormalizer.R")

pacman::p_load("randomForest", "e1071", "data.table", "parallel", "xlsx")

cl <- makeCluster(detectCores() / 2)

# 均值标准差折线图
plot_qc_line_width <- 800
plot_qc_line_height <- 400
# pca(qc vs sample)
plot_qc_pca_sample_width <- 800
plot_qc_pca_sample_height <- 450
# qc相关系数图
plot_qc_correlation_width <- 600
plot_qc_correlation_height <- 400
# cv 百分比分布图
plot_qc_cvpercent_distribution_width <- 500
plot_qc_cvpercent_distribution_height <- 400
# cv 柱状图
plot_qc_cvpercent_hist_height <- 350
plot_qc_cvpercent_hist_autowidth <- "auto"
# pca鸟瞰图
plot_qc_pca_overview_autowidth <- "auto"
plot_qc_pca_overview_autoheight <- "auto"


theme.scatter = theme(
  plot.title = element_text(size = 12, hjust = 0.5, face = 'bold', family = "Arial"),#title size.
  # axis.title = element_text(size = rel(2)),
  axis.text	 = element_text(colour = 'black'),
  panel.background = element_blank(),
  plot.background = element_blank(),
  legend.key = element_rect(fill = "white", colour = "white"),
  # legend.title = element_text(face = 'bold'),
  text=element_text(family="Arial")
)


drawQcLinePlot <- function(data, showpointlabel = FALSE, pc1, pointsize) {
  plot <- ggplot(data, aes(x = time, y = value)) + 
    geom_line(color = "black", size = 1) +
    geom_point(color = "#4CAF50", size = pointsize)
  if (showpointlabel == TRUE) {
    plot <- plot + geom_text(aes(label = label), color = "#4CAF50", hjust = 0.5, vjust = -0.8)
  }
  plot <- plot + 
    geom_hline(yintercept = (mean(pc1) - 2 * sd(pc1)), color = "orange", size = 1, linetype = 2) +
    geom_text(aes(x = 0, y = mean(pc1) - 2 * sd(pc1), label = "2 std. dev."), hjust = 0.5, vjust = -0.5) +
    geom_hline(yintercept = (mean(pc1) - 3 * sd(pc1)), color = "red", size = 1, linetype = 2) +
    geom_text(aes(x = 0, y = mean(pc1) - 3 * sd(pc1), label = "3 std. dev."), hjust = 0.5, vjust = -0.5) +
    geom_hline(yintercept = (mean(pc1) + 2 * sd(pc1)), color = "orange", size = 1, linetype = 2) +
    geom_text(aes(x = 0, y = mean(pc1) + 2 * sd(pc1), label = "2 std. dev."), hjust = 0.5, vjust = +1.5) +
    geom_hline(yintercept = (mean(pc1) + 3 * sd(pc1)), color = "red", size = 1, linetype = 2) +
    geom_text(aes(x = 0, y = mean(pc1) + 3 * sd(pc1), label = "3 std. dev."), hjust = 0.5, vjust = +1.5) +
    # scale_x_continuous(expand = c(0,0)) +
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    labs(x="Analytical order", y="PC1", title = paste0("Time Series Plot of PC1 in PCA of QC Evaluation")) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}


drawQcCorrelationPlot <- function(data, log10scale) {
  first = 1
  last = length(colnames(data))
  corrtest = corr.test(data[[colnames(data)[first]]], data[[colnames(data)[last]]], method = "spearman")
  correlation = corrtest$r
  plot <- NULL
  if (log10scale ==  TRUE) {
    plot <- ggplot(data, aes(x = log10(.data[[colnames(data)[first]]]), y = log10(.data[[colnames(data)[last]]])))
  } else {
    plot <- ggplot(data, aes(x = .data[[colnames(data)[first]]], y = .data[[colnames(data)[last]]]))
  }
  plot <- plot + 
    geom_point(shape = 1, colour = "orange", size = 3) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(axis.line.x = element_line(linetype = 1, color = "darkblue", size = 1),
          axis.line.y = element_line(linetype = 1, color = "darkblue", size = 1),
          axis.ticks.x = element_line(color = "darkblue", size = 1),
          axis.ticks.y = element_line(color = "darkblue", size = 1),
          axis.ticks.length = unit(.4, "lines")) +
    labs(title = paste0("spearman correlation of the first and last QC: ", round(correlation, 4))) +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}


qualityControlUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useToastr(),
    tags$head(tags$style(".modal-content {-webkit-border-radius: 5px !important;-moz-border-radius: 5px !important;border-radius: 5px !important;}
                          .modal-header {border-top-left-radius: 5px; border-top-right-radius: 5px;}
                          .modal-header {border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;}")),
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    
    fluidRow(
      box(
        # title = "Upload Data",
        title = "上传数据",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        # fluidRow(column(width = 2, offset = 10, helpButton(ns("help_upload")))),
        fluidRow(column(width = 2, offset = 10, actionButton(inputId = ns("help_upload"), 
                                                             label = "帮助文档", 
                                                             class = "help-button"
                                                             )
                        )
                 ),
        fluidRow(
          column(width = 12,
                 fileInput(
                   inputId = ns("dataloader"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " 上传数据... "),
                   # placeholder = "Click the button to select a file, or directly drag and drop the file here.",
                   placeholder = "点击按钮选择文件, 或拖拽文件至此。"
                 )
          )
        ),
        fluidRow(
          column(width = 12, uiOutput(ns("data_summary")))
        ),
      ),
      box(
        id = "box-qc",
        title = "质量控制",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        fluidRow(column(width = 2, offset = 10, helpButton(ns("help_quality_control")))),
        fluidRow(
          sidebarPanel(
            width = 2,
            fluid  = TRUE,
            br(),
            actionButton(ns("qc_setting"), 
                         # label = "Setting", 
                         label = "参数设置", 
                         icon = icon("sliders"),
                         class = "setting-button"),
            actionButton(ns("qc_execute"), 
                         # label = "Execute", 
                         label = "执行", 
                         icon = icon("play"),
                         class = "setting-button"),
          ),
          mainPanel(
            useShinyjs(),
            width = 10,
            tabsetPanel(
              tabPanel(title = "数据",
                       icon = icon("table"),
                       hidden(div(id = ns("qcResultTable"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectInput(inputId = ns("select_qc_method_of_table"),
                                                       label = "",
                                                       choices = list(`Normalization Method` = list("no normalization", 
                                                                                                    "mTIC normalization", 
                                                                                                    "batchwise-loess normalization", 
                                                                                                    "SERRF normalization", 
                                                                                                    "SVM5 normalization", 
                                                                                                    "sum normalization", 
                                                                                                    "median normalization", 
                                                                                                    "PQN normalization", 
                                                                                                    "contrast normalization", 
                                                                                                    "quantile normalization", 
                                                                                                    "linear normalization", 
                                                                                                    # "liwong normalization", 
                                                                                                    "cubic normalization", 
                                                                                                    "batchratio normalization",
                                                                                                    "MetNormalization"),
                                                                      `Normalization Performance` = list("normalization performance", 
                                                                                                         "detailed QC.CV performance", 
                                                                                                         "detailed validate RSD performance")),
                                                       selected = "no")
                                           )
                                  ),
                                  withSpinner(DT::DTOutput(outputId = ns("qc_result_table"))),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_result_table"),
                                                                                class = "download-button",
                                                                                label = "下载数据",
                                                                                icon = icon("download")
                                    )
                                    )
                                  )
                       ))
              ),
              tabPanel(title = "PCA鸟瞰图",
                       icon = tags$i(class = "iconfont icon-grid", role="presentation"),
                       hidden(div(id = ns("qcResultPCAOverview"),
                                  br(),
                                  withSpinner(plotOutput(outputId = ns("qc_result_pca_overview"), 
                                                         width = plot_qc_pca_overview_autowidth, 
                                                         height = plot_qc_pca_overview_autoheight)),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_pca_overview"),
                                                                                class = "download-button",
                                                                                label = "下载图表",
                                                                                icon = icon("download")
                                    )
                                    )
                                  )
                                  )
                              )
              ),
              tabPanel(title = "CV%柱状图",
                       icon = icon("chart-bar"),
                       hidden(div(id = ns("qcResultCVHist"),
                                  br(),
                                  withSpinner(plotOutput(outputId = ns("qc_result_cv_hist"), 
                                                         height = paste0(plot_qc_cvpercent_hist_height, "px"), 
                                                         width = plot_qc_cvpercent_hist_autowidth)),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_cv_hist"),
                                                                                class = "download-button",
                                                                                label = "下载图表",
                                                                                icon = icon("download")
                                    )
                                    )
                                  )
                                  )
                              )
              ),
              tabPanel(title = "CV%分布图",
                       icon = tags$i(class = "iconfont icon-zhuxiantu", role="presentation"),
                       hidden(div(id = ns("qcResultCVDistribution"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectInput(inputId = ns("select_qc_method_of_cvdistplot"),
                                                       label = "质控方法",
                                                       choices = list("no normalization" = "none",
                                                                      "mTIC normalization" = "mTIC", 
                                                                      "batchwise-loess normalization" = "loess", 
                                                                      "SERRF normalization" = "SERRF", 
                                                                      "SVM5 normalization" = "SVM", 
                                                                      "sum normalization" = "sum", 
                                                                      "median normalization" = "median", 
                                                                      "PQN normalization" = "PQN", 
                                                                      "contrast normalization" = "contrast", 
                                                                      "quantile normalization" = "quantile", 
                                                                      "linear normalization" = "linear", 
                                                                      # "liwong normalization" = "liwong", 
                                                                      "cubic normalization" = "cubic", 
                                                                      "batchratio normalization" = "batchratio",
                                                                      "MetNormalization" = "Met")),
                                           style = "padding-top: 10px"
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 9,
                                           withSpinner(plotOutput(outputId = ns("qc_result_cv_distribution"), 
                                                                  width = paste0(plot_qc_cvpercent_distribution_width, "px"), 
                                                                  height = paste0(plot_qc_cvpercent_distribution_height, "px"))
                                                       ),
                                           ),
                                    column(width = 3,
                                           class = "plot-setting-column",
                                           tags$div(
                                             class = "plot-setting",
                                             box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 fluidRow(
                                                   column(width = 12,
                                                          selectInput(inputId = ns("select_labelformat_of_cvdistplot"), 
                                                                      label = "Label 格式", 
                                                                      choices = c("show label only less than 30%", "show all"), 
                                                                      selected = "show label only less than 30%")
                                                          )
                                                 ),
                                                 )
                                           ),
                                           tags$div(
                                             class = "plot-setting",
                                             box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 fluidRow(
                                                   column(width = 12,
                                                          selectInput(inputId = ns("select_palette_of_cvdistplot"), 
                                                                      label = "调色板", 
                                                                      choices = c("palette-1", "palette-2", "palette-3"), 
                                                                      selected = "palette-1")
                                                   )
                                                 ),
                                             )
                                           )
                                           )
                                    
                                  ),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_cv_distribution"),
                                                                                class = "download-button",
                                                                                label = "下载图表",
                                                                                icon = icon("download")
                                    )
                                    )
                                  )
                                  )
                              )
              ),
              tabPanel(title = "PCA图",
                       icon = tags$i(class = "iconfont icon-a-zhuchengfenfenxiPCA", role="presentation"),
                       hidden(div(id = ns("qcResultPCAQcVsSample"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectInput(inputId = ns("select_qc_method_of_pcaplot"),
                                                       label = "质控方法",
                                                       choices = list("no normalization" = "none",
                                                                      "mTIC normalization" = "mTIC", 
                                                                      "batchwise-loess normalization" = "loess", 
                                                                      "SERRF normalization" = "SERRF", 
                                                                      "SVM5 normalization" = "SVM", 
                                                                      "sum normalization" = "sum", 
                                                                      "median normalization" = "median", 
                                                                      "PQN normalization" = "PQN", 
                                                                      "contrast normalization" = "contrast", 
                                                                      "quantile normalization" = "quantile", 
                                                                      "linear normalization" = "linear", 
                                                                      "liwong normalization" = "liwong", 
                                                                      "cubic normalization" = "cubic", 
                                                                      "batchratio normalization" = "batchratio",
                                                                      "MetNormalization" = "Met")),
                                           style = "padding-top:10px"
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 9,
                                           withSpinner(plotOutput(outputId = ns("qc_result_pca_qcvssample"), 
                                                                  width = paste0(plot_qc_pca_sample_width, "px"), 
                                                                  height = paste0(plot_qc_pca_sample_height, "px"))
                                                       )
                                           ),
                                    column(width = 3,
                                           class = "plot-setting-column",
                                           tags$div(
                                             class = "plot-setting",
                                             box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 fluidRow(
                                                   column(width = 8,
                                                          selectInput(inputId = ns("select_confidence_of_pcaplot"),
                                                                      label = "置信区间",
                                                                      choices = list(0.99, 0.95, 0.90), 
                                                                      selected = 0.99),
                                                          ),
                                                 ),
                                                 )
                                           ),
                                           tags$div(
                                             class = "plot-setting",
                                             box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 fluidRow(
                                                   column(width = 8,
                                                          selectInput(inputId = ns("select_palette_of_pcaplot"),
                                                                      label = "调色板", 
                                                                      choices = c('green-red', 'yellow-brown'))
                                                   ),
                                                 ),
                                             )
                                           ),
                                           )
                                    
                                  ),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_result_pca_qcvssample"),
                                                                                class = "download-button",
                                                                                label = "下载图表",
                                                                                icon = icon("download"))
                                           )
                                  )
                                  
                       ))
              ),
              tabPanel(title = "QC折线图",
                       icon = tags$i(class = "iconfont icon-zhexiantu-xianxing", role="presentation"),
                       hidden(div(id = ns("qcResultQCLinePlot"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectInput(inputId = ns("select_qc_method_of_qclineplot"),
                                                       label = "质控方法",
                                                       choices = list("no normalization" = "none",
                                                                      "mTIC normalization" = "mTIC", 
                                                                      "batchwise-loess normalization" = "loess", 
                                                                      "SERRF normalization" = "SERRF", 
                                                                      "SVM5 normalization" = "SVM", 
                                                                      "sum normalization" = "sum", 
                                                                      "median normalization" = "median", 
                                                                      "PQN normalization" = "PQN", 
                                                                      "contrast normalization" = "contrast", 
                                                                      "quantile normalization" = "quantile", 
                                                                      "linear normalization" = "linear", 
                                                                      # "liwong normalization" = "liwong", 
                                                                      "cubic normalization" = "cubic", 
                                                                      "batchratio normalization" = "batchratio",
                                                                      "MetNormalization" = "Met")),
                                           style = "padding-top: 10px"
                                    ),
                                  ),
                                  fluidRow(
                                    column(width = 9,
                                           withSpinner(plotOutput(outputId = ns("qc_result_qc_lineplot"), 
                                                                  width = paste0(plot_qc_line_width, "px"), 
                                                                  height = paste0(plot_qc_line_height, "px"))
                                                       )
                                           ),
                                    column(width = 3,
                                           class = "plot-setting-column",
                                           tags$div(
                                             class = "plot-setting",
                                             box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 fluidRow(
                                                   column(
                                                     width = 8,
                                                     numericInput(inputId = ns("pointsize_of_qclineplot"), 
                                                                  label = "point size", value = 3, 
                                                                  min = 2, max = 10),
                                                   ),
                                                   column(
                                                     width = 8,
                                                     checkboxInput(inputId = ns("show_pointlabel_of_qclineplot"), 
                                                                   label = "show point label", 
                                                                   value = FALSE)
                                                   )
                                                 )
                                                 )
                                           )
                                           )
                                  ),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_result_qc_lineplot"),
                                                                                class = "download-button",
                                                                                label = "下载图表",
                                                                                icon = icon("download")
                                                                                )
                                           )
                                  )
                                  
                                  
                       ))
              ),
              tabPanel(title = "QC相关系数图",
                       icon = tags$i(class = "iconfont icon-xiangguanxishujuzhen", role="presentation"),
                       hidden(div(id = ns("qcResultCorrelationPlot"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectInput(inputId = ns("select_qc_method_of_correlationplot"),
                                                       label = "",
                                                       choices = list("no normalization" = "none",
                                                                      "mTIC normalization" = "mTIC", 
                                                                      "batchwise-loess normalization" = "loess", 
                                                                      "SERRF normalization" = "SERRF", 
                                                                      "SVM5 normalization" = "SVM", 
                                                                      "sum normalization" = "sum", 
                                                                      "median normalization" = "median", 
                                                                      "PQN normalization" = "PQN", 
                                                                      "contrast normalization" = "contrast", 
                                                                      "quantile normalization" = "quantile", 
                                                                      "linear normalization" = "linear", 
                                                                      # "liwong normalization" = "liwong", 
                                                                      "cubic normalization" = "cubic", 
                                                                      "batchratio normalization" = "batchratio",
                                                                      "MetNormalization" = "Met"))
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 9,
                                           withSpinner(plotOutput(outputId = ns("qc_result_correlationplot"), 
                                                                  width = paste0(plot_qc_correlation_width, "px"), 
                                                                  height = paste0(plot_qc_correlation_height, "px"))
                                                       ),
                                           ),
                                    column(width = 3,
                                           class = "plot-setting-column",
                                           tags$div(
                                             class = "plot-setting",
                                             box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                 fluidRow(
                                                   column(width = 8,
                                                          checkboxInput(inputId = ns("log10scale_of_correlationplot"), 
                                                                        label = "log10 scale", 
                                                                        value = TRUE)
                                                          )
                                                 )
                                                 )
                                             )
                                           )
                                   
                                  ),
                                  fluidRow(
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_qc_result_correlationplot"),
                                                                                class = "download-button",
                                                                                label = "下载图表",
                                                                                icon = icon("download")
                                                                                )
                                           )
                                  )
                       ))
              )
            )
          ),
        )
      )
    )
  )
}


qualityControlServer <- function(input, output, session) {
  
  ns <- session$ns
  data <- NULL
  data_qc_no <- NULL
  data_qc_mTIC <- NULL
  data_qc_batchwise_loess <- NULL
  data_qc_serrf <- NULL
  data_qc_svm5 <- NULL
  data_qc_sum <- NULL
  data_qc_median <- NULL
  data_qc_pqn <- NULL
  data_qc_contrast <- NULL
  data_qc_quantile <- NULL
  data_qc_linear <- NULL
  data_qc_liwong <- NULL
  data_qc_cubic <- NULL
  data_qc_batchratio <- NULL
  data_qc_met <- NULL
  data_qc_performance <- NULL
  data_qc_cv_performance_variable <- NULL
  data_qc_validatersd_performance_variable <- NULL
  plot_qc_pca_overview <- NULL
  plot_qc_pca_sample <- NULL
  plot_qc_cvpercent_hist <- NULL
  plot_qc_line <- NULL
  plot_qc_correlation <- NULL
  plot_qc_cvpercent_distribution <- NULL
  selected_qc_method <- c("no")
  
  observeEvent(input$dataloader$datapath, {
    # read data
    data <<- readData(input$dataloader$datapath)
    output$data_summary <- renderUI({
      p("样本数: ", ncol(data$e),
        "变量数: ", nrow(data$e),
      )
    })
    
    # 自动展开
    js$collapse("box-qc")
  })
  
  
  observeEvent(input$qc_setting, {
    showModal(modalDialog(
      title = "质控方法选择",
      size = "l",
      fluidPage(
        checkboxGroupInput(inputId = ns("qc_methods"), label = "质控方法选择:",
                           choices = c("no normalization" = "no", 
                                       "mTIC normalization" = "mTIC", 
                                       "batchwise-loess normalization" = "batchwise-loess", 
                                      "SERRF normalization" = "SERRF", 
                                      "SVM5 normalization" = "SVM5", 
                                      "sum normalization" = "sum", 
                                      "median normalization" = "median", 
                                      "PQN normalization" = "PQN", 
                                      "contrast normalization" = "contrast", 
                                      "quantile normalization" = "quantile", 
                                      "linear normalization" = "linear", 
                                      # "liwong normalization" = "liwong",
                                      "cubic normalization" = "cubic", 
                                      "batchratio normalization" = "batchratio",
                                      "MetNormalizer"),
                           selected = selected_qc_method
        ),
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = ns("qc_setting_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    ))
  })
  
  observeEvent(input$qc_execute, {
    if (!is.null(data)) {
      shinyjs::show("qcResultTable")
      shinyjs::show("qcResultPCAOverview")
      shinyjs::show("qcResultCVHist")
      shinyjs::show("qcResultCVDistribution")
      shinyjs::show("qcResultPCAQcVsSample")
      shinyjs::show("qcResultQCLinePlot")
      shinyjs::show("qcResultCorrelationPlot")
      
      p = data$p
      p$`Acq. Date-Time` = p$time
      p$`Stat Level 1` = p$type
      p$`Stat Level 1`[p$`Stat Level 1`=='validate'] = "NIST"
      f = data$f
      e = as.matrix(data$e)
      
      if(sum(is.na(e)) > 0){
        cat(paste0("NOTE: ",sum(is.na(e)), " missing values detected in the data. They will be replaced by the half-minimum for each compound."))
        missing_compounds = which(is.na(e), arr.ind = T)[,1]
        for(i in missing_compounds){
          e[i, is.na(e[i,])] = 1/2 * min(e[i,!is.na(e[i,])])
        }
      }
      
      e = data.matrix(e)
      batch = p$batch
      batch = matrix(rep(batch,nrow(f)), nrow = nrow(f), byrow = T, ncol = nrow(p))
      # check if data has NIST (validate)
      NISTavailable = sum(p$`Stat Level 1`=="NIST") > 0
      
      cat(paste0("validate samples are not detected."))
      
      # results will be saved in the result_norm.
      result_norm = list()
      
      cat("set up Monte Carlo cross-validation index. 5-fold 8/2 split.\n");
      QC.index.train = QC.index.test = list()
      # 交叉验证倍数
      n_CV = 2
      seed = 8
      set.seed(seed)
      for(j in 1:n_CV) {
        QC.index = which(p$`Stat Level 1` == "QC")
        QC.index.train.temp = sample(QC.index,round(length(QC.index) * (1 - 1/n_CV)))
        QC.index.test.temp = QC.index[!QC.index%in%QC.index.train.temp]
        QC.index.train. = rep(F,ncol(e))
        QC.index.test. = rep(F,ncol(e))
        QC.index.train.[QC.index.train.temp] = T
        QC.index.test.[QC.index.test.temp] = T
        QC.index.train[[j]] = QC.index.train.
        QC.index.test[[j]] = QC.index.test.
      }
      
      cat("\n<========== Normalizations Started! ==========>\n");
      qc_cv_rsd_list <- c()
      performance_qc_cv_df <- data.frame(var = data$original[5:nrow(data$original), 2])
      
      withProgress(message = 'Normalizations Started',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:length(selected_qc_method)) {
                       incProgress(1/length(selected_qc_method), detail = paste0("executing ", selected_qc_method[i], " normalization..."))
                       if (selected_qc_method[i] == "no") {
                         cat("\n<========== No Normalization Started! ==========>\n")
                         print(Sys.time())
                         result_norm[['none']] = (none_norm(e=e,f=f,p=p))
                         none.QC.CV = RSD(result_norm[['none']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         
                         cat(paste0("No normalization QC CV RSD is ", signif(median(none.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(none.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           none.validate = RSD(result_norm[['none']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat( "No normalization validate QC RSD is ", signif(median(none.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(none.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         # dir.create("normalized-data-sets")
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['none']]$e
                         data_qc_no <<- dta
                         cat("\n<========== No Normalization Finished! ==========>\n")
                         print(Sys.time())
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-none-normalization.csv", row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(none = median(none.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(none = none.QC.CV),
                                performance_qc_cv_df$none <- none.QC.CV)
                       } else if (selected_qc_method[i] == "mTIC") {
                         cat("\n<========== mTIC Normalization Started! ==========>\n")
                         result_norm[['mTIC']] = (mTIC_norm(e=e,f=f,p=p))
                         mTIC.QC.CV = RSD(result_norm[['mTIC']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         cat(paste0("mTIC normalization QC CV RSD is ", signif(median(mTIC.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(mTIC.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           mTIC.validate = RSD(result_norm[['mTIC']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("mTIC normalization validate QC RSD is ", signif(median(mTIC.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(mTIC.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['mTIC']]$e
                         data_qc_mTIC <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-mTIC-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(mTIC = median(mTIC.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(mTIC = mTIC.QC.CV),
                                performance_qc_cv_df$mTIC <- mTIC.QC.CV)
                       } else if (selected_qc_method[i] == "batchwise-loess") {
                         cat("\n<========== Batch-wise LOESS Normalization Started! ==========>\n")
                         result_norm[['loess']] = loess_norm(e = e, f=f, p=p,batch,QC.index,"Acq. Date-Time",span.para = 0.75)
                         result_norm_loess_CV = list()
                         loess.QC.CV.  = list()
                         withProgress(message = "Batch-wise LOESS Normalization Started!",
                                      detail = "This may take a while...", value = 0, {
                                        for(j in 1:n_CV) {
                                          incProgress(1/n_CV, detail = paste0("executing cross validation of fold ", j))
                                          time = p$`Acq. Date-Time`
                                          qc. = QC.index.train[[j]]
                                          norms = parSapply(cl, X = 1:nrow(f), function(i,eData,qc,batch,time,remove_outlier,span_para,get_loess_para,loess.span.limit){
                                            models = by(data.frame(v=eData[i,qc],t=time[qc]),
                                                        batch[i,qc],function(x){
                                                          # x = data.frame(v=e[i,qc],t=time[qc])[batch[i,qc]=="B",]
                                                          if(length(remove_outlier(x$v)[[2]])>0){# if outlier exists.
                                                            span = ifelse(span_para=='auto',
                                                                          get_loess_para(x=x$t[-remove_outlier(x$v)[[2]]],y=remove_outlier(x$v)[[1]],
                                                                                         loess.span.limit = loess.span.limit),span_para) # find a proper span.
                                                          }else{
                                                            span = ifelse(span_para=='auto',
                                                                          get_loess_para(x=x$t,y=x$v,
                                                                                         loess.span.limit = loess.span.limit),span_para) # find a proper span.

                                                          }
                                                          if(length(remove_outlier(x$v)[[2]])>0){
                                                            tryCatch(loess(v~t,data=x[-remove_outlier(x$v)[[2]],],span=span),error = function(e){
                                                              NA
                                                            })
                                                          }else{
                                                            tryCatch(loess(v~t,data=x,span=span), error = function(e){
                                                              NA
                                                            })
                                                          }
                                                        })

                                            # predict using the models.
                                            norm = mapply(function(u,v){
                                              o = tryCatch({
                                                predict(u,newdata = v)
                                              },
                                              error = function(e){
                                                print(e)
                                                rep(0,length(v))
                                              })
                                            },models,by(time,batch[i,],function(x){x}))


                                            norm = unlist(norm)
                                            # replace NA with the closest value.
                                            if(length(which(is.na(norm)))>0){
                                              for(j in which(is.na(norm))){
                                                NA_batch = batch[1,][j]
                                                time_notNA = time[batch[1,]%in%NA_batch][-which(is.na(norm[batch[1,]%in%NA_batch]))]
                                                closest_time = time_notNA[which.min(abs(time_notNA-time[j]))]
                                                norm[j] = norm[batch[1,]%in%NA_batch][which(time[batch[1,]%in%NA_batch]==closest_time)[1]]
                                              }
                                            }
                                            return(norm)
                                          },e,qc.,batch,time,remove_outlier,0.75,get_loess_para,0.1)
                                          norms = t(norms)
                                          e_norm = matrix(NA,nrow=nrow(e),ncol=ncol(e))
                                          # if(divide){
                                          for(k in 1:nrow(e)){
                                            e_norm[k,] = e[k,]/(norms[k,]/median(e[k,],na.rm = T))
                                          }
                                          result = e_norm
                                          loess.QC.CV.[[j]] = RSD(result[,QC.index.test[[j]]], f, p[QC.index.test[[j]],], cl=cl)
                                        }
                                      })
                         loess.QC.CV = apply(do.call("cbind",loess.QC.CV.),1,mean, na.rm=T)
                         cat(paste0("loess normalization QC CV RSD is ", signif(median(loess.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(loess.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           loess.validate = RSD(result_norm[['loess']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
                           cat("loess normalization validate QC RSD is ", signif(median(loess.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(loess.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['loess']]$e
                         data_qc_batchwise_loess <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-loess-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(loess = median(loess.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(loess = loess.QC.CV),
                                performance_qc_cv_df$loess <- loess.QC.CV)
                       } else if (selected_qc_method[i] == "SERRF") {
                         cat("\n<========== SERRF Normalization Started! ==========>\n")
                         cat("This may take some time. \n")
                         qc = rep(F, nrow(p))
                         qc[QC.index] = T
                         e. = e
                         result_norm[['SERRF']] = SERRF_norm(e., f, p, batch, QC.index, time = "Acq. Date-Time")
                         for(i in 1:nrow(e)){ # MAKE SURE THE QC AND SAMPLES ARE AT THE SAME LEVEL. This is critical for SERRF algorithm (and other tree-based machine learning algorithm) because when building each tree, the split on each leaf considers the level of the values. If the values are not consistant, then the RF models will be wrong and the RF will bias the intensity level after normalization (although the relative position won't change.)
                           e.[i,qc] = unlist(by(data.frame(e.[i,],qc),batch[1,],function(x){# x = data.frame(e.[i,],qc)[batch[1,]=='A',]
                             x[x[,2],1] - (median(x[x[,2],1]) - median(x[!x[,2],1]))
                           }))
                         }
                         result_norm_SERRF_CV = list()
                         SERRF.QC.CV.  = list()
                         withProgress(message = "SERRF Normalization Started!",
                                      detail = "This may take some time...", value = 0, {
                                        for(i in 1:n_CV) {
                                          incProgress(1/n_CV, detail = paste0("executing cross validation of fold ", i))
                                          time = p$`Acq. Date-Time`
                                          
                                          # 这里似乎是原版代码的bug
                                          # qc. = QC.index.train[[j]]
                                          qc. = QC.index.train[[i]]
                                          
                                          e_SERRF_pred = parSapply(cl, X = 1:nrow(f), function(j,eData,batch,randomForest, qc., time){
                                            data = data.frame(y = eData[j,], t(eData[-j,]), batch = batch[1,], time = time)
                                            colnames(data) = c("y", paste0("X",1:nrow(eData))[-j], "batch", "time")
                                            model = randomForest(y~., data = data,subset = qc., importance = F, ntree = 500)
                                            newdata = data.frame(t(eData[-j,]), batch = batch[1,], time = time)
                                            colnames(newdata) =   c(paste0("X",1:nrow(eData))[-j], "batch", "time")
                                            new = (eData[j,]/predict(model,newdata = newdata)) * median(eData[j,])
                                            return(new)
                                          }, e.,batch,randomForest, qc., p$`Acq. Date-Time`)
                                          e_SERRF_pred = t(e_SERRF_pred)

                                          dta = e_SERRF_pred[,QC.index.test[[i]]]

                                          # dta = mTIC_norm(e=dta,f=f,p=p[QC.index.test[[i]],])$e

                                          SERRF.QC.CV.[[i]] = RSD(dta, f, p[QC.index.test[[i]],], cl=cl)
                                        }
                                      })

                         SERRF.QC.CV = apply(do.call("cbind",SERRF.QC.CV.),1,mean, na.rm=T)
                         cat(paste0("SERRF normalization QC CV RSD is ", signif(median(SERRF.QC.CV, na.rm = T), 4)*100,"%. ",signif(sum(SERRF.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           SERRF.validate = RSD(result_norm[['SERRF']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
                           cat("SERRF normalization validate QC RSD is ", signif(median(SERRF.validate, na.rm = T), 4)*100,"%. ",signif(sum(SERRF.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n")
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['SERRF']]$e
                         data_qc_serrf <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-SERRF-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(SERRF = median(SERRF.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(SERRF = SERRF.QC.CV),
                                performance_qc_cv_df$SERRF <- SERRF.QC.CV)
                       } else if (selected_qc_method[i] == "SVM5") {
                         # SVM5
                         cat("\n<========== SVM Normalization Started! ==========>\n")
                         withProgress(message = "SVM Normalization Started!",
                                      detail = "This may take a while...", value = 0, {
                                        multiple = 5
                                        result_norm[['SVM']] = SVM_norm(e, f, p, QC.index, multiple = 5, time = "Acq. Date-Time")
                                        result_norm_SVM_CV = list()
                                        SVM.QC.CV. = SVM.validate. = list()
                                        for(j in 1:n_CV){
                                          incProgress(1/n_CV, detail = paste0("executing cross validation of fold ", j))
                                          e_SVM_pred = e
                                          qc. = QC.index.train[[j]]
                                          for(i in 1:nrow(f)){
                                            all.cor <- apply(e[,qc.], 1,function(x) {cor(e[1,qc.], x)})
                                            cor.peak <-match(sort(all.cor, decreasing = TRUE)[2:(as.numeric(multiple)+1)], all.cor)
                                            if (multiple != 1) {
                                              svr.reg <- svm(t(e[cor.peak,qc.]),e[i,qc.])
                                              newdata = t(e[cor.peak, ])
                                              pred = predict(svr.reg, newdata = newdata)
                                              e_SVM_pred[i,] = (e[i,]/pred)*median(e[i,qc.])
                                            } else{
                                              svr.reg <- svm(e[i,qc.] ~ p$`Acq. Date-Time`[qc.])
                                              pred = predict(svr.reg, newdata = p$`Acq. Date-Time`)
                                              e_SVM_pred[i,] = (e[i,]/pred)*median(e[i,qc.])
                                            }
                                          }
                                          result = e_SVM_pred
                                          SVM.QC.CV.[[j]] = RSD(result[,QC.index.test[[j]]], f, p[QC.index.test[[j]],], cl=cl)
                                        }
                                        SVM.QC.CV = apply(do.call("cbind",SVM.QC.CV.),1,mean, na.rm=T)
                                        cat(paste0("SVM normalization QC CV RSD is ", signif(median(SVM.QC.CV, na.rm = T), 4)*100,"%. ",signif(sum(SVM.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                                        if(NISTavailable) {
                                          SVM.validate = RSD(result_norm[['SVM']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
                                          cat("SVM normalization validate QC RSD is ", signif(median(SVM.validate, na.rm = T), 4)*100,"%. ",signif(sum(SVM.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n")
                                        }
                                        dta = data$original
                                        dta[5:nrow(dta),3:ncol(dta)] = result_norm[['SVM']]$e
                                        data_qc_svm5 <<- dta
                                        # write.csv(dta, file="normalized-data-sets\\normalization-result-svm-normalization.csv",row.names=FALSE)
                                      })
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(SVM = median(SVM.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(SVM5 = SVM.QC.CV),
                                performance_qc_cv_df$SVM5 <- SVM.QC.CV)
                       } else if (selected_qc_method[i] == "sum") {
                         cat("\n<========== sum Normalization Started! ==========>\n")
                         result_norm[['sum']] = (sum_norm(e=e,f=f,p=p))
                         sum.QC.CV = RSD(result_norm[['sum']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         cat(paste0("sum normalization QC CV RSD is ", signif(median(sum.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(sum.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           sum.validate = RSD(result_norm[['sum']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("sum normalization validate QC RSD is ", signif(median(sum.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(sum.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['sum']]$e
                         data_qc_sum <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-sum-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(sum = median(sum.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(sum = sum.QC.CV),
                                performance_qc_cv_df$sum <- sum.QC.CV)
                       } else if (selected_qc_method[i] == "median") {
                         # median normalization.
                         cat("\n<========== median Normalization Started! ==========>\n")
                         result_norm[['median']] = (median_norm(e=e,f=f,p=p))
                         
                         median.QC.CV = RSD(result_norm[['median']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         
                         cat(paste0("median normalization QC CV RSD is ", signif(median(median.QC.CV, na.rm = T), 4)*100,"%. ", signif(median(median.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           median.validate = RSD(result_norm[['median']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("median normalization validate QC RSD is ", signif(median(median.validate, na.rm = T), 4)*100,"%. ", paste0(signif(median(median.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['median']]$e
                         data_qc_median <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-median-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(median = median(median.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(median = median.QC.CV),
                                performance_qc_cv_df$median <- median.QC.CV)
                       } else if (selected_qc_method[i] == "PQN") {
                         # PQN normalization.
                         cat("\n<========== PQN Normalization Started! ==========>\n")
                         result_norm[['PQN']] = (PQN_norm(e=e,f=f,p=p))
                         PQN.QC.CV = RSD(result_norm[['PQN']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         cat(paste0("PQN normalization QC CV RSD is ", signif(median(PQN.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(PQN.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           PQN.validate = RSD(result_norm[['PQN']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("PQN normalization validate QC RSD is ", signif(median(PQN.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(PQN.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['PQN']]$e
                         data_qc_pqn <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-PQN-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(PQN = median(PQN.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(PQN = PQN.QC.CV),
                                performance_qc_cv_df$PQN <- PQN.QC.CV)
                       } else if (selected_qc_method[i] == "contrast") {
                         cat("\n<========== contrast Normalization Started! ==========>\n")
                         result_norm[['contrast']] = (contrast_norm(e=e,f=f,p=p))
                         contrast.QC.CV = RSD(result_norm[['contrast']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         cat(paste0("contrast normalization QC CV RSD is ", signif(median(contrast.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(contrast.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           contrast.validate = RSD(result_norm[['contrast']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("contrast normalization validate QC RSD is ", signif(median(contrast.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(contrast.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['contrast']]$e
                         data_qc_contrast <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-contrast-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(contrast = median(contrast.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(contrast = contrast.QC.CV),
                                performance_qc_cv_df$contrast <- contrast.QC.CV)
                       } else if (selected_qc_method[i] == "quantile") {
                         cat("\n<========== quantile Normalization Started! ==========>\n")
                         result_norm[['quantile']] = (quantile_norm(e=e,f=f,p=p))
                         quantile.QC.CV = RSD(result_norm[['quantile']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         cat(paste0("quantile normalization QC CV RSD is ", signif(median(quantile.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(quantile.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           quantile.validate = RSD(result_norm[['quantile']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("quantile normalization validate QC RSD is ", signif(median(quantile.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(quantile.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['quantile']]$e
                         data_qc_quantile <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-quantile-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(quantile = median(quantile.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(quantile = quantile.QC.CV),
                                performance_qc_cv_df$quantile <- quantile.QC.CV)
                       } else if (selected_qc_method[i] == "linear") {
                         cat("\n<========== linear Normalization Started! ==========>\n")
                         result_norm[['linear']] = (linear_norm(e=e,f=f,p=p))
                         linear.QC.CV = RSD(result_norm[['linear']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                         cat(paste0("linear normalization QC CV RSD is ", signif(median(linear.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(linear.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           linear.validate = RSD(result_norm[['linear']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                           cat("linear normalization validate QC RSD is ", signif(median(linear.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(linear.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['linear']]$e
                         data_qc_linear <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-linear-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(linear = median(linear.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(linear = linear.QC.CV),
                                performance_qc_cv_df$linear <- linear.QC.CV)
                       } else if (selected_qc_method[i] == "liwong") {
                         cat("\n<========== Li-Wong Normalization Started! ==========>\n")
                         result_norm[['liwong']] = tryCatch(liwong_norm(e=e,f=f,p=p), error = function(e){
                           return(NA)
                         })
                         if(is.na(result_norm[['liwong']])){
                           liwong.QC.CV = liwong.validate = NA
                         }else{
                           liwong.QC.CV = RSD(result_norm[['liwong']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                           cat(paste0("liwong normalization QC CV RSD is ", signif(median(liwong.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(liwong.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                           if(NISTavailable){
                             liwong.validate = RSD(result_norm[['liwong']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                             cat("liwong normalization validate QC RSD is ", signif(median(liwong.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(liwong.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                           }
                           dta = data$original
                           dta[5:nrow(dta),3:ncol(dta)] = result_norm[['liwong']]$e
                           data_qc_liwong <<- dta
                           # write.csv(dta, file="normalized-data-sets\\normalization-result-liwong-normalization.csv",row.names=FALSE)
                         }
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(liwong = median(liwong.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(liwong = liwong.QC.CV),
                                performance_qc_cv_df$liwong <- liwong.QC.CV)
                       } else if (selected_qc_method[i] == "cubic") {
                         cat("\n<========== Cubic Normalization Started! ==========>\n")
                         result_norm[['cubic']] = tryCatch(cubic_norm(e=e,f=f,p=p), error = function(e){
                           return(NA)
                         })
                         if(is.na(result_norm[['cubic']])){
                           cubic.QC.CV = cubic.validate = NA
                         }else{
                           cubic.QC.CV = RSD(result_norm[['cubic']]$e[,p$`Stat Level 1`=='QC'],f,p[p$`Stat Level 1`=='QC',],cl=cl)
                           cat(paste0("cubic normalization QC CV RSD is ", signif(median(cubic.QC.CV, na.rm = T), 4)*100,"%. ", signif(sum(cubic.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                           if(NISTavailable){
                             cubic.validate = RSD(result_norm[['cubic']]$e[,p$`Stat Level 1`=='NIST'],f,p,cl=cl)
                             cat( "cubic normalization validate QC RSD is ", signif(median(cubic.validate, na.rm = T), 4)*100,"%. ", paste0(signif(sum(cubic.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                           }
                           dta = data$original
                           dta[5:nrow(dta),3:ncol(dta)] = result_norm[['cubic']]$e
                           data_qc_cubic <<- dta
                           # write.csv(dta, file="normalized-data-sets\\normalization-result-cubic-normalization.csv",row.names=FALSE)
                         }
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(cubic = median(cubic.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(cubic = cubic.QC.CV),
                                performance_qc_cv_df$cubic <- cubic.QC.CV)
                       } else if (selected_qc_method[i] == "batchratio") {
                         cat("\n<========== Batch-Ratio Median Normalization Started! ==========>\n")
                         result_norm[['batchratio']] = batchratio_norm(e, f, p,batch,QC.index)
                         result_norm_batchratio_CV = list()
                         batchratio.QC.CV. = batchratio.validate. = list()
                         for(j in 1:n_CV){
                           qc. = QC.index.train[[j]]
                           e_batch_norm = matrix(,nrow=nrow(e),ncol=ncol(e))
                           for(i in 1:nrow(f)){
                             means = by(as.numeric(e[i,qc.]),batch[i,qc.], mean, na.rm=T)
                             mean_means = mean(means)
                             e_batch_norm[i,] = as.numeric(e[i,])/(rep(means,times=table(batch[i,]))/mean_means)
                           }
                           batchratio.QC.CV.[[j]] = RSD(e_batch_norm[,QC.index.test[[j]]],f,p[QC.index.test[[j]],],cl=cl)
                         }
                         batchratio.QC.CV = apply(do.call("cbind",batchratio.QC.CV.),1,mean, na.rm=T)
                         cat(paste0("batchratio normalization QC CV RSD is ", signif(median(batchratio.QC.CV, na.rm = T), 4)*100,"%. "))
                         cat(paste0(signif(sum(batchratio.QC.CV<0.10, na.rm = T)/nrow(f),4)*100,"% of QC CV RSD < 10%.\n"))
                         if(NISTavailable){
                           batchratio.validate = RSD(result_norm[['batchratio']]$e[,p$`Stat Level 1`=="NIST"],f,p[p$`Stat Level 1`=="NIST",],cl=cl)
                           cat(paste0("batchratio normalization validate QC RSD is ", signif(median(batchratio.validate, na.rm = T), 4)*100,"%. "))
                           cat(paste0(signif(sum(batchratio.validate<0.10, na.rm = T)/nrow(f),4)*100,"% of validate QC RSD < 10%.\n"))
                         }
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['batchratio']]$e
                         data_qc_batchratio <<- dta
                         # write.csv(dta, file="normalized-data-sets\\normalization-result-batchratio-normalization.csv",row.names=FALSE)
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(batchratio = median(batchratio.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(batchratio = batchratio.QC.CV),
                                performance_qc_cv_df$batchratio <- batchratio.QC.CV)
                       } else if (selected_qc_method[i] == "MetNormalizer") {
                         cat("\n<========== MetNormalization Started! ==========>\n")
                         sample.info = p[c("label", "time", "type")]
                         colnames(sample.info) = c("sample.name", "injection.order",	"class")
                         
                         metdf = as.data.frame(e, row.names = FALSE)
                         metdf["name"] = rownames(e)
                         metdf["mz"] = 102
                         metdf["rt"] = 3.41
                         rownames(metdf) = rownames(e)
                         
                         metNorRes = metNor(
                           data = metdf,
                           sample.info = sample.info,
                           minfrac.qc = 0,
                           minfrac.sample = 0,
                           optimization = TRUE,
                           multiple = 5,
                           threads = 3
                         )
                         
                         if (typeof(metNorRes) == "character") {
                           metNorRes = as.data.frame(metNorRes)
                           for (col in 1:ncol(metNorRes)) {
                             metNorRes[[col]] = as.double(metNorRes[[col]])
                           }
                         }
                         
                         result_norm[['Met']] = list(e = subset(metNorRes, select = c(colnames(e))), f = f, p = p)
                         metNorResDf = as.data.frame(metNorRes)
                         Met.QC.CV = metNorResDf[["QC.nor.rsd"]]
                         dta = data$original
                         dta[5:nrow(dta),3:ncol(dta)] = result_norm[['Met']]$e
                         
                         data_qc_met <<- dta
                         qc_cv_rsd_list <- append(qc_cv_rsd_list, c(Met = median(Met.QC.CV, na.rm = T)))
                         ifelse(nrow(performance_qc_cv_df) == 0,
                                performance_qc_cv_df <- data.frame(Met = Met.QC.CV),
                                performance_qc_cv_df$Met <- Met.QC.CV)
                       }
                      }
                   })
      
      withProgress(message = "Aggregating calculation results", detail = "This may take a while...", 
                   value = 0, {
                     incProgress(1/3, detail = "Calculating performance CV QC RSD and validate RSD...", )
                     # save performance CV QC RSD and validate RSD
                     performance <<- data.frame(method = names(result_norm),
                                                QC.CV.RSD = qc_cv_rsd_list[names(result_norm)],
                                                validate.QC.RSD = sapply(names(result_norm), function(x) {
                                                  # for(x in 1:length(result_norm)){
                                                  if (sum(is.na(result_norm[[x]])) > 0) {
                                                    return(NA)
                                                  } else {
                                                    if(NISTavailable) {
                                                      return(median(RSD(result_norm[[x]]$e[,p$`Stat Level 1`=='NIST'], f, p[p$`Stat Level 1`=='NIST',], cl=cl), na.rm = T))
                                                    } else {
                                                      return(NA)
                                                    }
                                                    
                                                  }
                                                  # }
                                                })
                     )
                     if(!NISTavailable) {
                       performance = performance[,1:2]
                     }
                     data_qc_performance <<- performance
                     # write.csv(performance, "normalization-performance.csv")
                     
                     incProgress(1/3, detail = "Calculating detailed performance on each compound...")
                     data_qc_cv_performance_variable <<- performance_qc_cv_df
                     # detailed performance on each compound.
                     # rownames(performanceQC.CV) = f$label
                     # write.csv(performanceQC.CV, "normalization-performance-QCCVRSD-eachCompound.csv")
                     
                     incProgress(1/3, detail = "Calculating detailed validate RSD performance on each compound...")
                     if(NISTavailable) {
                       performancevalidateCV = sapply(names(result_norm),function(x) {
                         if(is.na(result_norm[[x]])){
                           return(NA)
                         }else{
                           return(RSD(result_norm[[x]]$e[,p$`Stat Level 1`=='NIST'],f,p[p$`Stat Level 1`=='NIST',],cl=cl))
                         }
                       })
                       
                       # write.csv(performancevalidateCV, "normalization-performance-validateRSD-eachCompound.csv")
                       data_qc_validatersd_performance_variable <<- cbind(var = data$original[5:nrow(data$original), 2], performancevalidateCV)
                     }
                     
                   })
      
      # pca overview  plot
      output$qc_result_pca_overview <- renderPlot({
        plots <- list()
        for(method in names(result_norm)){
          if(sum(is.na(result_norm[[method]]))>0){
            
          }else{
            index1 = apply(result_norm[[method]]$e, 1, function(x){
              !sd(x, na.rm = T) == 0
            })# remove zero standard deviation
            index2 = !is.na(index1)
            index = index1 & index2
            dta = result_norm[[method]]$e[index,]
            for(i in 1:nrow(dta)){
              dta[i,is.na(dta[i,])] = median(as.double(dta[i,!is.na(dta[i,])]))
            }
            plot <- generate_PCA(dta, f[index,], p, batch = batch[index,], QC.index =  QC.index, method)
            plots <- append(plots, list(method = plot))
          }
        }
        plot_qc_pca_overview <<- plot_grid(plotlist = plots, nrow = ceiling(length(names(result_norm)) / 4))
        return(plot_qc_pca_overview)
      }, height = function() {
        len = length(names(result_norm))
        if (1 <= len && len <= 4) {
          plot_qc_pca_overview_autoheight <<- 300
        } else if (len >= 5 && len <= 8) {
          plot_qc_pca_overview_autoheight <<- 600
        } else if (len >= 9 && len <= 12) {
          plot_qc_pca_overview_autoheight <<- 900
        } else {
          plot_qc_pca_overview_autoheight <<- 1200
        }
        return(plot_qc_pca_overview_autoheight)
      }, width = function() {
        len = length(names(result_norm))
        if (len == 1) {
          plot_qc_pca_overview_autowidth <<- 300
        } else if (len == 2) {
          plot_qc_pca_overview_autowidth <<- 600
        } else if (len == 3) {
          plot_qc_pca_overview_autowidth <<- 900
        } else {
          plot_qc_pca_overview_autowidth <<- 1200
        }
        return(plot_qc_pca_overview_autowidth)
      })
      
      # %cv hist
      output$qc_result_cv_hist <- renderPlot({
        cat("The QC CV RSD and validate QC RSD can be summarized in the bar plots.\n")
        QC.CVRSD = sort(c(qc_cv_rsd_list[names(result_norm)]), decreasing = TRUE)
        cat("\nCONCLUSION: The ", names(QC.CVRSD)[length(QC.CVRSD)], " outperformed others with a average CV QC RSD of ", signif(QC.CVRSD[length(QC.CVRSD)])*100, "%.\n")
        QC.CVRSD.ggplot2Data = data.frame(RSD = signif(QC.CVRSD,3), methods = factor(names(QC.CVRSD), levels = names(QC.CVRSD)))
        
        plot_qc_cvpercent_hist <<- ggplot(QC.CVRSD.ggplot2Data, aes(x = methods, y = RSD, label = RSD))
        if (length(QC.CVRSD) <= 2) {
          plot_qc_cvpercent_hist <<- plot_qc_cvpercent_hist + geom_bar(stat = "identity", fill = c(rep("black", length(QC.CVRSD))), width=0.5)
        } else {
          plot_qc_cvpercent_hist <<- plot_qc_cvpercent_hist + geom_bar(stat = "identity", fill = c(rep("black", length(QC.CVRSD) - 2), 'red', 'gold'), width=0.5)
        }
        plot_qc_cvpercent_hist <<- plot_qc_cvpercent_hist + 
          theme_bw() +
          geom_text(aes(label = RSD), vjust = -0.2, colour = "black") +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()) +
          scale_y_continuous(expand = expansion(mult = c(0, .1))) +
          labs(x = "")
        return(plot_qc_cvpercent_hist)
      }, width = function() {
          plot_qc_cvpercent_hist_autowidth <<- 60 * length(qc_cv_rsd_list) + 60
          return(plot_qc_cvpercent_hist_autowidth)
        }
      )
      
      # pca(QC v.s. sample)
      output$qc_result_pca_qcvssample <- renderPlot({
        method = input$select_qc_method_of_pcaplot
        confidence = input$select_confidence_of_pcaplot
        palettename = input$select_palette_of_pcaplot
        if (method %in% names(result_norm)) {
          index1 = apply(result_norm[[method]]$e, 1, function(x){
            !sd(x, na.rm = T) == 0
          })# remove zero standard deviation
          index2 = !is.na(index1)
          
          index = index1 & index2
          
          dta = result_norm[[method]]$e[index,]
          
          for(i in 1:nrow(dta)){
            dta[i,is.na(dta[i,])] = median(as.double(dta[i,!is.na(dta[i,])]))
          }
          
          batch = batch[index,]
          
          pca = prcomp(t(dta), center = T, scale. = T)
          variance = pca$sdev^2/sum(pca$sdev^2)
          pca.data = data.frame(pca$x, class = p$type)
          
          # 准备3种配色, 分别对应sample/QC/validate
          palette = c("mediumseagreen", "firebrick", "royalblue")
          if (palettename == "yellow-brown") {
            palette = c("#F6C241", "#953D38", "royalblue")
          }
          
          plot_qc_pca_sample <<- ggplot(pca.data, aes(x = PC1, y = PC2, color = class)) +
            geom_point() +
            geom_hline(yintercept = 0) + 
            geom_vline(xintercept = 0) +
            stat_ellipse(aes(x = PC1, y = PC2), linetype = 2, size = 0.5, level = as.numeric(confidence), inherit.aes = FALSE) + theme_bw() +
            scale_color_manual(values=palette) +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank()) +
            labs(x = paste0("PC1: ", signif(variance[1]*100, 3), "%"), y = paste0("PC2: ", signif(variance[2]*100, 3), "%"), title = paste0("PCA of QC Evaluation")) +
            theme(plot.title = element_text(hjust = 0.5))
          
          return(plot_qc_pca_sample)
        }
      })
      
      # qc折线图
      output$qc_result_qc_lineplot <- renderPlot({
        method = input$select_qc_method_of_qclineplot
        if (method %in% names(result_norm)) {
          # prepare qc line data
          index1 = apply(result_norm[[method]]$e, 1, function(x){
            !sd(x, na.rm = T) == 0
          })# remove zero standard deviation
          index2 = !is.na(index1)
          
          index = index1 & index2
          
          dta = result_norm[[method]]$e[index,]
          
          for(i in 1:nrow(dta)){
            dta[i, is.na(dta[i,])] = median(as.double(dta[i, !is.na(dta[i,])]))
          }
          
          # scaling
          t_dta = t(dta)
          for (i in 1:ncol(t_dta)) {
            var = t_dta[, i]
            var_scaling = (var - mean(var)) / sd(var)
            t_dta[, i] = var_scaling
          }
          
          pca = prcomp(t_dta, center = T, scale. = T)
          pca.data = data.frame(pca$x, batch = batch[1,], order = 1:nrow(pca$x))
          
          # QC样本原始数据
          qcdf <- dta[, QC.index]
          qc_label <- t(p)["label", QC.index]
          qc_time <- t(p)["time", QC.index]
          
          # 各qc样本的均值(标准化处理后的变量均值)
          qcline_df <- data.frame(label = qc_label, time = as.numeric(qc_time), value = pca.data$PC1[QC.index])
          
          plot_qc_line <<- drawQcLinePlot(data = qcline_df, showpointlabel = input$show_pointlabel_of_qclineplot, 
                                          pc1 = pca.data$PC1, 
                                          pointsize = input$pointsize_of_qclineplot)
          return(plot_qc_line)
        }
      })
      
      # qc相关系数图
      output$qc_result_correlationplot <- renderPlot({
        method = input$select_qc_method_of_correlationplot
        if (method %in% names(result_norm)) {
          # prepare data
          corrdata <- as.data.frame(result_norm[[method]]$e[, QC.index])
          plot_qc_correlation <<- drawQcCorrelationPlot(corrdata, input$log10scale_of_correlationplot)
          return(plot_qc_correlation)
        }
      })
      
      # cv 百分比分布图
      output$qc_result_cv_distribution <- renderPlot({
        method = input$select_qc_method_of_cvdistplot
        if (method %in% names(result_norm)) {
          # prepare data
          qcdata <- as.data.frame(t(result_norm[[method]]$e[, QC.index]))
          # cv distribution
          rsd = c()
          for (var in colnames(qcdata)) {
            rsd = append(rsd, sd(qcdata[[var]] / mean(qcdata[[var]])))
          }
          count_lessthan10 = 0
          count_10_15 = 0
          count_15_20 = 0
          count_20_30 = 0
          count_morethan30 = 0
          for (i in 1:length(rsd)) {
            if (rsd[i] < 0.10) {
              count_lessthan10 = count_lessthan10 + 1
            } else if (rsd[i] >= 0.10 && rsd[i] < 0.15) {
              count_10_15 = count_10_15 + 1
            } else if (rsd[i] >= 0.15 && rsd[i] < 0.20) {
              count_15_20 = count_15_20 + 1
            } else if (rsd[i] >= 0.20 && rsd[i] < 0.30) {
              count_20_30 = count_20_30 + 1
            } else {
              count_morethan30 = count_morethan30 + 1
            }
          }
          distribution <- data.frame(group = c("<10", "10-15", "15-20", "20-30", ">30"),
                                     count = c(count_lessthan10, count_10_15, count_15_20, count_20_30, count_morethan30),
                                     percentage = c(count_lessthan10 / length(rsd),
                                                    count_10_15 / length(rsd),
                                                    count_15_20 / length(rsd),
                                                    count_20_30 / length(rsd),
                                                    count_morethan30 / length(rsd)) * 100,
                                     percentage_accumulate = c(count_lessthan10 / length(rsd), 
                                                               (count_lessthan10 + count_10_15) / length(rsd), 
                                                               (count_lessthan10 + count_10_15 + count_15_20) / length(rsd),
                                                               (count_lessthan10 + count_10_15 + count_15_20 + count_20_30) / length(rsd),
                                                               (count_lessthan10 + count_10_15 + count_15_20 + count_20_30 + count_morethan30) / length(rsd)) * 100)
          distribution[["percentage_accumulate_transform"]] = distribution[["percentage_accumulate"]] / 100 * max(distribution[["percentage"]])
          distribution[["group"]] <- factor(distribution[["group"]], levels = as.character(distribution[["group"]]))
          
          value_label = c("", "", "", "", "")
          if (input$select_labelformat_of_cvdistplot == "show label only less than 30%") {
            value_label = c("", "", "", paste0(round(distribution[["percentage_accumulate"]][4], 2), "%"), "")
          } else if (input$select_labelformat_of_cvdistplot == "show all") {
            value_label = paste0(round(distribution[["percentage_accumulate"]], 2), "%")
          }
          
          bar_color <- ''
          line_color <- ''
          if (input$select_palette_of_cvdistplot == "palette-1") {
            bar_color = '#168aad'
            line_color = '#800080'
          } else if (input$select_palette_of_cvdistplot == "palette-2") {
            bar_color = 'blue'
            line_color = 'red'
          } else if (input$select_palette_of_cvdistplot == "palette-3") {
            bar_color = '#FC8D62'
            line_color = '#66C2A5'
          }
          
          plot_qc_cvpercent_distribution <<- ggplot(distribution, aes(x = group, y = percentage)) + 
            geom_bar(stat = "identity", fill = bar_color) +
            theme_minimal() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank()) +
            theme(axis.line.x = element_line(linetype = 1, color = 'darkblue', size = 1),
                  axis.line.y = element_line(linetype = 1, color = 'darkblue', size = 1),
                  axis.ticks.x = element_line(color = 'darkblue', size = 1),
                  axis.ticks.y = element_line(color = 'darkblue', size = 1),
                  axis.ticks.length = unit(.4, "lines")) +
            geom_line(aes(x = group, y = percentage_accumulate_transform, group = 1), size = 1, color = line_color) +
            geom_point(aes(x = group, y = percentage_accumulate_transform, group = 1), size = 3, shape = 19, color = line_color) +
            geom_text(aes(x = group, y = percentage_accumulate_transform, label = value_label), color = "red", hjust = 0.5, vjust = -0.9) +
            scale_y_continuous(limits = c(0, max(distribution[["percentage"]])), 
                               breaks = c(seq(0, ceiling(max(distribution[["percentage"]]) / 10) * 10, 5)), 
                               sec.axis = sec_axis(~./0.99, name = "Accumulated Percentage of metabolites(%)", 
                                                   breaks = seq(0, max(distribution[["percentage"]]), max(distribution[["percentage"]]) / 10), 
                                                   labels = paste0(seq(0, 100, 10)))) +
            labs(title = paste0("CV% distribution"), x = "RSD distribution(%)", y = "Percentage of metabolites(%)") +
            theme(plot.title = element_text(hjust = 0.5))
          return(plot_qc_cvpercent_distribution)
        }
      })

    }
  })
  
  observeEvent(input$qc_setting_ok, {
    selected_qc_method <<- input$qc_methods
    removeModal()
  })
  
  observeEvent(input$select_qc_method_of_table, {
    if (input$select_qc_method_of_table == "no normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_no
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "mTIC normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_mTIC
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "batchwise-loess normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_batchwise_loess
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "SERRF normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_serrf
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "SVM5 normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_svm5
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "sum normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_sum
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "median normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_median
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "PQN normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_pqn
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "contrast normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_contrast
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "quantile normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_quantile
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "linear normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_linear
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "liwong normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_liwong
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "cubic normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_cubic
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "batchratio normalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_batchratio
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "MetNormalization") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_met
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "normalization performance") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_performance
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "detailed QC.CV performance") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_cv_performance_variable
        },
        options = dataTableOptions,
        selection = 'none',
        style = 'bootstrap4',
        class = 'cell-border stripe compact datatable',
        rownames = FALSE
        )
      })
    } else if (input$select_qc_method_of_table == "detailed validate RSD performance") {
      output$qc_result_table <- DT::renderDT({
        DT::datatable({
          data_qc_validatersd_performance_variable
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
  
  observeEvent(input$export_qc_result_table, {
    showModal(modalDialog(
      title = "下载数据",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_qc_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        # selectInput(inputId = ns("export_qc_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
        selectInput(inputId = ns("export_qc_result_table_format"), label = "Choose format", choices = c(".csv"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_qc_result_table_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    ))
  })
  
  output$export_qc_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_result_table_name) || input$export_qc_result_table_name == "") {
        paste("normalization-result-", gsub(" ", "-", input$select_qc_method_of_table), input$export_qc_result_table_format, sep="")
      } else {
        paste(input$export_qc_result_table_name, input$export_qc_result_table_format, sep = "")
      }
    },
    content = function(file) {
      qc_result_table <- NULL
      if (input$select_qc_method_of_table == "no normalization") {
        qc_result_table <- data_qc_no
      } else if (input$select_qc_method_of_table == "mTIC normalization") {
        qc_result_table <- data_qc_mTIC
      } else if (input$select_qc_method_of_table == "batchwise-loess normalization") {
        qc_result_table <- data_qc_batchwise_loess
      } else if (input$select_qc_method_of_table == "SERRF normalization") {
        qc_result_table <- data_qc_serrf
      } else if (input$select_qc_method_of_table == "SVM5 normalization") {
        qc_result_table <- data_qc_svm5
      } else if (input$select_qc_method_of_table == "sum normalization") {
        qc_result_table <- data_qc_sum
      } else if (input$select_qc_method_of_table == "median normalization") {
        qc_result_table <- data_qc_median
      } else if (input$select_qc_method_of_table == "PQN normalization") {
        qc_result_table <- data_qc_pqn
      } else if (input$select_qc_method_of_table == "contrast normalization") {
        qc_result_table <- data_qc_contrast
      } else if (input$select_qc_method_of_table == "quantile normalization") {
        qc_result_table <- data_qc_quantile
      } else if (input$select_qc_method_of_table == "linear normalization") {
        qc_result_table <- data_qc_linear
      } else if (input$select_qc_method_of_table == "liwong normalization") {
        qc_result_table <- data_qc_liwong
      } else if (input$select_qc_method_of_table == "cubic normalization") {
        qc_result_table <- data_qc_cubic
      } else if (input$select_qc_method_of_table == "batchratio normalization") {
        qc_result_table <- data_qc_batchratio
      } else if (input$select_qc_method_of_table == "MetNormalization") {
        qc_result_table <- data_qc_met
      } else if (input$select_qc_method_of_table == "normalization performance") {
        qc_result_table <- data_qc_performance
      } else if (input$select_qc_method_of_table == "detailed QC.CV performance") {
        qc_result_table <- data_qc_cv_performance_variable
      } else if (input$select_qc_method_of_table == "detailed validate RSD performance") {
        qc_result_table <- data_qc_validatersd_performance_variable
      }
      
      if (input$export_qc_result_table_format == ".csv") {
        write.csv(qc_result_table, file, row.names = FALSE)
      }
      # else if (input$export_qc_result_table_format == ".xlsx") {
      #   write.xlsx(qc_result_table, file, row.names = FALSE)
      # }
      removeModal()
    }
  )
  
  # 导出PCA图
  observeEvent(input$export_qc_pca_overview, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_qc_result_pca_overview_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_qc_result_pca_overview_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_qc_result_pca_overview_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_qc_result_pca_overview_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_result_pca_overview_name) || input$export_qc_result_pca_overview_name == "") {
        paste("pca-overview-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_qc_result_pca_overview_format, sep="")
      } else {
        paste(input$export_qc_result_pca_overview_name, input$export_qc_result_pca_overview_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_qc_pca_overview, 
             height = plot_qc_pca_overview_autoheight / plot_size_fold, 
             width = plot_qc_pca_overview_autowidth / plot_size_fold, 
             units = "mm")
      removeModal()
    }
  )
  
  # 导出cv%柱状图
  observeEvent(input$export_qc_cv_hist, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_qc_cv_hist_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_qc_cv_hist_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_qc_cv_hist_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_qc_cv_hist_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_cv_hist_name) || input$export_qc_cv_hist_name == "") {
        paste("cvpercent-hist-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_qc_cv_hist_format, sep="")
      } else {
        paste(input$export_qc_cv_hist_name, input$export_qc_cv_hist_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_qc_cvpercent_hist, 
             width = plot_qc_cvpercent_hist_autowidth / plot_size_fold, 
             height = plot_qc_cvpercent_hist_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  # 导出cv%分布图
  observeEvent(input$export_qc_cv_distribution, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_qc_cv_distribution_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_qc_cv_distribution_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_qc_cv_distribution_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_qc_cv_distribution_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_cv_distribution_name) || input$export_qc_cv_distribution_name == "") {
        paste("cvdistribution-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_qc_cv_distribution_format, sep="")
      } else {
        paste(input$export_qc_cv_distribution_name, input$export_qc_cv_distribution_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_qc_cvpercent_distribution, 
             width = plot_qc_cvpercent_distribution_width / plot_size_fold, 
             height = plot_qc_cvpercent_distribution_height / plot_size_fold, 
             units = "mm")
      removeModal()
    }
  )
  
  # 导出pca(sample vs qc)
  observeEvent(input$export_qc_result_pca_qcvssample, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_qc_result_pca_qcvssample_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_qc_result_pca_qcvssample_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_qc_result_pca_qcvssample_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_qc_result_pca_qcvssample_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_result_pca_qcvssample_name) || input$export_qc_result_pca_qcvssample_name == "") {
        paste("pca-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_qc_result_pca_qcvssample_format, sep="")
      } else {
        paste(input$export_qc_result_pca_qcvssample_name, input$export_qc_result_pca_qcvssample_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_qc_pca_sample, 
             width = plot_qc_pca_sample_width / plot_size_fold, 
             height = plot_qc_pca_sample_height / plot_size_fold, 
             units = "mm")
      removeModal()
    }
  )
  
  # 导出qc均值方差折线图
  observeEvent(input$export_qc_result_qc_lineplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_qc_result_qc_lineplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_qc_result_qc_lineplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_qc_result_qc_lineplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_qc_result_qc_lineplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_result_qc_lineplot_name) || input$export_qc_result_qc_lineplot_name == "") {
        paste("qc-line-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_qc_result_qc_lineplot_format, sep="")
      } else {
        paste(input$export_qc_result_qc_lineplot_name, input$export_qc_result_qc_lineplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_qc_line, 
             width = plot_qc_line_width / plot_size_fold, 
             height = plot_qc_line_height / plot_size_fold, 
             units = "mm")
      removeModal()
    }
  )
  
  # 导出qc相关系数图
  observeEvent(input$export_qc_result_correlationplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_qc_result_correlationplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_qc_result_correlationplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_qc_result_correlationplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_qc_result_correlationplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_qc_result_correlationplot_name) || input$export_qc_result_correlationplot_name == "") {
        paste("qc-correlation-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_qc_result_correlationplot_format, sep="")
      } else {
        paste(input$export_qc_result_correlationplot_name, input$export_qc_result_correlationplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = plot_qc_correlation, 
             width = plot_qc_correlation_width / plot_size_fold, 
             height = plot_qc_correlation_height / plot_size_fold, 
             units = "mm")
      removeModal()
    }
  )
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "上传数据",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/QualityControl_UploadData.md")),
        # fluidRow(column(width = 2,
        #                 downloadButton(outputId = ns("download_sampledata"), 
        #                                label = "下载质控数据", icon = icon("download"), 
        #                                style = STYLES$help_download_sampledata_button))
        # ),
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        modalButton(label = "确认")
      ),
      style="max-height:500px; overflow:auto;"
    ))
  })
  
  # output$download_sampledata <- downloadHandler(
  #   filename = "qc_exampledata.xlsx",
  #   content = function(file) {
  #     sampledata <- read.csv("help/data/da_main.csv", check.names = FALSE)
  #     write.csv(sampledata, file, row.names = FALSE)
  #   }
  # )
  
  callModule(helpServer, "help_quality_control", title = "质量控制", size = "l", file = "help/QualityControl.md")
  
}