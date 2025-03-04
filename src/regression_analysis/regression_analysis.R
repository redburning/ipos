source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


lasso <- function(dataset, mapping) {
  set.seed(100)
  x <- subset(dataset, select = -1)
  y <- dataset[1]
  cvfit <- cv.glmnet(x = as.matrix(x), y = as.matrix(y))
  coef_cvfit = coef(cvfit, s = "lambda.min")
  coef_cvfit <- data.frame(beta = rownames(coef_cvfit), coefficients = coef_cvfit[1:length(coef_cvfit)], stringsAsFactors = FALSE)
  coef_cvfit <- subset(coef_cvfit, coef_cvfit["coefficients"] != 0)
  # replace with original feature name
  for (i in 1:length(coef_cvfit[["beta"]])) {
    if (coef_cvfit[["beta"]][i] %in% colnames(mapping)) {
      coef_cvfit[["beta"]][i] <- as.character(mapping[[coef_cvfit[["beta"]][i]]][1])
    }
  }
  # rename column name
  colnames(coef_cvfit) <- c(" ", "1")
  coef_cvfit["1"] <- round(coef_cvfit["1"], 4)
  
  # plot
  df <- data.frame(lambda = cvfit$lambda, cvm = cvfit$cvm, cvup = cvfit$cvup, cvlo = cvfit$cvlo, cvsd = cvfit$cvsd)
  plot <- ggplot(df, aes(x = log(lambda), y = cvm)) + 
    geom_errorbar(aes(ymin = cvlo, ymax = cvup), colour="grey") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_rect(fill=NA, color="black", linetype="solid")) +
    geom_vline(xintercept = log(cvfit$lambda.min), linetype="dashed", size=0.01) +
    geom_point(size = 2, color = "red") +
    theme(plot.margin = unit(c(1, 1, 1, 0.1), "cm")) +
    xlab("Log(λ)") +
    ylab("Mean-Squared Error")
  
  result <- list(data = coef_cvfit, 
                 params = data.frame(param = c("lambda", "MSE"), 
                                     value = c(round(cvfit$lambda.min, 4), round(min(cvfit$cvm), 4))),
                 plot = plot)
  return(result)
}


elasticnet <- function(dataset, mapping) {
  set.seed(200)
  x <- subset(dataset, select = -1)
  y <- dataset[1]
  minCvm <- 10000
  alpha <- 0
  
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (a in seq(0, 1, by = 0.1)) {
                   cvfit <- cv.glmnet(x = as.matrix(x), y = as.matrix(y), alpha = a)
                   if (min(cvfit$cvm) < minCvm) {
                     minCvm <- min(cvfit$cvm)
                     alpha <- a
                   }
                   incProgress(1/10)
                 }
               })
  
  cvfit <- cv.glmnet(x = as.matrix(x), y = as.matrix(y), alpha = alpha)
  
  coef_cvfit = coef(cvfit, s = "lambda.min")
  coef_cvfit <- data.frame(beta = rownames(coef_cvfit), coefficients = coef_cvfit[1:length(coef_cvfit)], stringsAsFactors = FALSE)
  coef_cvfit <- subset(coef_cvfit, coef_cvfit["coefficients"] != 0)
  # replace with original feature name
  for (i in 1:length(coef_cvfit[["beta"]])) {
    if (coef_cvfit[["beta"]][i] %in% colnames(mapping)) {
      coef_cvfit[["beta"]][i] <- as.character(mapping[[coef_cvfit[["beta"]][i]]][1])
    }
  }
  # rename column name
  colnames(coef_cvfit) <- c(" ", "1")
  coef_cvfit["1"] <- round(coef_cvfit["1"], 4)
  
  # plot
  df <- data.frame(lambda = cvfit$lambda, cvm = cvfit$cvm, cvup = cvfit$cvup, cvlo = cvfit$cvlo, cvsd = cvfit$cvsd)
  plot <- ggplot(df, aes(x = log(lambda), y = cvm)) + 
    geom_errorbar(aes(ymin = cvlo, ymax = cvup), colour="grey") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_rect(fill=NA, color="black", linetype="solid")) +
    geom_vline(xintercept = log(cvfit$lambda.min), linetype="dashed", size=0.01) +
    geom_point(size = 2, color = "red") +
    theme(plot.margin = unit(c(1, 1, 1, 0.1), "cm")) +
    xlab("Log(λ)") +
    ylab("Mean-Squared Error")
  
  result <- list(data = coef_cvfit, 
                 params = data.frame(param = c("lambda", "alpha", "MSE"), 
                                     value = c(round(cvfit$lambda.min, 4), alpha, round(min(cvfit$cvm)))), 
                 plot = plot)
  return(result)
}


regressionAnalysisUI <- function(id) {
  ns <- NS(id)
  
  useToastr()
  fluidPage(
    tags$head(tags$style(".modal-content {-webkit-border-radius: 5px !important;-moz-border-radius: 5px !important;border-radius: 5px !important;}
                          .modal-header {border-top-left-radius: 5px; border-top-right-radius: 5px;}
                          .modal-header {border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;}")),
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    
    fluidRow(
      box(
        title = "上传数据",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        # fluidRow(column(width = 2, offset = 10, helpButton(ns("help_upload")))),
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
                   accept = ".csv"
                 )
          )
        ),
        fluidRow(
          column(width = 12, uiOutput(ns("data_summary")))
        )
      ),
      box(
        title = "回归估计",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        fluidRow(column(width = 2, offset = 10, helpButton(ns("help_regression")))),
        fluidRow(
          sidebarPanel(
            width = 2,
            fluid  = TRUE,
            selectInput(inputId = ns("select_regression_model"),
                        # label = "regression model",
                        label = "回归模型",
                        choices = c("LASSO" = "lasso", "Elastic-Net" = "elasticnet")),
            br(),
            actionButton(ns("train"), 
                         # label = "train model", 
                         label = "训练模型", 
                         style = STYLES$execute_button)
          ),
          conditionalPanel(
            condition = "input.select_regression_model == 'lasso'",
            mainPanel(
              useShinyjs(),
              width = 10,
              tabsetPanel(
                tabPanel(title = "params",
                         hidden(div(id = ns("lassoResultParams"),
                                    withSpinner(DT::DTOutput(outputId = ns("lasso_result_params"), width = "600px")),
                                    br(),
                                    withSpinner(plotOutput(outputId = ns("lasso_result_mseplot"), width = "800px"))
                         ))
                ),
                tabPanel(title = "data",  
                         hidden(div(id = ns("lassoResultTable"), 
                                    withSpinner(DT::DTOutput(outputId = ns("lasso_result_table"))),
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_lasso_result_table"),
                                                                                style = STYLES$export_button,
                                                                                label = "download data",
                                                                                icon = icon("download")))
                         ))
                )
              )
            ),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.select_regression_model == 'elasticnet'",
            mainPanel(
              useShinyjs(),
              width = 9,
              tabsetPanel(
                tabPanel(title = "params",
                         hidden(div(id = ns("elasticnetResultParams"),
                                    withSpinner(DT::DTOutput(outputId = ns("elasticnet_result_params"), width = "600px")),
                                    br(),
                                    withSpinner(plotOutput(outputId = ns("elasticnet_result_mseplot"), width = "800px"))
                         ))
                ),
                tabPanel(title = "data",  
                         hidden(div(id = ns("elasticnetResultTable"), 
                                    withSpinner(DT::DTOutput(outputId = ns("elasticnet_result_table"))),
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_elasticnet_result_table"),
                                                                                style = STYLES$export_button,
                                                                                label = "download data",
                                                                                icon = icon("download")))
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


regressionAnalysisServer <- function(input, output, session) {
  
  ns <- session$ns
  dataset <- NULL
  mapping <- NULL
  lasso_result_params <- NULL
  lasso_result_table <- NULL
  lasso_result_mseplot <- NULL
  elasticnet_result_params <- NULL
  elasticnet_result_table <- NULL
  elasticnet_result_mseplot <- NULL
  
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
  })
  
  observeEvent(input$train, {
    if (!is.null(dataset)) {
      if (input$select_regression_model == "lasso") {
        shinyjs::show("lassoResultParams")
        shinyjs::show("lassoResultTable")
        
        # train lasso
        lasso_result <- lasso(dataset, mapping)
        lasso_result_table <<- lasso_result$data
        lasso_result_params <<- lasso_result$params
        lasso_result_mseplot <<- lasso_result$plot
        
        # rf reuslt params
        output$lasso_result_params <- DT::renderDT({
          DT::datatable({
            lasso_result_params
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
        
        # lasso result mseplot
        output$lasso_result_mseplot <- renderPlot({
          return(lasso_result_mseplot)
        })
        
        # lasso result table
        output$lasso_result_table <- DT::renderDT({
          DT::datatable({
            lasso_result_table
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
                         searching = FALSE
          ),
          selection = 'multiple',
          style = 'bootstrap',
          class = 'cell-border stripe compact',
          rownames = FALSE
          )
        })
      } else if (input$select_regression_model == "elasticnet") {
        shinyjs::show("elasticnetResultParams")
        shinyjs::show("elasticnetResultTable")
        
        # train elasticnet 
        elasticnet_result <- elasticnet(dataset, mapping)
        elasticnet_result_table <<- elasticnet_result$data
        elasticnet_result_params <<- elasticnet_result$params
        elasticnet_result_mseplot <<- elasticnet_result$plot
        
        # elastic-net reuslt params
        output$elasticnet_result_params <- DT::renderDT({
          DT::datatable({
            elasticnet_result_params
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
        
        output$elasticnet_result_mseplot <- renderPlot({
          return(elasticnet_result_mseplot)
        })
        
        # elastic-net result table
        output$elasticnet_result_table <- DT::renderDT({
          DT::datatable({
            elasticnet_result_table
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
                         searching = FALSE
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
  
  observeEvent(input$export_lasso_result_table, {
    showModal(modalDialog(
      title = "Download Data",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_lasso_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_lasso_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_lasso_result_table_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    ))
  })
  
  output$export_lasso_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_lasso_result_table_name) || input$export_lasso_result_table_name == "") {
        paste("lasso-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_lasso_result_table_format, sep="")
      } else {
        paste(input$export_lasso_result_table_name, input$export_lasso_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_lasso_result_table_format == ".csv") {
        write.csv(lasso_result_table, file, row.names = FALSE)
      } else if (input$export_lasso_result_table_format == ".xlsx") {
        write.xlsx(lasso_result_table, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  observeEvent(input$export_elasticnet_result_table, {
    showModal(modalDialog(
      title = "Download Data",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_elasticnet_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_elasticnet_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_elasticnet_result_table_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    ))
  })
  
  output$export_elasticnet_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_elasticnet_result_table_name) || input$export_elasticnet_result_table_name == "") {
        paste("elasticnet-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_elasticnet_result_table_format, sep="")
      } else {
        paste(input$export_elasticnet_result_table_name, input$export_elasticnet_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_elasticnet_result_table_format == ".csv") {
        write.csv(elasticnet_result_table, file, row.names = FALSE)
      } else if (input$export_elasticnet_result_table_format == ".xlsx") {
        write.xlsx(elasticnet_result_table, file, row.names = FALSE)
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
        includeMarkdown(file.path(getwd(), "help/RegressionAnalysis_UploadData.md")),
        fluidRow(column(width = 2,
                        downloadButton(outputId = ns("download_regression_sampledata"), label = "下载回归数据", icon = icon("download"), style = STYLES$help_download_sampledata_button))
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
  
  output$download_regression_sampledata <- downloadHandler(
    filename = "muscle_metabolite.csv",
    content = function(file) {
      sampledata1 <- read.csv("help/data/muscle_metabolite.csv", check.names = FALSE)
      write.csv(sampledata1, file, row.names = FALSE)
    }
  )
  
  callModule(helpServer, "help_regression", title = "回归分析", size = "l", file = "help/RegressionAnalysis.md")
  
}