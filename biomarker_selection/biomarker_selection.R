source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


# roc曲线图尺寸
roccurveplot_width <- 600
roccurveplot_height <- 450


calculateRoc <- function(data, mapping, group_test, group_control) {
  # class 列转换为factor
  data[[1]] <- factor(data[[1]], levels = c(group_control, group_test))
  data <- subset(data, (data[1] == group_control) | (data[1] == group_test))
  
  var = c()
  auc = c()
  spe = c()
  sen = c()
  ci = c()
  threshold = c()
  ci = c()
  for (i in (2:ncol(data))) {
    varname = colnames(data)[i]
    var = append(var, mapping[[varname]])
    roc = roc(data[[1]], data[[i]], levels = c(group_control, group_test))
    auc = append(auc, round(as.numeric(gsub("Area under the curve: ", "", roc$auc)), digits = 2))
    coords = coords(roc, "best")
    threshold = append(threshold, paste(round(coords$threshold, digits = 2), collapse = ", "))
    spe = append(spe, paste(round(coords$specificity, digits = 2), collapse = ", "))
    sen = append(sen, paste(round(coords$sensitivity, digits = 2), collapse = ", "))
    ci = append(ci, paste(round(ci.auc(roc)[1], digits = 2), round(ci.auc(roc)[3], digits = 2), sep = "-"))
  }
  report = data.frame(var = var, spe = spe, sen = sen, auc = auc, threshold = threshold, ci = ci)
  colnames(report) = c("Variable", "Specificity", "Sensitivity", "AUC", "Threshold", "95% CI (DeLong)")
  # 根据auc降序排序
  report = report[order(-report["AUC"]),]
  return(report)
}


drawRocCurvePlot <- function(data, reverse_mapping, group_test, group_control, variables, setting) {
  # class 列转换为factor
  data[[1]] <- factor(data[[1]], levels = c(group_control, group_test))
  data_control <- subset(data, data[1] == group_control)
  data_test <- subset(data, data[1] == group_test)
  data <- rbind(data_control, data_test)
  data_reverse <- rbind(data_test, data_control)
  
  # 提取多变量名称
  variables <- c(as.character(variables))
  if (length(variables) > 0) {
    variables <- gsub("`", "", variables)
  } else {
    variables <- NULL
  }
  
  # 映射原始变量名称
  origvarname = c()
  for (variable in variables) {
    name = as.character(reverse_mapping[[variable]])
    origvarname = append(origvarname, name)
  }
  
  # 将所有变量的数据组合为一个dataframe
  d = c()
  m = c()
  variable = c()
  for (i in 1:length(origvarname)) {
    # 正向计算一个auc
    roc = roc(data[[1]], data[[origvarname[i]]], direction = '<')
    # 调换control和case组的direction, 计算auc
    roc_reverse = roc(data[[1]], data[[origvarname[i]]], direction = '>')
    if (roc$auc > roc_reverse$auc) {
      d = append(d, as.numeric(factor(data[[1]], levels = c(group_control, group_test))) - 1)
    } else {
      d = append(d, as.numeric(factor(data[[1]], levels = c(group_test, group_control))) - 1)
    }
    m = append(m, data[[origvarname[i]]])
    variable = append(variable, rep(variables[i], length(data[[origvarname[i]]])))
  }
  df_plot = data.frame(d = d, m = m, variable = variable)
  
  plot <- ggplot(df_plot)

  # 是否显示label
  if (setting$showlabel) {
    plot <- plot +
      geom_roc(mapping = aes(d = d, m = m, color = variable), 
               size = setting$linewidth, 
               pointsize = setting$pointsize, 
               labels = TRUE) +
      style_roc(xlab = "1 - Specificity", ylab = "Sensitivity", guide = FALSE)
  } else {
    plot <- plot +
      geom_roc(mapping = aes(d = d, m = m, color = variable), 
               size = setting$linewidth, 
               pointsize = setting$pointsize, 
               labels = FALSE, 
               n.cuts = 0) +
      style_roc(xlab = "1 - Specificity", ylab = "Sensitivity", guide = FALSE)
  }
  
  # 显示置信区间
  if (setting$showrocci) {
    plot <- plot + geom_rocci(mapping = aes(d = d, m = m, color = variable), 
                              linetype = 1, 
                              size = 0.5)
  }

  # 是否显示基线
  if (setting$showguideline) {
    plot <- plot + annotate("segment", x = 0, xend = 1, y = 0, yend = 1, colour = "grey", size = 0.5)
  }

  # auc注释
  annotate = ""
  annotate_var = c()
  annotate_auc = c()
  for (i in 1:length(variables)) {
    roc = roc(data[[1]], data[[origvarname[i]]], levels = c(group_control, group_test))
    annotate = paste(annotate, "AUC of ", variables[i], ": ", round(roc$auc, 2), "\n")
    annotate_var = append(annotate_var, variables[i])
    annotate_auc = append(annotate_auc, round(roc$auc, 2))
  }
  annotate_df = data.frame(variable = annotate_var, auc = paste("AUC of ", annotate_var, ": ", annotate_auc))
  
  # 图形分身
  if (setting$showfacets) {
    plot <- plot + 
      facet_wrap(~variable, nrow = setting$facetrow, ncol = setting$facetcol)
    if (setting$showannotation) {
      plot <- plot + geom_text(data = annotate_df, aes(x = setting$annotationx, y = setting$annotationy, label = auc), 
                               size = setting$annotationfontsize,
                               parse = FALSE,
                               hjust = 0)
    }
  } else {
    if (setting$showannotation) {
      plot <- plot + annotate("text", 
                              x = setting$annotationx, 
                              y = setting$annotationy, 
                              label = annotate,
                              size = setting$annotationfontsize,
                              hjust = 0)
    }
  }
  
  plot <- plot +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
    # scale_y_continuous(expand = expansion(add = 0.1)) + scale_x_continuous(expand = expansion(add = 0.1))
  
  return(plot)
}


biomarkerSelectionUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useToastr(),
    extendShinyjs(text = jscode, functions = c("collapse")),
    
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$style(type="text/css", ".checkbox {margin-top: 20px;"),
              tags$base(target = "_blank")
              ),
    
    fluidRow(
      tags$div(
        class = "biomarker_selection",
        fluidRow(
          column(width = 12,
                 box(
                   title = "上传数据",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   width = 12,
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
                              # placeholder = "Click the button to select a file, or directly drag and drop the file here.",
                              placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                              accept = ".csv"
                              )
                            ),
                   ),
                   fluidRow(
                     column(width = 12, uiOutput(ns("data_summary")))
                     ),
                   ),
                 )
          ),
        fluidRow(
          column(width = 12,
                 box(
                   id = "box-bs",
                   title = "标志物分析",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   width = 12,
                   fluidRow(column(width = 2, offset = 10, helpButton(ns("help_analysis")))),
                   fluidRow(
                     sidebarPanel(
                       width = 2,
                       fluid  = TRUE,
                       actionButton(ns("ttest_setting"),
                                    label = "参数设置",
                                    icon = icon("sliders"),
                                    class = "setting-button"),
                       actionButton(ns("execute"), 
                                    # label = "Execute", 
                                    label = "训练模型", 
                                    icon = icon("play"),
                                    class = "setting-button"),
                     ),
                     mainPanel(
                       useShinyjs(),
                       width = 10,
                       tabsetPanel(
                         tabPanel(title = "参数",
                                  icon = icon("sliders"),
                                  hidden(div(id = ns("rocResultParams"),
                                             withSpinner(DT::DTOutput(outputId = ns("result_params"), height = "400px"))
                                             )
                                         )
                                  ),
                         tabPanel(title = "数据", 
                                  icon = icon("table"),
                                  hidden(div(id = ns("rocResultTable"), 
                                             withSpinner(DT::DTOutput(outputId = ns("result_table"))),
                                             column(width = 2, offset = 10, actionButton(inputId = ns("export_result_table"),
                                                                                         class = "download-button",
                                                                                         label = "下载数据",
                                                                                         icon = icon("download")))
                                             )
                                         )
                                  ),
                         tabPanel(title = "ROC曲线图",
                                  # icon = tags$i(class = "iconfont icon-boxplot", role="presentation"),
                                  hidden(div(id = ns("rocCurvePlot"),
                                             fluidRow(
                                               br(),
                                               column(width = 9,
                                                      withSpinner(plotOutput(outputId = ns("result_roccurveplot"), 
                                                                             width = "auto", 
                                                                             height = "auto")
                                                                  )
                                                      ),
                                               column(width = 3,
                                                      class = "plot-setting-column",
                                                      tags$div(
                                                        class = "plot-setting",
                                                        box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12, 
                                                            fluidRow(
                                                              column(width = 12, uiOutput(outputId = ns("biomarker_of_roccurveplot"))),
                                                              ),
                                                            fluidRow(
                                                              column(width = 6, 
                                                                     numericInput(inputId = ns("roccurveplot_width"),
                                                                                  label = "宽度",
                                                                                  value = roccurveplot_width)
                                                                     ),
                                                              column(width = 6,
                                                                     numericInput(inputId = ns("roccurveplot_height"),
                                                                                  label = "高度",
                                                                                  value = roccurveplot_height)
                                                                     )
                                                              ),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     numericInput(inputId = ns("linewidth_of_roccurveplot"),
                                                                                  label = "线条宽度",
                                                                                  value = 0.5,
                                                                                  max = 2,
                                                                                  min = 0, 
                                                                                  step = 0.1)
                                                                     ),
                                                              column(width = 6,
                                                                     numericInput(inputId = ns("pointsize_of_roccurveplot"),
                                                                                  label = "点大小",
                                                                                  value = 0.2,
                                                                                  min = 0, 
                                                                                  max = 1,
                                                                                  step = 0.1
                                                                                  )
                                                                     )
                                                            ),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     checkboxInput(inputId = ns("showguideline_of_roccurveplot"),
                                                                                   label = "显示基线",
                                                                                   value = TRUE)
                                                                     ),
                                                              column(width = 6,
                                                                     checkboxInput(inputId = ns("showlabel_of_roccurveplot"),
                                                                                   label = "显示Label",
                                                                                   value = TRUE)
                                                                     ),
                                                              ),
                                                            )
                                                        ),
                                                      tags$div(
                                                        class = "plot-setting",
                                                        box(title = "高级设置", collapsible = TRUE, collapsed = FALSE, width = 12, 
                                                            fluidRow(
                                                              column(width = 6,
                                                                     checkboxInput(inputId = ns("showrocci_of_roccurveplot"),
                                                                                   label = "显示置信区间",
                                                                                   value = FALSE),
                                                                     ),
                                                              ),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     checkboxInput(inputId = ns("showfacets_of_roccurveplot"),
                                                                                   label = "图形分身",
                                                                                   value = FALSE)
                                                                     ),
                                                              ),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     numericInput(inputId = ns("facetrow_of_roccurveplot"),
                                                                                  label = "行数",
                                                                                  value = NULL,
                                                                                  min = 1)
                                                                     ),
                                                              column(width = 6,
                                                                     numericInput(inputId = ns("facetcol_of_roccurveplot"),
                                                                                  label = "列数",
                                                                                  value = NULL,
                                                                                  min = 1)
                                                                     )
                                                              ),
                                                            fluidRow(
                                                              column(width = 6,
                                                                     checkboxInput(inputId = ns("showannotation_of_roccurveplot"),
                                                                                   label = "显示AUC",
                                                                                   value = TRUE)
                                                                     ),
                                                              ),
                                                            fluidRow(
                                                              column(width = 4,
                                                                     numericInput(inputId = ns("annotationx_of_roccurveplot"),
                                                                                  label = "x坐标",
                                                                                  value = 0.5,
                                                                                  min = 0,
                                                                                  max = 1,
                                                                                  step = 0.05)
                                                                     ),
                                                              column(width = 4,
                                                                     numericInput(inputId = ns("annotationy_of_roccurveplot"),
                                                                                  label = "y坐标",
                                                                                  value = 0.25,
                                                                                  min = 0,
                                                                                  max = 1,
                                                                                  step = 0.05)
                                                                     ),
                                                              column(width = 4,
                                                                     numericInput(inputId = ns("annotationfontsize_of_roccurveplot"),
                                                                                  label = "字号",
                                                                                  value = 4,
                                                                                  min = 0,
                                                                                  step = 1)
                                                                     )
                                                              ),
                                                            )
                                                        ),
                                                      )
                                               ),
                                             fluidRow(
                                               column(width = 2, offset = 10, actionButton(inputId = ns("export_roccurveplot"),
                                                                                           class = "download-button",
                                                                                           label = "下载图表",
                                                                                           icon = icon("download")
                                                                                           )
                                                      ),
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


biomarkerSelectionServer <- function(input, output, session) {
  
  ns <- session$ns
  dataset <- NULL
  origdataset <- NULL
  dataset_stringasfactors_false <- NULL
  mapping <- NULL
  reverse_mapping <- NULL
  classes <- c()
  result_params <- NULL
  result_table <- NULL
  group_test <- NULL
  group_control <- NULL
  result_roccurveplot <- NULL
  
  observeEvent(input$dataloader$datapath, {
    # dataset with original column names
    origdataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
    dataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE)
    # 将第一列分组名称转换为Group,便于后续列名选取
    colnames(dataset)[1] <<- "Group"
    dataset_stringasfactors_false <<- read.csv(input$dataloader$datapath, stringsAsFactors = FALSE)
    
    # 保存原始变量名->标准变量名的映射
    mapping <<- data.frame(matrix(ncol = ncol(dataset), nrow = 0), check.names = FALSE)
    mapping <<- rbind(mapping, colnames(origdataset))
    colnames(mapping) <<- colnames(dataset)
    
    # 保存标准变量名->原始变量名的映射
    reverse_mapping <<- data.frame(matrix(ncol = ncol(dataset), nrow = 0), check.names = FALSE)
    reverse_mapping <<- rbind(reverse_mapping, colnames(dataset))
    colnames(reverse_mapping) <<- colnames(origdataset)
    
    classes <<- levels(unique(dataset[[1]]))
    
    group_control <<- classes[1]
    group_test <<- classes[2]

    output$data_summary <- renderUI({
      p("样本数: ", nrow(origdataset),
        "变量数: ", ncol(origdataset) - 1,
      )
    })
    
    # 自动展开
    js$collapse("box-bs")
    
    choices = data.frame(variable = colnames(origdataset)[2:ncol(origdataset)])
    choices = as.data.frame(t(choices[["variable"]]))
    colnames(choices) = colnames(origdataset)[2:ncol(origdataset)]
    
    output$biomarker_of_roccurveplot <- renderUI({
      # selectInput(inputId = ns("feature_to_draw_roccurveplot"), label = "select feature", 
      #             choices = colnames(origdataset)[2:ncol(origdataset)],
      #             selected = colnames(origdataset)[2])
      
      varSelectInput(inputId = ns("feature_to_draw_roccurveplot"),
                     label = "变量选择",
                     data = choices,
                     selected = colnames(origdataset)[2],
                     multiple = TRUE)
    })
    
  })
  
  observeEvent(input$ttest_setting, {
    showModal(modalDialog(
      title = "分组设置",
      size = "l",
      fluidPage(
        div(id = ns(paste("group_setting")),
            fluidRow(
              column(width = 5,
                     selectInput(inputId = ns("group_control"), label = "control group", choices = classes, selected = group_control)),
              column(width = 5,
                     selectInput(inputId = ns("group_test"), label = "test group", choices = classes, selected = group_test))
            )
        ),
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = ns("setting_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })

  observeEvent(input$setting_ok, {
    group_test <<- input$group_test
    group_control <<- input$group_control
    removeModal()
  })
  
  # 图形分身功能的行列控制
  observeEvent(input$showfacets_of_roccurveplot, {
    if(input$showfacets_of_roccurveplot == TRUE) {
      shinyjs::show("facetrow_of_roccurveplot")
      shinyjs::show("facetcol_of_roccurveplot")
    } else {
      shinyjs::hide("facetrow_of_roccurveplot")
      shinyjs::hide("facetcol_of_roccurveplot")
    }
  })

  # 是否显示auc值及其显示位置
  observeEvent(input$showannotation_of_roccurveplot, {
    if (input$showannotation_of_roccurveplot == TRUE) {
      shinyjs::show("annotationx_of_roccurveplot")
      shinyjs::show("annotationy_of_roccurveplot")
      shinyjs::show("annotationfontsize_of_roccurveplot")
    } else {
      shinyjs::hide("annotationx_of_roccurveplot")
      shinyjs::hide("annotationy_of_roccurveplot")
      shinyjs::hide("annotationfontsize_of_roccurveplot")
    }
  })
  
  observeEvent(input$execute, {
    if (!is.null(dataset)) {
      shinyjs::show("rocResultParams")
      shinyjs::show("rocResultTable")
      shinyjs::show("rocCurvePlot")

      # calculate roc
      result_table <<- calculateRoc(dataset, mapping, group_test, group_control)

      result_params <<- data.frame(param = c("Group setting"),
                                   value = c(paste0(group_control, "  v.s.  ", group_test)),
                                   stringsAsFactors = FALSE)
      
      # reuslt params
      output$result_params <- DT::renderDT({
        DT::datatable({
          result_params
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

      # result table
      output$result_table <- DT::renderDT({
        DT::datatable({
          result_table
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

      # t-test result boxplot
      output$result_roccurveplot <- renderPlot({
        if (!is.null(input$feature_to_draw_roccurveplot) && !is.null(group_test) && !is.null(group_control) && length(input$feature_to_draw_roccurveplot) > 0) {
          setting = list(linewidth = input$linewidth_of_roccurveplot,
                         pointsize = input$pointsize_of_roccurveplot,
                         showguideline = input$showguideline_of_roccurveplot,
                         showlabel = input$showlabel_of_roccurveplot,
                         showfacets = input$showfacets_of_roccurveplot,
                         facetrow = input$facetrow_of_roccurveplot,
                         facetcol = input$facetcol_of_roccurveplot,
                         showrocci = input$showrocci_of_roccurveplot,
                         showannotation = input$showannotation_of_roccurveplot,
                         annotationx = input$annotationx_of_roccurveplot,
                         annotationy = input$annotationy_of_roccurveplot,
                         annotationfontsize = input$annotationfontsize_of_roccurveplot)
          result_roccurveplot <<- drawRocCurvePlot(dataset, reverse_mapping, group_test, group_control, input$feature_to_draw_roccurveplot, setting)
        } else {
          result_roccurveplot <<- NULL
        }
        return(result_roccurveplot)
      }, height = function() {
        roccurveplot_height <<- input$roccurveplot_height
        return(roccurveplot_height)
      }, width = function() {
        roccurveplot_width <<- input$roccurveplot_width
        return(roccurveplot_width)
      })
      
    }
  })

  # 导出数据
  observeEvent(input$export_result_table, {
    showModal(modalDialog(
      title = "下载数据",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_result_table_name"), label = "文件名称",
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_result_table_format"), label = "选择格式", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_result_table_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  output$export_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_result_table_name) || input$export_result_table_name == "") {
        paste("roc-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_result_table_format, sep="")
      } else {
        paste(input$export_result_table_name, input$export_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_result_table_format == ".csv") {
        write.csv(result_table, file, row.names = FALSE)
      } else if (input$export_result_table_format == ".xlsx") {
        write.xlsx(result_table, file, row.names = FALSE)
      }
      removeModal()
    }
  )

  observeEvent(input$export_roccurveplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_roccurveplot_name"), label = "文件名称",
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_roccurveplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_ttest_result_boxplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })

  output$export_ttest_result_boxplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_roccurveplot_name) || input$export_roccurveplot_name == "") {
        paste("boxplot-", input$feature_to_draw_roccurveplot, "-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_roccurveplot_format, sep="")
      } else {
        paste(input$export_roccurveplot_name, input$export_roccurveplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = result_roccurveplot,
             width = roccurveplot_width / plot_size_fold,
             height = roccurveplot_height / plot_size_fold,
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
        includeMarkdown(file.path(getwd(), "help/BiomarkerSelection_UploadData.md")),
        fluidRow(column(width = 2,
                        downloadButton(outputId = ns("download_sampledata_main"), 
                                       label = "下载分类数据", icon = icon("download"), 
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
  
  output$download_sampledata_main <- downloadHandler(
    filename = "biomarker_selection.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_main.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  callModule(helpServer, "help_analysis", title = "标志物分析", size = "l", file = "help/BiomarkerSelection.md")
  
}