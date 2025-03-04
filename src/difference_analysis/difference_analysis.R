source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


# 箱线图尺寸
singletest_result_boxplot_width <- 600
singletest_result_boxplot_height <- 500
multipletest_result_boxplot_width <- 600
multipletest_result_boxplot_height <- 500
#zscore plot尺寸
singletest_result_zscoreplot_width <- 600
singletest_result_zscoreplot_height <- 800
#火山图尺寸
singletest_result_volcanoplot_width <- 600
singletest_result_volcanoplot_height <- 600
# 种类统计图
singletest_result_categorydistributionplot_width <- 500
singletest_result_categorydistributionplot_height <- 350
# 环形玫瑰图
singletest_result_roseplot_width <- 600
singletest_result_roseplot_height <- 600


differenceAnalysisUI <- function(id) {
  ns <- NS(id)
  
  # uploaded schedule or not
  uploadschedule <- FALSE
  
  fluidPage(
    useToastr(),
    extendShinyjs(text = jscode, functions = c("collapse", "showDiv", "hideDiv", "updateStyle")),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/difference-analysis.css")
    ),
    
    fluidRow(
      box(
        id = "box-da",
        title = "Difference Analysis",
        # title = "差异分析",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        style = "min-height: calc(100vh - 120px); height: calc(100vh - 120px);",
        width = 12,
        tags$div(
          style = "display:flex; gap:12px; height:100%;",
          tags$div(
            id="drawer-da", class="drawer",
            tags$div(style = "padding:15px 20px 20px 20px;",
                     tags$div(
                       style = "padding:10px 10px 30px; border-radius:10px; border:1px solid #dfe1e5; margin-bottom:20px;",
                       fileInput(
                         inputId = ns("dataloader"),
                         label = "",
                         buttonLabel = div(icon("folder-open"), " Upload metabolite matrix... "),
                         placeholder = "Click button to select, or drag file here.",
                         accept = ".csv"
                       ),
                       uiOutput(ns("data_summary_matrix")),
                       tags$div(
                         id = 'heatmap_help_matrix',
                         class = 'annotation-area',
                         tags$h5("What data formats do we support?"),
                         tags$p("We support the data format where each row represents a variable and each column represents a sample, 
                            with the first column being the variable names, and the first row being the sample names. As shown in the table below."),
                         tags$div(
                           style = "display:flex; justify-content: space-between; align-items:flex-end;",
                           tags$img(src = 'help/da-matrix.svg', style = 'height:140px'),
                           downloadButton(outputId = ns("download_sampledata_da_matrix"), 
                                          label = "Download sample data", icon = icon("download"), 
                                          class = "download-button-mini", style = "margin-bottom:5px;")
                         )
                       )
                     ),
                     tags$div(
                       style = "padding:10px 10px 30px; border-radius:10px; border:1px solid #dfe1e5; margin-bottom:20px;",
                       fileInput(
                         inputId = ns("attachmentloader_sample_group"),
                         label = "",
                         buttonLabel = div(icon("folder-open"), " Upload sample grouping data... "),
                         placeholder = "Click button to select, or drag file here.",
                         accept = ".csv"
                       ),
                       uiOutput(ns("data_summary_sample_class")),
                       tags$div(
                         id = 'heatmap_help_sample_group',
                         class = 'annotation-area',
                         tags$p(
                           "The supplementary table indicating the categories to which the ",
                           tags$span(style = "color:#1f62e0", "samples"),
                           " belong."
                         ),
                         tags$div(
                           style = "display:flex; justify-content: space-between; align-items:flex-end;",
                           tags$img(src = 'help/da-group-settings.svg', style = 'height:140px'),
                           downloadButton(outputId = ns("download_sampledata_da_group"), 
                                          label = "Download sample data", icon = icon("download"), 
                                          class = "download-button-mini", style = "margin-bottom:5px;")
                         )
                       )
                     ),
                     tags$div(
                       style = "padding:10px 10px 30px; border-radius:10px; border:1px solid #dfe1e5; margin-bottom:20px;",
                       fileInput(
                         inputId = ns("attachmentloader_var_class"),
                         label = "",
                         buttonLabel = div(icon("folder-open"), " Upload metabolite classification data... "),
                         placeholder = "Click button to select, or drag file here.",
                         accept = ".csv"
                       ),
                       uiOutput(ns("data_summary_variable_class")),
                       tags$div(
                         id = 'heatmap_help_variable_class',
                         class = 'annotation-area',
                         tags$p(
                           "The supplementary table indicating the categories to which the ",
                           tags$span(style = "color:#1f62e0", "variables"),
                           " belong."
                         ),
                         tags$div(
                           style = "display:flex; justify-content: space-between; align-items:flex-end;",
                           tags$img(src = 'help/da-var-class.svg', style = 'height:120px'),
                           downloadButton(outputId = ns("download_sampledata_da_class"), 
                                          label = "Download sample data", icon = icon("download"), 
                                          class = "download-button-mini", style = "margin-bottom:5px;")
                         )
                       )
                     )
            )
          ),
          tags$div(id="overlay-da", class="overlay",
                   onclick = onOverlayClick('drawer-da', 'overlay-da')),
          
          tags$div(
            style = 'width:280px; height:100%; background-color:white; max-height:calc(100vh - 150px); overflow-y:scroll;',
            tags$div(
              class = 'accordion-item-first',
              buildAccordionItem(title = 'My Data', collapsed = FALSE),
              tags$div(class = 'collapse-item-body',
                       # view data
                       tags$button(class = "action-button-primary", 
                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                   tags$div(tags$i(class="fas fa-cloud-arrow-up"),
                                            tags$span("Upload data"), 
                                   ),
                                   onclick = onInspectDataBtnClick('drawer-da', 'overlay-da')
                       ),
              )
            ),
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Experiment Design'),
              tags$div(class = 'collapse-item-body',
                       selectInput(inputId = ns('experiment_design'), label = '', 
                                   choices = c('Unpaired Two', 'Paired Two', 'Multiple Groups'), selected = 'Unpaired Two'),
              )
            ),
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Normal Distribution'),
              tags$div(class = 'collapse-item-body',
                       selectInput(inputId = ns('normal_distribution'), label = '', choices = c('Yes', 'No'), selected = 'Yes'),
              )
            ),
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Test Method'),
              tags$div(class = 'collapse-item-body',
                       uiOutput(outputId = ns("test_method_ui"))
              )
            ),
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Grouping Selection'),
              tags$div(class = 'collapse-item-body',
                       uiOutput(outputId = ns('groupcol_selection_ui')),
              )
            ),
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Grouping Comparison'),
              tags$div(class = 'collapse-item-body',
                       uiOutput(outputId = ns("groups_selection_ui")),
              )
            ),
            tags$div(
              class = 'accordion-item',
              buildAccordionItem(title = 'Data Preparation'),
              tags$div(class = 'collapse-item-body', 
                       selectInput(inputId = ns('settings_data_transformation'), label = 'Transformation', 
                                   choices = c('None', 'Log2', 'Log10'), selected = 'None')
              )
            ),
            tags$div(
              class = 'collapse-item-body accordion-item-last',
              style = 'padding-top:12px; padding-bottom:12px;',
              actionButton(ns("execute"),
                           label = "Execute",
                           icon = icon("play"),
                           class = "action-button-primary"),
            )
          ),
          
          tags$div(
            style= "width:calc(100% - 280px)",
            conditionalPanel(
              condition = "input.experiment_design != 'Multiple Groups'",
              ns = ns,
              tabsetPanel(
                tabPanel(title = "Overview",
                         icon = icon("sliders"),
                         hidden(div(id = ns("result-overview"),
                                    withSpinner(uiOutput(outputId = ns("result_overview")))
                         ))
                ),
                tabPanel(title = "Preprocessed",
                         icon = icon("table"),
                         hidden(div(id = ns("result-preprocessed-data"),
                                    withSpinner(DT::DTOutput(outputId = ns("preprocessed_data"))),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      downloadButton(outputId = ns("export_preprocessed_data_singletest"),
                                                     class = "action-button-primary",
                                                     label = "Download",
                                                     icon = icon("download"))
                                    )
                         ))
                ),
                tabPanel(title = "Result",
                         icon = icon("table"),
                         hidden(div(id = ns("result-single-test-table"),
                                    withSpinner(DT::DTOutput(outputId = ns("result_single_test_table"))),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      downloadButton(outputId = ns("export_single_test_result_table"),
                                                     class = "action-button-primary",
                                                     label = "Download",
                                                     icon = icon("download"))
                                    )
                         )
                         )
                ),
                tabPanel(title = "Box",
                         icon = tags$i(class = "iconfont icon-boxplot", role="presentation"),
                         hidden(div(id = ns("result-boxplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotlyOutput(outputId = ns("result_boxplot"),
                                                                   width = paste0(singletest_result_boxplot_width, "px"),
                                                                   height = paste0(singletest_result_boxplot_height, "px"))
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-singletest-boxplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_singletest_boxplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("singletest_result_boxplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-singletest-boxplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-singletest-boxplot', 'overlay-singletest-boxplot')),
                                        
                                        tags$div(
                                          style = 'width:270px; height:calc(100vh - 210px); z-index:101;',
                                          tags$div(
                                            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Datapoint settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Data', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', 
                                                       # inspect data
                                                       # tags$button(class = "action-button-primary", 
                                                       #             style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                       #             tags$div(tags$i(class="far fa-eye"),
                                                       #                      tags$span("Inspect data"), 
                                                       #             ), 
                                                       #             onclick = onInspectDataBtnClick('drawer-singletest-boxplot', 'overlay-singletest-boxplot')
                                                       # ),
                                                       # 可选变量
                                                       uiOutput(outputId = ns("singletest_boxplot_selected_variable_ui")),
                                                       # 可选颜色
                                                       selectInput(inputId = ns("singletest_boxplot_palette"),
                                                                   label = "Palette",
                                                                   choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 
                                                                               'simpson', 'startrek', 'tron', 'dark', 'light', 'random'),
                                                                   selected = 'd3'
                                                       )
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Title', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('singletest_boxplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('singletest_boxplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('singletest_boxplot_title_fontsize'), label = 'Font size', min = 1, value = 14, step = 1),
                                                       selectInput(inputId = ns('singletest_boxplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                                       selectInput(inputId = ns('singletest_boxplot_subtitle'), label = 'Subtitle', choices = c('P-value', 'Q-value'), selected = 'P-value')
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Axis', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           selectInput(inputId = ns('singletest_boxplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('singletest_boxplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 11, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           selectInput(inputId = ns('singletest_boxplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('singletest_boxplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                              )
                                            ),
                                            # Size
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Size', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_boxplot_width'), label = 'Width', min = 100, step = 10, value = singletest_result_boxplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_boxplot_height'), label = 'Height', min = 100, step = 10, value = singletest_result_boxplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            # Export
                                            tags$div(
                                              buildAccordionItem(title = 'Export', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', style = 'display:block;',
                                                       # export plot
                                                       tags$div(
                                                         actionButton(inputId = ns("export_singletest_result_boxplot"), 
                                                                      label = "Export plot",
                                                                      icon = icon("download"),
                                                                      class = "action-button-primary"),
                                                         style = "padding:6px 12px 20px 12px;"
                                                       )
                                              )
                                            )
                                            
                                          )
                                        )
                                      )
                                    )
                         ))
                ),
                tabPanel(title = "Z-Score",
                         icon = tags$i(class = "iconfont icon-a-geometricscaling", role="presentation"),
                         hidden(div(id = ns("result-zscoreplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width:calc(100% - 270px); max-height:calc(100vh - 210px); overflow-y:scroll; overflow-x:scroll;',
                                          withSpinner(plotlyOutput(outputId = ns("result_zscoreplot"),
                                                                   width = paste0(singletest_result_zscoreplot_width, "px"),
                                                                   height = paste0(singletest_result_zscoreplot_height, "px"))
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-singletest-zscoreplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_singletest_zscoreplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("singletest_result_zscoreplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-singletest-zscoreplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-singletest-zscoreplot', 'overlay-singletest-zscoreplot')),
                                        
                                        tags$div(
                                          style = 'width:270px; height:calc(100vh - 210px); z-index:101;',
                                          tags$div(
                                            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Data', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', 
                                                       # inspect data
                                                       # tags$button(class = "action-button-primary", 
                                                       #             style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                       #             tags$div(tags$i(class="far fa-eye"),
                                                       #                      tags$span("Inspect data"), 
                                                       #             ), 
                                                       #             onclick = onInspectDataBtnClick('drawer-singletest-zscoreplot', 'overlay-singletest-zscoreplot')
                                                       # ),
                                                       # p-value 筛选
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'p-value', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_zscoreplot_data_pvalue_enable"), label = "Enable p-value filter",
                                                                           value = TRUE),
                                                             tags$div(
                                                               style = "display:flex; justify-content:space-between;",
                                                               numericInput(inputId = ns("singletest_zscoreplot_data_pvalue_min"), min = 0, max = 1, step = 0.01, value = 0.00, label = "p-value(min)"),
                                                               numericInput(inputId = ns("singletest_zscoreplot_data_pvalue_max"), min = 0, max = 1, step = 0.01, value = 0.05, label = "p-value(max)")
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       # q-value 筛选
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'q-value', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_zscoreplot_data_qvalue_enable"), label = "Enable q-value filter",
                                                                           value = FALSE),
                                                             tags$div(
                                                               style = "display:flex; justify-content:space-between;",
                                                               numericInput(inputId = ns("singletest_zscoreplot_data_qvalue_min"), min = 0, max = 1, step = 0.01, value = 0.00, label = "q-value(min)"),
                                                               numericInput(inputId = ns("singletest_zscoreplot_data_qvalue_max"), min = 0, max = 1, step = 0.01, value = 0.05, label = "q-value(max)")
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       # point
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'Point', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             numericInput(inputId = ns("singletest_zscoreplot_data_pointsize"), label = "Point size", value = 1, min = 0.5, step = 0.5),
                                                             colourInput(inputId = ns("singletest_zscoreplot_data_pointcolor1"),
                                                                         label = "Point color",
                                                                         value = "mediumseagreen",
                                                                         returnName = FALSE,
                                                                         palette = "limited",
                                                                         closeOnClick = TRUE,
                                                                         allowedCols = c(ColorBrewr$custom)
                                                             ),
                                                             colourInput(inputId = ns("singletest_zscoreplot_data_pointcolor2"),
                                                                         label = "",
                                                                         value = "firebrick",
                                                                         returnName = FALSE,
                                                                         palette = "limited",
                                                                         closeOnClick = TRUE,
                                                                         allowedCols = c(ColorBrewr$custom)
                                                             )
                                                           )
                                                         )
                                                       ),
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Title', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('singletest_zscoreplot_title_text'), label = 'Text'),
                                                       numericInput(inputId = ns('singletest_zscoreplot_title_fontsize'), label = 'Font size', min = 1, value = 14, step = 1),
                                                       selectInput(inputId = ns('singletest_zscoreplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Axis', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('singletest_zscoreplot_yaxis_text'), label = 'Text', value = "Z-Score"),
                                                           numericInput(inputId = ns('singletest_zscoreplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 10, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('singletest_zscoreplot_xaxis_text'), label = 'Text', value = ""),
                                                           numericInput(inputId = ns('singletest_zscoreplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 10, step = 1)
                                                         )
                                                       ),
                                              )
                                            ),
                                            # Size
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Size', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_zscoreplot_width'), label = 'Width', min = 100, step = 10, value = singletest_result_zscoreplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_zscoreplot_height'), label = 'Height', min = 100, step = 10, value = singletest_result_zscoreplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            # Export
                                            tags$div(
                                              buildAccordionItem(title = 'Export', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', style = 'display:block;',
                                                       # export plot
                                                       tags$div(
                                                         actionButton(inputId = ns("export_singletest_result_zscoreplot"), 
                                                                      label = "Export plot",
                                                                      icon = icon("download"),
                                                                      class = "action-button-primary"),
                                                         style = "padding:6px 12px 20px 12px;"
                                                       )
                                              )
                                            )
                                            
                                          )
                                        )
                                      )
                                    )
                         ))
                ),
                tabPanel(title = "Volcano",
                         icon = tags$i(class = "iconfont icon-volcano", role="presentation"),
                         hidden(div(id = ns("result-volcanoplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width:calc(100% - 270px); max-height:calc(100vh - 210px); overflow-y:scroll; overflow-x:scroll;',
                                          withSpinner(plotlyOutput(outputId = ns("result_volcanoplot"),
                                                                   width = paste0(singletest_result_volcanoplot_width, "px"),
                                                                   height = paste0(singletest_result_volcanoplot_height, "px"))
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-singletest-volcanoplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_singletest_volcanoplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("singletest_result_volcanoplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-singletest-volcanoplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-singletest-volcanoplot', 'overlay-singletest-volcanoplot')),
                                        
                                        tags$div(
                                          style = 'width:270px; height:calc(100vh - 210px); z-index:101;',
                                          tags$div(
                                            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Data', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', 
                                                       # inspect data
                                                       # tags$button(class = "action-button-primary", 
                                                       #             style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                       #             tags$div(tags$i(class="far fa-eye"),
                                                       #                      tags$span("Inspect data"), 
                                                       #             ), 
                                                       #             onclick = onInspectDataBtnClick('drawer-singletest-volcanoplot', 'overlay-singletest-volcanoplot')
                                                       # ),
                                                       # 对p-value/q-value绘图
                                                       selectInput(inputId = ns("singletest_volcanoplot_data_yfield"), label = "Y-Field",
                                                                   choices = c("P-value", "Q-value"), selected = "P-value"),
                                                       # P-value/Q-value cutoff
                                                       numericInput(inputId = ns("singletest_volcanoplot_data_pqvalue_cutoff"),
                                                                    label = "P-Value cutoff", value = 0.05, step = 0.01),
                                                       numericInput(inputId = ns("singletest_volcanoplot_data_foldchange_cutoff"),
                                                                    label = "Fold-Change cutoff", value = 1.01, step = 0.1),
                                                       # 颜色组合
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'Points', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           numericInput(inputId = ns('singletest_volcanoplot_data_pointsize'), label = "Point size", value = 1.5, step = 0.5),
                                                           tags$div(
                                                             colourInput(inputId = ns("singletest_volcanoplot_data_pointcolor1"), label = "Point color", 
                                                                         value = "rgba(59, 59, 59, 1)", allowTransparent = TRUE, closeOnClick = TRUE),
                                                             colourInput(inputId = ns("singletest_volcanoplot_data_pointcolor2"), label = "", 
                                                                         value = "rgba(178, 34, 34, 1)", allowTransparent = TRUE, closeOnClick = TRUE),
                                                             colourInput(inputId = ns("singletest_volcanoplot_data_pointcolor3"), label = "", 
                                                                         value = "rgba(190, 190, 190, 1)", allowTransparent = TRUE, closeOnClick = TRUE),
                                                             colourInput(inputId = ns("singletest_volcanoplot_data_pointcolor4"), label = "", 
                                                                         value = "rgba(60, 179, 113, 1)", allowTransparent = TRUE, closeOnClick = TRUE)
                                                             
                                                           )
                                                         )
                                                       )
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Title', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('singletest_volcanoplot_title_text'), label = 'Text', value = "Volcano plot"),
                                                       numericInput(inputId = ns('singletest_volcanoplot_title_fontsize'), label = 'Font size', min = 1, value = 14, step = 1),
                                                       selectInput(inputId = ns('singletest_volcanoplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Axis', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('singletest_volcanoplot_xaxis_text'), label = 'Text', value = "Log2(Fold Change)"),
                                                           numericInput(inputId = ns('singletest_volcanoplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('singletest_volcanoplot_yaxis_text'), label = 'Text', value = "-Log10(P-value)"),
                                                           numericInput(inputId = ns('singletest_volcanoplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                              )
                                            ),
                                            # Size
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Size', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_volcanoplot_width'), label = 'Width', min = 100, step = 10, value = singletest_result_volcanoplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_volcanoplot_height'), label = 'Height', min = 100, step = 10, value = singletest_result_volcanoplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            # Export
                                            tags$div(
                                              buildAccordionItem(title = 'Export', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', style = 'display:block;',
                                                       # export plot
                                                       tags$div(
                                                         actionButton(inputId = ns("export_singletest_result_volcanoplot"), 
                                                                      label = "Export plot",
                                                                      icon = icon("download"),
                                                                      class = "action-button-primary"),
                                                         style = "padding:6px 12px 20px 12px;"
                                                       )
                                              )
                                            )
                                            
                                          )
                                        )
                                      )
                                    )
                         ))
                ),
                tabPanel(title = "Category",
                         icon = tags$i(class = "iconfont icon-jiyinfuji", role="presentation"),
                         hidden(div(id = ns("result-categorydistributionplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width:calc(100% - 270px); max-height:calc(100vh - 210px); overflow-y:scroll; overflow-x:scroll;',
                                          withSpinner(
                                            tags$div(
                                              id = 'difference_analysis_categorydistributionplot',
                                              style = 'display:flex',
                                              plotlyOutput(outputId = ns("result_categorydistributionplot_count"),
                                                           width = "100%",
                                                           height = "100%"),
                                              plotlyOutput(outputId = ns("result_categorydistributionplot_ratio"),
                                                           width = "100%",
                                                           height = "100%")
                                            )
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-singletest-categorydistributionplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_singletest_categorydistributionplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("singletest_result_categorydistributionplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-singletest-categorydistributionplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-singletest-categorydistributionplot', 'overlay-singletest-categorydistributionplot')),
                                        
                                        tags$div(
                                          style = 'width:270px; height:calc(100vh - 210px); z-index:101;',
                                          tags$div(
                                            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Data', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', 
                                                       # inspect data
                                                       # tags$button(class = "action-button-primary", 
                                                       #             style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                       #             tags$div(tags$i(class="far fa-eye"),
                                                       #                      tags$span("Inspect data"), 
                                                       #             ), 
                                                       #             onclick = onInspectDataBtnClick('drawer-singletest-categorydistributionplot', 'overlay-singletest-categorydistributionplot')
                                                       # ),
                                                       # p-value 筛选
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'p-value', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_categorydistributionplot_data_pvalue_enable"), label = "Enable p-value filter",
                                                                           value = TRUE),
                                                             tags$div(
                                                               style = "display:flex; justify-content:space-between;",
                                                               numericInput(inputId = ns("singletest_categorydistributionplot_data_pvalue_min"), min = 0, max = 1, step = 0.01, value = 0.00, label = "p-value(min)"),
                                                               numericInput(inputId = ns("singletest_categorydistributionplot_data_pvalue_max"), min = 0, max = 1, step = 0.01, value = 0.05, label = "p-value(max)")
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       # q-value 筛选
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'q-value', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_categorydistributionplot_data_qvalue_enable"), label = "Enable q-value filter",
                                                                           value = FALSE),
                                                             tags$div(
                                                               style = "display:flex; justify-content:space-between;",
                                                               numericInput(inputId = ns("singletest_categorydistributionplot_data_qvalue_min"), min = 0, max = 1, step = 0.01, value = 0.00, label = "q-value(min)"),
                                                               numericInput(inputId = ns("singletest_categorydistributionplot_data_qvalue_max"), min = 0, max = 1, step = 0.01, value = 0.05, label = "q-value(max)")
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       # Bar
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'Bar', collapsed = FALSE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:block;',
                                                           tags$div(
                                                             selectInput(inputId = ns("singletest_categorydistributionplot_data_bar_display"), label = "Display style",
                                                                         choices = c("Row", "Column"), selected = "Row"),
                                                             colourInput(inputId = ns("singletest_categorydistributionplot_data_barcolor"),
                                                                         label = "Bar color",
                                                                         value = "#1E90FF",
                                                                         returnName = FALSE,
                                                                         palette = "limited",
                                                                         closeOnClick = TRUE,
                                                                         allowedCols = c(ColorBrewr$custom)
                                                             ),
                                                             checkboxInput(inputId = ns("singletest_categorydistributionplot_data_bar_showallclasses"), 
                                                                           label = "Show all classes", value = FALSE)
                                                           )
                                                         )
                                                       ),
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Title', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('singletest_categorydistributionplot_count_title_text'), 
                                                                 label = 'Text of count subplot', value = 'number of differential metabolites'),
                                                       textInput(inputId = ns('singletest_categorydistributionplot_ratio_title_text'), 
                                                                 label = 'Text of ratio subplot', value = 'change ratio of differential metabolites'),
                                                       numericInput(inputId = ns('singletest_categorydistributionplot_title_fontsize'), label = 'Font size', min = 1, value = 14, step = 1),
                                                       selectInput(inputId = ns('singletest_categorydistributionplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Axis', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('singletest_categorydistributionplot_count_yaxis_text'), label = 'Text of count subplot', value = "number"),
                                                           textInput(inputId = ns('singletest_categorydistributionplot_ratio_yaxis_text'), label = 'Text of ratio subplot', 
                                                                     value = "changed metabolites/total of this category(%)"),
                                                           numericInput(inputId = ns('singletest_categorydistributionplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           # textInput(inputId = ns('singletest_categorydistributionplot_yaxis_text'), label = 'Text', value = ""),
                                                           numericInput(inputId = ns('singletest_categorydistributionplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                              )
                                            ),
                                            # Size
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Size', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_categorydistributionplot_width'), label = 'Width', min = 100, step = 10, value = singletest_result_categorydistributionplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_categorydistributionplot_height'), label = 'Height', min = 100, step = 10, value = singletest_result_categorydistributionplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            # Export
                                            tags$div(
                                              buildAccordionItem(title = 'Export', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', style = 'display:block;',
                                                       # export plot
                                                       tags$div(
                                                         actionButton(inputId = ns("export_singletest_result_categorydistributionplot"), 
                                                                      label = "Export plot",
                                                                      icon = icon("download"),
                                                                      class = "action-button-primary"),
                                                         style = "padding:6px 12px 20px 12px;"
                                                       )
                                              )
                                            )
                                            
                                          )
                                        )
                                      )
                                    )
                         ))
                ),
                tabPanel(title = "Rose",
                         icon = tags$i(class = "iconfont icon-nandinggeermeiguitu", role="presentation"),
                         hidden(div(id = ns("result-roseplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width:calc(100% - 270px); max-height:calc(100vh - 210px); overflow-y:scroll; overflow-x:scroll;',
                                          withSpinner(
                                            plotOutput(outputId = ns("result_roseplot"),
                                                       width = "auto", height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-singletest-roseplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_singletest_roseplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("singletest_result_roseplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-singletest-roseplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-singletest-roseplot', 'overlay-singletest-roseplot')),
                                        
                                        tags$div(
                                          style = 'width:270px; height:calc(100vh - 210px); z-index:101;',
                                          tags$div(
                                            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Data', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', 
                                                       # inspect data
                                                       # tags$button(class = "action-button-primary", 
                                                       #             style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                       #             tags$div(tags$i(class="far fa-eye"),
                                                       #                      tags$span("Inspect data"), 
                                                       #             ), 
                                                       #             onclick = onInspectDataBtnClick('drawer-singletest-roseplot', 'overlay-singletest-roseplot')
                                                       # ),
                                                       # p-value 筛选
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'p-value', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_roseplot_data_pvalue_enable"), label = "Enable p-value filter",
                                                                           value = TRUE),
                                                             tags$div(
                                                               style = "display:flex; justify-content:space-between;",
                                                               numericInput(inputId = ns("singletest_roseplot_data_pvalue_min"), min = 0, max = 1, step = 0.01, value = 0.00, label = "p-value(min)"),
                                                               numericInput(inputId = ns("singletest_roseplot_data_pvalue_max"), min = 0, max = 1, step = 0.01, value = 0.05, label = "p-value(max)")
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       # q-value 筛选
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'q-value', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_roseplot_data_qvalue_enable"), label = "Enable q-value filter",
                                                                           value = FALSE),
                                                             tags$div(
                                                               style = "display:flex; justify-content:space-between;",
                                                               numericInput(inputId = ns("singletest_roseplot_data_qvalue_min"), min = 0, max = 1, step = 0.01, value = 0.00, label = "q-value(min)"),
                                                               numericInput(inputId = ns("singletest_roseplot_data_qvalue_max"), min = 0, max = 1, step = 0.01, value = 0.05, label = "q-value(max)")
                                                             )
                                                           )
                                                         )
                                                       ),
                                                       # Metric
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'Metric', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             selectInput(inputId = ns("singletest_roseplot_data_metric"), label = "Metric", choices = c("Fold Change"), 
                                                                         selected = "Fold Change"),
                                                             numericInput(inputId = ns("singletest_roseplot_data_metric_min"), step = 1, value = -1, label = "metric-value(min)"),
                                                             numericInput(inputId = ns("singletest_roseplot_data_metric_max"), step = 1, value = 4, label = "metric-value(max)")
                                                           )
                                                         )
                                                       ),
                                                       # Bar
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'Bar', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             selectInput(inputId = ns("singletest_roseplot_data_barcolor"),
                                                                         label = "Color palette",
                                                                         selected = "Palette 1",
                                                                         choices = c("Palette 1", "Palette 2", "Palette 3", "Palette 4", 
                                                                                     "Palette 5", "Palette 6", "Palette 7", "Palette 8", "Palette 9")),
                                                             checkboxInput(inputId = ns("singletest_roseplot_data_bar_showboundaries"), 
                                                                           label = "Show class boundaries", value = FALSE),
                                                             checkboxInput(inputId = ns("singletest_roseplot_data_bar_showlegend"), 
                                                                           label = "Show legend", value = TRUE)
                                                           )
                                                         )
                                                       ),
                                                       # Baseline
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'Baseline', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           tags$div(
                                                             checkboxInput(inputId = ns("singletest_roseplot_data_bar_showbaseline"), 
                                                                           label = "Show baseline", value = FALSE),
                                                             numericInput(inputId = ns("singletest_roseplot_data_bar_baseline_value"),
                                                                          label = "Value", value = 1)
                                                           )
                                                         )
                                                       )
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Axis', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         numericInput(inputId = ns('singletest_roseplot_fontsize'), label = 'Font size', min = 1, value = 4, step = 1)
                                                       ),
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Size', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_roseplot_width'), label = 'Width', min = 100, step = 10, value = singletest_result_roseplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('singletest_result_roseplot_height'), label = 'Height', min = 100, step = 10, value = singletest_result_roseplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            # Export
                                            tags$div(
                                              buildAccordionItem(title = 'Export', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', style = 'display:block;',
                                                       # export plot
                                                       tags$div(
                                                         actionButton(inputId = ns("export_singletest_result_roseplot"), 
                                                                      label = "Export plot",
                                                                      icon = icon("download"),
                                                                      class = "action-button-primary"),
                                                         style = "padding:6px 12px 20px 12px;"
                                                       )
                                              )
                                            )
                                            
                                          )
                                        )
                                      )
                                    )
                         ))
                ),
              )
            ),
            conditionalPanel(
              condition = "input.experiment_design == 'Multiple Groups'",
              ns = ns,
              tabsetPanel(
                tabPanel(title = "Overview",
                         icon = icon("sliders"),
                         hidden(div(id = ns("result-overview-multipletest"),
                                    withSpinner(uiOutput(outputId = ns("result_overview_multipletest")))
                         ))
                ),
                tabPanel(title = "Preprocessed",
                         icon = icon("table"),
                         hidden(div(id = ns("result-preprocessed-data-multipletest"),
                                    withSpinner(DT::DTOutput(outputId = ns("preprocessed_data_multipletest"))),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      downloadButton(outputId = ns("export_preprocessed_data_multipletest"),
                                                     class = "action-button-primary",
                                                     label = "Download",
                                                     icon = icon("download"))
                                    )
                         ))
                ),
                tabPanel(title = "Result",
                         icon = icon("table"),
                         hidden(div(id = ns("result-table-multipletest"),
                                    withSpinner(DT::DTOutput(outputId = ns("result_multiple_test_table"))),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      downloadButton(outputId = ns("export_result_table_multipletest"),
                                                     class = "action-button-primary",
                                                     label = "Download",
                                                     icon = icon("download"))
                                    )
                         )
                         )
                ),
                tabPanel(title = "Box",
                         icon = tags$i(class = "iconfont icon-boxplot", role="presentation"),
                         hidden(div(id = ns("result-boxplot-multipletest"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotlyOutput(outputId = ns("result_boxplot_multipletest"),
                                                                   width = paste0(multipletest_result_boxplot_width, "px"),
                                                                   height = paste0(multipletest_result_boxplot_height, "px"))
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-multiple-boxplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_multipletest_boxplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("multipletest_result_boxplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-multiple-boxplot", class="overlay",
                                                 onclick = onOverlayClick('drawer-multiple-boxplot', 'overlay-multiple-boxplot')),
                                        
                                        tags$div(
                                          style = 'width:270px; height:calc(100vh - 210px); z-index:101;',
                                          tags$div(
                                            style = 'max-height:100%; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Datapoint settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Data', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body',
                                                       # inspect data
                                                       # tags$button(class = "action-button-primary",
                                                       #             style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                       #             tags$div(tags$i(class="far fa-eye"),
                                                       #                      tags$span("Inspect data"),
                                                       #             ),
                                                       #             onclick = onInspectDataBtnClick('drawer-multiple-boxplot', 'overlay-multiple-boxplot')
                                                       # ),
                                                       # 可选变量
                                                       uiOutput(outputId = ns("multipletest_boxplot_selected_variable_ui")),
                                                       # 可选颜色
                                                       selectInput(inputId = ns("multipletest_boxplot_palette"),
                                                                   label = "Palette",
                                                                   choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick',
                                                                               'simpson', 'startrek', 'tron', 'dark', 'light', 'random'),
                                                                   selected = 'd3'
                                                       )
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Title', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('multipletest_boxplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('multipletest_boxplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('multipletest_boxplot_title_fontsize'), label = 'Font size', min = 1, value = 14, step = 1),
                                                       selectInput(inputId = ns('multipletest_boxplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                                       selectInput(inputId = ns('multipletest_boxplot_subtitle'), label = 'Subtitle', choices = c('P-value', 'Q-value'), selected = 'P-value')
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Axis', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           selectInput(inputId = ns('multipletest_boxplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('multipletest_boxplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 11, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           class = 'collapse-subitem-body', style = 'display:none;',
                                                           selectInput(inputId = ns('multipletest_boxplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('multipletest_boxplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                              )
                                            ),
                                            # Size
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(title = 'Size', collapsed = TRUE),
                                              tags$div(class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('multipletest_result_boxplot_width'), label = 'Width', min = 100, step = 10, value = multipletest_result_boxplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('multipletest_result_boxplot_height'), label = 'Height', min = 100, step = 10, value = multipletest_result_boxplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            # Export
                                            tags$div(
                                              buildAccordionItem(title = 'Export', collapsed = FALSE),
                                              tags$div(class = 'collapse-item-body', style = 'display:block;',
                                                       # export plot
                                                       tags$div(
                                                         actionButton(inputId = ns("export_multipletest_result_boxplot"),
                                                                      label = "Export plot",
                                                                      icon = icon("download"),
                                                                      class = "action-button-primary"),
                                                         style = "padding:6px 12px 20px 12px;"
                                                       )
                                              )
                                            )
                                            
                                          )
                                        )
                                      )
                                    )
                         ))
                ),
              )
            )
          )
        )
      )
    )
  )
}


differenceAnalysisServer <- function(input, output, session) {
  
  ns <- session$ns
  
  # data matrix
  dataset <- NULL
  origdataset <- NULL
  dataset_attachment_sample_group <- NULL   # 样本分类附表
  dataset_attachment_var_class <- NULL      # 变量分类附表
  mapping <- NULL
  preprocessed_data <- NULL                 # 预处理(log transform + scaling)之后的数据
  preprocessed_data_selected_group <- NULL  # preprocessed_data添加了group列，根据所选分组进行子集筛选
  
  # table
  result_single_test_table <- NULL
  result_multiple_test_table <- NULL
  
  # plot & plotly
  singletest_boxplot <- NULL
  singletest_boxplotly <- NULL
  multipletest_boxplot <- NULL
  multipletest_boxplotly <- NULL
  singletest_zscoreplot <- NULL
  singletest_zscoreplotly <- NULL
  singletest_volcanoplot <- NULL
  singletest_volcanoplotly <- NULL
  singletest_categorydistribution_countplot <- NULL
  singletest_categorydistribution_countplotly <- NULL
  singletest_categorydistribution_ratioplot <- NULL
  singletest_categorydistribution_ratioplotly <- NULL
  singletest_roseplot <- NULL
  
  observeEvent(input$execute, {
    tryCatch({
      if (is.null(dataset)) {
        toastr_warning(title = "请上传数据")
      }
      else {
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
        # if (input$settings_data_scaling == 'Centering') {
        #   df_scaled <- preprocessed_data
        #   df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
        #     if (is.numeric(x)) {
        #       mean_val <- mean(x)
        #       return(x - mean_val)
        #     } else {
        #       return(x)  # 如果列不是数值类型，则保持不变
        #     }
        #   }))
        #   preprocessed_data <<- df_scaled
        # } else if (input$settings_data_scaling == 'Unit Variance scaling') {
        #   df_scaled <- preprocessed_data
        #   df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
        #     if (is.numeric(x)) {
        #       mean_val <- mean(x)
        #       sd_val <- sd(x)
        #       return(round((x - mean_val) / sd_val, 6))
        #     } else {
        #       return(x)  # 如果列不是数值类型，则保持不变
        #     }
        #   }))
        #   preprocessed_data <<- df_scaled
        # } else if (input$settings_data_scaling == 'Pareto scaling') {
        #   df_scaled <- preprocessed_data
        #   df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
        #     if (is.numeric(x)) {
        #       mean_val <- mean(x)
        #       sd_val <- sd(x)
        #       return(round((x - mean_val) / sqrt(sd_val), 6))
        #     } else {
        #       return(x)  # 如果列不是数值类型，则保持不变
        #     }
        #   }))
        #   preprocessed_data <<- df_scaled
        # } else if (input$settings_data_scaling == 'Range scaling') {
        #   df_scaled <- preprocessed_data
        #   df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
        #     if (is.numeric(x)) {
        #       mean_val <- mean(x)
        #       max_val <- max(x)
        #       min_val <- min(x)
        #       return(round((x - mean_val) / (max_val - min_val), 6))
        #     } else {
        #       return(x)  # 如果列不是数值类型，则保持不变
        #     }
        #   }))
        #   preprocessed_data <<- df_scaled
        # } else if (input$settings_data_scaling == 'Vast scaling') {
        #   df_scaled <- preprocessed_data
        #   df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
        #     if (is.numeric(x)) {
        #       mean_val <- mean(x)
        #       sd_val <- sd(x)
        #       return(round(((x - mean_val) / sd_val) * (mean_val / sd_val), 6))
        #     } else {
        #       return(x)  # 如果列不是数值类型，则保持不变
        #     }
        #   }))
        #   preprocessed_data <<- df_scaled
        # } else if (input$settings_data_scaling == 'Level scaling') {
        #   df_scaled <- preprocessed_data
        #   df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
        #     if (is.numeric(x)) {
        #       mean_val <- mean(x)
        #       return(round((x - mean_val) / mean_val, 6))
        #     } else {
        #       return(x)  # 如果列不是数值类型，则保持不变
        #     }
        #   }))
        #   preprocessed_data <<- df_scaled
        # }
        
        # 替换为原始的变量名称
        header <- c()
        for (i in 1:length(colnames(preprocessed_data))) {
          header <- append(header, as.character(mapping[[colnames(preprocessed_data)[i]]][1]))
        }
        preprocessed_data[[1]] <- as.factor(preprocessed_data[[1]])
        colnames(preprocessed_data) <- header
        rownames(preprocessed_data) <- preprocessed_data[, 1]
        
        # 3. 根据选定的分组列名，新增一列Group补充到matrix数据中, Group作为第二列
        dataset_withgroup <- preprocessed_data
        dataset_withgroup <- cbind(dataset_withgroup[1], 
                                   Group = dataset_attachment_sample_group[[input$settings_groupcol_selection]],
                                   dataset_withgroup[2:ncol(dataset_withgroup)])
        
        # 4. 根据选择的数据分组进行数据筛选
        selected_group_data <- dataset_withgroup[dataset_withgroup$Group %in% input$select_data_group, ]
        preprocessed_data_selected_group <<- selected_group_data
        
        # result table of preprocessed data
        output$preprocessed_data <- DT::renderDT({
          DT::datatable({
            preprocessed_data_selected_group
          },
          options = dataTableOptions_pageLength25,
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        output$preprocessed_data_multipletest <- DT::renderDT({
          DT::datatable({
            preprocessed_data_selected_group
          },
          options = dataTableOptions_pageLength25,
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        # 判断input$select_data_group勾选了几个分组
        if (input$test_method == "Student's t-test" || input$test_method == "Paired t test" || 
            input$test_method == "Wilcoxon singed-rank test" || input$test_method == "Welch's t-test" || 
            input$test_method == "Mann-Whitney U test" || input$test_method == "Kolmogorov-Smirnov test") {
          if (length(input$select_data_group) == 2) {
            
            shinyjs::show("result-overview")
            shinyjs::show("result-preprocessed-data")
            shinyjs::show("result-single-test-table")
            shinyjs::show("result-boxplot")
            shinyjs::show("result-zscoreplot")
            shinyjs::show("result-volcanoplot")
            shinyjs::show("result-categorydistributionplot")
            shinyjs::show("result-roseplot")
            
            result_single_test_table <<- doSingleTest(method = input$test_method, data = selected_group_data,
                                                      classA = input$select_data_group[1], classB = input$select_data_group[2], 
                                                      logtransform = FALSE)
            output$result_single_test_table <- DT::renderDT({
              DT::datatable({
                result_single_test_table
              },
              options = dataTableOptions_pageLength25,
              selection = 'none',
              style = 'bootstrap4',
              class = 'cell-border stripe compact datatable',
              rownames = FALSE
              )
            })
          } else {
            toastr_warning(title = paste0("Test method ", input$test_method, " is only applicable to two sets of data, but you have chosen ", 
                                          length(input$select_data_group)), message = '')
          }
        } else if (input$test_method == "One-way ANOVA" || input$test_method == "Welch's ANOVA" || 
                   input$test_method == "Kruskal-Wallis test") {
          
          shinyjs::show("result-overview-multipletest")
          shinyjs::show("result-preprocessed-data-multipletest")
          shinyjs::show("result-table-multipletest")
          shinyjs::show("result-boxplot-multipletest")
          
          result_multiple_test_table <<- doMultipleTest(method = input$test_method, data = selected_group_data)
          output$result_multiple_test_table <- DT::renderDT({
            DT::datatable({
              result_multiple_test_table
            },
            options = dataTableOptions_pageLength25,
            selection = 'none',
            style = 'bootstrap4',
            class = 'cell-border stripe compact datatable',
            rownames = FALSE
            )
          })
        }
        
        # Overview
        experiment_design <- input$experiment_design
        settings_data_transformation <- input$settings_data_transformation
        select_data_group <- paste(input$select_data_group, collapse = ", ")
        normal_distribution <- input$normal_distribution
        test_method <- input$test_method
        overviewUI <- renderUI(
          tags$div(
            style = 'display:flex; gap:20px; margin-top:15px;',
            tags$div(
              class = 'overview-desc-card',
              style = "height:auto; min-height:240px; box-shadow:none;",
              tags$div(
                style = 'display:flex; align-items:center; gap:10px;',
                tags$img(src = './data-desc.png', style = 'width:25px;'),
                tags$div('Data description', class = 'overview-desc-title')
              ),
              tags$div(
                class = 'overview-desc-detail',
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of samples(total):'),
                  tags$span(nrow(origdataset), class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of metabolites'),
                  tags$span(ncol(origdataset) - 1, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Log method:'),
                  tags$span(settings_data_transformation, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Grouping Comparison:'),
                  tags$span(select_data_group, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of samples(selected):'),
                  tags$span(nrow(selected_group_data), class = 'overview-desc-item-highlight')
                )
              )
            ),
            tags$div(
              class = 'overview-desc-card',
              style = "height:auto; min-height:240px; box-shadow:none;",
              tags$div(
                style = 'display:flex; align-items:center; gap:10px;',
                tags$img(src = './result-desc.png', style = 'width:25px;'),
                tags$div('Method information', class = 'overview-desc-title')
              ),
              tags$div(
                class = 'overview-desc-detail',
                tags$div(
                  icon('calendar-check'),
                  tags$span('Experiment Design:'),
                  tags$span(experiment_design, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Normal Distribution:'),
                  tags$span(normal_distribution, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Test Method:'),
                  tags$span(test_method, class = 'overview-desc-item-highlight')
                )
              )
            )
          )
        )
        output$result_overview <- overviewUI
        output$result_overview_multipletest <- overviewUI
        
        # Box Plot - SingleTest
        output$singletest_boxplot_selected_variable_ui <- renderUI({
          selectInput(inputId = ns("singletest_boxplot_selected_variable"), label = "Select variable",
                      choices = colnames(selected_group_data)[3:ncol(selected_group_data)],
                      selected = colnames(selected_group_data)[3])
        })
        output$result_boxplot <- renderPlotly({
          if (is.null(input$singletest_boxplot_selected_variable) || is.null(result_single_test_table)) {
            return(NULL)
          }
          
          # for ggplot
          singletest_boxplot <<- drawBoxPlot(data = selected_group_data,
                                             testResult = result_single_test_table,
                                             selectVar = input$singletest_boxplot_selected_variable,
                                             title = input$singletest_boxplot_title_text,
                                             subtitle = input$singletest_boxplot_subtitle,
                                             palettename = input$singletest_boxplot_palette,
                                             themeXAxisFontSize = input$singletest_boxplot_xaxis_fontsize,
                                             themeXAxisFontFamily = input$singletest_boxplot_xaxis_fontfamily,
                                             themeYAxisFontSize = input$singletest_boxplot_yaxis_fontsize,
                                             themeYAxisFontFamily = input$singletest_boxplot_yaxis_fontfamily,
                                             themeTitleFontSize = input$singletest_boxplot_title_fontsize,
                                             themeTitleFontFamily = input$singletest_boxplot_title_fontfamily,
                                             themeTitlePos = input$singletest_boxplot_title_position
                                             )
          
          # for plotly
          boxplotly <- drawBoxPlotly(data = selected_group_data,
                                     testResult = result_single_test_table, 
                                     selectVar = input$singletest_boxplot_selected_variable,
                                     title = input$singletest_boxplot_title_text,
                                     subtitle = input$singletest_boxplot_subtitle,
                                     palettename = input$singletest_boxplot_palette,
                                     themeXAxisFontSize = input$singletest_boxplot_xaxis_fontsize,
                                     themeXAxisFontFamily = input$singletest_boxplot_xaxis_fontfamily,
                                     themeYAxisFontSize = input$singletest_boxplot_yaxis_fontsize,
                                     themeYAxisFontFamily = input$singletest_boxplot_yaxis_fontfamily,
                                     themeTitleFontSize = input$singletest_boxplot_title_fontsize,
                                     themeTitleFontFamily = input$singletest_boxplot_title_fontfamily,
                                     themeTitlePos = input$singletest_boxplot_title_position
                                     )
          
          # 获取p-value/q-value作为subtitle
          subtitle_value <- result_single_test_table %>%
            filter(Variable == input$singletest_boxplot_selected_variable) %>%
            pull(sym(input$singletest_boxplot_subtitle))
          subtitle <- paste0(input$singletest_boxplot_subtitle, ": ", format(subtitle_value, scientific = TRUE))
          
          singletest_boxplotly <<- config(ggplotly(boxplotly, tooltip = c("label1", "label2")), displayModeBar = FALSE) %>% 
            layout(width = input$singletest_result_boxplot_width,
                   height = input$singletest_result_boxplot_height,
                   title = list(text = paste0(input$singletest_boxplot_title_text,
                                              '<br>',
                                              '<span style="font-size:16px; margin-top:8px; margin-bottom:16px;">', subtitle,'</span>'))
                   )
          return(singletest_boxplotly)
        })
        
        # Box plot - MultipleTest
        output$multipletest_boxplot_selected_variable_ui <- renderUI({
          selectInput(inputId = ns("multipletest_boxplot_selected_variable"), label = "Select variable",
                      choices = colnames(selected_group_data)[3:ncol(selected_group_data)],
                      selected = colnames(selected_group_data)[3])
        })
        output$result_boxplot_multipletest <- renderPlotly({
          if (is.null(input$multipletest_boxplot_selected_variable) || is.null(result_multiple_test_table)) {
            return(NULL)
          }
          
          # for ggplot
          multipletest_boxplot <<- drawBoxPlot(data = selected_group_data,
                                               testResult = result_multiple_test_table,
                                               selectVar = input$multipletest_boxplot_selected_variable,
                                               title = input$multipletest_boxplot_title_text,
                                               subtitle = input$multipletest_boxplot_subtitle,
                                               palettename = input$multipletest_boxplot_palette,
                                               themeXAxisFontSize = input$multipletest_boxplot_xaxis_fontsize,
                                               themeXAxisFontFamily = input$multipletest_boxplot_xaxis_fontfamily,
                                               themeYAxisFontSize = input$multipletest_boxplot_yaxis_fontsize,
                                               themeYAxisFontFamily = input$multipletest_boxplot_yaxis_fontfamily,
                                               themeTitleFontSize = input$multipletest_boxplot_title_fontsize,
                                               themeTitleFontFamily = input$multipletest_boxplot_title_fontfamily,
                                               themeTitlePos = input$multipletest_boxplot_title_position
                                               )
          
          # for plotly
          boxplotly <- drawBoxPlotly(data = selected_group_data,
                                     testResult = result_multiple_test_table, 
                                     selectVar = input$multipletest_boxplot_selected_variable,
                                     title = input$multipletest_boxplot_title_text,
                                     subtitle = input$multipletest_boxplot_subtitle,
                                     palettename = input$multipletest_boxplot_palette,
                                     themeXAxisFontSize = input$multipletest_boxplot_xaxis_fontsize,
                                     themeXAxisFontFamily = input$multipletest_boxplot_xaxis_fontfamily,
                                     themeYAxisFontSize = input$multipletest_boxplot_yaxis_fontsize,
                                     themeYAxisFontFamily = input$multipletest_boxplot_yaxis_fontfamily,
                                     themeTitleFontSize = input$multipletest_boxplot_title_fontsize,
                                     themeTitleFontFamily = input$multipletest_boxplot_title_fontfamily,
                                     themeTitlePos = input$multipletest_boxplot_title_position
                                     )
          
          # 获取p-value/q-value作为subtitle
          subtitle_value <- result_multiple_test_table %>%
            filter(Variable == input$multipletest_boxplot_selected_variable) %>%
            pull(sym(input$multipletest_boxplot_subtitle))
          subtitle <- paste0(input$multipletest_boxplot_subtitle, ": ", format(subtitle_value, scientific = TRUE))
          
          multipletest_boxplotly <<- config(ggplotly(boxplotly, tooltip = c("label1", "label2")), displayModeBar = FALSE) %>% 
            layout(width = input$multipletest_result_boxplot_width,
                   height = input$multipletest_result_boxplot_height,
                   title = list(text = paste0(input$multipletest_boxplot_title_text,
                                              '<br>',
                                              '<span style="font-size:16px; margin-top:8px; margin-bottom:16px;">', subtitle,'</span>'))
            )
          return(multipletest_boxplotly)
        })
        
        # Z-Score plot
        output$result_zscoreplot <- renderPlotly({
          singletest_zscoreplot <<- drawZScorePlot(selected_group_data,
                                                   result_single_test_table,
                                                   input$select_data_group[1],
                                                   input$select_data_group[2],
                                                   logtransform = FALSE,
                                                   pValueFilter = input$singletest_zscoreplot_data_pvalue_enable,
                                                   qValueFilter = input$singletest_zscoreplot_data_qvalue_enable,
                                                   pmin = input$singletest_zscoreplot_data_pvalue_min,
                                                   pmax = input$singletest_zscoreplot_data_pvalue_max,
                                                   qmin = input$singletest_zscoreplot_data_qvalue_min,
                                                   qmax = input$singletest_zscoreplot_data_qvalue_max,
                                                   palette = c(input$singletest_zscoreplot_data_pointcolor1, input$singletest_zscoreplot_data_pointcolor2),
                                                   pointSize = input$singletest_zscoreplot_data_pointsize,
                                                   xlab_text = input$singletest_zscoreplot_yaxis_text,
                                                   ylab_text = input$singletest_zscoreplot_xaxis_text,
                                                   title_text = input$singletest_zscoreplot_title_text,
                                                   xlab_fontsize = input$singletest_zscoreplot_xaxis_fontsize,
                                                   ylab_fontsize = input$singletest_zscoreplot_yaxis_fontsize,
                                                   title_fontsize = input$singletest_zscoreplot_title_fontsize,
                                                   title_position = input$singletest_zscoreplot_title_position
                                                   )
          singletest_zscoreplotly <<- config(ggplotly(singletest_zscoreplot, tooltip = c("sample", "variable")), displayModeBar = FALSE) %>%
            layout(width = input$singletest_result_zscoreplot_width, 
                   height = input$singletest_result_zscoreplot_height,
                   legend = list(yanchor = "middle", y = 0.5))
          return(singletest_zscoreplotly)
        })
        
        # Volcano plot
        output$result_volcanoplot <- renderPlotly({
          singletest_volcanoplot <<- drawVolcanoPlot(data = result_single_test_table, yFieldName = input$singletest_volcanoplot_data_yfield,
                                                     pqvalue_cutoff = input$singletest_volcanoplot_data_pqvalue_cutoff,
                                                     foldchange_cutoff = input$singletest_volcanoplot_data_foldchange_cutoff,
                                                     point_color_palette = c(input$singletest_volcanoplot_data_pointcolor1, 
                                                                             input$singletest_volcanoplot_data_pointcolor2,
                                                                             input$singletest_volcanoplot_data_pointcolor3, 
                                                                             input$singletest_volcanoplot_data_pointcolor4),
                                                     point_size = input$singletest_volcanoplot_data_pointsize,
                                                     xlab_text = input$singletest_volcanoplot_xaxis_text,
                                                     ylab_text = input$singletest_volcanoplot_yaxis_text,
                                                     xlab_fontsize = input$singletest_volcanoplot_xaxis_fontsize,
                                                     ylab_fontsize = input$singletest_volcanoplot_yaxis_fontsize,
                                                     title_text = input$singletest_volcanoplot_title_text,
                                                     title_fontsize = input$singletest_volcanoplot_title_fontsize,
                                                     title_position = input$singletest_volcanoplot_title_position
                                                     )
          singletest_volcanoplotly <<- config(ggplotly(singletest_volcanoplot, tooltip = c("varname", "xlabel", "ylabel")), displayModeBar = FALSE) %>%
            layout(width = input$singletest_result_volcanoplot_width, 
                   height = input$singletest_result_volcanoplot_height,
                   legend = list(yanchor = "middle", y = 0.5))
          
          return(singletest_volcanoplotly)
        })
        
        # 种类统计图-count
        output$result_categorydistributionplot_count <- renderPlotly({
          plot <- drawCategoryDistributionPlot(test_result = result_single_test_table, 
                                               var_class = dataset_attachment_var_class,
                                               pValueFilter = input$singletest_categorydistributionplot_data_pvalue_enable,
                                               qValueFilter = input$singletest_categorydistributionplot_data_qvalue_enable,
                                               pmin = input$singletest_categorydistributionplot_data_pvalue_min,
                                               pmax = input$singletest_categorydistributionplot_data_pvalue_max,
                                               qmin = input$singletest_categorydistributionplot_data_qvalue_min,
                                               qmax = input$singletest_categorydistributionplot_data_qvalue_max,
                                               showZeroCount = input$singletest_categorydistributionplot_data_bar_showallclasses,
                                               palette = input$singletest_categorydistributionplot_data_barcolor,
                                               title1 = input$singletest_categorydistributionplot_count_title_text,
                                               title2 = input$singletest_categorydistributionplot_ratio_title_text,
                                               title_fontsize = input$singletest_categorydistributionplot_title_fontsize,
                                               title_position = input$singletest_categorydistributionplot_title_position,
                                               ylab1 = input$singletest_categorydistributionplot_count_yaxis_text,
                                               ylab2 = input$singletest_categorydistributionplot_ratio_yaxis_text,
                                               ylab_fontsize = input$singletest_categorydistributionplot_yaxis_fontsize,
                                               xlab_fontsize = input$singletest_categorydistributionplot_xaxis_fontsize)
          singletest_categorydistribution_countplot <<- plot$count
          singletest_categorydistribution_countplotly <<- config(ggplotly(singletest_categorydistribution_countplot), displayModeBar = FALSE) %>%
            layout(width = input$singletest_result_categorydistributionplot_width,
                   height = input$singletest_result_categorydistributionplot_height)
          return(singletest_categorydistribution_countplotly)
        })
        
        # 种类统计图-ratio
        output$result_categorydistributionplot_ratio <- renderPlotly({
          plot <- drawCategoryDistributionPlot(test_result = result_single_test_table, 
                                               var_class = dataset_attachment_var_class,
                                               pValueFilter = input$singletest_categorydistributionplot_data_pvalue_enable,
                                               qValueFilter = input$singletest_categorydistributionplot_data_qvalue_enable,
                                               pmin = input$singletest_categorydistributionplot_data_pvalue_min,
                                               pmax = input$singletest_categorydistributionplot_data_pvalue_max,
                                               qmin = input$singletest_categorydistributionplot_data_qvalue_min,
                                               qmax = input$singletest_categorydistributionplot_data_qvalue_max,
                                               showZeroCount = input$singletest_categorydistributionplot_data_bar_showallclasses,
                                               palette = input$singletest_categorydistributionplot_data_barcolor,
                                               title1 = input$singletest_categorydistributionplot_count_title_text,
                                               title2 = input$singletest_categorydistributionplot_ratio_title_text,
                                               title_fontsize = input$singletest_categorydistributionplot_title_fontsize,
                                               title_position = input$singletest_categorydistributionplot_title_position,
                                               ylab1 = input$singletest_categorydistributionplot_count_yaxis_text,
                                               ylab2 = input$singletest_categorydistributionplot_ratio_yaxis_text,
                                               ylab_fontsize = input$singletest_categorydistributionplot_yaxis_fontsize,
                                               xlab_fontsize = input$singletest_categorydistributionplot_xaxis_fontsize)
          singletest_categorydistribution_ratioplot <<- plot$ratio
          singletest_categorydistribution_ratioplotly <<- config(ggplotly(singletest_categorydistribution_ratioplot), displayModeBar = FALSE) %>%
            layout(width = input$singletest_result_categorydistributionplot_width,
                   height = input$singletest_result_categorydistributionplot_height)
          return(singletest_categorydistribution_ratioplotly)
        })
        
        # 环形玫瑰图
        output$result_roseplot <- renderPlot({
          singletest_roseplot <<- drawCircularBarPlot(dataset = result_single_test_table,
                                                      var_class = dataset_attachment_var_class,
                                                      metric = input$singletest_roseplot_data_metric,
                                                      metric_min = input$singletest_roseplot_data_metric_min,
                                                      metric_max = input$singletest_roseplot_data_metric_max,
                                                      pvalueFilter = input$singletest_roseplot_data_pvalue_enable,
                                                      pmin = input$singletest_roseplot_data_pvalue_min,
                                                      pmax = input$singletest_roseplot_data_pvalue_max,
                                                      qvalueFilter = input$singletest_roseplot_data_qvalue_enable,
                                                      qmin = input$singletest_roseplot_data_qvalue_min,
                                                      qmax = input$singletest_roseplot_data_qvalue_max,
                                                      palettename = input$singletest_roseplot_data_barcolor,
                                                      showclassboundary = input$singletest_roseplot_data_bar_showboundaries,
                                                      showlegend = input$singletest_roseplot_data_bar_showlegend,
                                                      showbaseline = input$singletest_roseplot_data_bar_showbaseline,
                                                      baseline_value = input$singletest_roseplot_data_bar_baseline_value,
                                                      fontsize = input$singletest_roseplot_fontsize)
          return(singletest_roseplot)
        }, width = function() {
          singletest_result_roseplot_width <<- input$singletest_result_roseplot_width
          return(singletest_result_roseplot_width)
        }, height = function() {
          singletest_result_roseplot_height <<- input$singletest_result_roseplot_height
          return(singletest_result_roseplot_height)
        })
        
      }
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "运行时遇到错误")
    })
  })
  
  
  ##########################################
  # Reactive
  ##########################################
  # 切换Experiment Design和Normal Distribution时, 显示对应的Test Method
  observeEvent(input$experiment_design, {
    if (input$experiment_design == 'Unpaired Two') {
      if (input$normal_distribution == 'Yes') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', selected = "Student's t-test",
                      choices = c("Student's t-test", "Welch's t-test"))
        })
      } else {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', 
                      choices = c("Mann-Whitney U test", "Kolmogorov-Smirnov test"))
        })
      }
    } else if (input$experiment_design == 'Paired Two') {
      if (input$normal_distribution == 'Yes') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', choices = c("Paired t test"))
        })
      } else {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', choices = c("Wilcoxon singed-rank test"))
        })
      }
    } else if (input$experiment_design == 'Multiple Groups') {
      if (input$normal_distribution == 'Yes') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', choices = c("One-way ANOVA", "Welch's ANOVA"))
        })
      } else {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', choices = c("Kruskal-Wallis test"))
        })
      }
    }
  })
  
  observeEvent(input$normal_distribution, {
    if (input$normal_distribution == 'Yes') {
      if (input$experiment_design == 'Unpaired Two') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', selected = "Student's t-test",
                      choices = c("Welch's t-test", "Student's t-test"))
        })
      } else if (input$experiment_design == 'Paired Two') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', 
                      choices = c("Paired t test"))
        })
      } else if (input$experiment_design == 'Multiple Groups') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', 
                      choices = c("One-way ANOVA", "Welch's ANOVA"))
        })
      }
    } else {
      if (input$experiment_design == 'Unpaired Two') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', 
                      choices = c("Mann-Whitney U test", "Kolmogorov-Smirnov test"))
        })
      } else if (input$experiment_design == 'Paired Two') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', 
                      choices = c("Wilcoxon singed-rank test"))
        })
      } else if (input$experiment_design == 'Multiple Groups') {
        output$test_method_ui <- renderUI({
          selectInput(inputId = ns('test_method'), label = '', 
                      choices = c("Kruskal-Wallis test"))
        })
      }
    }
  })
  
  # Box Plot
  # 切换select variable时，自动更新plot title
  observeEvent(input$singletest_boxplot_selected_variable, {
    updateTextInput(inputId = "singletest_boxplot_title_text", value = input$singletest_boxplot_selected_variable)
  })
  observeEvent(input$multipletest_boxplot_selected_variable, {
    updateTextInput(inputId = "multipletest_boxplot_title_text", value = input$multipletest_boxplot_selected_variable)
  })
  
  # Z-Score plot p-value/q-value 过滤控制
  observeEvent(input$singletest_zscoreplot_data_pvalue_enable, {
    if (input$singletest_zscoreplot_data_pvalue_enable == FALSE) {
      shinyjs::disable("singletest_zscoreplot_data_pvalue_min")
      shinyjs::disable("singletest_zscoreplot_data_pvalue_max")
    } else {
      shinyjs::enable("singletest_zscoreplot_data_pvalue_min")
      shinyjs::enable("singletest_zscoreplot_data_pvalue_max")
    }
  })
  observeEvent(input$singletest_zscoreplot_data_qvalue_enable, {
    if (input$singletest_zscoreplot_data_qvalue_enable == FALSE) {
      shinyjs::disable("singletest_zscoreplot_data_qvalue_min")
      shinyjs::disable("singletest_zscoreplot_data_qvalue_max")
    } else {
      shinyjs::enable("singletest_zscoreplot_data_qvalue_min")
      shinyjs::enable("singletest_zscoreplot_data_qvalue_max")
    }
  })
  # volcano plot y-field字段选择
  observeEvent(input$singletest_volcanoplot_data_yfield, {
    if (input$singletest_volcanoplot_data_yfield == "P-value") {
      updateNumericInput(inputId = 'singletest_volcanoplot_data_pqvalue_cutoff', label = "P-value cutoff")
      updateTextInput(inputId = 'singletest_volcanoplot_yaxis_text', value = '-Log10(P-value)')
    } else {
      updateNumericInput(inputId = 'singletest_volcanoplot_data_pqvalue_cutoff', label = "Q-value cutoff")
      updateTextInput(inputId = 'singletest_volcanoplot_yaxis_text', value = '-Log10(Q-value)')
    }
  })
  # 种类统计图
  observeEvent(input$singletest_categorydistributionplot_data_bar_display, {
    if (input$singletest_categorydistributionplot_data_bar_display == 'Row') {
      js$updateStyle(id = 'difference_analysis_categorydistributionplot', property = 'flex-direction', style = 'row')
    } else {
      js$updateStyle(id = 'difference_analysis_categorydistributionplot', property = 'flex-direction', style = 'column')
      js$updateStyle(id = 'difference_analysis_categorydistributionplot', property = 'gap', style = '20px')
    }
  })
  
  
  #------------------------------------------------
  # Export & Download
  #------------------------------------------------
  # result table - single test
  output$export_single_test_result_table <- downloadHandler(
    filename = function() {
      paste0(input$test_method, ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(result_single_test_table, file, asTable = TRUE)
    }
  )
  # result table - multiple test
  output$export_result_table_multipletest <- downloadHandler(
    filename = function() {
      paste0(input$test_method, ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(result_multiple_test_table, file, asTable = TRUE)
    }
  )
  # 预处理结果 - single test
  output$export_preprocessed_data_singletest <- downloadHandler(
    filename = function() {
      paste0("preprocessed-data", ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(preprocessed_data_selected_group, file, asTable = TRUE)
    }
  )
  # 预处理结果 - multiple test
  output$export_preprocessed_data_multipletest <- downloadHandler(
    filename = function() {
      paste0("preprocessed-data", ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(preprocessed_data_selected_group, file, asTable = TRUE)
    }
  )
  
  # boxplot - singletest
  observeEvent(input$export_singletest_result_boxplot, {
    showModal(modalDialog(
      title = "Export plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_singletest_result_boxplot_format"), label = "Choose format", 
                    choices = c("html" = ".html", "png" = ".png", "tiff" = ".tiff", "jpeg" = ".jpeg", "pdf" = ".pdf"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_singletest_result_boxplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  output$export_singletest_result_boxplot_ok <- downloadHandler(
    filename = function() {
      paste("boxplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_singletest_result_boxplot_format, sep="")
    },
    content = function(file) {
      if (input$export_singletest_result_boxplot_format == ".html") {
        saveWidget(singletest_boxplotly, file = file)
      } else {
        dpi <- 96
        ggsave(filename = file, 
               plot = singletest_boxplot,
               width = input$singletest_result_boxplot_width / dpi,
               height = input$singletest_result_boxplot_height / dpi,
               dpi = dpi)
      }
      removeModal()
    }
  )
  # boxplot - multipletest
  observeEvent(input$export_multipletest_result_boxplot, {
    showModal(modalDialog(
      title = "Export plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_multipletest_result_boxplot_format"), label = "Choose format", 
                    choices = c("html" = ".html", "png" = ".png", "tiff" = ".tiff", "jpeg" = ".jpeg", "pdf" = ".pdf"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_multipletest_result_boxplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  output$export_multipletest_result_boxplot_ok <- downloadHandler(
    filename = function() {
      paste("boxplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_multipletest_result_boxplot_format, sep="")
    },
    content = function(file) {
      if (input$export_multipletest_result_boxplot_format == ".html") {
        saveWidget(multipletest_boxplotly, file = file)
      } else {
        dpi <- 96
        ggsave(filename = file, 
               plot = multipletest_boxplot,
               width = input$multipletest_result_boxplot_width / dpi,
               height = input$multipletest_result_boxplot_height / dpi,
               dpi = dpi)
      }
      removeModal()
    }
  )
  
  #Z-Score plot
  observeEvent(input$export_singletest_result_zscoreplot, {
    showModal(modalDialog(
      title = "Export plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_singletest_result_zscoreplot_format"), label = "Choose format", 
                    choices = c("html" = ".html", "png" = ".png", "tiff" = ".tiff", "jpeg" = ".jpeg", "pdf" = ".pdf"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_singletest_result_zscoreplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  output$export_singletest_result_zscoreplot_ok <- downloadHandler(
    filename = function() {
      paste("zscoreplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_singletest_result_zscoreplot_format, sep="")
    },
    content = function(file) {
      if (input$export_singletest_result_zscoreplot_format == ".html") {
        saveWidget(singletest_zscoreplotly, file = file)
      } else {
        dpi <- 96
        ggsave(filename = file, 
               plot = singletest_zscoreplot,
               width = input$singletest_result_zscoreplot_width / dpi,
               height = input$singletest_result_zscoreplot_height / dpi,
               dpi = dpi)
      }
      removeModal()
    }
  )
  
  #Volcano plot
  observeEvent(input$export_singletest_result_volcanoplot, {
    showModal(modalDialog(
      title = "Export plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_singletest_result_volcanoplot_format"), label = "Choose format", 
                    choices = c("html" = ".html", "png" = ".png", "tiff" = ".tiff", "jpeg" = ".jpeg", "pdf" = ".pdf"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_singletest_result_volcanoplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  output$export_singletest_result_volcanoplot_ok <- downloadHandler(
    filename = function() {
      paste("volcanoplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_singletest_result_volcanoplot_format, sep="")
    },
    content = function(file) {
      if (input$export_singletest_result_volcanoplot_format == ".html") {
        saveWidget(singletest_volcanoplotly, file = file)
      } else {
        dpi <- 96
        ggsave(filename = file, 
               plot = singletest_volcanoplot,
               width = input$singletest_result_volcanoplot_width / dpi,
               height = input$singletest_result_volcanoplot_height / dpi,
               dpi = dpi)
      }
      removeModal()
    }
  )
  
  #种类统计图
  observeEvent(input$export_singletest_result_categorydistributionplot, {
    showModal(modalDialog(
      title = "Export plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_singletest_result_categorydistributionplot_plotname"), label = "Choose subplot", 
                    choices = c("count subplot", "ratio subplot"), selected = "count subplot"),
        selectInput(inputId = ns("export_singletest_result_categorydistributionplot_format"), label = "Choose format", 
                    choices = c("html" = ".html", "png" = ".png", "tiff" = ".tiff", "jpeg" = ".jpeg", "pdf" = ".pdf"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_singletest_result_categorydistributionplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  output$export_singletest_result_categorydistributionplot_ok <- downloadHandler(
    filename = function() {
      paste("category-distribution-plot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_singletest_result_categorydistributionplot_format, sep="")
    },
    content = function(file) {
      if (input$export_singletest_result_categorydistributionplot_format == ".html") {
        if (input$export_singletest_result_categorydistributionplot_plotname == "count subplot") {
          saveWidget(singletest_categorydistribution_countplotly, file = file)
        } else {
          saveWidget(singletest_categorydistribution_ratioplotly, file = file)
        }
      } else {
        dpi <- 96
        if (input$export_singletest_result_categorydistributionplot_plotname == "count subplot") {
          ggsave(filename = file, 
                 plot = singletest_categorydistribution_countplot,
                 width = input$singletest_result_categorydistributionplot_width / dpi,
                 height = input$singletest_result_categorydistributionplot_height / dpi,
                 dpi = dpi)
        } else {
          ggsave(filename = file,
                 plot = singletest_categorydistribution_ratioplot,
                 width = input$singletest_result_categorydistributionplot_width / dpi,
                 height = input$singletest_result_categorydistributionplot_height / dpi,
                 dpi = dpi)
        }
      }
      removeModal()
    }
  )
  
  #rose plot
  observeEvent(input$export_singletest_result_roseplot, {
    showModal(modalDialog(
      title = "Export plot",
      size = "m",
      fluidPage(
        selectInput(inputId = ns("export_singletest_result_roseplot_format"), label = "Choose format", 
                    choices = c("png" = ".png", "tiff" = ".tiff", "jpeg" = ".jpeg", "pdf" = ".pdf"), selected = 'html')
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_singletest_result_roseplot_ok"), label = "OK", icon = NULL),
        modalButton(label = "Cancel")
      )
    )
    )
  })
  output$export_singletest_result_roseplot_ok <- downloadHandler(
    filename = function() {
      paste("roseplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_singletest_result_roseplot_format, sep="")
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, 
             plot = singletest_roseplot,
             width = input$singletest_result_roseplot_width / dpi,
             height = input$singletest_result_roseplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  #----------------------------------------------
  # 上传样本x变量矩阵数据
  #----------------------------------------------
  observeEvent(input$dataloader$datapath, {
    tryCatch({
      # dataset with original column names
      origdataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
      dataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE)
      
      mapping <<- data.frame(matrix(ncol = ncol(dataset), nrow = 0), check.names = FALSE)
      mapping <<- rbind(mapping, colnames(origdataset))
      colnames(mapping) <<- colnames(dataset)
      
      output$data_summary_matrix <- renderUI({
        tags$div(
          style = 'margin-top:-15px;',
          tags$span("Sample size: ", style = 'background-color:#5c5c5c; color:white; padding:2px 5px; font-size:12px; border-radius: 4px 0 0 4px;'),
          tags$span(nrow(origdataset), style = 'background-color: #55b599; color:white; padding:2px 5px; font-size:12px; border-radius: 0 4px 4px 0;'),
          tags$span("Number of variables: ", style = 'margin-left:10px; background-color:#5c5c5c; color:white; padding:2px 5px; font-size:12px; border-radius: 4px 0 0 4px;'),
          tags$span(ncol(origdataset) - 1, style = 'background-color:#f37f40; color:white; padding:2px 5px; font-size:12px; border-radius: 0 4px 4px 0;')
        )
      })
    }, error = function (e) {
      print(paste(e))
    })
  })
  
  # 上传样本分组数据
  observeEvent(input$attachmentloader_sample_group$datapath, {
    # dataset with original column names
    tryCatch({
      attachment <- read.csv(input$attachmentloader_sample_group$datapath, stringsAsFactors = TRUE, check.names = FALSE)
      dataset_attachment_sample_group <<- attachment
      
      output$data_summary_sample_class <- renderUI({
        tags$div(
          style = 'margin-top:-15px;',
          tags$span("Sample size: ", style = 'background-color:#5c5c5c; color:white; padding:2px 5px; font-size:12px; border-radius: 4px 0 0 4px;'),
          tags$span(nrow(attachment), style = 'background-color: #55b599; color:white; padding:2px 5px; font-size:12px; border-radius: 0 4px 4px 0;')
        )
      })
      
      # 样本分组列名称选择
      output$groupcol_selection_ui <- renderUI({
        selectInput(inputId = ns('settings_groupcol_selection'), label = "", 
                    choices = colnames(dataset_attachment_sample_group)[2: length(colnames(dataset_attachment_sample_group))])
      })
    }, error = function(e) {
      print(paste(e))
    })
  })
  
  # 样本分组列改变，所有的分类也相应改变
  observeEvent(input$settings_groupcol_selection, {
    classes <- unique(dataset_attachment_sample_group[[input$settings_groupcol_selection]])
    # 数据分组选择, 默认不选择
    output$groups_selection_ui <- renderUI({
      checkboxGroupInput(inputId = ns("select_data_group"), label = "",
                         choices = classes
                         )
    })
  })
  
  # 上传变量分组数据
  observeEvent(input$attachmentloader_var_class$datapath, {
    tryCatch({
      attachment <- read.csv(input$attachmentloader_var_class$datapath, stringsAsFactors = TRUE, check.names = FALSE)
      if (nrow(attachment) > 0 && ncol(attachment) == 2) {
        dataset_attachment_var_class <<- attachment
        output$data_summary_variable_class <- renderUI({
          tags$div(
            style = 'margin-top:-15px;',
            tags$span("Number of variables: ", style = 'background-color:#5c5c5c; color:white; padding:2px 5px; font-size:12px; border-radius: 4px 0 0 4px;'),
            tags$span(nrow(attachment), style = 'background-color:#f37f40; color:white; padding:2px 5px; font-size:12px; border-radius: 0 4px 4px 0;')
          )
        })
        
      } else {
        toastr_warning(message = paste0("附表数据应该仅有两列，第一列为变量名称，第二列为变量所属类别，您上传的数据有", ncol(attachment), "列，请检查！"), 
                       title = "请检查附表数据格式")
      }
    }, error = function(e) {
      print(paste(e))
    })
  })
  
  
  # 样例数据下载
  output$download_sampledata_da_matrix <- downloadHandler(
    filename = "differenceanalysis_matrix.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_matrix.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  output$download_sampledata_da_group <- downloadHandler(
    filename = "differenceanalysis_sample_group.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_sample_group.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  output$download_sampledata_da_class <- downloadHandler(
    filename = "differenceanalysis_var_class.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_var_class.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  callModule(helpServer, "help_analysis", title = "差异分析", size = "l", file = "help/DifferenceAnalysis.md")
  
}


#
#
# 判别boxplot的outlier
#
#
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#
# 绘制Box Plotly
#
# @data
# 
#
drawBoxPlotly <- function(data, testResult, selectVar, palettename, title, subtitle, 
                        themeTitleFontSize, themeTitleFontFamily, themeTitlePos,
                        themeXAxisFontSize, themeXAxisFontFamily, themeYAxisFontSize, themeYAxisFontFamily) {
  if (is.null(selectVar) || is.null(testResult)) {
    return(NULL)
  }
  
  # 将Group列转换为factor以保持顺序
  data[[2]] = factor(data[[2]], levels = unique(data[[2]]))
  # 根据Group列的唯一值的数量取色
  unique_groups <- unique(data[[2]])
  n <- length(unique_groups)
  mycolor <- ggsciColorPalette(palettename = palettename, size = n)
  
  theme_settings <- theme_classic() +
    theme(plot.title = element_text(family = themeTitleFontFamily, size = themeTitleFontSize, vjust = 10, hjust = themeTitlePos),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(color = 'black', size = themeXAxisFontSize, family = themeXAxisFontFamily, face = 'plain'),
          axis.text.y = element_text(color = 'black', size = themeYAxisFontSize, family = themeYAxisFontFamily, face = 'plain'),
          axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
          axis.ticks = element_line(color = 'black', size = 0.5),
          axis.line.x = element_line(linetype = 1, color="black", size = 1),
          axis.line.y = element_line(linetype = 1, color="black", size = 1),
    )
  
  # 添加is_outlier列
  data <- data %>%
    group_by(!!sym(names(data)[2])) %>%
    mutate(is_outlier = ifelse(is_outlier(.data[[selectVar]]), 'Yes', 'No'))
  
  # outlier data filter
  outlier_data <- data %>%
    filter(is_outlier == "Yes")
  
  # 设置outlier points的颜色与box group一致，第二列为group
  points_color <- c()
  index = 1
  for (class in unique(data[[2]])) {
    points_color <- append(points_color, rep(mycolor[index], sum(outlier_data[[2]] == class)))
    index = index + 1
  }
  
  # 获取p-value/q-value作为subtitle
  subtitle_value <- testResult %>%
    filter(Variable == selectVar) %>%
    pull(sym(subtitle))
  subtitle <- paste0(subtitle, ": ", format(subtitle_value, scientific = TRUE))
  
  boxplot <- ggplot(data, aes(x = !!sym(names(data)[2]), y = !!sym(selectVar))) +
    geom_boxplot(colour = mycolor) +
    geom_point(data = outlier_data, aes(group = !!sym(names(data)[2]),
                                        label1 = !!sym(names(data)[1]),
                                        label2 = !!sym(selectVar)),
               size = 1, colour = points_color) +
    theme_settings +
    labs(title = title, subtitle = subtitle, x = "", y = "")
  
  return(boxplot)
}


#
# 绘制Box Plot
#
# @data
# 
#
drawBoxPlot <- function(data, testResult, selectVar, palettename, title, subtitle, 
                        themeTitleFontSize, themeTitleFontFamily, themeTitlePos,
                        themeXAxisFontSize, themeXAxisFontFamily, themeYAxisFontSize, themeYAxisFontFamily) {
  if (is.null(selectVar) || is.null(testResult)) {
    return(NULL)
  }
  
  # 将Group列转换为factor以保持顺序
  data[[2]] = factor(data[[2]], levels = unique(data[[2]]))
  # 根据Group列的唯一值的数量取色
  unique_groups <- unique(data[[2]])
  n <- length(unique_groups)
  mycolor <- ggsciColorPalette(palettename = palettename, size = n)
  
  theme_settings <- theme_classic() +
    theme(plot.title = element_text(family = themeTitleFontFamily, size = themeTitleFontSize, vjust = 10, hjust = themeTitlePos),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(color = 'black', size = themeXAxisFontSize, family = themeXAxisFontFamily, face = 'plain'),
          axis.text.y = element_text(color = 'black', size = themeYAxisFontSize, family = themeYAxisFontFamily, face = 'plain'),
          axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
          axis.ticks = element_line(color = 'black', size = 0.5),
          axis.line.x = element_line(linetype = 1, color="black", size = 1),
          axis.line.y = element_line(linetype = 1, color="black", size = 1),
    )
  
  # 获取p-value/q-value作为subtitle
  subtitle_value <- testResult %>%
    filter(Variable == selectVar) %>%
    pull(sym(subtitle))
  subtitle <- paste0(subtitle, ": ", format(subtitle_value, scientific = TRUE))
  
  boxplot <- ggplot(data, aes(x = !!sym(names(data)[2]), y = !!sym(selectVar))) + 
    stat_boxplot(geom = "errorbar", width = 0.2, colour = mycolor, lwd = 0.7) +  # ggplot需要，ggplotly不需要
    geom_boxplot(colour = mycolor) +
    theme_settings +
    labs(title = title, subtitle = subtitle, x = "", y = "")
  
  return(boxplot)
}


#
# 绘制Z-Score Plot
#
# @data
# @ttest_result
# 
drawZScorePlot <- function(data, ttest_result, group_control, group_test, logtransform = FALSE, pValueFilter, qValueFilter, 
                           pmin = 0, pmax = 1, qmin = 0, qmax = 1, palette = c("mediumseagreen", "firebrick"), 
                           pointSize, title_text = "", title_fontsize = 14, title_position = 0.5,
                           xlab_text = "", xlab_fontsize = 10, ylab_text = "Z-Score", ylab_fontsize = 10) {
  # 是否log变换
  if (logtransform == TRUE) {
    for (i in 2:length(colnames(data))) {
      varname <- colnames(data)[i]
      for (j in 1:length(data[[varname]])) {
        data[[varname]][j] <- log2(data[[varname]][j])
      }
    }
  }
  # pvalue/qvalue筛选
  if (!is.null(pValueFilter) && pValueFilter == TRUE) {
    ttest_result = subset(ttest_result, ttest_result[["P-value"]] > pmin & ttest_result[["P-value"]] < pmax)
  }
  
  if (!is.null(qValueFilter) && qValueFilter == TRUE) {
    ttest_result = subset(ttest_result, ttest_result[["Q-value"]] > qmin & ttest_result[["Q-value"]] < qmax)
  }
  
  if (nrow(ttest_result) == 0) {
    toastr_warning(message = "未找到符合条件的数据")
  } else {
    # data 根据所选特征筛选子集
    data = data[1:nrow(data), c(colnames(data)[1:2], ttest_result[["Variable"]])]
    
    # 筛选出控制组和测试组
    data_control = subset(data, data[[2]] == group_control)
    data_test = subset(data, data[[2]] == group_test)
    
    # zscore
    for (i in 3:length(colnames(data))) {
      varname = colnames(data)[i]
      data_control_new = (data_control[[varname]] - mean(data_control[[varname]])) / sd(data_control[[varname]])
      data_test_new = (data_test[[varname]] - mean(data_control[[varname]])) / sd(data_control[[varname]])
      data_control[[varname]] = data_control_new
      data_test[[varname]] = data_test_new
    }
    
    # 将zscore结果组合
    data_control_test <- data.frame(sample = c(data_control[[1]], data_test[[1]]), group = c(data_control[[2]], data_test[[2]]))
    for (i in 3:length(colnames(data))) {
      varname = colnames(data)[i]
      data_control_test[[varname]] = c(data_control[[varname]], data_test[[varname]])
    }
    
    # 中位数
    m = c()
    for (i in 3:ncol(data_control_test)) { m = append(m, median(data_control_test[[i]])) }
    # 中位数的秩
    r = rank(m)
    
    variable = c()
    value = c()
    sample = c()
    group = c()
    
    for (i in 3:ncol(data_control_test)) {
      for (j in 1:length(r)) {
        # 因为两个数据相差2列:sample, group
        if (r[j] == i - 2) {
          varname = colnames(data_control_test)[j + 2]
          variable = append(variable, rep(varname, nrow(data_control_test)))
          value = append(value, data_control_test[[varname]])
          sample = append(sample, data_control_test[[1]])
          group = append(group, data_control_test[[2]])
          break;
        }
      }
    }
    df <- data.frame(variable = variable, value = value, group = group, sample = sample)
    df[["variable"]] <- factor(df[["variable"]], levels = rev(unique(df[["variable"]])))
    
    plot <- ggplot(data = df, aes(x = variable, y = value, color = group)) +
      geom_point(size = pointSize, aes(sample = sample)) +
      geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
      coord_flip() +
      scale_color_manual(values = palette) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = title_fontsize, hjust = title_position),
            axis.title.x = element_text(color = 'black', size = ylab_fontsize),
            axis.title.y = element_text(color = 'black', size = xlab_fontsize)) +
      labs(title = title_text, x = ylab_text, y = xlab_text)
    return (plot)
  }
}


#
# 绘制火山图
#
# @data
#
drawVolcanoPlot <- function(data, yFieldName = 'P-value', pqvalue_cutoff = 0.05, foldchange_cutoff = 1.01,
                            point_color_palette = c("#3b3b3b", "#e74d4d", "#bebebe", "#4caf50"), 
                            point_size = 1.5, showlegend = FALSE, xlab_text = "Log2(Fold Change)", ylab_text = "-Log10(P-value)",
                            xlab_fontsize = 12, ylab_fontsize = 12, title_text = "Volcano plot", title_fontsize = 14, title_position = 0.5) {
  xFieldName <- 'Fold Change'
  
  ##对满足不同条件的数据给不同的标记，放入condition列，颜色放入color列
  data$condition = ifelse(log2(data[[xFieldName]]) > log2(foldchange_cutoff) & data[[yFieldName]] < pqvalue_cutoff, "up",
                          ifelse(log2(data[[xFieldName]]) < -log2(foldchange_cutoff) & data[[yFieldName]] < pqvalue_cutoff, "up",
                                 ifelse(log2(data[[xFieldName]]) >= -log2(foldchange_cutoff) & log2(data[[xFieldName]] <= log2(foldchange_cutoff)) & data[[yFieldName]] < pqvalue_cutoff, "down",
                                        ifelse(log2(data[[xFieldName]]) < -log2(foldchange_cutoff) & data[[yFieldName]] > pqvalue_cutoff, "green",
                                               ifelse(log2(data[[xFieldName]]) > log2(foldchange_cutoff) & data[[yFieldName]] > pqvalue_cutoff, "green",
                                                      "normal")))))
  
  xmin <- min(log2(data[[xFieldName]]))
  xmax <- max(log2(data[[xFieldName]]))
  ymin <- min(-log10(data[[yFieldName]]))
  ymax <- max(-log10(data[[yFieldName]]))
  
  plot <- ggplot(data = data, aes(x = log2(.data[[xFieldName]]), y = -log10(.data[[yFieldName]]), colour = condition)) +
    geom_hline(yintercept = -log10(pqvalue_cutoff), linetype = 2, size = 0.2) +
    geom_vline(xintercept = c(-log2(foldchange_cutoff), log2(foldchange_cutoff)), linetype = 2, size = 0.2)
  
  if (yFieldName == 'P-value') {
    plot <- plot + 
      geom_point(data = data, size = point_size, aes(varname = !!sym(names(data)[1]), xlabel = !!sym(names(data)[7]), ylabel = !!sym(names(data)[4])))
  } else {
    plot <- plot + 
      geom_point(data = data, size = point_size, aes(varname = !!sym(names(data)[1]), xlabel = !!sym(names(data)[7]), ylabel = !!sym(names(data)[5])))
  }
  
  plot <- plot +
    scale_color_manual(values=c('up' = point_color_palette[1], 'down' = point_color_palette[2], 'normal' = point_color_palette[3], 'green' = point_color_palette[4])) +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          plot.title = element_text(size = title_fontsize, hjust = title_position),
          axis.title.x = element_text(color = 'black', size = xlab_fontsize),
          axis.title.y = element_text(color = 'black', size = ylab_fontsize),
          axis.ticks.x = element_line(color = "black", linewidth = 0.01),
          axis.ticks.y = element_line(color = "black", linewidth = 0.01),
          plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = title_text, x = xlab_text, y = ylab_text) +
    scale_y_continuous(expand = c(0, 0)) +
    xlim(-max(abs(xmin), abs(xmax)) - (xmax - xmin) / 20, max(abs(xmin), abs(xmax)) + (xmax - xmin) / 20) +
    ylim(ymin - (ymax - ymin) / 20, ymax + (ymax - ymin) / 20)
  
  # remove legend
  if (showlegend == FALSE) {
    plot <- plot + theme(legend.position = 'none')
  }
  
  return(plot)
}

# 为变量添加class
attachVarClass <- function(test_result, var_class_df) {
  # prepare data
  variable_class_mapping <- data.frame(matrix(ncol = nrow(test_result), nrow = 0), check.names = FALSE)
  variable_class_mapping <- rbind(variable_class_mapping, as.character(var_class_df[[2]]))
  colnames(variable_class_mapping) <- var_class_df[[1]]
  
  # 变量种类全集
  variable_class_fulllist <- var_class_df[[2]]
  variable_class <- c()
  # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
  for (v in test_result[["Variable"]]) {
    if (v %in% colnames(variable_class_mapping)) {
      variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
    } else {
      toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
      variable_class <- append(variable_class, "unknown")
    }
  }
  # 将变量类别列组合到结果中
  test_result["class"] = variable_class
  
  return(test_result)
}


#
# 绘制种类统计图
# 
#
drawCategoryDistributionPlot <- function(test_result, var_class, pValueFilter = TRUE, qValueFilter = FALSE, pmin = 0, pmax = 0.05, 
                                         qmin = 0, qmax = 0.05, fontsize = 10, palette = 'royalblue', showZeroCount = TRUE,
                                         title1 = "number of differential metabolites", title2 = "change ratio of differential metabolites",
                                         title_fontsize = 14, title_position = 0.5,
                                         ylab1 = "number", ylab2 = "changed metabolites/total of this category(%)",
                                         xlab_fontsize = 12, ylab_fontsize = 12) {
  if (is.null(var_class) || nrow(var_class) == 0) {
    toastr_warning(message = paste0("Please upload metabolite classification data"), title = "Variable class not specified")
  } else {
    tryCatch({
      classenrich_data <- attachVarClass(test_result, var_class)
      # 变量种类全集
      variable_class_fulllist <- var_class[[2]]
      
      filtered_data <- classenrich_data
      
      if (pValueFilter == TRUE) {
        filtered_data <- subset(filtered_data, (filtered_data["P-value"] >= pmin) & (filtered_data["P-value"] <= pmax))
      }
      if (qValueFilter == TRUE) {
        filtered_data <- subset(filtered_data, (filtered_data["Q-value"] >= qmin) & (filtered_data["Q-value"] <= qmax))
      }
      
      # count for each class of full class set
      count <- c()
      classenrich_df_count <- NULL
      if (showZeroCount == TRUE) {
        for (c in as.character(unique(variable_class_fulllist))) {
          num <- 0
          for (i in (1:nrow(filtered_data))) {
            if (as.character(filtered_data[["class"]][i]) == c) {
              num = num + 1
            }
          }
          count <- append(count, num)
        }
        classenrich_df_count <- data.frame(class = as.character(unique(variable_class_fulllist)), count = count)
      } else {
        for (c in as.character(unique(filtered_data[["class"]]))) {
          num <- 0
          for (i in (1:nrow(filtered_data))) {
            if (as.character(filtered_data[["class"]][i]) == c) {
              num = num + 1
            }
          }
          count <- append(count, num)
        }
        classenrich_df_count <- data.frame(class = as.character(unique(filtered_data[["class"]])), count = count)
      }
      
      # sort desc
      classenrich_df_count <- classenrich_df_count[order(classenrich_df_count["count"]),]
      # variable column as factor to keep order
      classenrich_df_count[["class"]] <- factor(classenrich_df_count[["class"]], levels = as.character(classenrich_df_count[["class"]]))
      
      count_all <- c()
      classenrich_df_ratio <- NULL
      if (showZeroCount == TRUE) {
        for (c in as.character(unique(variable_class_fulllist))) {
          # 计算原来数据集每个类有多少个变量
          num <- 0
          for (i in (1:nrow(classenrich_data))) {
            if (as.character(classenrich_data[["class"]][i]) == c) {
              num = num + 1
            }
          }
          count_all <- append(count_all, num)
        }
        classenrich_df_ratio <- data.frame(class = as.character(unique(variable_class_fulllist)),
                                           ratio = count / count_all)
      } else {
        for (c in as.character(unique(filtered_data[["class"]]))) {
          # 计算原来数据集每个类有多少个变量
          num <- 0
          for (i in (1:nrow(classenrich_data))) {
            if (as.character(classenrich_data[["class"]][i]) == c) {
              num = num + 1
            }
          }
          count_all <- append(count_all, num)
        }
        classenrich_df_ratio <- data.frame(class = as.character(unique(filtered_data[["class"]])),
                                           ratio = count / count_all)
      }
      
      # sort desc
      classenrich_df_ratio <- classenrich_df_ratio[order(classenrich_df_ratio["ratio"]),]
      # variable column as factor to keep order
      classenrich_df_ratio[["class"]] <- factor(classenrich_df_ratio[["class"]], levels = as.character(classenrich_df_ratio[["class"]]))
      
      # plot
      plot_count <- ggplot(classenrich_df_count, aes(x = class, y = count)) + 
        geom_bar(stat = "identity", fill = palette) + 
        coord_flip() +
        theme_classic() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = 'none',
              axis.line.x = element_line(linetype = 1, color=palette, size = 1),
              axis.line.y = element_line(linetype = 1, color=palette, size = 1),
              axis.ticks.x = element_line(color=palette, size=1),
              axis.ticks.y = element_line(color=palette, size=1),
              axis.text.x = element_text(size = fontsize),
              axis.text.y = element_text(size = fontsize),
              axis.title.x = element_text(color = 'black', size = xlab_fontsize),
              axis.title.y = element_text(color = 'black', size = ylab_fontsize),
              plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              plot.title = element_text(size = title_fontsize, hjust = title_position)) +
        labs(title = title1, x = "", y = ylab1) +
        scale_y_continuous(expand = c(0.01, 0.01))
      
      plot_ratio <- ggplot(classenrich_df_ratio, aes(x = class, y = ratio)) + 
        geom_bar(stat = "identity", fill=palette) + 
        coord_flip() +
        theme_classic() + 
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = 'none',
              axis.line.x = element_line(linetype = 1, color=palette, size = 1),
              axis.line.y = element_line(linetype = 1, color=palette, size = 1),
              axis.ticks.x = element_line(color=palette, size=1),
              axis.ticks.y = element_line(color=palette, size=1),
              axis.text.x = element_text(size = fontsize), 
              axis.text.y = element_text(size = fontsize),
              axis.title.x = element_text(color = 'black', size = xlab_fontsize),
              axis.title.y = element_text(color = 'black', size = ylab_fontsize),
              plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              plot.title = element_text(size = title_fontsize, hjust = title_position)) +
        labs(title = title2, x = "", y = ylab2) +
        scale_y_continuous(limits = c(0, 1), expand = c(0.0001, 0.01), labels = percent)
      
      return (list(count = plot_count, ratio = plot_ratio))
    }, warning = function(w) {
      toastr_warning(title = "绘制种类统计图失败", message = paste(e))
    }, error = function(e) {
      toastr_error(title = "绘制种类统计图失败", message = paste(e))
    })
  }
}


#
# paired test + unpaired test
#
# @method:
# @data 第一列为样本名，第二列为分组名，从第三列开始为变量
# @classA 第一分组
# @classB 第二分组
# @logtransform log2变换
#
doSingleTest <- function(method, data, classA, classB, logtransform) {
  # 保留一份原始数据备份, foldchange计算需要用到
  dataorig <- data
  if (logtransform == TRUE) {
    for (i in 3:length(colnames(data))) {
      varname <- colnames(data)[i]
      for (j in 1:length(data[[varname]])) {
        data[[varname]][j] <- log2(data[[varname]][j])
      }
    }
  }
  variable <- colnames(data)[3: length(data)]
  data_a <- subset(data, data[2] == classA)
  data_b <- subset(data, data[2] == classB)
  dataorig_a <- subset(dataorig, dataorig[2] == classA)
  dataorig_b <- subset(dataorig, dataorig[2] == classB)
  pvalue <- c()
  foldchange <- c()
  for(i in 3:length(colnames(data))) {
    varname <- colnames(data)[i]
    if (method == "Student's t-test") {
      result <- t.test(data_a[[varname]], data_b[[varname]], alternative = "two.sided", paired = FALSE, var.equal = TRUE)
      pvalue <- append(pvalue, result$p.value)
    } else if (method == "Paired t test") {
      result <- t.test(data_a[[varname]], data_b[[varname]], alternative = "two.sided", paired = TRUE, var.equal = TRUE)
      pvalue <- append(pvalue, result$p.value)
    } else if (method == "Welch's t-test") {
      result <- t.test(data_a[[varname]], data_b[[varname]], var.equal = FALSE)
      pvalue <- append(pvalue, result$p.value)
    } else if (method == "Wilcoxon singed-rank test") {
      result <- wilcox.test(data_a[[varname]], data_b[[varname]], alternative = "two.sided", paired = TRUE)
      pvalue <- append(pvalue, result$p.value)
    } else if (method == "Mann-Whitney U test") {
      result <- wilcox.test(data_a[[varname]], data_b[[varname]], paired = FALSE)
      pvalue <- append(pvalue, result$p.value)
    } else if (method == "Kolmogorov-Smirnov test") {
      result <- ks.test(data_a[[varname]], data_b[[varname]])
      pvalue <- append(pvalue, result$p.value)
    }
    
    # foldchange算法修正
    # if (logtransform == TRUE) {
    #   foldchange <- append(foldchange, 2^(mean(data_a[[varname]]) - mean(data_b[[varname]])))
    # } else {
    #   foldchange <- append(foldchange, mean(data_a[[varname]]) / mean(data_b[[varname]]))
    # }
    foldchange <- append(foldchange, mean(dataorig_a[[varname]]) / mean(dataorig_b[[varname]]))
  }
  qvalue <- p.adjust(pvalue, method = "BH")
  
  df <- data.frame(variable = variable,
                   pvalue = signif(pvalue, digits = 4),
                   qvalue = signif(qvalue, digits = 4),
                   logpvalue = signif(-log10(pvalue), digits = 4),
                   logqvalue = signif(-log10(qvalue), digits = 4),
                   foldchange = signif(foldchange, digits = 4),
                   logfoldchange = signif(log2(foldchange), digits = 4),
                   stringsAsFactors = FALSE)
  colnames(df) <- c("Variable", "P-value", "Q-value", "-Log10(P-value)", "-Log10(Q-value)", "Fold Change", "Log2(Fold Change)")
  return(df)
}


# -------------------------------------------
# Anova test
# -------------------------------------------
doMultipleTest <- function(method, data) {
  tryCatch({
    variable <- c()
    pvalue <- c()
    group <- colnames(data)[2]
    for(i in 3:length(colnames(data))) {
      varname <- colnames(data)[i]
      variable <- append(variable, varname)
      if (method == "One-way ANOVA") {
        aov <- aov(as.formula(paste0("`", varname, "`", "~", group)), data)
        df <- data.frame(unclass(summary(aov)), check.names = FALSE, stringsAsFactors = FALSE)
        pvalue <- append(pvalue, df[["Pr(>F)"]][1])
      } else if (method == "Welch's ANOVA") {
        oneway <- oneway.test(as.formula(paste0("`", varname, "`", "~", group)), data = data, var.equal = FALSE)
        pvalue <- append(pvalue, oneway$p.value)
      } else if (method == "Kruskal-Wallis test") {
        kruskal <- kruskal.test(as.formula(paste0("`", varname, "`", "~", group)), data = data)
        pvalue <- append(pvalue, kruskal$p.value)
      }
    }
    qvalue <- p.adjust(pvalue, method = "BH")
    df <- data.frame(variable = variable,
                     pvalue = signif(pvalue, digits = 4),
                     qvalue = signif(qvalue, digits = 4),
                     stringsAsFactors = FALSE)
    colnames(df) <- c("Variable", "P-value", "Q-value")
    return(df)
  }, warning = function(w) {
    toastr_warning(title = "Multiple test warning", message = paste(e))
  }, error = function(e) {
    toastr_error(title = "Multiple test error", message = paste(e))
  })
  
}


#
# 环形玫瑰图添加类别边界
#
insertClassBoundary <- function(data) {
  colClass <- data[["class"]]
  if (length(unique(colClass)) > 1) {
    uniqueIndex <- c()
    i = 2
    index = 1
    while (i <= length(colClass)) {
      if (colClass[i] != colClass[i - 1]) {
        uniqueIndex[index] = i - 1
        index = index + 1
      }
      i = i + 1
    }
    res = rbind(c(NA, NA, NA), data[1 : uniqueIndex[1], ])
    index = 2
    lastIndex = uniqueIndex[1]
    while (index <= length(uniqueIndex)) {
      res = rbind(res, c(NA, NA, NA), data[(lastIndex + 1) : uniqueIndex[index], ])
      lastIndex = uniqueIndex[index]
      index = index + 1
    }
    res = rbind(res, c(NA, NA, NA), data[(lastIndex + 1) : nrow(data), ])
    return(res)
  } else {
    return(data)
  }
}


#
# 环形玫瑰图
#
#
drawCircularBarPlot <- function(dataset, var_class, metric = "Fold Change", metric_min = -1, metric_max = 4, 
                                pvalueFilter = FALSE, pmin = 0, pmax = 1, 
                                qvalueFilter = FALSE, qmin = 0, qmax = 1, 
                                fontsize = 3, showbaseline = TRUE, baseline_value = 1, 
                                showclassboundary = FALSE, 
                                palettename = "Palette 1", showlegend = FALSE) {
  if (is.null(var_class) || nrow(var_class) == 0) {
    toastr_warning(message = paste0("Please upload metabolite classification data"), title = "Variable class not specified")
  } else {
    
    tryCatch({
      dataset <- attachVarClass(dataset, var_class)
      if (pvalueFilter == TRUE) {
        dataset <- subset(dataset, (dataset["P-value"] >= pmin) & (dataset["P-value"] <= pmax))
      }
      if (qvalueFilter == TRUE) {
        dataset <- subset(dataset, (dataset["Q-value"] >= qmin) & (dataset["Q-value"] <= qmax))
      }
      
      # insert class boundary
      if (showclassboundary) {
        dataset <- insertClassBoundary(dataset)
      }
      
      # add id column
      n_row <- nrow(dataset)
      dataset$id <- seq(1, n_row)
      
      if (n_row %% 2 == 0) {
        slope <- (-360)/(n_row - 1) + 360/(n_row * (n_row - 1))
        intercept <- 90 - (180 / n_row) - slope
        # dataset$angle <- intercept + dataset$id * slope
        dataset$angle <- ifelse(dataset$id <= n_row / 2, 
                                intercept + dataset$id * slope, 
                                intercept + dataset$id * slope + 180)
      } else {
        slope <- (-360)/(n_row - 1) + 360/(n_row * (n_row - 1))
        intercept <- 90 - slope
        # dataset$angle <- intercept + dataset$id * slope
        dataset$angle <- ifelse(dataset$id <= (n_row + 1) / 2, 
                                intercept + dataset$id * slope, 
                                intercept + dataset$id * slope + 180)
      }
      
      plot <- ggplot(dataset, aes(x = as.factor(.data[["id"]]), y = .data[[metric]])) + 
        geom_bar(stat="identity", aes(fill = .data[["class"]])) +
        coord_polar() +
        geom_text(aes(x = .data[["id"]], 
                      y = ifelse(.data[[metric]] < 1.05, 1.1, .data[[metric]] + 0.1),
                      label = .data[["Variable"]], 
                      angle = angle, 
                      hjust = ifelse(id <= ceiling(n_row / 2), "bottom", "top")), 
                  size = fontsize) +
        ylab("") +
        xlab("") +
        theme_light() +
        theme(axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(), 
              axis.text.x = element_blank(), 
              panel.grid = element_blank(),
              panel.border = element_blank()) +
        scale_fill_manual("", values = getColorPalette(palettename, classnum = length(unique(dataset[[3]]))))
      if (showlegend == FALSE) {
        plot <- plot + theme(legend.position = 'none')
      }
      if (showbaseline) {
        plot <- plot + geom_hline(yintercept = baseline_value, linetype = 2, color = "red")
      }
      if (!is.null(metric_min) & !is.null(metric_max)) {
        plot <- plot + ylim(metric_min, metric_max)
      }
      return(plot)
    }, warning = function(w) {
      toastr_warning(title = "绘制玫瑰图失败", message = paste(e))
    }, error = function(e) {
      toastr_error(title = "绘制玫瑰图失败", message = paste(e))
    })
  }
}


shinyApp(ui = differenceAnalysisUI(id = "difference_analysis"), server = differenceAnalysisServer)