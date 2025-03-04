source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


# scoreplot尺寸
opls_result_scoreplot_width <- 1000
opls_result_scoreplot_height <- 600

# loading plot尺寸
opls_result_loadingplot_width <- 1000
opls_result_loadingplot_height <- 600

# bio plot 尺寸
opls_result_bioplot_width <- 600
opls_result_bioplot_height <- 500

# vip loilipop plot 尺寸
opls_result_viploilipopplot_width <- 600
opls_result_viploilipopplot_height <- 500

# vip bubble plot 尺寸
opls_result_vipbubbleplot_width <- 600
opls_result_vipbubbleplot_height <- 500

# splot 尺寸
opls_result_splot_width <- 600
opls_result_splot_height <- 500

# vplot 尺寸
opls_result_vplot_width <- 600
opls_result_vplot_height <- 500

# permutation plot 尺寸
opls_result_permutationplot_width <- 600
opls_result_permutationplot_height <- 500


dimensionReductionOPLSUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useToastr(),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("collapse")),
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$style(type="text/css", ".checkbox {margin-top: 20px;"),
              tags$base(target = "_blank")
    ),
    
    fluidRow(
      box(
        title = "Upload Data",
        status = "danger",
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
          column(width = 4,
                 fileInput(
                   inputId = ns("dataloader"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " Upload metabolite matrix... "),
                   placeholder = "Click button to select, or drag file here.",
                   accept = ".csv"
                 ),
                 uiOutput(ns("data_summary_matrix"))
          ),
          column(width = 4,
                 fileInput(
                   inputId = ns("attachmentloader_sample_group"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " Upload sample grouping data... "),
                   placeholder = "",
                   accept = ".csv"
                 ),
                 uiOutput(ns("data_summary_sample_class"))
          ),
          column(width = 4,
                 fileInput(
                   inputId = ns("attachmentloader_var_class"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " Upload metabolite classification data... "),
                   placeholder = "",
                   accept = ".csv"
                 ),
                 uiOutput(ns("data_summary_variable_class"))
          )
          
        )
      ),
      box(
        id = "box-dr-opls",
        title = "OPLS",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        fluidRow(
          tags$div(
            style = "display:flex; gap:10px; padding-left:15px; padding-right:15px;",
            tags$div(
              style = 'width:250px',
              tags$div(
                style = 'border:1px solid rgba(36, 41, 46, 0.12); border-radius: 5px 5px 0px 0px;',
                buildAccordionItem('settings-group-selection', 'Grouping Selection'),
                uiOutput(outputId = ns('groupcol_selection_ui'))
              ),
              tags$div(
                style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
                buildAccordionItem('settings-group-comparison', 'Grouping Comparison'),
                uiOutput(outputId = ns("groups_selection_ui"))
              ),
              tags$div(
                style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
                buildAccordionItem('settings-data-preparation', 'Data Preparation'),
                tags$div(id = 'settings-data-preparation', class = 'collapse-item-body', style = 'display:block;',
                         selectInput(inputId = ns('settings_data_transformation'), label = 'Transformation', choices = c('None', 'Log2', 'Log10'), selected = 'None'),
                         selectInput(inputId = ns('settings_data_scaling'), label = 'Scaling', choices = c('None', 'Centering', 'Unit Variance scaling', 'Pareto scaling', 'Range scaling', 'Vast scaling', 'Level scaling'), selected = 'None')
                )
              ),
              tags$div(
                class = 'collapse-item-body',
                style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none; border-radius: 0px 0px 5px 5px; padding-top:12px; padding-bottom:12px;',
                actionButton(ns("execute"),
                             label = "Execute",
                             icon = icon("play"),
                             class = "action-button-primary"),
              )
            ),
            tags$div(
              style= "width:calc(100% - 250px)",
              tabsetPanel(
                tabPanel(title = "Overview",
                         icon = icon("sliders"),
                         hidden(div(id = ns("result-overview"),
                                    withSpinner(uiOutput(outputId = ns("opls_result_overview")))
                         ))
                ),
                tabPanel(title = "Preprocessed Data",
                         icon = icon("table"),
                         hidden(div(id = ns("preprocessed-data"),
                                    withSpinner(DT::DTOutput(outputId = ns("preprocessed_data"))),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_preprocessed_data"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                tabPanel(title = "Score plot",
                         icon = tags$i(class = "iconfont icon-a-zhuchengfenfenxiPCA", role="presentation"),
                         hidden(div(id = ns("result-scoreplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 250px)',
                                          withSpinner(plotOutput(outputId = ns("score_plot"),
                                                                 width = "100%",
                                                                 height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-scoreplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_scoreplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_scoreplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-scoreplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-opls-scoreplot', 'overlay-opls-scoreplot')),
                                        
                                        tags$div(
                                          style = 'width: 250px; z-index: 200;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Datapoint settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'datapoint-settings-opls-scoreplot', title = 'Data points', collapsed = TRUE),
                                              tags$div(id = 'datapoint-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # inspect data
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-scoreplot', 'overlay-opls-scoreplot')
                                                       ),
                                                       selectInput(inputId = ns('opls_scoreplot_datapoint_fillcolor'), label = 'Fill color', selected = 'd3', 
                                                                   choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                       numericInput(inputId = ns('opls_scoreplot_datapoint_fillalpha'), label = 'Fill transparency', min = 0.01, max = 1, step = 0.1, value = 1),
                                                       selectInput(inputId = ns('opls_scoreplot_datapoint_border_color'), label = 'Border color', selected = 'd3', 
                                                                   choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                       selectInput(inputId = ns('opls_scoreplot_datapoint_shape'), label = 'Shape', choices = c('Circle' = 16, 'Square' = 15, 'Rhomboid' = 18, 'Triangle' = 17)),
                                                       numericInput(inputId = ns('opls_scoreplot_datapoint_size'), label = 'Size', min = 1, value = 5, step = 1)
                                              )
                                            ),
                                            
                                            # Confidence interval settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'confidence-settings-opls-scoreplot', title = 'Confidence level', collapsed = TRUE),
                                              tags$div(id = 'confidence-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       numericInput(inputId = ns('opls_scoreplot_confidence_interval'), label = 'Confidence level', min = 0, max = 1, step = 0.05, value = 0.95),
                                                       selectInput(inputId = ns('opls_scoreplot_confidence_datacoverage'), label = 'Data coverage', choices = c('For all data' = FALSE, 'For group data' = TRUE)),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'confidence-settings-fill-opls-scoreplot', title = 'Fill'),
                                                         tags$div(
                                                           id = 'confidence-settings-fill-opls-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           # 是否填充颜色
                                                           radioButtons(inputId = ns('opls_scoreplot_confidence_fillornot'), label = '', inline = TRUE, selected = 'noFill', choices = c('No fill' = 'noFill', 'Solid color'= 'solidColor')),
                                                           # 多分组颜色选择器
                                                           selectInput(inputId = ns('opls_scoreplot_confidence_fillcolor_multi'), label = 'Color', selected = 'd3', 
                                                                       choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                           # 单分组颜色选择器
                                                           colourInput(inputId = ns("opls_scoreplot_confidence_fillcolor_single"), label = "Color", value = "grey30",
                                                                       returnName = TRUE, palette = "limited", closeOnClick = TRUE, allowedCols = c(ColorBrewr$custom)),
                                                           numericInput(inputId = ns('opls_scoreplot_confidence_fillalpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 0.1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'confidence-settings-line-opls-scoreplot', title = 'Border'),
                                                         tags$div(
                                                           id = 'confidence-settings-line-opls-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           radioButtons(inputId = ns('opls_scoreplot_confidence_withlineornot'), label = '', choices = c('No border' = 'noLine', 'Solid line' = 'solidLine'), inline = TRUE, selected = 'solidLine'),
                                                           selectInput(inputId = ns('opls_scoreplot_confidence_linetype'), label = 'Type', selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash', 'blank')),
                                                           numericInput(inputId = ns('opls_scoreplot_confidence_lineweight'), label = 'Weight', min = 0, max = 1, step = 0.1, value = 0.5),
                                                           numericInput(inputId = ns('opls_scoreplot_confidence_linealpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 1)
                                                         )
                                                       )
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-scoreplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_scoreplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_scoreplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_scoreplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_scoreplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-scoreplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-scoreplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_scoreplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-scoreplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_scoreplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-scoreplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-scoreplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-scoreplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_scoreplot_xaxis_label'), label = 'Label', value = 't[1]'),
                                                           selectInput(inputId = ns('opls_scoreplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_scoreplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-scoreplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-scoreplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_scoreplot_yaxis_label'), label = 'Label', value = 't[2]'),
                                                           selectInput(inputId = ns('opls_scoreplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_scoreplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),

                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-scoreplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_scoreplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_scoreplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_scoreplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_scoreplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_scoreplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                        )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-scoreplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_scoreplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_scoreplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_scoreplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_scoreplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_scoreplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                tabPanel(title = "Loading plot",
                         hidden(div(id = ns("result-loadingplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotOutput(outputId = ns("loading_plot"),
                                                                 width = "100%",
                                                                 height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-loadingplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_loadingplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_loadingplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-loadingplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-opls-loadingplot', 'overlay-opls-loadingplot')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Datapoint settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'datapoint-settings-opls-loadingplot', title = 'Data points', collapsed = TRUE),
                                              tags$div(id = 'datapoint-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # inspect data
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-loadingplot', 'overlay-opls-loadingplot')
                                                       ),
                                                       
                                                       # point
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-point-settings-opls-loadingplot', title = 'Point'),
                                                         tags$div(
                                                           id = 'datapoint-point-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           selectInput(inputId = ns('opls_loadingplot_datapoint_fillpalette'), label = 'Fill color', selected = 'd3', 
                                                                       choices = c('d3(10)' = 'd3', 'aaas(10)' = 'aaas', 'jama(7)' = 'jama', 'jco(10)' = 'jco', 'lancet(9)' = 'lancet', 'locuszoom(7)' = 'locuszoom', 'nejm(8)' = 'nejm', 'npg(10)' = 'npg', 'simpsons(16)' = 'simpsons')),
                                                           numericInput(inputId = ns('opls_loadingplot_datapoint_fillalpha'), label = 'Fill transparency', min = 0.01, max = 1, step = 0.1, value = 1),
                                                           selectInput(inputId = ns('opls_loadingplot_datapoint_shape'), label = 'Point shape', selected = '21', 
                                                                       choices = c('Circle' = '21', 'Square' = '22', 'rhomboid' = '23', 'triangle' = '24')),
                                                           numericInput(inputId = ns('opls_loadingplot_datapoint_size'), label = 'Size', min = 1, value = 3, step = 1)
                                                         )
                                                       ),
                                                       # h-line
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'hline-settings-opls-loadingplot', title = 'h-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'hline-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_loadingplot_showhline'), value = TRUE, label = 'Show h-line'),
                                                           numericInput(inputId = ns('opls_loadingplot_hline_value'), label = 'Reference value', step = 0.5, value = 0),
                                                           numericInput(inputId = ns('opls_loadingplot_hline_size'), label = 'Line width', step = 0.5, value = 0.5),
                                                           selectInput(inputId = ns('opls_loadingplot_hline_shape'), label = 'Line shape', 
                                                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_loadingplot_hline_color"), label = "Line color", value = "gray33",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       # v-line
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'vline-settings-opls-loadingplot', title = 'v-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'vline-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_loadingplot_showvline'), value = TRUE, label = 'Show v-line'),
                                                           numericInput(inputId = ns('opls_loadingplot_vline_value'), label = 'Reference value', step = 0.5, value = 0),
                                                           numericInput(inputId = ns('opls_loadingplot_vline_size'), label = 'Line width', step = 0.5, value = 0.5),
                                                           selectInput(inputId = ns('opls_loadingplot_vline_shape'), label = 'Line shape', 
                                                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_loadingplot_vline_color"), label = "Line color", value = "gray33",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       )
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-loadingplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_loadingplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_loadingplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_loadingplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_loadingplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-loadingplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-loadingplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_loadingplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-loadingplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_loadingplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-loadingplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-loadingplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_loadingplot_xaxis_label'), label = 'Label', value = 't[1]'),
                                                           selectInput(inputId = ns('opls_loadingplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_loadingplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-loadingplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_loadingplot_yaxis_label'), label = 'Label', value = 'o[1]'),
                                                           selectInput(inputId = ns('opls_loadingplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_loadingplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-loadingplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_loadingplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_loadingplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_loadingplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_loadingplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_loadingplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-loadingplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_loadingplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_loadingplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_loadingplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_loadingplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_loadingplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "Bio plot",
                         hidden(div(id = ns("result-bioplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotOutput(outputId = ns("bio_plot"),
                                                                 width = "100%",
                                                                 height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-bioplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_bioplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_bioplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-bioplot", class="overlay", 
                                                 onclick = onOverlayClick('drawer-opls-bioplot', 'overlay-opls-bioplot')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'data-settings-opls-bioplot', title = 'Data', collapsed = TRUE),
                                              tags$div(id = 'data-settings-opls-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-bioplot', 'overlay-opls-bioplot')
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-group1-settings-opls-bioplot', 
                                                                            title = 'Point(group-1)', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'datapoint-group1-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           numericInput(inputId = ns('opls_bioplot_point_group1_size'), label = 'Point size', min = 0.1, step = 0.5, value = 3.5),
                                                           colourInput(inputId = ns("opls_bioplot_datapoint_group1_fill_color"), label = "Fill color", value = "#D595A7",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           colourInput(inputId = ns("opls_bioplot_datapoint_group1_border_color"), label = "Border color", value = "#575757",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-group2-settings-opls-bioplot', 
                                                                            title = 'Point(group-2)', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'datapoint-group2-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           numericInput(inputId = ns('opls_bioplot_point_group2_size'), label = 'Point size', min = 0.1, step = 0.5, value = 3.5),
                                                           colourInput(inputId = ns("opls_bioplot_datapoint_group2_fill_color"), label = "Fill color", value = "#009966",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           colourInput(inputId = ns("opls_bioplot_datapoint_group2_border_color"), label = "Border color", value = "#575757",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'hline-settings-opls-bioplot', title = 'h-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'hline-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_bioplot_showhline'), value = TRUE, label = 'Show h-line'),
                                                           numericInput(inputId = ns('opls_bioplot_hline_value'), label = 'Reference value', step = 0.1, value = 0),
                                                           numericInput(inputId = ns('opls_bioplot_hline_size'), label = 'Line width', step = 0.1, value = 0.1),
                                                           selectInput(inputId = ns('opls_bioplot_hline_shape'), label = 'Line shape', 
                                                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_bioplot_hline_color"), label = "Line color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'vline-settings-opls-bioplot', title = 'v-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'vline-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_bioplot_showvline'), value = TRUE, label = 'Show v-line'),
                                                           numericInput(inputId = ns('opls_bioplot_vline_value'), label = 'Reference value', step = 0.1, value = 0),
                                                           numericInput(inputId = ns('opls_bioplot_vline_size'), label = 'Line width', step = 0.1, value = 0.1),
                                                           selectInput(inputId = ns('opls_bioplot_vline_shape'), label = 'Line shape', 
                                                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_bioplot_vline_color"), label = "Line color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-bioplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_bioplot_title_text'), label = 'Text', value = 'bioplot'),
                                                       selectInput(inputId = ns('opls_bioplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_bioplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_bioplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-bioplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-bioplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_bioplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-bioplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_bioplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-bioplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-bioplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_bioplot_xaxis_label'), label = 'Label', value = 'pc(cor)[1], t(cor)[1]'),
                                                           selectInput(inputId = ns('opls_bioplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_bioplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-bioplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_bioplot_yaxis_label'), label = 'Label', value = 'pc(cor)[2], t(cor)[2]'),
                                                           selectInput(inputId = ns('opls_bioplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_bioplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-bioplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_bioplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_bioplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_bioplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_bioplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_bioplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-bioplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_bioplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_bioplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_bioplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_bioplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_bioplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "VIP-loilipop plot",
                         hidden(div(id = ns("result-viploilipopplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotOutput(outputId = ns("viploilipop_plot"),
                                                                 width = "100%",
                                                                 height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-vip-loilipop", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_viploilipop_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_viploilipop_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-vip-loilipop", class="overlay", 
                                                 onclick = onOverlayClick('drawer-opls-vip-loilipop', 'overlay-opls-vip-loilipop')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'data-settings-opls-viploilipopplot', title = 'Data', collapsed = TRUE),
                                              tags$div(id = 'data-settings-opls-viploilipopplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-vip-loilipop', 'overlay-opls-vip-loilipop')
                                                       ),
                                                       numericInput(inputId = ns('opls_viploilipopplot_vip_threshold'), label = 'VIP value threshold', value = 1, min = 0),
                                                       selectInput(inputId = ns('opls_viploilipopplot_sortby'), label = 'Sort by',
                                                                   choices = c('Metabolite name asc' = 'nameasc',
                                                                               'Metabolite name desc' = 'namedesc',
                                                                               'VIP-value asc' = 'vipasc',
                                                                               'VIP-value desc' = 'vipdesc',
                                                                               'Original' = 'original'),
                                                                   selected = 'original'),

                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-settings-opls-viploilipopplot', title = 'Point', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'datapoint-settings-opls-viploilipopplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           numericInput(inputId = ns('opls_viploilipopplot_point_size'), label = 'Point size', min = 0.1, step = 0.1, value = 3.2),
                                                           checkboxInput(inputId = ns('opls_viploilipopplot_datapoint_class_colorful'), value = FALSE, label = 'Color different classes'),
                                                           selectInput(inputId = ns('opls_viploilipopplot_datapoint_color_palette'), label = 'Color palette',
                                                                       choices = c('d3(10)' = 'd3', 'aaas(10)' = 'aaas', 'jama(7)' = 'jama', 'jco(10)' = 'jco', 'lancet(9)' = 'lancet', 'locuszoom(7)' = 'locuszoom', 'nejm(8)' = 'nejm', 'npg(10)' = 'npg', 'simpsons(16)' = 'simpsons'),
                                                                       selected = 'd3'),
                                                           colourInput(inputId = ns("opls_viploilipopplot_datapoint_fill_color"), label = "Fill color", value = "#f8766d",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           colourInput(inputId = ns("opls_viploilipopplot_datapoint_border_color"), label = "Border color", value = "#f8766d",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datasegment-settings-opls-viploilipopplot', title = 'Segment', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'datasegment-settings-opls-viploilipopplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           numericInput(inputId = ns('opls_viploilipopplot_segment_size'), label = 'Line width', value = 0.5, step = 0.1, min = 0),
                                                           checkboxInput(inputId = ns('opls_viploilipopplot_segment_class_colorful'), value = FALSE, label = 'Color different classes'),
                                                           selectInput(inputId = ns('opls_viploilipopplot_segment_color_palette'), label = 'Color palette',
                                                                       choices = c('d3(10)' = 'd3', 'aaas(10)' = 'aaas', 'jama(7)' = 'jama', 'jco(10)' = 'jco', 'lancet(9)' = 'lancet', 'locuszoom(7)' = 'locuszoom', 'nejm(8)' = 'nejm', 'npg(10)' = 'npg', 'simpsons(16)' = 'simpsons'),
                                                                       selected = 'd3'),
                                                           colourInput(inputId = ns("opls_viploilipopplot_segment_color"), label = "Color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),

                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-viploilipopplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-viploilipopplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_viploilipopplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_viploilipopplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_viploilipopplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_viploilipopplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-viploilipopplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-viploilipopplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-viploilipopplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-viploilipopplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_viploilipopplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-viploilipopplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-viploilipopplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_viploilipopplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-viploilipopplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-viploilipopplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-viploilipopplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-viploilipopplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_viploilipopplot_xaxis_label'), label = 'Label', value = 'VIP-value'),
                                                           selectInput(inputId = ns('opls_viploilipopplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_viploilipopplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-viploilipopplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-viploilipopplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_viploilipopplot_yaxis_label'), label = 'Label', value = 'metabolites'),
                                                           selectInput(inputId = ns('opls_viploilipopplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_viploilipopplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-viploilipopplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-viploilipopplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_viploilipopplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_viploilipopplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_viploilipopplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_viploilipopplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_viploilipopplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-viploilipopplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-viploilipopplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_viploilipopplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_viploilipopplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_viploilipopplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_viploilipopplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_viploilipopplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "VIP-bubble plot",
                         hidden(div(id = ns("result-vipbubbleplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(
                                            plotOutput(outputId = ns("vipbubble_plot"),
                                                       width = "100%",
                                                       height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-vip-bubble", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_vip_bubble_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_vip_bubble_data"))
                                                   )
                                                   )
                                        ),
                                        tags$div(id="overlay-opls-vip-bubble", class="overlay", onclick = onOverlayClick('drawer-opls-vip-bubble', 'overlay-opls-vip-bubble')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'data-settings-opls-vipbubbleplot', title = 'Data', collapsed = TRUE),
                                              tags$div(id = 'data-settings-opls-vipbubbleplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                            ), 
                                                                            onclick = onInspectDataBtnClick('drawer-opls-vip-bubble', 'overlay-opls-vip-bubble')
                                                                    ),
                                                       selectInput(inputId = ns('opls_vipbubbleplot_xfield'), label = 'x field', selected = 'vip', choices = c('vip')),
                                                       selectInput(inputId = ns('opls_vipbubbleplot_yfield'), label = 'y field', selected = 'P-value', choices = c('P-value', 'Q-value')),
                                                       numericInput(inputId = ns('opls_vipbubbleplot_vip_threshold'), label = 'VIP value threshold', value = 1, min = 0),
                                                       selectInput(inputId = ns('opls_vipbubbleplot_pvalue_calc_method'), label = 'P-value calculation method', 
                                                                   selected = 'T-test', choices = c('T-test', 'Wilcox-test')),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'point-settings-opls-vipbubbleplot', title = 'Point', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'point-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_vipbubbleplot_showlabel'), label = 'Show metabolite name', value = FALSE),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_point_maxsize'), label = 'Max point size', min = 0.1, step = 1, value = 6),
                                                           selectInput(inputId = ns('opls_vipbubbleplot_datapoint_color_palette'), label = 'Color palette',
                                                                       choices = c('d3(10)' = 'd3', 'aaas(10)' = 'aaas', 'jama(7)' = 'jama', 'jco(10)' = 'jco', 'lancet(9)' = 'lancet', 'locuszoom(7)' = 'locuszoom', 'nejm(8)' = 'nejm', 'npg(10)' = 'npg', 'simpsons(16)' = 'simpsons'),
                                                                       selected = 'd3'),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_point_fillalpha'), label = 'Color transparency', min = 0, step = 0.05, value = 0.8)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'hline-settings-opls-vipbubbleplot', title = 'h-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'hline-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_vipbubbleplot_showhline'), value = TRUE, label = 'Show h-line'),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_hline_value'), label = 'Reference value', step = 0.1, value = 1.3),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_hline_size'), label = 'Line width', step = 0.1, value = 0.5),
                                                           selectInput(inputId = ns('opls_vipbubbleplot_hline_shape'), label = 'Line shape', 
                                                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_vipbubbleplot_hline_color"), label = "Line color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'vline-settings-opls-vipbubbleplot', title = 'v-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'vline-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_vipbubbleplot_showvline'), value = TRUE, label = 'Show v-line'),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_vline_value'), label = 'Reference value', step = 0.1, value = 1),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_vline_size'), label = 'Line width', step = 0.1, value = 0.5),
                                                           selectInput(inputId = ns('opls_vipbubbleplot_vline_shape'), label = 'Line shape', 
                                                                       selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_vipbubbleplot_vline_color"), label = "Line color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-vipbubbleplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-vipbubbleplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_vipbubbleplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_vipbubbleplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_vipbubbleplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_vipbubbleplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-vipbubbleplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-vipbubbleplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-vipbubbleplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_vipbubbleplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-vipbubbleplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_vipbubbleplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-vipbubbleplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-vipbubbleplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-vipbubbleplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_vipbubbleplot_xaxis_label'), label = 'Label', value = 'VIP'),
                                                           selectInput(inputId = ns('opls_vipbubbleplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-vipbubbleplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-vipbubbleplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_vipbubbleplot_yaxis_label'), label = 'Label', value = '-Log10(P-value)'),
                                                           selectInput(inputId = ns('opls_vipbubbleplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_vipbubbleplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-vipbubbleplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-vipbubbleplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_vipbubbleplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_vipbubbleplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_vipbubbleplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_vipbubbleplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_vipbubbleplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-vipbubbleplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-vipbubbleplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_vipbubbleplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_vipbubbleplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_vipbubbleplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_vipbubbleplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_vipbubbleplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "S-plot",
                         hidden(div(id = ns("result-splot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(
                                            plotOutput(outputId = ns("splot"),
                                                       width = "100%",
                                                       height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-splot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_splot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_splot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-splot", class="overlay", onclick = onOverlayClick('drawer-opls-splot', 'overlay-opls-splot')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'data-settings-opls-splot', title = 'Data', collapsed = TRUE),
                                              tags$div(id = 'data-settings-opls-splot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-splot', 'overlay-opls-splot')
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'point-settings-opls-splot', title = 'Point', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'point-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           selectInput(inputId = ns('opls_splot_color_scheme'), label = 'Coloring scheme', selected = 'Metabolite class', choices = c('Metabolite class', 'VIP value')),
                                                           numericInput(inputId = ns('opls_splot_vip_threshold'), label = 'VIP value threshold', value = 1, min = 0),
                                                           colourInput(inputId = ns("opls_splot_color_gtr_vip_threshold"), label = "Point color greater than threshold", value = "#CE3D32",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           colourInput(inputId = ns("opls_splot_color_ltr_vip_threshold"), label = "Point color less than threshold", value = "#4CAF50",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           selectInput(inputId = ns('opls_splot_datapoint_color_palette'), label = 'Color palette',
                                                                       choices = c('d3(10)' = 'd3', 'aaas(10)' = 'aaas', 'jama(7)' = 'jama', 'jco(10)' = 'jco', 'lancet(9)' = 'lancet', 'locuszoom(7)' = 'locuszoom', 'nejm(8)' = 'nejm', 'npg(10)' = 'npg', 'simpsons(16)' = 'simpsons', 'custom'),
                                                                       selected = 'd3'),
                                                           textAreaInput(inputId = ns('opls_splot_datapoint_custom_color_palette_inputs_rgb'), label = 'Custom color palette', 
                                                                         width = '100%', height = '120px', resize = 'vertical',
                                                                         value = '#E64B35, #3C5488, #91D1C2, #4DBBD5, #F39B7F, #DC0000, #E7C76F, #00A087, #8491B4, #7E6148, #339900, #924822, #99CC00, #5050FF, #CE3D32, #749B58, #F0E685, #466983, #0099CC, #BA6338, #5DB1DD, #802268, #6BD76B, #D595A7, #837B8D, #C75127, #D58F5C, #7A65A5, #E4AF69, #3B1B53, #CDDEB7, #612A79, #8EC4CB, #4CAF50, #FF8C3E'),
                                                           uiOutput(outputId = ns('opls_splot_datapoint_custom_color_palette_block')),
                                                           numericInput(inputId = ns('opls_splot_point_fillalpha'), label = 'Color transparency', min = 0, max = 1, step = 0.05, value = 1),
                                                           numericInput(inputId = ns('opls_splot_point_size'), label = 'Point size', step = 0.1, value = 2.1)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'hline-settings-opls-splot', title = 'h-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'hline-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_splot_showhline'), value = TRUE, label = 'Show h-line'),
                                                           numericInput(inputId = ns('opls_splot_hline_value'), label = 'Reference value', step = 0.1, value = 0),
                                                           numericInput(inputId = ns('opls_splot_hline_size'), label = 'Line width', step = 0.1, value = 0.5),
                                                           selectInput(inputId = ns('opls_splot_hline_shape'), label = 'Line shape', 
                                                                       selected = 'solid', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_splot_hline_color"), label = "Line color", value = "gray",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'vline-settings-opls-splot', title = 'v-line', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'vline-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           checkboxInput(inputId = ns('opls_splot_showvline'), value = TRUE, label = 'Show v-line'),
                                                           numericInput(inputId = ns('opls_splot_vline_value'), label = 'Reference value', step = 0.1, value = 0),
                                                           numericInput(inputId = ns('opls_splot_vline_size'), label = 'Line width', step = 0.1, value = 0.5),
                                                           selectInput(inputId = ns('opls_splot_vline_shape'), label = 'Line shape', 
                                                                       selected = 'solid', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash')),
                                                           colourInput(inputId = ns("opls_splot_vline_color"), label = "Line color", value = "gray",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       
                                                       
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-splot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-splot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_splot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_splot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_splot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_splot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-splot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-splot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-splot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_splot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-splot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_splot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-splot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-splot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-splot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_splot_xaxis_label'), label = 'Label', value = 't[1]'),
                                                           selectInput(inputId = ns('opls_splot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_splot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-splot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-splot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_splot_yaxis_label'), label = 'Label', value = 't(corr)[1]'),
                                                           selectInput(inputId = ns('opls_splot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_splot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-splot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-splot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_splot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_splot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_splot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_splot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_splot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-splot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-splot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_splot_width'), label = 'Width', min = 100, step = 10, value = opls_result_splot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_splot_height'), label = 'Height', min = 100, step = 10, value = opls_result_splot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_splot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "V-plot",
                         hidden(div(id = ns("result-vplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(
                                            plotOutput(outputId = ns("vplot"),
                                                       width = "100%",
                                                       height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-vplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_vplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_vplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-vplot", class="overlay", onclick = onOverlayClick('drawer-opls-vplot', 'overlay-opls-vplot')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'data-settings-opls-vplot', title = 'Data', collapsed = TRUE),
                                              tags$div(id = 'data-settings-opls-vplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-vplot', 'overlay-opls-vplot')
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'point-settings-opls-vplot', title = 'Point', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'point-settings-opls-vplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           numericInput(inputId = ns('opls_vplot_vip_threshold'), label = 'VIP value threshold', value = 1, min = 0),
                                                           colourInput(inputId = ns("opls_vplot_color_gtr_vip_threshold"), label = "Point color greater than threshold", value = "#CE3D32",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           colourInput(inputId = ns("opls_vplot_color_ltr_vip_threshold"), label = "Point color less than threshold", value = "#4CAF50",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           numericInput(inputId = ns('opls_vplot_point_size'), label = 'Point size', step = 0.1, value = 2.1),
                                                           checkboxInput(inputId = ns('opls_vplot_show_label'), label = 'Show label', value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-vplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-vplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_vplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_vplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_vplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_vplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-vplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-vplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-vplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-vplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_vplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-vplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-vplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_vplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-vplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-vplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-vplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-vplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_vplot_xaxis_label'), label = 'Label', value = 'p(corr)'),
                                                           selectInput(inputId = ns('opls_vplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_vplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-vplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-vplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_vplot_yaxis_label'), label = 'Label', value = 'VIP(variable importance to projection)'),
                                                           selectInput(inputId = ns('opls_vplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_vplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-vplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-vplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_vplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_vplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_vplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_vplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_vplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-vplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-vplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_vplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_vplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_vplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_vplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_vplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "Permutation-plot",
                         hidden(div(id = ns("result-permutationplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(
                                            plotOutput(outputId = ns("permutationplot"),
                                                       width = "100%",
                                                       height = "auto")
                                          )
                                        ),
                                        
                                        tags$div(
                                          id="drawer-opls-permutationplot", class="drawer",
                                          tags$div(style = "padding:15px 20px 20px 20px;",
                                                   downloadButton(outputId = ns("download_opls_permutationplot_data"),
                                                                  class = "action-button-primary",
                                                                  style = "width:140px !important; margin-bottom:10px; display:flex; align-items:center; gap:8px; justify-content:center;",
                                                                  label = "Download",
                                                                  icon = icon("download")),
                                                   tags$div(
                                                     style = "width:100%; max-height:calc(100vh - 70px); overflow-y:scroll; overflow-x:scroll;",
                                                     DT::DTOutput(outputId = ns("opls_permutationplot_data"))
                                                   )
                                          )
                                        ),
                                        tags$div(id="overlay-opls-permutationplot", class="overlay", onclick = onOverlayClick('drawer-opls-permutationplot', 'overlay-opls-permutationplot')),
                                        
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Data settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'data-settings-opls-permutationplot', title = 'Data', collapsed = TRUE),
                                              tags$div(id = 'data-settings-opls-permutationplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$button(class = "action-button-primary", 
                                                                   style = "height:26px; font-size:13px; margin-bottom:8px;",
                                                                   tags$div(tags$i(class="far fa-eye"),
                                                                            tags$span("Inspect data"), 
                                                                   ), 
                                                                   onclick = onInspectDataBtnClick('drawer-opls-permutationplot', 'overlay-opls-permutationplot')
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'point-r2-settings-opls-permutationplot', title = 'Point(R2)', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'point-r2-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           colourInput(inputId = ns("opls_permutationplot_point_r2_color"), label = "Point color", value = "#3F51B5",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           numericInput(inputId = ns('opls_permutationplot_point_r2_size'), label = 'Point size', step = 0.1, value = 3.5),
                                                           selectInput(inputId = ns('opls_permutationplot_point_r2_shape'), label = 'Point shape', 
                                                                       selected = '22', choices = c('Circle' = '21', 'Square' = '22', 'rhomboid' = '23', 'triangle' = '24'))
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'point-q2-settings-opls-permutationplot', title = 'Point(Q2)', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'point-q2-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           colourInput(inputId = ns("opls_permutationplot_point_q2_color"), label = "Point color", value = "#4CAF50",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           numericInput(inputId = ns('opls_permutationplot_point_q2_size'), label = 'Point size', step = 0.1, value = 3.5),
                                                           selectInput(inputId = ns('opls_permutationplot_point_q2_shape'), label = 'Point shape', 
                                                                       selected = '21', choices = c('Circle' = '21', 'Square' = '22', 'rhomboid' = '23', 'triangle' = '24'))
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'line-r2-settings-opls-permutationplot', title = 'Line(R2)', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'line-r2-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           colourInput(inputId = ns("opls_permutationplot_line_r2_color"), label = "Line color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           selectInput(inputId = ns('opls_permutationplot_line_r2_type'), label = 'Line type', selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash', 'blank')),
                                                           numericInput(inputId = ns('opls_permutationplot_line_r2_weight'), label = 'Line weight', min = 0, step = 0.5, value = 0.5)
                                                         )
                                                       ),
                                                       
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'line-q2-settings-opls-permutationplot', title = 'Line(Q2)', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'line-q2-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           colourInput(inputId = ns("opls_permutationplot_line_q2_color"), label = "Line color", value = "black",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE),
                                                           selectInput(inputId = ns('opls_permutationplot_line_q2_type'), label = 'Line type', selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash', 'blank')),
                                                           numericInput(inputId = ns('opls_permutationplot_line_q2_weight'), label = 'Line weight', min = 0, step = 0.5, value = 0.5)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-opls-permutationplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-opls-permutationplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('opls_permutationplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('opls_permutationplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('opls_permutationplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('opls_permutationplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-opls-permutationplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-opls-permutationplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-opls-permutationplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("opls_permutationplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-opls-permutationplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('opls_permutationplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-opls-permutationplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-opls-permutationplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-opls-permutationplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_permutationplot_xaxis_label'), label = 'Label', value = ''),
                                                           selectInput(inputId = ns('opls_permutationplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_permutationplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-opls-permutationplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-opls-permutationplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('opls_permutationplot_yaxis_label'), label = 'Label', value = ''),
                                                           selectInput(inputId = ns('opls_permutationplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('opls_permutationplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-opls-permutationplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-opls-permutationplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('opls_permutationplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('opls_permutationplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'opls_permutationplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_permutationplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_permutationplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-opls-permutationplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-opls-permutationplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_permutationplot_width'), label = 'Width', min = 100, step = 10, value = opls_result_permutationplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('opls_result_permutationplot_height'), label = 'Height', min = 100, step = 10, value = opls_result_permutationplot_height)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                          )
                                        )
                                      )
                                    ),
                                    tags$div(
                                      style = 'width:120px; float:right;',
                                      actionButton(inputId = ns("export_opls_result_permutationplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
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


dimensionReductionOPLSServer <- function(input, output, session) {
  ns <- session$ns
  dataset <- NULL
  origdataset <- NULL
  dataset_attachment_var_class <- NULL
  dataset_attachment_sample_group <- NULL
  mapping <- NULL
  classes <- c()              # 所有的数据分组
  preprocessed_data <- NULL   # 预处理(log transform + scaling)之后的数据
  
  opls_result_scoreplot <- NULL
  opls_result_scoreplot_data <- NULL
  
  opls_result_loadingplot <- NULL
  opls_result_loadingplot_data <- NULL
  
  opls_result_bioplot <- NULL
  opls_result_bioplot_data <- NULL
  
  opls_result_viploilipopplot_filterdata <- NULL
  opls_result_viploilipopplot <- NULL
  opls_result_viploilipop_data <- NULL
  
  opls_result_vipbubbleplot <- NULL
  opls_result_vipbubble_data <- NULL
  
  opls_result_splot <- NULL
  opls_result_splot_data <- NULL
  
  opls_result_vplot <- NULL
  opls_result_vplot_data <- NULL
  
  opls_result_permutationplot <- NULL
  opls_result_permutationplot_data <- NULL
  
  # 上传样本x变量矩阵数据
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
      
      # 自动展开
      js$collapse("box-dr-opls")
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
        tags$div(id='settings-group-selection', class = 'collapse-item-body', style = 'display:block;',
                 selectInput(inputId = ns('settings_groupcol_selection'), label = "", 
                             choices = colnames(dataset_attachment_sample_group)[2: length(colnames(dataset_attachment_sample_group))])
        )
      })
    }, error = function(e) {
      print(paste(e))
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
  
  # 样本分组列改变，所有的分类也相应改变
  observeEvent(input$settings_groupcol_selection, {
    classes <- unique(dataset_attachment_sample_group[[input$settings_groupcol_selection]])
    # 数据分组选择, 默认不选择
    output$groups_selection_ui <- renderUI({
      tags$div(id = 'settings-group-comparison', class = 'collapse-item-body', style = 'display:block;',
               checkboxGroupInput(inputId = ns("select_data_group"), label = "",
                                  choices = classes
               )
      )
    })
  })
  
  observeEvent(input$select_dimension_reduction_model, {
    if (input$select_dimension_reduction_model == 'custom') {
      showModal(modalDialog(
        title = "自定义参数",
        size = "l",
        fluidPage(
          selectInput(inputId = ns("custom_model_params_algoC"), label = "algoC", choices = c("default", "nipals", "svd"), selected = "default"),
          selectInput(inputId = ns("custom_model_params_log10L"), label = "log10L", choices = c("T", "F")),
          numericInput(inputId = ns("custom_model_params_perml"), label = "perml", value = 10),
          selectInput(inputId = ns("custom_model_params_scaleC"), label = "scaleC", choices = c()),
          selectInput(inputId = ns("custom_model_params_subset"), label = "subset", choices = c())
        ),
        easyClose = FALSE,
        fade = FALSE,
        footer = tagList(
          actionButton(inputId = ns("custom_model_params_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    }
  })
  
  observeEvent(input$execute, {
    tryCatch({
      if (is.null(dataset)) {
        toastr_warning(title = "Please upload metabolite matrix data!", message = '')
      } else if (is.null(dataset_attachment_sample_group)) {
        toastr_warning(title = "Please upload sample grouping data!", message = '')
      } else if (length(input$select_data_group) != 2) {
        toastr_warning(title = "OPLS-DA only available for binary classification (use PLS-DA for multiple classes)!", message = '')
      } else {
        shinyjs::show("result-overview")
        shinyjs::show("preprocessed-data")
        shinyjs::show("result-scoreplot")
        shinyjs::show("result-loadingplot")
        shinyjs::show("result-bioplot")
        
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
        if (input$settings_data_scaling == 'Centering') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              return(x - mean_val)
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Unit Variance scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              sd_val <- sd(x)
              return(round((x - mean_val) / sd_val, 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Pareto scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              sd_val <- sd(x)
              return(round((x - mean_val) / sqrt(sd_val), 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Range scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              max_val <- max(x)
              min_val <- min(x)
              return(round((x - mean_val) / (max_val - min_val), 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Vast scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              sd_val <- sd(x)
              return(round(((x - mean_val) / sd_val) * (mean_val / sd_val), 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        } else if (input$settings_data_scaling == 'Level scaling') {
          df_scaled <- preprocessed_data
          df_scaled[, -1] <- as.data.frame(lapply(df_scaled[, -1], function(x) {
            if (is.numeric(x)) {
              mean_val <- mean(x)
              return(round((x - mean_val) / mean_val, 6))
            } else {
              return(x)  # 如果列不是数值类型，则保持不变
            }
          }))
          preprocessed_data <<- df_scaled
        }
        
        # 替换为原始的变量名称
        header <- c()
        for (i in 1:length(colnames(preprocessed_data))) {
          header <- append(header, as.character(mapping[[colnames(preprocessed_data)[i]]][1]))
        }
        preprocessed_data[[1]] <- as.factor(preprocessed_data[[1]])
        colnames(preprocessed_data) <- header
        rownames(preprocessed_data) <- preprocessed_data[, 1]
        
        # result table of preprocessed data
        output$preprocessed_data <- DT::renderDT({
          DT::datatable({
            preprocessed_data
          },
          options = dataTableOptions_pageLength25,
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        # 3. 根据选定的分组列名，新增一列group补充到matrix数据中
        dataset_withgroup <- preprocessed_data
        dataset_withgroup['group'] <- dataset_attachment_sample_group[[input$settings_groupcol_selection]]
        
        # 4. 根据选择的数据分组进行数据筛选
        selected_group_data <- dataset_withgroup[dataset_withgroup$group %in% input$select_data_group, ]
        
        # 去掉第一列的样本名称和group列，数据矩阵作为OPLS的分析对象
        data_matrix <- selected_group_data[, -(1)]
        data_matrix <- subset(data_matrix, select = -group)
        
        # 选择数据对应的样本名称
        sample_name_of_selected_data <- selected_group_data[[1]]
        
        # OPLS
        model <- opls(x = data_matrix,
                      y = as.character(selected_group_data$group),
                      scaleC = "none",# 不选择默认'UV',默认选择“none”，不可更改
                      predI = 1, 
                      orthoI = 1,# 输入框，正交主成分数量，【若不填写数值默认NA】。
                      # algoC = c("default", "nipals", "svd")[1], # 建模方法，单选，选项：default，svd，nipls
                      crossvalI = 7,# 输入框，交叉验证段数，整数，默认7，必须比x行数小”；
                      permI = 9) # 输入框，置换检验次数
        
        # 统计各个组的样本数
        group_count_df = t(as.data.frame(table(selected_group_data$group)))
        colnames(group_count_df) = group_count_df[1, ]
        
        # overview
        output$opls_result_overview <- renderUI(
          tags$div(
            style = 'display:flex; gap:20px; margin-top:15px;',
            tags$div(
              class = 'overview-desc-card',
              tags$div(
                style = 'display:flex; align-items:center; gap:10px;',
                tags$img(src = './data-desc.png', style = 'width:25px;'),
                tags$div('Data description', class = 'overview-desc-title')
              ),
              tags$div(
                class = 'overview-desc-detail',
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of samples:'),
                  tags$span(model@descriptionMC[1], class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of metabolites'),
                  tags$span(model@descriptionMC[2], class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Near zero excluded:'),
                  tags$span(model@descriptionMC[3], class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Y variables:'),
                  tags$span(model@descriptionMC[4], class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Missing values:'),
                  tags$span(model@descriptionMC[5], class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Log method:'),
                  tags$span(input$settings_data_transformation, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Scaling method:'),
                  tags$span(input$settings_data_scaling, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of samples of each groups:'),
                  tags$div(
                    DT::renderDT({
                      DT::datatable({
                        t(as.data.frame(group_count_df[-1,]))
                      },
                      options = dataTableOptions_mini,
                      selection = 'none',
                      style = 'bootstrap4',
                      class = 'cell-border stripe compact datatable-mini',
                      rownames = FALSE
                      )
                    })
                  )
                )
              )
            ),
            tags$div(
              class = 'overview-desc-card',
              tags$div(
                style = 'display:flex; align-items:center; gap:10px;',
                tags$img(src = './result-desc.png', style = 'width:25px;'),
                tags$div('Model information', class = 'overview-desc-title')
              ),
              tags$div(
                class = 'overview-desc-detail',
                tags$div(
                  icon('calendar-check'),
                  tags$span('Type of model:'),
                  tags$span(model@typeC, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Number of principle components:'),
                  tags$span(model@summaryDF$pre, class = 'overview-desc-item-highlight')
                ),
                tags$div(
                  icon('calendar-check'),
                  tags$span('Cumulative R2X:'),
                  tags$span(model@summaryDF$`R2X(cum)`, class = 'overview-desc-item-highlight')
                )
              )
            )
          )
        )
        
        # 提取OPLS绘图信息
        model.score = data.frame(
          sample = sample_name_of_selected_data,
          group = selected_group_data$group,
          'p1' = round(model@scoreMN, digits = 6),
          'o1' = round(model@orthoScoreMN, digits = 6)
        )
        
        # inspect data
        opls_result_scoreplot_data <<- model.score
        # inspect data
        output$opls_scoreplot_data <- DT::renderDT({
          DT::datatable({
            opls_result_scoreplot_data
          },
          options = list(initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().body()).css({'font-size': '12px'});",
            "$(this.api().table().header()).css({'font-size': '12px'});",
            "}"),
            columnDefs = list(list(className='dt-left', targets="_all")),
            scrollX = FALSE,
            searching = FALSE,
            paging = FALSE,
            bInfo = TRUE),
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        
        # score plot
        output$score_plot <- renderPlot({
          ellipse <- NULL
          # 所有数据点绘制一个置信椭圆
          if (input$opls_scoreplot_confidence_datacoverage == 'FALSE') {
            ellipse <- stat_ellipse(aes(x = p1, y = o1, label = sample),
                                    level = input$opls_scoreplot_confidence_interval,
                                    inherit.aes = FALSE,
                                    linetype = ifelse(input$opls_scoreplot_confidence_withlineornot == 'solidLine', input$opls_scoreplot_confidence_linetype, 'blank'),
                                    size = input$opls_scoreplot_confidence_lineweight,
                                    show.legend = FALSE,
                                    geom = ifelse(input$opls_scoreplot_confidence_fillornot == 'noFill', 'path', 'polygon'),
                                    fill = input$opls_scoreplot_confidence_fillcolor_single,
                                    color = input$opls_scoreplot_confidence_fillcolor_single,
                                    alpha = ifelse(input$opls_scoreplot_confidence_fillornot == 'noFill', 
                                                   input$opls_scoreplot_confidence_linealpha, input$opls_scoreplot_confidence_fillalpha)
                                    )
          }
          # 每组数据点绘制一个置信椭圆
          else {
            ellipse <- stat_ellipse(aes(x = p1, y = o1),
                                    level = input$opls_scoreplot_confidence_interval,
                                    inherit.aes = TRUE,
                                    linetype = ifelse(input$opls_scoreplot_confidence_withlineornot == 'solidLine', 
                                                      input$opls_scoreplot_confidence_linetype, 'blank'),
                                    size = input$opls_scoreplot_confidence_lineweight,
                                    show.legend = FALSE,
                                    geom = ifelse(input$opls_scoreplot_confidence_fillornot == 'noFill', 'path', 'polygon'),
                                    alpha = ifelse(input$opls_scoreplot_confidence_fillornot == 'noFill', 
                                                   input$opls_scoreplot_confidence_linealpha, input$opls_scoreplot_confidence_fillalpha),
                                    )
          }
          
          legend_position <- 'none'
          if (input$opls_scoreplot_legend_position == 'outside') {
            legend_position <- input$opls_scoreplot_legend_position_outside
          } else if (input$opls_scoreplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$opls_scoreplot_legend_position == 'inside') {
            legend_position <- c(input$opls_scoreplot_legend_position_inside_x, input$opls_scoreplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$opls_scoreplot_panel_background_fill_color),
                  plot.title = element_text(family = input$opls_scoreplot_title_fontfamily, size = input$opls_scoreplot_title_fontsize, vjust = 1, hjust = input$opls_scoreplot_title_position),
                  axis.title.x = element_text(family = input$opls_scoreplot_xaxis_fontfamily, size = input$opls_scoreplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$opls_scoreplot_yaxis_fontfamily, size = input$opls_scoreplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$opls_scoreplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          opls_result_scoreplot <<- ggplot(model.score, aes(x = p1, y = o1, color = group, fill = group)) +
            ellipse +
            scale_fill_manual(values = ggsciColorPalette(input$opls_scoreplot_confidence_fillcolor_multi, input$opls_scoreplot_confidence_fillalpha)) +
            geom_hline(yintercept = 0, linetype = 1, size = 0.5) +
            geom_vline(xintercept = 0, linetype = 1, size = 0.5) +
            geom_point(size = input$opls_scoreplot_datapoint_size, shape = as.numeric(input$opls_scoreplot_datapoint_shape), aes(fill = group)) +
            scale_color_manual(values = ggsciColorPalette(input$opls_scoreplot_datapoint_fillcolor, input$opls_scoreplot_datapoint_fillalpha)) +
            ggtitle(input$opls_scoreplot_title_text) +
            xlab(input$opls_scoreplot_xaxis_label) + ylab(input$opls_scoreplot_yaxis_label) +
            theme_settings
          
          return(opls_result_scoreplot)
        }, height = function() {
          opls_result_scoreplot_height <<- input$opls_result_scoreplot_height
          return(opls_result_scoreplot_height)
        }, width = function() {
          opls_result_scoreplot_width <<- input$opls_result_scoreplot_width
          return(opls_result_scoreplot_width)
        })
        
        # loading plot
        output$loading_plot <- renderPlot({
          
          if (!is.null(dataset_attachment_var_class)) {
            if (ncol(dataset_attachment_var_class) == 2 && nrow(dataset_attachment_var_class) > 0) {
              # prepare loading data
              loading_data = data.frame(
                'metabolite' = rownames(model@loadingMN),
                "class" = dataset_attachment_var_class[[2]],
                'p1' = round(model@loadingMN, digits = 6),
                'o1' = round(model@orthoLoadingMN, digits = 6)
              )
              loading_data$class = as.factor(loading_data$class)
              
              opls_result_loadingplot_data <<- loading_data
              # inspect data
              output$opls_loadingplot_data <- DT::renderDT({
                DT::datatable({
                  loading_data
                },
                options = list(initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().body()).css({'font-size': '12px'});",
                  "$(this.api().table().header()).css({'font-size': '12px'});",
                  "}"),
                  columnDefs = list(list(className='dt-left', targets="_all")),
                  scrollX = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  bInfo = TRUE),
                selection = 'none',
                style = 'bootstrap4',
                class = 'cell-border stripe compact datatable',
                rownames = FALSE
                )
              })
              
              legend_position <- 'none'
              if (input$opls_loadingplot_legend_position == 'outside') {
                legend_position <- input$opls_loadingplot_legend_position_outside
              } else if (input$opls_loadingplot_legend_position == 'none') {
                legend_position <- 'none'
              } else if (input$opls_loadingplot_legend_position == 'inside') {
                legend_position <- c(input$opls_loadingplot_legend_position_inside_x, input$opls_loadingplot_legend_position_inside_y)
              }
              
              theme_settings <- theme_bw() +
                theme(legend.position = legend_position,
                      legend.justification = "center",
                      legend.box = "vertical",
                      legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      panel.background = element_rect(fill = input$opls_loadingplot_panel_background_fill_color),
                      plot.title = element_text(family = input$opls_loadingplot_title_fontfamily, size = input$opls_loadingplot_title_fontsize, vjust = 1, hjust = input$opls_loadingplot_title_position),
                      axis.title.x = element_text(family = input$opls_loadingplot_xaxis_fontfamily, size = input$opls_loadingplot_xaxis_fontsize),
                      axis.title.y = element_text(family = input$opls_loadingplot_yaxis_fontfamily, size = input$opls_loadingplot_yaxis_fontsize),
                      axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      axis.ticks = element_line(color = 'black')
                )
              
              if (input$opls_loadingplot_panel_settings_showgrid == FALSE) {
                theme_settings <- theme_settings + theme(panel.grid = element_blank())
              }
              
              # plot
              opls_result_loadingplot <<- ggplot(loading_data, aes(p1, o1, label = metabolite))
              
              # h-line
              if (input$opls_loadingplot_showhline == TRUE) {
                opls_result_loadingplot <<- opls_result_loadingplot + geom_hline(yintercept = input$opls_loadingplot_hline_value, 
                                                                                 linetype = input$opls_loadingplot_hline_shape, 
                                                                                 size = input$opls_loadingplot_hline_size, 
                                                                                 color = input$opls_loadingplot_hline_color)
              }
              # v-line
              if (input$opls_loadingplot_showvline == TRUE) {
                opls_result_loadingplot <<- opls_result_loadingplot + geom_vline(xintercept = input$opls_loadingplot_vline_value, 
                                                                                 linetype = input$opls_loadingplot_vline_shape, 
                                                                                 size = input$opls_loadingplot_vline_size, 
                                                                                 color = input$opls_loadingplot_vline_color)
              }
              
              color_values <- c()
              if (input$opls_loadingplot_datapoint_fillpalette == 'd3') {
                color_values <- pal_d3()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'aaas') {
                color_values <- pal_aaas()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'jama') {
                color_values <- pal_jama()(7)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'jco') {
                color_values <- pal_jco()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'lancet') {
                color_values <- pal_lancet()(9)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'locuszoom') {
                color_values <- pal_locuszoom()(7)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'nejm') {
                color_values <- pal_nejm()(8)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'npg') {
                color_values <- pal_npg()(10)
              } else if (input$opls_loadingplot_datapoint_fillpalette == 'simpsons') {
                color_values <- pal_simpsons()(16)
              }
              # if color is not enough
              if (length(color_values) < length(levels(loading_data$class))) {
                color_values <- append(color_values, sample(pal_igv()(50), size = (length(levels(loading_data$class)) - length(color_values))))
              }
              
              opls_result_loadingplot <<- opls_result_loadingplot +
                geom_point(aes(fill = class), 
                           size = input$opls_loadingplot_datapoint_size, 
                           shape = as.numeric(input$opls_loadingplot_datapoint_shape), 
                           stroke = 0.1) +
                scale_fill_manual(values = color_values) +
                ggtitle(input$opls_loadingplot_title_text) + 
                xlab(input$opls_loadingplot_xaxis_label) + ylab(input$opls_loadingplot_yaxis_label) +
                theme_settings
              
              return(opls_result_loadingplot)
            } else {
              toastr_warning(message = "", title = "Please check the format of metabolite classification data")
              return(NULL)
            }
          } else {
            toastr_warning(message = "", title = "Please upload the metabolite classification data")
            return(NULL)
          }
          
        }, height = function() {
          opls_result_loadingplot_height <<- input$opls_result_loadingplot_height
          return(opls_result_loadingplot_height)
        }, width = function() {
          opls_result_loadingplot_width <<- input$opls_result_loadingplot_width
          return(opls_result_loadingplot_width)
        })
        
        if (!is.null(dataset_attachment_var_class)) {
          shinyjs::show("result-viploilipopplot")
          
          # vip-loilipop plot
          output$viploilipop_plot <- renderPlot({
            
            # prepare data
            vip.score <- data.frame('metabolite' = colnames(origdataset)[2:length(origdataset)],
                                    'vip' = round(model@vipVn, digits = 6),
                                    'p1' = round(model@loadingMN, digits = 6))
            
            variable_class_mapping <- data.frame(matrix(ncol = nrow(origdataset), nrow = 0), check.names = FALSE)
            variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment_var_class[[2]]))
            colnames(variable_class_mapping) <- dataset_attachment_var_class[[1]]
            variable_class <- c()
            # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
            for (v in vip.score[["metabolite"]]) {
              if (v %in% colnames(variable_class_mapping)) {
                variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
              } else {
                toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
                variable_class <- append(variable_class, "unknown")
              }
            }
            vip.score["class"] = variable_class
            opls_result_viploilipop_data <<- vip.score
            
            # inspect data
            output$opls_viploilipop_data <- DT::renderDT({
              DT::datatable({
                opls_result_viploilipop_data
              },
              options = list(initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px'});",
                "}"),
                columnDefs = list(list(className='dt-left', targets="_all")),
                scrollX = FALSE,
                searching = FALSE,
                paging = FALSE,
                bInfo = TRUE),
              selection = 'none',
              style = 'bootstrap4',
              class = 'cell-border stripe compact datatable',
              rownames = FALSE
              )
            })
            
            # sort
            vip.score.order <- vip.score
            vip.score.order$metabolite = factor(vip.score.order$metabolite, levels = vip.score.order$metabolite)
            if (input$opls_viploilipopplot_sortby == 'nameasc') {
              vip.score.order <- vip.score[order(vip.score$metabolite, decreasing = TRUE), ]
            } else if (input$opls_viploilipopplot_sortby == 'namedesc') {
              vip.score.order <- vip.score[order(vip.score$metabolite), ]
            } else if (input$opls_viploilipopplot_sortby == 'vipasc') {
              vip.score.order <- vip.score[order(vip.score$vip, decreasing = TRUE), ]
            } else if (input$opls_viploilipopplot_sortby == 'vipdesc') {
              vip.score.order <- vip.score[order(vip.score$vip), ]
            }
            
            legend_position <- 'none'
            if (input$opls_viploilipopplot_legend_position == 'outside') {
              legend_position <- input$opls_viploilipopplot_legend_position_outside
            } else if (input$opls_viploilipopplot_legend_position == 'none') {
              legend_position <- 'none'
            } else if (input$opls_viploilipopplot_legend_position == 'inside') {
              legend_position <- c(input$opls_viploilipopplot_legend_position_inside_x, input$opls_viploilipopplot_legend_position_inside_y)
            }
            
            theme_settings <- theme_bw() +
              theme(legend.position = legend_position,
                    legend.justification = "center",
                    legend.box = "vertical",
                    legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    panel.background = element_rect(fill = input$opls_viploilipopplot_panel_background_fill_color),
                    plot.title = element_text(family = input$opls_viploilipopplot_title_fontfamily, size = input$opls_viploilipopplot_title_fontsize, vjust = 1, hjust = input$opls_viploilipopplot_title_position),
                    axis.title.x = element_text(family = input$opls_viploilipopplot_xaxis_fontfamily, size = input$opls_viploilipopplot_xaxis_fontsize),
                    axis.title.y = element_text(family = input$opls_viploilipopplot_yaxis_fontfamily, size = input$opls_viploilipopplot_yaxis_fontsize),
                    axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    axis.ticks = element_line(color = 'black')
              )
            
            if (input$opls_viploilipopplot_panel_settings_showgrid == FALSE) {
              theme_settings <- theme_settings + theme(panel.grid = element_blank())
            }
            
            # filter
            opls_result_viploilipopplot_filterdata <<- vip.score.order[vip.score.order$vip >= input$opls_viploilipopplot_vip_threshold,]
            opls_result_viploilipopplot_filterdata$metabolite <- factor(opls_result_viploilipopplot_filterdata$metabolite, 
                                                                        levels = opls_result_viploilipopplot_filterdata$metabolite)
            
            # 自适应计算图的高度
            opls_result_viploilipopplot_height <<- nrow(opls_result_viploilipopplot_filterdata) * 18 + 100
            updateNumericInput(inputId = "opls_result_viploilipopplot_height", value = opls_result_viploilipopplot_height)
            
            geom_segment <- geom_segment(aes(x = 0, xend = vip, y = metabolite, yend = metabolite), 
                                         size = input$opls_viploilipopplot_segment_size, 
                                         color = input$opls_viploilipopplot_segment_color)
            geom_point <- geom_point(aes(vip, metabolite),
                                     shape = 21, 
                                     fill = input$opls_viploilipopplot_datapoint_fill_color, 
                                     color = input$opls_viploilipopplot_datapoint_border_color,
                                     size = input$opls_viploilipopplot_point_size)
            
            # datapoint不同类别着色
            if (input$opls_viploilipopplot_datapoint_class_colorful == TRUE) {
              geom_point <- geom_point(aes(vip, metabolite, fill = class),
                                       shape = 21, 
                                       color = 'white',
                                       size = input$opls_viploilipopplot_point_size)
            }
            
            # segment不同类别着色
            if (input$opls_viploilipopplot_segment_class_colorful == TRUE) {
              geom_segment <- geom_segment(aes(x = 0, xend = vip, y = metabolite, yend = metabolite, color = class), size = input$opls_viploilipopplot_segment_size)
            }
            
            opls_result_viploilipopplot <<- ggplot(opls_result_viploilipopplot_filterdata) +
              geom_segment +
              geom_vline(xintercept = input$opls_viploilipopplot_vip_threshold, linetype = 'dashed', color = 'lightgray') +
              geom_point +
              scale_x_continuous(expand = c(0, 0)) +
              theme_settings +
              ggtitle(input$opls_viploilipopplot_title_text) +
              xlab(input$opls_viploilipopplot_xaxis_label) + ylab(input$opls_viploilipopplot_yaxis_label)
            
            if (input$opls_viploilipopplot_datapoint_class_colorful == TRUE) {
              if (input$opls_viploilipopplot_datapoint_color_palette == 'd3') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_d3()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'aaas') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_aaas()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'jama') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_jama()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'jco') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_jco()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'lancet') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_lancet()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'locuszoom') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_locuszoom()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'nejm') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_nejm()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'npg') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_npg()
              } else if (input$opls_viploilipopplot_datapoint_color_palette == 'simpsons') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_fill_simpsons()
              }
            }
            
            if (input$opls_viploilipopplot_segment_class_colorful == TRUE) {
              if (input$opls_viploilipopplot_segment_color_palette == 'd3') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_d3()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'aaas') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_aaas()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'jama') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_jama()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'jco') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_jco()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'lancet') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_lancet()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'locuszoom') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_locuszoom()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'nejm') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_nejm()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'npg') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_npg()
              } else if (input$opls_viploilipopplot_segment_color_palette == 'simpsons') {
                opls_result_viploilipopplot <<- opls_result_viploilipopplot + scale_color_simpsons()
              }
            }
            
            return(opls_result_viploilipopplot)
            
          }, height = function() {
            opls_result_viploilipopplot_height <<- input$opls_result_viploilipopplot_height
            return(opls_result_viploilipopplot_height)
          }, width = function() {
            opls_result_viploilipopplot_width <<- input$opls_result_viploilipopplot_width
            return(opls_result_viploilipopplot_width)
          })
        } else {
          toastr_warning(message = "VIP-loilipop plot need metabolite classification data", 
                         title = "Please upload metabolite classification data")
        }
        
        if (!is.null(dataset_attachment_var_class)) {
          shinyjs::show("result-vipbubbleplot")
          
          # vip-bubble plot
          output$vipbubble_plot <- renderPlot({
            
            # prepare data
            vip.score <- data.frame('metabolite' = colnames(origdataset)[2:length(origdataset)],
                                    'vip' = round(model@vipVn, digits = 6),
                                    'p1' = round(model@loadingMN, digits = 6))
            
            # 根据选定的分组列名，新增一列group补充到matrix数据中
            dataset_withgroup <- preprocessed_data
            dataset_withgroup['group'] <- dataset_attachment_sample_group[[input$settings_groupcol_selection]]
            
            # 根据选择的数据分组进行数据筛选
            selected_data_group1 <- dataset_withgroup[dataset_withgroup$group == input$select_data_group[1], ]
            selected_data_group2 <- dataset_withgroup[dataset_withgroup$group == input$select_data_group[2], ]
            
            # 计算每个变量的fold-change
            # 注意第一列为样本名称，最后一列为样本分组
            fold_change <- c()
            for (i in 2:(ncol(selected_data_group1) - 1)) {
              fc = mean(selected_data_group1[, i]) / mean(selected_data_group2[, i])
              fold_change = append(fold_change, fc)
            }
            vip.score['Fold Change'] = round(fold_change, digits = 6)
            vip.score['Log2(Fold Change)'] = round(log(fold_change, base = 2), digits = 6)
            
            # 计算每个变量的p-value
            p_value <- c()
            for (i in 2:(ncol(selected_data_group1) - 1)) {
              if (input$opls_vipbubbleplot_pvalue_calc_method == 'T-test') {
                p = t.test(selected_data_group1[, i], selected_data_group2[, i], alternative = "two.sided", paired = FALSE, var.equal = TRUE)[["p.value"]]
              } else {
                p = wilcox.test(selected_data_group1[, i], selected_data_group2[, i], alternative = "two.sided", paired = FALSE)[["p.value"]]
              }
              p_value <- append(p_value, p)
            }
            vip.score['P-value'] = round(p_value, digits = 6)
            vip.score['Log10(P-value)'] = round(log10(p_value), digits = 6)
            
            # 计算Q-value
            q_value <- p.adjust(p_value, method = 'fdr')
            vip.score['Q-value'] = round(q_value, digits = 6)
            vip.score['Log10(Q-value)'] = round(log10(q_value), digits = 6)
            
            # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
            variable_class_mapping <- data.frame(matrix(ncol = nrow(origdataset), nrow = 0), check.names = FALSE)
            variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment_var_class[[2]]))
            colnames(variable_class_mapping) <- dataset_attachment_var_class[[1]]
            variable_class <- c()
            for (v in vip.score[["metabolite"]]) {
              if (v %in% colnames(variable_class_mapping)) {
                variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
              } else {
                toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
                variable_class <- append(variable_class, "unknown")
              }
            }
            vip.score["class"] = variable_class
            opls_result_vipbubble_data <<- vip.score
            
            # inspect data
            output$opls_vip_bubble_data <- DT::renderDT({
              DT::datatable({
                opls_result_vipbubble_data
              },
              options = list(initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px'});",
                "}"),
                columnDefs = list(list(className='dt-left', targets="_all")),
                scrollX = FALSE,
                searching = FALSE,
                paging = FALSE,
                bInfo = TRUE),
              selection = 'none',
              style = 'bootstrap4',
              class = 'cell-border stripe compact datatable',
              rownames = FALSE
              )
            })
            
            legend_position <- 'none'
            if (input$opls_vipbubbleplot_legend_position == 'outside') {
              legend_position <- input$opls_vipbubbleplot_legend_position_outside
            } else if (input$opls_vipbubbleplot_legend_position == 'none') {
              legend_position <- 'none'
            } else if (input$opls_vipbubbleplot_legend_position == 'inside') {
              legend_position <- c(input$opls_vipbubbleplot_legend_position_inside_x, input$opls_vipbubbleplot_legend_position_inside_y)
            }
            
            theme_settings <- theme_bw() +
              theme(legend.position = legend_position,
                    legend.justification = "center",
                    legend.box = "vertical",
                    legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    panel.background = element_rect(fill = input$opls_vipbubbleplot_panel_background_fill_color),
                    plot.title = element_text(family = input$opls_vipbubbleplot_title_fontfamily, size = input$opls_vipbubbleplot_title_fontsize, vjust = 1, hjust = input$opls_vipbubbleplot_title_position),
                    axis.title.x = element_text(family = input$opls_vipbubbleplot_xaxis_fontfamily, size = input$opls_vipbubbleplot_xaxis_fontsize),
                    axis.title.y = element_text(family = input$opls_vipbubbleplot_yaxis_fontfamily, size = input$opls_vipbubbleplot_yaxis_fontsize),
                    axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    axis.ticks = element_line(color = 'black')
              )
            
            if (input$opls_vipbubbleplot_panel_settings_showgrid == FALSE) {
              theme_settings <- theme_settings + theme(panel.grid = element_blank())
            }
            
            # filter
            opls_result_vipbubbleplot_filterdata <<- vip.score[vip.score$vip >= input$opls_vipbubbleplot_vip_threshold,]
            opls_result_vipbubbleplot_filterdata$metabolite <- factor(opls_result_vipbubbleplot_filterdata$metabolite, 
                                                                      levels = opls_result_vipbubbleplot_filterdata$metabolite)
            
            if (input$opls_vipbubbleplot_yfield == 'P-value') {
              opls_result_vipbubbleplot <<- ggplot(opls_result_vipbubbleplot_filterdata, aes(x = vip, y = -`Log10(P-value)`, color = class))
              updateTextInput(inputId = 'opls_vipbubbleplot_yaxis_label', value = '-Log10(P-value)')
            } else if (input$opls_vipbubbleplot_yfield == 'Q-value') {
              opls_result_vipbubbleplot <<- ggplot(opls_result_vipbubbleplot_filterdata, aes(x = vip, y = -`Log10(Q-value)`, color = class))
              updateTextInput(inputId = 'opls_vipbubbleplot_yaxis_label', value = '-Log10(Q-value)')
            }
            
            
            if (input$opls_vipbubbleplot_showhline == TRUE) {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + geom_hline(yintercept = input$opls_vipbubbleplot_hline_value, 
                                                                                   size = input$opls_vipbubbleplot_hline_size,
                                                                                   linetype = input$opls_vipbubbleplot_hline_shape,
                                                                                   color = input$opls_vipbubbleplot_hline_color)
            }
            
            if (input$opls_vipbubbleplot_showvline == TRUE) {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + geom_vline(xintercept = input$opls_vipbubbleplot_vline_value, 
                                                                                   size = input$opls_vipbubbleplot_vline_size,
                                                                                   linetype = input$opls_vipbubbleplot_vline_shape,
                                                                                   color = input$opls_vipbubbleplot_vline_color)
            }
            
            opls_result_vipbubbleplot <<- opls_result_vipbubbleplot +
              geom_point(aes(size = abs(`Log2(Fold Change)`)), alpha = input$opls_vipbubbleplot_point_fillalpha) +
              scale_size_area(max_size = input$opls_vipbubbleplot_point_maxsize) +
              theme_settings +
              ggtitle(input$opls_vipbubbleplot_title_text) +
              xlab(input$opls_vipbubbleplot_xaxis_label) + ylab(input$opls_vipbubbleplot_yaxis_label)
            
            if (input$opls_vipbubbleplot_showlabel == TRUE) {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + 
                geom_text_repel(box.padding = unit(0.1, "cm"), aes(label = metabolite), data = opls_result_vipbubbleplot_filterdata,
                                max.overlaps = 9999, size = 2.65, show.legend = F)
            }
            
            # color palette
            if (input$opls_vipbubbleplot_datapoint_color_palette == 'd3') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_d3()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'aaas') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_aaas()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'jama') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_jama()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'jco') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_jco()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'lancet') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_lancet()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'locuszoom') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_locuszoom()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'nejm') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_nejm()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'npg') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_npg()
            } else if (input$opls_vipbubbleplot_datapoint_color_palette == 'simpsons') {
              opls_result_vipbubbleplot <<- opls_result_vipbubbleplot + scale_color_simpsons()
            }
            
            return(opls_result_vipbubbleplot)
            
          }, height = function() {
            opls_result_vipbubbleplot_height <<- input$opls_result_vipbubbleplot_height
            return(opls_result_vipbubbleplot_height)
          }, width = function() {
            opls_result_vipbubbleplot_width <<- input$opls_result_vipbubbleplot_width
            return(opls_result_vipbubbleplot_width)
          })
        } else {
          toastr_warning(message = "VIP-bubble plot need metabolite classification data", 
                         title = "Please upload metabolite classification data")
        }
        
        # bio plot
        output$bio_plot <- renderPlot({
          
          # prepare bio data
          c <- ncol(data_matrix)
          r <- nrow(data_matrix)
          
          biodata <- data.frame('group' = c(rep("p(cor)", r), rep("t(cor)", c)),
                                'metabolite/sample ID' = c(colnames(data_matrix), as.vector(selected_group_data[[1]])),
                                'class/group' = c(dataset_attachment_var_class[[1]], selected_group_data$group))
          colnames(biodata) <- c('group', 'metabolite/sample ID', 'class/group')
          biodata$group <- as.factor(biodata$group)
          
          
          A1 <- c()
          A2 <- c()
          for (i in 1:c) {
            A1 <- append(A1, cor(data_matrix[, i], model@scoreMN[, 1]))
            A2 <- append(A2, cor(data_matrix[, i], model@orthoScoreMN[, 1]))
          }
          for (i in 1:r) {
            A1 <- append(A1, cor(unlist(data_matrix[i, ], use.names = FALSE), model@loadingMN[, 1]))
            A2 <- append(A2, cor(unlist(data_matrix[i, ], use.names = FALSE), model@orthoLoadingMN[, 1]))
          }
          
          biodata["A1"] <- round(A1, digits = 6)
          biodata["A2"] <- round(A2, digits = 6)
          
          opls_result_bioplot_data <<- biodata
          
          # inspect bioplot data
          output$opls_bioplot_data <- DT::renderDT({
            DT::datatable({
              opls_result_bioplot_data
            },
            options = list(initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px'});",
              "}"),
              columnDefs = list(list(className='dt-left', targets="_all")),
              scrollX = FALSE,
              searching = FALSE,
              paging = FALSE,
              bInfo = TRUE),
            selection = 'none',
            style = 'bootstrap4',
            class = 'cell-border stripe compact datatable',
            rownames = FALSE
            )
          })
          
          # ggplot
          legend_position <- 'none'
          if (input$opls_bioplot_legend_position == 'outside') {
            legend_position <- input$opls_bioplot_legend_position_outside
          } else if (input$opls_bioplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$opls_bioplot_legend_position == 'inside') {
            legend_position <- c(input$opls_bioplot_legend_position_inside_x, input$opls_bioplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$opls_bioplot_panel_background_fill_color),
                  plot.title = element_text(family = input$opls_bioplot_title_fontfamily, size = input$opls_bioplot_title_fontsize, vjust = 1, hjust = input$opls_bioplot_title_position),
                  axis.title.x = element_text(family = input$opls_bioplot_xaxis_fontfamily, size = input$opls_bioplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$opls_bioplot_yaxis_fontfamily, size = input$opls_bioplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$opls_bioplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          opls_result_bioplot <<- ggplot(biodata, aes(A1, A2, label = `metabolite/sample ID`))
          
          if (input$opls_bioplot_showhline == TRUE) {
            opls_result_bioplot <<- opls_result_bioplot + geom_hline(yintercept = input$opls_bioplot_hline_value,
                                                                     linetype = input$opls_bioplot_hline_shape,
                                                                     linewidth = input$opls_bioplot_hline_size,
                                                                     color = input$opls_bioplot_hline_color)
          }
          if (input$opls_bioplot_showvline == TRUE) {
            opls_result_bioplot <<- opls_result_bioplot + geom_vline(xintercept = input$opls_bioplot_vline_value,
                                                                     linetype = input$opls_bioplot_vline_shape,
                                                                     linewidth = input$opls_bioplot_vline_size,
                                                                     color = input$opls_bioplot_vline_color)
          }
          
          opls_result_bioplot <<- opls_result_bioplot +
            geom_circle(aes(x0 = 0, y0 = 0, r = 1), color ='gray65', linewidth = 0.1) +
            geom_circle(aes(x0 = 0, y0 = 0, r = 0.75), color ='gray65', linewidth = 0.1) +
            geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), color ='gray65', linewidth = 0.1) +
            geom_point(aes(fill = group, size = group, color = group), shape = 21) +
            scale_fill_manual(values = c(input$opls_bioplot_datapoint_group1_fill_color, input$opls_bioplot_datapoint_group2_fill_color)) +
            scale_size_manual(values = c(input$opls_bioplot_point_group1_size, input$opls_bioplot_point_group2_size)) +
            scale_color_manual(values = c(input$opls_bioplot_datapoint_group1_border_color, input$opls_bioplot_datapoint_group2_border_color)) +
            theme_settings +
            ggtitle(input$opls_bioplot_title_text) +
            xlab(input$opls_bioplot_xaxis_label) + ylab(input$opls_bioplot_yaxis_label)
          
          return(opls_result_bioplot)
        }, height = function() {
          opls_result_bioplot_height <<- input$opls_result_bioplot_height
          return(opls_result_bioplot_height)
        }, width = function() {
          opls_result_bioplot_width <<- input$opls_result_bioplot_width
          return(opls_result_bioplot_width)
        })
        
        if (!is.null(dataset_attachment_var_class)) {
          shinyjs::show("result-splot")
          
          # splot
          output$splot <- renderPlot({
            
            # prepare data
            splot_data <- data.frame('metabolite' = colnames(origdataset)[2:length(origdataset)],
                                    'vip' = round(model@vipVn, digits = 6),
                                    'p1' = round(model@loadingMN, digits = 6))
            
            # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
            variable_class_mapping <- data.frame(matrix(ncol = nrow(origdataset), nrow = 0), check.names = FALSE)
            variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment_var_class[[2]]))
            colnames(variable_class_mapping) <- dataset_attachment_var_class[[1]]
            variable_class <- c()
            for (v in splot_data[["metabolite"]]) {
              if (v %in% colnames(variable_class_mapping)) {
                variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
              } else {
                toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
                variable_class <- append(variable_class, "unknown")
              }
            }
            splot_data["class"] = variable_class
            
            
            # 根据选定的分组列名，新增一列group补充到matrix数据中
            dataset_withgroup <- preprocessed_data
            dataset_withgroup['group'] <- dataset_attachment_sample_group[[input$settings_groupcol_selection]]
            
            # 根据选择的数据分组进行数据筛选
            selected_data_group <- dataset_withgroup[dataset_withgroup$group == input$select_data_group[1] | dataset_withgroup$group == input$select_data_group[2], ]
            
            # 计算变量和model@scoreMN的相关性
            p_corr <- c()
            for (i in 2:(ncol(selected_data_group) - 1)) {
              corr <- cor(selected_data_group[, i], model@scoreMN)
              p_corr <- append(p_corr, corr)
            }
            splot_data['p(corr)'] <- round(p_corr, digits = 6)
            
            opls_result_splot_data <<- splot_data
            
            # inspect data
            output$opls_splot_data <- DT::renderDT({
              DT::datatable({
                opls_result_splot_data
              },
              options = list(initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'font-size': '12px'});",
                "$(this.api().table().header()).css({'font-size': '12px'});",
                "}"),
                columnDefs = list(list(className='dt-left', targets="_all")),
                scrollX = FALSE,
                searching = FALSE,
                paging = FALSE,
                bInfo = TRUE),
              selection = 'none',
              style = 'bootstrap4',
              class = 'cell-border stripe compact datatable',
              rownames = FALSE
              )
            })
            
            legend_position <- 'none'
            if (input$opls_splot_legend_position == 'outside') {
              legend_position <- input$opls_splot_legend_position_outside
            } else if (input$opls_splot_legend_position == 'none') {
              legend_position <- 'none'
            } else if (input$opls_splot_legend_position == 'inside') {
              legend_position <- c(input$opls_splot_legend_position_inside_x, input$opls_splot_legend_position_inside_y)
            }
            
            theme_settings <- theme_bw() +
              theme(legend.position = legend_position,
                    legend.justification = "center",
                    legend.box = "vertical",
                    legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    panel.background = element_rect(fill = input$opls_splot_panel_background_fill_color),
                    plot.title = element_text(family = input$opls_splot_title_fontfamily, size = input$opls_splot_title_fontsize, vjust = 1, hjust = input$opls_splot_title_position),
                    axis.title.x = element_text(family = input$opls_splot_xaxis_fontfamily, size = input$opls_splot_xaxis_fontsize),
                    axis.title.y = element_text(family = input$opls_splot_yaxis_fontfamily, size = input$opls_splot_yaxis_fontsize),
                    axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                    axis.ticks = element_line(color = 'black')
              )
            
            if (input$opls_splot_panel_settings_showgrid == FALSE) {
              theme_settings <- theme_settings + theme(panel.grid = element_blank())
            }
            
            if (input$opls_splot_color_scheme == 'Metabolite class') {
              opls_result_splot <<- ggplot(splot_data, aes(p1, `p(corr)`, fill = class))
            } else {
              opls_result_splot <<- ggplot(splot_data, aes(p1, `p(corr)`))
            }
            
            if (input$opls_splot_showhline == TRUE) {
              opls_result_splot <<- opls_result_splot + geom_hline(yintercept = input$opls_splot_hline_value, 
                                                                   size = input$opls_splot_hline_size,
                                                                   linetype = input$opls_splot_hline_shape,
                                                                   color = input$opls_splot_hline_color)
            }
            
            if (input$opls_splot_showvline == TRUE) {
              opls_result_splot <<- opls_result_splot + geom_vline(xintercept = input$opls_splot_vline_value, 
                                                                   size = input$opls_splot_vline_size,
                                                                   linetype = input$opls_splot_vline_shape,
                                                                   color = input$opls_splot_vline_color)
            }
            
            # 根据代谢物类别着色
            if (input$opls_splot_color_scheme == 'Metabolite class') {
              opls_result_splot <<- opls_result_splot + 
                geom_point(size = input$opls_splot_point_size, 
                           shape = 21, color = 'black', stroke = 0.2, 
                           alpha = input$opls_splot_point_fillalpha)
            }
            # 根据vip阈值着色
            else {
              opls_result_splot <<- opls_result_splot + 
                geom_point(size = input$opls_splot_point_size, shape = 20, 
                           color = ifelse(splot_data$vip >= input$opls_splot_vip_threshold, input$opls_splot_color_gtr_vip_threshold, input$opls_splot_color_ltr_vip_threshold))
            }
            
            # color palette
            color_values <- c()
            if (input$opls_splot_datapoint_color_palette == 'd3') {
              color_values <- pal_d3()(10)
            } else if (input$opls_splot_datapoint_color_palette == 'aaas') {
              color_values <- pal_aaas()(10)
            } else if (input$opls_splot_datapoint_color_palette == 'jama') {
              color_values <- pal_jama()(7)
            } else if (input$opls_splot_datapoint_color_palette == 'jco') {
              color_values <- pal_jco()(10)
            } else if (input$opls_splot_datapoint_color_palette == 'lancet') {
              color_values <- pal_lancet()(9)
            } else if (input$opls_splot_datapoint_color_palette == 'locuszoom') {
              color_values <- pal_locuszoom()(7)
            } else if (input$opls_splot_datapoint_color_palette == 'nejm') {
              color_values <- pal_nejm()(8)
            } else if (input$opls_splot_datapoint_color_palette == 'npg') {
              color_values <- pal_npg()(10)
            } else if (input$opls_splot_datapoint_color_palette == 'simpsons') {
              color_values <- pal_simpsons()(16)
            } else if (input$opls_splot_datapoint_color_palette == 'custom') {
              my_color_map <- unlist(lapply(strsplit(input$opls_splot_datapoint_custom_color_palette_inputs_rgb, split = ","), function(x) str_trim(x)))
              n <- length(levels(factor(splot_data$class)))
              color_values <- my_color_map[1:n]
            }
            # if color is not enough
            n <- length(levels(factor(splot_data$class)))
            if (length(color_values) < n) {
              color_values <- append(color_values, sample(pal_igv()(50), size = (n - length(color_values))))
            }
            
            opls_result_splot <<- opls_result_splot +
              scale_fill_manual(values = color_values) +
              theme_settings +
              ggtitle(input$opls_splot_title_text) +
              xlab(input$opls_splot_xaxis_label) + ylab(input$opls_splot_yaxis_label)
            
            return(opls_result_splot)
            
          }, height = function() {
            opls_result_splot_height <<- input$opls_result_splot_height
            return(opls_result_splot_height)
          }, width = function() {
            opls_result_splot_width <<- input$opls_result_splot_width
            return(opls_result_splot_width)
          })
        } else {
          toastr_warning(message = "s-plot need metabolite classification data", 
                         title = "Please upload metabolite classification data")
        }
        
        
        shinyjs::show("result-vplot")
        # vplot
        output$vplot <- renderPlot({
          
          # prepare data
          vplot_data <- data.frame('metabolite' = colnames(origdataset)[2:length(origdataset)],
                                   'vip' = round(model@vipVn, digits = 6),
                                   'p1' = round(model@loadingMN, digits = 6))
          
          # 根据选定的分组列名，新增一列group补充到matrix数据中
          dataset_withgroup <- preprocessed_data
          dataset_withgroup['group'] <- dataset_attachment_sample_group[[input$settings_groupcol_selection]]
          
          # 根据选择的数据分组进行数据筛选
          selected_data_group <- dataset_withgroup[dataset_withgroup$group == input$select_data_group[1] | dataset_withgroup$group == input$select_data_group[2], ]
          
          # 计算变量和model@scoreMN的相关性
          p_corr <- c()
          for (i in 2:(ncol(selected_data_group) - 1)) {
            corr <- cor(selected_data_group[, i], model@scoreMN)
            p_corr <- append(p_corr, corr)
          }
          vplot_data['p(corr)'] <- round(p_corr, digits = 6)
          
          opls_result_vplot_data <<- vplot_data
          
          # inspect data
          output$opls_vplot_data <- DT::renderDT({
            DT::datatable({
              opls_result_vplot_data
            },
            options = list(initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px'});",
              "}"),
              columnDefs = list(list(className='dt-left', targets="_all")),
              scrollX = FALSE,
              searching = FALSE,
              paging = FALSE,
              bInfo = TRUE),
            selection = 'none',
            style = 'bootstrap4',
            class = 'cell-border stripe compact datatable',
            rownames = FALSE
            )
          })
          
          legend_position <- 'none'
          if (input$opls_vplot_legend_position == 'outside') {
            legend_position <- input$opls_vplot_legend_position_outside
          } else if (input$opls_vplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$opls_vplot_legend_position == 'inside') {
            legend_position <- c(input$opls_vplot_legend_position_inside_x, input$opls_vplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$opls_vplot_panel_background_fill_color),
                  plot.title = element_text(family = input$opls_vplot_title_fontfamily, size = input$opls_vplot_title_fontsize, vjust = 1, hjust = input$opls_vplot_title_position),
                  axis.title.x = element_text(family = input$opls_vplot_xaxis_fontfamily, size = input$opls_vplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$opls_vplot_yaxis_fontfamily, size = input$opls_vplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$opls_vplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          opls_result_vplot <<- ggplot(vplot_data, aes(`p(corr)`, vip)) +
            geom_point(size = input$opls_vplot_point_size, shape = 20, 
                       color = ifelse(vplot_data$vip >= input$opls_vplot_vip_threshold, input$opls_vplot_color_gtr_vip_threshold, input$opls_vplot_color_ltr_vip_threshold))
          
          if (input$opls_vplot_show_label == TRUE) {
            opls_result_vplot <<- opls_result_vplot + 
              geom_text_repel(data = vplot_data[vplot_data$vip >= input$opls_vplot_vip_threshold, ], aes(label = metabolite),
                              size = 2.3, segment.size = 0.1, max.overlaps = 9999)
          }
          
          opls_result_vplot <<- opls_result_vplot + 
            theme_settings +
            ggtitle(input$opls_vplot_title_text) +
            xlab(input$opls_vplot_xaxis_label) + ylab(input$opls_vplot_yaxis_label)
          
          return(opls_result_vplot)
        }, height = function() {
          opls_result_vplot_height <<- input$opls_result_vplot_height
          return(opls_result_vplot_height)
        }, width = function() {
          opls_result_vplot_width <<- input$opls_result_vplot_width
          return(opls_result_vplot_width)
        })
        
        
        shinyjs::show("result-permutationplot")
        # permutationplot
        output$permutationplot <- renderPlot({
          
          # prepare data
          permutationplot_data1 <- model@suppLs$permMN %>% as.data.frame() %>% select(`sim`,`R2Y(cum)`)
          colnames(permutationplot_data1) <- c('x', 'y')
          permutationplot_data1 <- permutationplot_data1 %>% mutate("index" = "R2")
          
          permutationplot_data2 <- model@suppLs$permMN %>% as.data.frame() %>% select(`sim`,`Q2(cum)`)
          colnames(permutationplot_data2) <- c('x', 'y')
          permutationplot_data2 <- permutationplot_data2 %>% mutate("index" = "Q2")
          
          permutationplot_data <- rbind(permutationplot_data1, permutationplot_data2)
          
          permutationplot_data$index <- as.factor(permutationplot_data$index)
          
          permutationplot_data$x1 = (permutationplot_data$x - min(permutationplot_data$x)) / (max(permutationplot_data$x) - min(permutationplot_data$x)) / 4 # x数据归一化到1/4
          br <- quantile(permutationplot_data %>% filter(index == "R2") %>% select(y) %>% unlist(), 0.25)
          bq <- quantile(permutationplot_data %>% filter(index == "Q2") %>% select(y) %>% unlist(), 0.25)
          kr <- (model@summaryDF$`R2Y(cum)` - br) / permutationplot_data$x1[1]
          kq <- (model@summaryDF$`Q2(cum)` - bq) / permutationplot_data$x1[1]
          
          opls_result_permutationplot_data <<- model@suppLs$permMN %>% as.data.frame() %>% select(`sim`, `R2Y(cum)`, `Q2(cum)`)
          
          # inspect data
          output$opls_permutationplot_data <- DT::renderDT({
            DT::datatable({
              opls_result_permutationplot_data
            },
            options = list(initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().body()).css({'font-size': '12px'});",
              "$(this.api().table().header()).css({'font-size': '12px'});",
              "}"),
              columnDefs = list(list(className='dt-left', targets="_all")),
              scrollX = FALSE,
              searching = FALSE,
              paging = FALSE,
              bInfo = TRUE),
            selection = 'none',
            style = 'bootstrap4',
            class = 'cell-border stripe compact datatable',
            rownames = FALSE
            )
          })
          
          legend_position <- 'none'
          if (input$opls_permutationplot_legend_position == 'outside') {
            legend_position <- input$opls_permutationplot_legend_position_outside
          } else if (input$opls_permutationplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$opls_permutationplot_legend_position == 'inside') {
            legend_position <- c(input$opls_permutationplot_legend_position_inside_x, input$opls_permutationplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$opls_permutationplot_panel_background_fill_color),
                  plot.title = element_text(family = input$opls_permutationplot_title_fontfamily, size = input$opls_permutationplot_title_fontsize, vjust = 1, hjust = input$opls_permutationplot_title_position),
                  axis.title.x = element_text(family = input$opls_permutationplot_xaxis_fontfamily, size = input$opls_permutationplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$opls_permutationplot_yaxis_fontfamily, size = input$opls_permutationplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$opls_permutationplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          opls_result_permutationplot <<- ggplot(permutationplot_data, aes(x1, y, fill = index, shape = index, size = index))+
            geom_abline(slope = kr, intercept = br, 
                        linetype = input$opls_permutationplot_line_r2_type, 
                        size = input$opls_permutationplot_line_r2_weight, 
                        color = input$opls_permutationplot_line_r2_color) +
            geom_abline(slope = kq, intercept = bq, 
                        linetype = input$opls_permutationplot_line_q2_type,
                        size = input$opls_permutationplot_line_q2_weight,
                        color = input$opls_permutationplot_line_q2_color) +
            geom_point() +
            scale_shape_manual(values = c(as.numeric(input$opls_permutationplot_point_r2_shape), as.numeric(input$opls_permutationplot_point_q2_shape))) +
            scale_fill_manual(values = c(input$opls_permutationplot_point_r2_color, input$opls_permutationplot_point_q2_color)) +
            scale_size_manual(values = c(input$opls_permutationplot_point_r2_size, input$opls_permutationplot_point_q2_size)) +
            theme_settings +
            ggtitle(input$opls_permutationplot_title_text) +
            xlab(input$opls_permutationplot_xaxis_label) + ylab(input$opls_permutationplot_yaxis_label)
          
          return(opls_result_permutationplot)
        }, height = function() {
          opls_result_permutationplot_height <<- input$opls_result_permutationplot_height
          return(opls_result_permutationplot_height)
        }, width = function() {
          opls_result_permutationplot_width <<- input$opls_result_permutationplot_width
          return(opls_result_permutationplot_width)
        })
        
      }
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "运行时遇到错误", message = '')
    })
  })
  
  # 点击按钮后，显示非模态对话框
  observeEvent(input$open_score_plot_settings, {
    shinyjs::show("score-plot-settings")
  })
  
  # 点击关闭按钮后，隐藏非模态对话框
  observeEvent(input$close_score_plot_settings, {
    shinyjs::hide("score-plot-settings")
  })
  
  # 改变所选择调色板的背景图片
  observeEvent(input$opls_scoreplot_confidence_fillcolor_multi, {
    runjs(changeBgOfSelectedColorPalette('opls_scoreplot_confidence_fillcolor_multi', input$opls_scoreplot_confidence_fillcolor_multi))
  })
  
  # 改变所选择调色板的背景图片
  observeEvent(input$opls_scoreplot_datapoint_fillcolor, {
    runjs(changeBgOfSelectedColorPalette('opls_scoreplot_datapoint_fillcolor', input$opls_scoreplot_datapoint_fillcolor))
  })
  
  # 改变所选择线条类型的背景图片
  observeEvent(input$opls_scoreplot_confidence_linetype, {
    runjs(changeBgOfSelectedLineType(input$opls_scoreplot_confidence_linetype))
  })
  
  # 是否填充颜色
  observeEvent(input$opls_scoreplot_confidence_fillornot, {
    if (input$opls_scoreplot_confidence_fillornot == 'noFill') {
      shinyjs::hide('opls_scoreplot_confidence_fillcolor_single')
      shinyjs::hide('opls_scoreplot_confidence_fillcolor_multi')
      shinyjs::hide('opls_scoreplot_confidence_fillalpha')
    } else if (input$opls_scoreplot_confidence_fillornot == 'solidColor') {
      shinyjs::show('opls_scoreplot_confidence_fillalpha')
      if (input$opls_scoreplot_confidence_datacoverage == 'TRUE') {
        shinyjs::show('opls_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_single')
      } else {
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_multi')
        shinyjs::show('opls_scoreplot_confidence_fillcolor_single')
      }
    }
  })
  
  # 是否显示线条
  observeEvent(input$opls_scoreplot_confidence_withlineornot, {
    if (input$opls_scoreplot_confidence_withlineornot == 'solidLine') {
      shinyjs::show('opls_scoreplot_confidence_linetype')
      shinyjs::show('opls_scoreplot_confidence_lineweight')
      shinyjs::show('opls_scoreplot_confidence_linealpha')
    } else {
      shinyjs::hide('opls_scoreplot_confidence_linetype')
      shinyjs::hide('opls_scoreplot_confidence_lineweight')
      shinyjs::hide('opls_scoreplot_confidence_linealpha')
    }
  })
  
  # 选择for all data时，显示单个颜色选择器
  # 选择for group data时，显示分组颜色选择器
  observeEvent(input$opls_scoreplot_confidence_datacoverage, {
    if (input$opls_scoreplot_confidence_datacoverage == 'TRUE') {
      if (input$opls_scoreplot_confidence_fillornot == 'noFill') {
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_single')
        shinyjs::hide('opls_scoreplot_confidence_fillalpha')
      } else {
        shinyjs::show('opls_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_single')
        shinyjs::show('opls_scoreplot_confidence_fillalpha')
      }
    } else {
      if (input$opls_scoreplot_confidence_fillornot == 'noFill') {
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_single')
        shinyjs::hide('opls_scoreplot_confidence_fillalpha')
      } else {
        shinyjs::hide('opls_scoreplot_confidence_fillcolor_multi')
        shinyjs::show('opls_scoreplot_confidence_fillcolor_single')
        shinyjs::show('opls_scoreplot_confidence_fillalpha')
      }
    }
    
  })
  
  # scoreplot legend显示位置
  observeEvent(input$opls_scoreplot_legend_position, {
    if (input$opls_scoreplot_legend_position == 'none') {
      shinyjs::hide('opls_scoreplot_legend_position_outside')
      shinyjs::hide('opls_scoreplot_legend_position_inside_x')
      shinyjs::hide('opls_scoreplot_legend_position_inside_y')
    } else if (input$opls_scoreplot_legend_position == 'inside') {
      shinyjs::hide('opls_scoreplot_legend_position_outside')
      shinyjs::show('opls_scoreplot_legend_position_inside_x')
      shinyjs::show('opls_scoreplot_legend_position_inside_y')
    } else if (input$opls_scoreplot_legend_position == 'outside') {
      shinyjs::show('opls_scoreplot_legend_position_outside')
      shinyjs::hide('opls_scoreplot_legend_position_inside_x')
      shinyjs::hide('opls_scoreplot_legend_position_inside_y')
    }
  })
  
  # loadingplot legend显示位置
  observeEvent(input$opls_loadingplot_legend_position, {
    if (input$opls_loadingplot_legend_position == 'none') {
      shinyjs::hide('opls_loadingplot_legend_position_outside')
      shinyjs::hide('opls_loadingplot_legend_position_inside_x')
      shinyjs::hide('opls_loadingplot_legend_position_inside_y')
    } else if (input$opls_loadingplot_legend_position == 'inside') {
      shinyjs::hide('opls_loadingplot_legend_position_outside')
      shinyjs::show('opls_loadingplot_legend_position_inside_x')
      shinyjs::show('opls_loadingplot_legend_position_inside_y')
    } else if (input$opls_loadingplot_legend_position == 'outside') {
      shinyjs::show('opls_loadingplot_legend_position_outside')
      shinyjs::hide('opls_loadingplot_legend_position_inside_x')
      shinyjs::hide('opls_loadingplot_legend_position_inside_y')
    }
  })
  
  # bioplot legend显示位置
  observeEvent(input$opls_bioplot_legend_position, {
    if (input$opls_bioplot_legend_position == 'none') {
      shinyjs::hide('opls_bioplot_legend_position_outside')
      shinyjs::hide('opls_bioplot_legend_position_inside_x')
      shinyjs::hide('opls_bioplot_legend_position_inside_y')
    } else if (input$opls_bioplot_legend_position == 'inside') {
      shinyjs::hide('opls_bioplot_legend_position_outside')
      shinyjs::show('opls_bioplot_legend_position_inside_x')
      shinyjs::show('opls_bioplot_legend_position_inside_y')
    } else if (input$opls_bioplot_legend_position == 'outside') {
      shinyjs::show('opls_bioplot_legend_position_outside')
      shinyjs::hide('opls_bioplot_legend_position_inside_x')
      shinyjs::hide('opls_bioplot_legend_position_inside_y')
    }
  })
  
  # bioplot 是否显示hline,vline
  observeEvent(input$opls_bioplot_showhline, {
    if (input$opls_bioplot_showhline == TRUE) {
      shinyjs::show('opls_bioplot_hline_value')
      shinyjs::show('opls_bioplot_hline_shape')
      shinyjs::show('opls_bioplot_hline_size')
      shinyjs::show('opls_bioplot_hline_color')
    } else {
      shinyjs::hide('opls_bioplot_hline_value')
      shinyjs::hide('opls_bioplot_hline_shape')
      shinyjs::hide('opls_bioplot_hline_size')
      shinyjs::hide('opls_bioplot_hline_color')
    }
  })
  
  # viploilipopplot legend显示位置
  observeEvent(input$opls_viploilipopplot_legend_position, {
    if (input$opls_viploilipopplot_legend_position == 'none') {
      shinyjs::hide('opls_viploilipopplot_legend_position_outside')
      shinyjs::hide('opls_viploilipopplot_legend_position_inside_x')
      shinyjs::hide('opls_viploilipopplot_legend_position_inside_y')
    } else if (input$opls_viploilipopplot_legend_position == 'inside') {
      shinyjs::hide('opls_viploilipopplot_legend_position_outside')
      shinyjs::show('opls_viploilipopplot_legend_position_inside_x')
      shinyjs::show('opls_viploilipopplot_legend_position_inside_y')
    } else if (input$opls_viploilipopplot_legend_position == 'outside') {
      shinyjs::show('opls_viploilipopplot_legend_position_outside')
      shinyjs::hide('opls_viploilipopplot_legend_position_inside_x')
      shinyjs::hide('opls_viploilipopplot_legend_position_inside_y')
    }
  })
  
  
  # viploilipopplot datapoint 是否为不同类别着色，如果是则显示调色板
  observeEvent(input$opls_viploilipopplot_datapoint_class_colorful, {
    if (input$opls_viploilipopplot_datapoint_class_colorful == TRUE) {
      shinyjs::show('opls_viploilipopplot_datapoint_color_palette')
      shinyjs::hide('opls_viploilipopplot_datapoint_fill_color')
      shinyjs::hide('opls_viploilipopplot_datapoint_border_color')
    } else {
      shinyjs::hide('opls_viploilipopplot_datapoint_color_palette')
      shinyjs::show('opls_viploilipopplot_datapoint_fill_color')
      shinyjs::show('opls_viploilipopplot_datapoint_border_color')
    }
  })
  
  # viploilipopplot segment 是否为不同类别着色，如果是则显示调色板
  observeEvent(input$opls_viploilipopplot_segment_class_colorful, {
    if (input$opls_viploilipopplot_segment_class_colorful == TRUE) {
      shinyjs::show('opls_viploilipopplot_segment_color_palette')
      shinyjs::hide('opls_viploilipopplot_segment_color')
    } else {
      shinyjs::hide('opls_viploilipopplot_segment_color_palette')
      shinyjs::show('opls_viploilipopplot_segment_color')
    }
  })
  
  # splot legend显示位置
  observeEvent(input$opls_splot_legend_position, {
    if (input$opls_splot_legend_position == 'none') {
      shinyjs::hide('opls_splot_legend_position_outside')
      shinyjs::hide('opls_splot_legend_position_inside_x')
      shinyjs::hide('opls_splot_legend_position_inside_y')
    } else if (input$opls_splot_legend_position == 'inside') {
      shinyjs::hide('opls_splot_legend_position_outside')
      shinyjs::show('opls_splot_legend_position_inside_x')
      shinyjs::show('opls_splot_legend_position_inside_y')
    } else if (input$opls_splot_legend_position == 'outside') {
      shinyjs::show('opls_splot_legend_position_outside')
      shinyjs::hide('opls_splot_legend_position_inside_x')
      shinyjs::hide('opls_splot_legend_position_inside_y')
    }
  })
  
  # splot 着色方案
  observeEvent(input$opls_splot_color_scheme, {
    if (input$opls_splot_color_scheme == 'Metabolite class') {
      shinyjs::hide('opls_splot_vip_threshold')
      shinyjs::hide('opls_splot_color_gtr_vip_threshold')
      shinyjs::hide('opls_splot_color_ltr_vip_threshold')
      shinyjs::show('opls_splot_datapoint_color_palette')
      shinyjs::show('opls_splot_point_fillalpha')
    } else {
      shinyjs::show('opls_splot_vip_threshold')
      shinyjs::show('opls_splot_color_gtr_vip_threshold')
      shinyjs::show('opls_splot_color_ltr_vip_threshold')
      shinyjs::hide('opls_splot_datapoint_color_palette')
      shinyjs::hide('opls_splot_point_fillalpha')
    }
  })
  
  observeEvent(input$opls_splot_datapoint_color_palette, {
    if (input$opls_splot_datapoint_color_palette == 'custom') {
      shinyjs::show('opls_splot_datapoint_custom_color_palette_inputs_rgb')
      shinyjs::show('opls_splot_datapoint_custom_color_palette_block')
    } else {
      shinyjs::hide('opls_splot_datapoint_custom_color_palette_inputs_rgb')
      shinyjs::hide('opls_splot_datapoint_custom_color_palette_block')
    }
  })
  
  
  # vplot legend显示位置
  observeEvent(input$opls_vplot_legend_position, {
    if (input$opls_vplot_legend_position == 'none') {
      shinyjs::hide('opls_vplot_legend_position_outside')
      shinyjs::hide('opls_vplot_legend_position_inside_x')
      shinyjs::hide('opls_vplot_legend_position_inside_y')
    } else if (input$opls_vplot_legend_position == 'inside') {
      shinyjs::hide('opls_vplot_legend_position_outside')
      shinyjs::show('opls_vplot_legend_position_inside_x')
      shinyjs::show('opls_vplot_legend_position_inside_y')
    } else if (input$opls_vplot_legend_position == 'outside') {
      shinyjs::show('opls_vplot_legend_position_outside')
      shinyjs::hide('opls_vplot_legend_position_inside_x')
      shinyjs::hide('opls_vplot_legend_position_inside_y')
    }
  })
  
  # permutationplot legend显示位置
  observeEvent(input$opls_permutationplot_legend_position, {
    if (input$opls_permutationplot_legend_position == 'none') {
      shinyjs::hide('opls_permutationplot_legend_position_outside')
      shinyjs::hide('opls_permutationplot_legend_position_inside_x')
      shinyjs::hide('opls_permutationplot_legend_position_inside_y')
    } else if (input$opls_permutationplot_legend_position == 'inside') {
      shinyjs::hide('opls_permutationplot_legend_position_outside')
      shinyjs::show('opls_permutationplot_legend_position_inside_x')
      shinyjs::show('opls_permutationplot_legend_position_inside_y')
    } else if (input$opls_permutationplot_legend_position == 'outside') {
      shinyjs::show('opls_permutationplot_legend_position_outside')
      shinyjs::hide('opls_permutationplot_legend_position_inside_x')
      shinyjs::hide('opls_permutationplot_legend_position_inside_y')
    }
  })
  
  
  # 导出数据预处理结果
  observeEvent(input$export_preprocessed_data, {
    showModal(modalDialog(
      title = "Download data",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_opls_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_opls_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_opls_result_table_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  output$export_opls_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_table_name) || input$export_opls_result_table_name == "") {
        paste("preprocessed-data-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_table_format, sep="")
      } else {
        paste(input$export_opls_result_table_name, input$export_opls_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_opls_result_table_format == ".csv") {
        write.csv(preprocessed_data, file, row.names = FALSE)
      } else if (input$export_opls_result_table_format == ".xlsx") {
        write.xlsx(preprocessed_data, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  
  # 导出pca得分图
  observeEvent(input$export_opls_result_scoreplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_scoreplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_scoreplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_scoreplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_scoreplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_scoreplot_name) || input$export_opls_result_scoreplot_name == "") {
        paste("scoreplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_scoreplot_format, sep="")
      } else {
        paste(input$export_opls_result_scoreplot_name, input$export_opls_result_scoreplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_scoreplot,
             width = opls_result_scoreplot_width / dpi,
             height = opls_result_scoreplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载 scoreplot 对应的数据
  output$download_opls_scoreplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-scoreplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_scoreplot_data, file, asTable = TRUE)
    }
  )
  
  # 导出 opls loading plot
  observeEvent(input$export_opls_result_loadingplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_loadingplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_loadingplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_loadingplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_loadingplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_loadingplot_name) || input$export_opls_result_loadingplot_name == "") {
        paste("loadingplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_loadingplot_format, sep="")
      } else {
        paste(input$export_opls_result_loadingplot_name, input$export_opls_result_loadingplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_loadingplot,
             width = opls_result_loadingplot_width / dpi,
             height = opls_result_loadingplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载 loadingplot 对应的数据
  output$download_opls_loadingplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-loadingplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_loadingplot_data, file, asTable = TRUE)
    }
  )
  
  # 导出 opls bio plot
  observeEvent(input$export_opls_result_bioplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_bioplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_bioplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_bioplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_bioplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_bioplot_name) || input$export_opls_result_bioplot_name == "") {
        paste("bioplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_bioplot_format, sep="")
      } else {
        paste(input$export_opls_result_bioplot_name, input$export_opls_result_bioplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_bioplot,
             width = opls_result_bioplot_width / dpi,
             height = opls_result_bioplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载 bioplot 对应的数据
  output$download_opls_bioplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-bioplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_bioplot_data, file, asTable = TRUE)
    }
  )
  
  
  # 导出 opls vip-loilipopplot
  observeEvent(input$export_opls_result_viploilipopplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_viploilipopplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_viploilipopplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_viploilipopplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_viploilipopplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_viploilipopplot_name) || input$export_opls_result_viploilipopplot_name == "") {
        paste("viploilipopplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_viploilipopplot_format, sep="")
      } else {
        paste(input$export_opls_result_viploilipopplot_name, input$export_opls_result_viploilipopplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_viploilipopplot,
             width = opls_result_viploilipopplot_width / dpi,
             height = opls_result_viploilipopplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载vip loilipopplot 对应的数据
  output$download_opls_viploilipop_data <- downloadHandler(
    filename = function() {
      paste0("opls-vip-loilipopplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_viploilipop_data, file, asTable = TRUE)
    }
  )
  
  # 导出 opls vip-bubbleplot
  observeEvent(input$export_opls_result_vipbubbleplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_vipbubbleplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_vipbubbleplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_vipbubbleplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_vipbubbleplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_vipbubbleplot_name) || input$export_opls_result_vipbubbleplot_name == "") {
        paste("vipbubbleplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_vipbubbleplot_format, sep="")
      } else {
        paste(input$export_opls_result_vipbubbleplot_name, input$export_opls_result_vipbubbleplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_vipbubbleplot,
             width = opls_result_vipbubbleplot_width / dpi,
             height = opls_result_vipbubbleplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载vip-bubble-plot对应的数据
  output$download_opls_vip_bubble_data <- downloadHandler(
    filename = function() {
      paste0("opls-vip-bubbleplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_vipbubble_data, file, asTable = TRUE)
    }
  )
  
  # 导出 opls splot
  observeEvent(input$export_opls_result_splot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_splot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_splot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_splot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_splot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_splot_name) || input$export_opls_result_splot_name == "") {
        paste("splot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_splot_format, sep="")
      } else {
        paste(input$export_opls_result_splot_name, input$export_opls_result_splot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_splot,
             width = opls_result_splot_width / dpi,
             height = opls_result_splot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载splot对应的数据
  output$download_opls_splot_data <- downloadHandler(
    filename = function() {
      paste0("opls-splot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_splot_data, file, asTable = TRUE)
    }
  )
  
  
  # splot自定义调色板
  observeEvent(input$opls_splot_datapoint_custom_color_palette_inputs_rgb, {
    inputRgbs <- unlist(strsplit(input$opls_splot_datapoint_custom_color_palette_inputs_rgb, ","))
    output$opls_splot_datapoint_custom_color_palette_block <- renderUI(
      tags$div(
        style = 'display:flex; margin-bottom: 14px;',
        lapply(seq_along(inputRgbs), function(index) {
          tags$span(style = paste0('width:14px; height:14px; background-color:', inputRgbs[index], ';'))
        })
      )
    )
  })
  
  
  # 导出 opls vplot
  observeEvent(input$export_opls_result_vplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_vplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_vplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_vplot_ok"), label = "OK", icon = NULL),
          modalButton(label = "Cancel")
        )
      ))
    )
  })
  
  output$export_opls_result_vplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_vplot_name) || input$export_opls_result_vplot_name == "") {
        paste("vplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_vplot_format, sep="")
      } else {
        paste(input$export_opls_result_vplot_name, input$export_opls_result_vplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_vplot,
             width = opls_result_vplot_width / dpi,
             height = opls_result_vplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载vplot对应的数据
  output$download_opls_vplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-vplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_vplot_data, file, asTable = TRUE)
    }
  )
  
  # 导出 opls permutationplot
  observeEvent(input$export_opls_result_permutationplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_opls_result_permutationplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_opls_result_permutationplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_opls_result_permutationplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_opls_result_permutationplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_opls_result_permutationplot_name) || input$export_opls_result_permutationplot_name == "") {
        paste("permutationplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_opls_result_permutationplot_format, sep="")
      } else {
        paste(input$export_opls_result_permutationplot_name, input$export_opls_result_permutationplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = opls_result_permutationplot,
             width = opls_result_permutationplot_width / dpi,
             height = opls_result_permutationplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 下载permutationplot对应的数据
  output$download_opls_permutationplot_data <- downloadHandler(
    filename = function() {
      paste0("opls-permutationplot-data.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(opls_result_permutationplot_data, file, asTable = TRUE)
    }
  )
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "上传数据",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/DimensionReduction_opls_UploadData.md")),
        tags$div(
          style = 'width:260px; display:flex; align-items:flex-start;',
          downloadButton(outputId = ns("download_matrix"), 
                         label = "Download metabolite matrix data", icon = icon("download"), 
                         style = STYLES$help_download_sampledata_button)
        ),
        tags$div(
          style = 'width:260px; display:flex; align-items:flex-start;',
          downloadButton(outputId = ns("download_samplegrouping_attachment"),
                         label = "Download sample grouping data", icon = icon("download"),
                         style = STYLES$help_download_sampledata_button)
        ),
        tags$div(
          style = 'width:310px; display:flex; align-items:flex-start;',
          downloadButton(outputId = ns("download_variableclass_attachment"),
                         label = "Download metabolite classification data", icon = icon("download"),
                         style = STYLES$help_download_sampledata_button)
        )
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        modalButton(label = "确认")
      ),
      style="max-height:500px; overflow:auto;"
    ))
  })
  
  output$download_matrix <- downloadHandler(
    filename = "dimension_reduction_opls_matrix.csv",
    content = function(file) {
      matrixdata <- read.csv("help/data/dimension_reduction_opls_matrix.csv", check.names = FALSE)
      write.csv(matrixdata, file, row.names = FALSE)
    }
  )
  
  output$download_samplegrouping_attachment <- downloadHandler(
    filename = "dimension_reduction_opls_sample_group.csv",
    content = function(file) {
      sample_group_data <- read.csv("help/data/dimension_reduction_opls_sample_group.csv", check.names = FALSE)
      write.csv(sample_group_data, file, row.names = FALSE)
    }
  )
  
  output$download_variableclass_attachment <- downloadHandler(
    filename = "dimension_reduction_opls_metabolite_class.csv",
    content = function(file) {
      variable_class_data <- read.csv("help/data/dimension_reduction_opls_metabolite_class.csv", check.names = FALSE)
      write.csv(variable_class_data, file, row.names = FALSE)
    }
  )
  
}