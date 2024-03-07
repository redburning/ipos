source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


# scoreplot尺寸
pca_result_scoreplot_width <- 1000
pca_result_scoreplot_height <- 600

# loading plot尺寸
pca_result_loadingplot_width <- 1000
pca_result_loadingplot_height <- 600

# scree plot 尺寸
pca_result_screeplot_width <- 600
pca_result_screeplot_height <- 500

# bio plot 尺寸
pca_result_bioplot_width <- 600
pca_result_bioplot_height <- 500


buildAccordionItem <- function(id, title, collapsed = FALSE) {
  collapseBtnArrowDown <- paste0('collapse-btn-arrow-down-', id)
  collapseBtnArrowRight <- paste0('collapse-btn-arrow-right-', id)
  return(tags$div(
    class = "collapse-item-header",
    tags$button(class = "collapse-btn", 
                tags$div(style = "display:flex;", 
                         tags$svg(id=collapseBtnArrowDown, viewBox="0 0 24 24", width="18", height="18", style=ifelse(collapsed == FALSE, "display:block", "display:none"), tags$path(d="M17,9.17a1,1,0,0,0-1.41,0L12,12.71,8.46,9.17a1,1,0,0,0-1.41,0,1,1,0,0,0,0,1.42l4.24,4.24a1,1,0,0,0,1.42,0L17,10.59A1,1,0,0,0,17,9.17Z")), 
                         tags$svg(id=collapseBtnArrowRight, viewBox="0 0 24 24", width="18", height="18", style=ifelse(collapsed == FALSE, "display:none", "display:block"), tags$path(d="M14.83,11.29,10.59,7.05a1,1,0,0,0-1.42,0,1,1,0,0,0,0,1.41L12.71,12,9.17,15.54a1,1,0,0,0,0,1.41,1,1,0,0,0,.71.29,1,1,0,0,0,.71-.29l4.24-4.24A1,1,0,0,0,14.83,11.29Z"))), 
                onclick = sprintf("(function () { 
                            if (document.getElementById('%s') !== null && document.getElementById('%s').style.display === 'block') { 
                              document.getElementById('%s').style.display = 'none'; 
                              document.getElementById('%s').style.display = 'none'; 
                              document.getElementById('%s').style.display = 'block'; 
                            } else if (document.getElementById('%s') !== null && document.getElementById('%s').style.display === 'none') { 
                              document.getElementById('%s').style.display = 'block'; 
                              document.getElementById('%s').style.display = 'block'; 
                              document.getElementById('%s').style.display = 'none';
                              }
                            })()", id, id, id, collapseBtnArrowDown, collapseBtnArrowRight, id, id, id, collapseBtnArrowDown, collapseBtnArrowRight)
    ),
    tags$a(onclick = sprintf("(function () { 
        if (document.getElementById('%s') !== null && document.getElementById('%s').style.display === 'block') { 
          document.getElementById('%s').style.display = 'none'; 
          document.getElementById('%s').style.display = 'none'; 
          document.getElementById('%s').style.display = 'block'; 
        } else if (document.getElementById('%s') !== null && document.getElementById('%s').style.display === 'none') { 
          document.getElementById('%s').style.display = 'block'; 
          document.getElementById('%s').style.display = 'block'; 
          document.getElementById('%s').style.display = 'none'; 
        }
      })()", id, id, id, collapseBtnArrowDown, collapseBtnArrowRight, id, id, id, collapseBtnArrowDown, collapseBtnArrowRight),
           tags$span(title, class = "collapse-item-title"),
           style = "width:100%"
    )
  ))
}

changeBgOfSelectedColorPalette <- function (id, name) {
  return(sprintf("
    var target = document.getElementById('dimension_reduction-%s');
    if (target !== null) {
      var element = target.nextElementSibling.firstChild;
      element.style.height = '30px';
      element.style.background = 'url(./color-palette/pal_%s.png) 10%% 50%% / 70%% 70%% no-repeat';
    }", id, name)
    )
}

changeBgOfSelectedLineType <- function (name) {
  return(sprintf("
    var target = document.getElementById('dimension_reduction-pca_scoreplot_confidence_linetype');
    if (target !== null) {
      var element = target.nextElementSibling.firstChild;
      element.style.height = '30px';
      element.style.background = 'url(./linetype/linetype-%s.png) 10%% 50%% / 70%% 70%% no-repeat';
      element.style.backgroundColor = 'white';
    }", name)
  )
}

dimensionReductionPCAUI <- function(id) {
  ns <- NS(id)
  
  uploadschedule <- FALSE
  
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
        id = "box-dr",
        title = "PCA",
        status = "danger",
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
              # tags$div(
              #   style = 'border:1px solid rgba(36, 41, 46, 0.12); border-top:none;',
              #   buildAccordionItem('settings-model-selection', 'Model Selection'),
              #   tags$div(id = 'settings-model-selection', class = 'collapse-item-body', style = 'display:block;',
              #            selectInput(inputId = ns('settings_model_selection'), label = '', choices = c('PCA'), selected = 'PCA')
              #   )
              # ),
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
                                                                 height = "600px")
                                          )
                                        ),
                                        tags$div(
                                          style = 'width: 250px; z-index: 200;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            
                                            # Confidence interval settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'confidence-settings-pca-scoreplot', title = 'Confidence level', collapsed = TRUE),
                                              tags$div(id = 'confidence-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       numericInput(inputId = ns('pca_scoreplot_confidence_interval'), label = 'Confidence level', min = 0, max = 1, step = 0.05, value = 0.95),
                                                       selectInput(inputId = ns('pca_scoreplot_confidence_datacoverage'), label = 'Data coverage', choices = c('For all data' = FALSE, 'For group data' = TRUE)),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'confidence-settings-fill-pca-scoreplot', title = 'Fill'),
                                                         tags$div(
                                                           id = 'confidence-settings-fill-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           # 是否填充颜色
                                                           radioButtons(inputId = ns('pca_scoreplot_confidence_fillornot'), label = '', inline = TRUE, selected = 'noFill', choices = c('No fill' = 'noFill', 'Solid color'= 'solidColor')),
                                                           # 多分组颜色选择器
                                                           selectInput(inputId = ns('pca_scoreplot_confidence_fillcolor_multi'), label = 'Color', selected = 'd3', 
                                                                       choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                           # 单分组颜色选择器
                                                           colourInput(inputId = ns("pca_scoreplot_confidence_fillcolor_single"), label = "Color", value = "grey30",
                                                                       returnName = TRUE, palette = "limited", closeOnClick = TRUE, allowedCols = c(ColorBrewr$custom)),
                                                           numericInput(inputId = ns('pca_scoreplot_confidence_fillalpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 0.1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'confidence-settings-line-pca-scoreplot', title = 'Border'),
                                                         tags$div(
                                                           id = 'confidence-settings-line-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           radioButtons(inputId = ns('pca_scoreplot_confidence_withlineornot'), label = '', choices = c('No border' = 'noLine', 'Solid line' = 'solidLine'), inline = TRUE, selected = 'solidLine'),
                                                           selectInput(inputId = ns('pca_scoreplot_confidence_linetype'), label = 'Type', selected = 'dashed', choices = c('dashed', 'solid', 'dotted', 'dotdash', 'longdash', 'twodash', 'blank')),
                                                           numericInput(inputId = ns('pca_scoreplot_confidence_lineweight'), label = 'Weight', min = 0, max = 1, step = 0.1, value = 0.5),
                                                           numericInput(inputId = ns('pca_scoreplot_confidence_linealpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 1)
                                                         )
                                                       )
                                              )
                                            ),

                                            # Datapoint settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'datapoint-settings-pca-scoreplot', title = 'Data points', collapsed = TRUE),
                                              tags$div(id = 'datapoint-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-settings-fill-pca-scoreplot', title = 'Fill'),
                                                         tags$div(
                                                           id = 'datapoint-settings-fill-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           selectInput(inputId = ns('pca_scoreplot_datapoint_fillcolor'), label = 'Color', selected = 'd3', 
                                                                       choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                           numericInput(inputId = ns('pca_scoreplot_datapoint_fillalpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-settings-border-pca-scoreplot', title = 'Border'),
                                                         tags$div(
                                                           id = 'datapoint-settings-border-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           selectInput(inputId = ns('pca_scoreplot_datapoint_border_color'), label = 'Color', selected = 'd3', 
                                                                       choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                           numericInput(inputId = ns('pca_scoreplot_datapoint_border_alpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 1)
                                                         )
                                                       ),
                                                       selectInput(inputId = ns('pca_scoreplot_datapoint_shape'), label = 'Shape', choices = c('Circle' = 16, 'Square' = 15, 'Rhomboid' = 18, 'Triangle' = 17)),
                                                       numericInput(inputId = ns('pca_scoreplot_datapoint_size'), label = 'Size', min = 1, value = 5, step = 1)
                                              )
                                            ),

                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-pca-scoreplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('pca_scoreplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('pca_scoreplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('pca_scoreplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('pca_scoreplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-pca-scoreplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-pca-scoreplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("pca_scoreplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-pca-scoreplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('pca_scoreplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-pca-scoreplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-pca-scoreplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_scoreplot_xaxis_label'), label = 'Label', value = 't[1]'),
                                                           selectInput(inputId = ns('pca_scoreplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_scoreplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-pca-scoreplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-pca-scoreplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_scoreplot_yaxis_label'), label = 'Label', value = 't[2]'),
                                                           selectInput(inputId = ns('pca_scoreplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_scoreplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),

                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-pca-scoreplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('pca_scoreplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('pca_scoreplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'pca_scoreplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_scoreplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_scoreplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                        )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-pca-scoreplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-pca-scoreplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_scoreplot_width'), label = 'Width', min = 100, step = 10, value = pca_result_scoreplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_scoreplot_height'), label = 'Height', min = 100, step = 10, value = pca_result_scoreplot_height)
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
                                      actionButton(inputId = ns("export_pca_result_scoreplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                tabPanel(title = "Loading plot",
                         # icon = tags$i(class = "iconfont icon-a-zhuchengfenfenxiPCA", role="presentation"),
                         hidden(div(id = ns("result-loadingplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotOutput(outputId = ns("loading_plot"),
                                                                 width = "100%",
                                                                 height = "600px")
                                          )
                                        ),
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Datapoint settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'datapoint-settings-pca-loadingplot', title = 'Data points', collapsed = TRUE),
                                              tags$div(id = 'datapoint-settings-pca-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-settings-fill-pca-loadingplot', title = 'Fill'),
                                                         tags$div(
                                                           id = 'datapoint-settings-fill-pca-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           selectInput(inputId = ns('pca_loadingplot_datapoint_fillcolor'), label = 'Color', selected = 'd3', 
                                                                       choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                           numericInput(inputId = ns('pca_loadingplot_datapoint_fillalpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'datapoint-settings-border-pca-loadingplot', title = 'Border'),
                                                         tags$div(
                                                           id = 'datapoint-settings-border-pca-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           selectInput(inputId = ns('pca_loadingplot_datapoint_border_color'), label = 'Color', selected = 'd3', 
                                                                       choices = c('d3', 'aaas', 'jama', 'jco', 'lancet', 'locus', 'nejm', 'npg', 'rick', 'simpson', 'startrek', 'tron', 'dark', 'light')),
                                                           numericInput(inputId = ns('pca_loadingplot_datapoint_border_alpha'), label = 'Transparency', min = 0.01, max = 1, step = 0.1, value = 1)
                                                         )
                                                       ),
                                                       selectInput(inputId = ns('pca_loadingplot_datapoint_shape'), label = 'Shape', choices = c('Circle' = 16, 'Square' = 15, 'Rhomboid' = 18, 'Triangle' = 17)),
                                                       numericInput(inputId = ns('pca_loadingplot_datapoint_size'), label = 'Size', min = 1, value = 5, step = 1)
                                              )
                                            ),
                                            
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-pca-loadingplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-pca-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('pca_loadingplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('pca_loadingplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('pca_loadingplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('pca_loadingplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-pca-loadingplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-pca-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-pca-loadingplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-pca-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("pca_loadingplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-pca-loadingplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-pca-loadingplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('pca_loadingplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-pca-loadingplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-pca-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-pca-loadingplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-pca-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_loadingplot_xaxis_label'), label = 'Label', value = 't[1]'),
                                                           selectInput(inputId = ns('pca_loadingplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_loadingplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-pca-loadingplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-pca-loadingplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_loadingplot_yaxis_label'), label = 'Label', value = 't[2]'),
                                                           selectInput(inputId = ns('pca_loadingplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_loadingplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-pca-loadingplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-pca-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('pca_loadingplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('pca_loadingplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'pca_loadingplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_loadingplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_loadingplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-pca-loadingplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-pca-loadingplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_loadingplot_width'), label = 'Width', min = 100, step = 10, value = pca_result_loadingplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_loadingplot_height'), label = 'Height', min = 100, step = 10, value = pca_result_loadingplot_height)
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
                                      actionButton(inputId = ns("export_pca_result_loadingplot"),
                                                   class = "action-button-primary",
                                                   label = "Download",
                                                   icon = icon("download"))
                                    )
                         ))
                ),
                
                tabPanel(title = "Scree plot",
                         # icon = tags$i(class = "iconfont icon-a-zhuchengfenfenxiPCA", role="presentation"),
                         hidden(div(id = ns("result-screeplot"),
                                    fluidRow(
                                      tags$div(
                                        style = 'display:flex; gap:15px; padding:15px;',
                                        tags$div(
                                          style = 'width: calc(100% - 270px)',
                                          withSpinner(plotOutput(outputId = ns("scree_plot"),
                                                                 width = "100%",
                                                                 height = "600px")
                                          )
                                        ),
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-pca-screeplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-pca-screeplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('pca_screeplot_title_text'), label = 'Text'),
                                                       selectInput(inputId = ns('pca_screeplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('pca_screeplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('pca_screeplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-pca-screeplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-pca-screeplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-pca-screeplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-pca-screeplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("pca_screeplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-pca-screeplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-pca-screeplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('pca_screeplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-pca-screeplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-pca-screeplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-pca-screeplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-pca-screeplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_screeplot_xaxis_label'), label = 'Label', value = 'Comp No.'),
                                                           selectInput(inputId = ns('pca_screeplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_screeplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-pca-screeplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-pca-screeplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_screeplot_yaxis_label'), label = 'Label', value = 'R2X'),
                                                           selectInput(inputId = ns('pca_screeplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_screeplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-pca-screeplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-pca-screeplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('pca_screeplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('pca_screeplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'pca_screeplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_screeplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_screeplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-pca-screeplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-pca-screeplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_screeplot_width'), label = 'Width', min = 100, step = 10, value = pca_result_screeplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_screeplot_height'), label = 'Height', min = 100, step = 10, value = pca_result_screeplot_height)
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
                                      actionButton(inputId = ns("export_pca_result_screeplot"),
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
                                                                 height = "600px")
                                          )
                                        ),
                                        tags$div(
                                          style = 'width: 270px; z-index:101;',
                                          tags$div(
                                            style = 'max-height:600px; overflow-y:scroll; border:1px solid rgba(36, 41, 46, 0.12); border-radius:5px; background-color:rgb(247,247,247);',
                                            # Title settings
                                            tags$div(
                                              style = 'border-bottom:1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'title-settings-pca-bioplot', title = 'Title', collapsed = TRUE),
                                              tags$div(id = 'title-settings-pca-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       textInput(inputId = ns('pca_bioplot_title_text'), label = 'Text', value = 'bioplot'),
                                                       selectInput(inputId = ns('pca_bioplot_title_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                       numericInput(inputId = ns('pca_bioplot_title_fontsize'), label = 'Font size', min = 1, value = 18, step = 1),
                                                       selectInput(inputId = ns('pca_bioplot_title_position'), label = 'Position', choices = c('Top center' = 0.5, 'Justification left' = 0, 'Justification right'= 1)),
                                              )
                                            ),
                                            
                                            # Panel settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'panel-settings-pca-bioplot', title = 'Panel', collapsed = TRUE),
                                              tags$div(id = 'panel-settings-pca-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-background-settings-pca-bioplot', title = 'Background', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-background-settings-pca-bioplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           colourInput(inputId = ns("pca_bioplot_panel_background_fill_color"), label = "Fill color", value = "rgba(255, 255, 255, 0)",
                                                                       allowTransparent = TRUE, closeOnClick = TRUE)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'panel-grid-settings-pca-bioplot', title = 'Grid', collapsed = FALSE),
                                                         tags$div(
                                                           id = 'panel-grid-settings-pca-bioplot', class = 'collapse-subitem-body', style = 'display:block;',
                                                           checkboxInput(inputId = ns('pca_bioplot_panel_settings_showgrid'), label = "Show grid lines", value = FALSE)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Axis settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'axis-settings-pca-bioplot', title = 'Axis', collapsed = TRUE),
                                              tags$div(id = 'axis-settings-pca-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'xaxis-settings-pca-bioplot', title = 'xAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'xaxis-settings-pca-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_bioplot_xaxis_label'), label = 'Label', value = 'pc(cor)[1], t(cor)[1]'),
                                                           selectInput(inputId = ns('pca_bioplot_xaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_bioplot_xaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       tags$div(
                                                         class = 'collapse-subitem',
                                                         buildAccordionItem(id = 'yaxis-settings-pca-bioplot', title = 'yAxis', collapsed = TRUE),
                                                         tags$div(
                                                           id = 'yaxis-settings-pca-bioplot', class = 'collapse-subitem-body', style = 'display:none;',
                                                           textInput(inputId = ns('pca_bioplot_yaxis_label'), label = 'Label', value = 'pc(cor)[2], t(cor)[2]'),
                                                           selectInput(inputId = ns('pca_bioplot_yaxis_fontfamily'), label = 'Font family', choices = c('sans', 'mono', 'serif'), selected = 'sans'),
                                                           numericInput(inputId = ns('pca_bioplot_yaxis_fontsize'), label = 'Font size', min = 1, value = 12, step = 1)
                                                         )
                                                       ),
                                                       
                                              )
                                            ),
                                            
                                            # Legend settings
                                            tags$div(
                                              style = 'border-bottom: 1px solid rgba(36, 41, 46, 0.12);',
                                              buildAccordionItem(id = 'legend-settings-pca-bioplot', title = 'Legend', collapsed = TRUE),
                                              tags$div(id = 'legend-settings-pca-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       # legend显示在内部、外部、不显示
                                                       radioButtons(inputId = ns('pca_bioplot_legend_position'), label = '', inline = TRUE, selected = 'outside', choices = c('None' = 'none', 'Inside'= 'inside', 'Outside' = 'outside')),
                                                       selectInput(inputId = ns('pca_bioplot_legend_position_outside'), label = 'Position', selected = 'right', choices = c('Top' = 'top', 'Bottom' = 'bottom', 'Left' = 'left', 'Right' = 'right')),
                                                       tags$div(id = 'pca_bioplot_legend_position_inside', style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_bioplot_legend_position_inside_x'), label = 'x', min = 0, max = 1, step = 0.1, value = 0.9)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_bioplot_legend_position_inside_y'), label = 'y', min = 0, max = 1, step = 0.1, value = 0.2)
                                                                )
                                                       )
                                              )
                                            ),
                                            
                                            # Size
                                            tags$div(
                                              buildAccordionItem(id = 'size-settings-pca-bioplot', title = 'Size', collapsed = TRUE),
                                              tags$div(id = 'size-settings-pca-bioplot', class = 'collapse-item-body', style = 'display:none;',
                                                       tags$div(style = 'display:flex; align-items:center; justify-content:space-between; gap:10px;',
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_bioplot_width'), label = 'Width', min = 100, step = 10, value = pca_result_bioplot_width)
                                                                ),
                                                                tags$div(
                                                                  style = 'width:50%',
                                                                  numericInput(inputId = ns('pca_result_bioplot_height'), label = 'Height', min = 100, step = 10, value = pca_result_bioplot_height)
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
                                      actionButton(inputId = ns("export_pca_result_bioplot"),
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


dimensionReductionPCAServer <- function(input, output, session) {
  ns <- session$ns
  dataset <- NULL
  origdataset <- NULL
  dataset_attachment_var_class <- NULL
  dataset_attachment_sample_group <- NULL
  dataset_stringasfactors_false <- NULL
  mapping <- NULL
  classes <- c()              # 所有的数据分组
  preprocessed_data <- NULL   # 预处理(log transform + scaling)之后的数据
  pca_result_scoreplot <- NULL
  pca_result_loadingplot <- NULL
  pca_result_screeplot <- NULL
  pca_result_bioplot <- NULL
  
  # 上传样本x变量矩阵数据
  observeEvent(input$dataloader$datapath, {
    tryCatch({
      # dataset with original column names
      origdataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
      dataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE)
      dataset_stringasfactors_false <<- read.csv(input$dataloader$datapath, stringsAsFactors = FALSE)
      
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
      js$collapse("box-dr")
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
    # dataset with original column names
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
    # 数据分组选择, 默认全选
    output$groups_selection_ui <- renderUI({
      tags$div(id = 'settings-group-comparison', class = 'collapse-item-body', style = 'display:block;',
               checkboxGroupInput(inputId = ns("select_data_group"), label = "",
                                  choices = classes,
                                  selected = classes
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
      } else {
        shinyjs::show("preprocessed-data")
        shinyjs::show("result-scoreplot")
        shinyjs::show("result-loadingplot")
        shinyjs::show("result-screeplot")
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
        dataset_withgroup <- dataset
        dataset_withgroup['group'] <- dataset_attachment_sample_group[[input$settings_groupcol_selection]]
        
        # 4. 根据选择的数据分组进行数据筛选
        selected_group_data <- dataset_withgroup[dataset_withgroup$group %in% input$select_data_group, ]
        
        # 去掉第一列的样本名称和group列，数据矩阵作为PCA的分析对象
        data_matrix <- selected_group_data[, -(1)]
        data_matrix <- subset(data_matrix, select = -group)
        # PCA
        pca <- opls(data_matrix)
        # 提取PCA绘图信息
        pca.score = pca@scoreMN %>%
          as.data.frame() %>%
          mutate(group = selected_group_data$group)
        
        # score plot
        output$score_plot <- renderPlot({
          ellipse <- NULL
          # 所有数据点绘制一个置信椭圆
          if (input$pca_scoreplot_confidence_datacoverage == 'FALSE') {
            ellipse <- stat_ellipse(aes(x = p1, y = p2),
                                    level = input$pca_scoreplot_confidence_interval,
                                    inherit.aes = FALSE,
                                    linetype = ifelse(input$pca_scoreplot_confidence_withlineornot == 'solidLine', input$pca_scoreplot_confidence_linetype, 'blank'),
                                    size = input$pca_scoreplot_confidence_lineweight,
                                    show.legend = FALSE,
                                    geom = ifelse(input$pca_scoreplot_confidence_fillornot == 'noFill', 'path', 'polygon'),
                                    fill = input$pca_scoreplot_confidence_fillcolor_single,
                                    color = input$pca_scoreplot_confidence_fillcolor_single,
                                    alpha = ifelse(input$pca_scoreplot_confidence_fillornot == 'noFill', 
                                                   input$pca_scoreplot_confidence_linealpha, input$pca_scoreplot_confidence_fillalpha)
                                    )
          }
          # 每组数据点绘制一个置信椭圆
          else {
            ellipse <- stat_ellipse(aes(x = p1, y = p2),
                                    level = input$pca_scoreplot_confidence_interval,
                                    inherit.aes = TRUE,
                                    linetype = ifelse(input$pca_scoreplot_confidence_withlineornot == 'solidLine', 
                                                      input$pca_scoreplot_confidence_linetype, 'blank'),
                                    size = input$pca_scoreplot_confidence_lineweight,
                                    show.legend = FALSE,
                                    geom = ifelse(input$pca_scoreplot_confidence_fillornot == 'noFill', 'path', 'polygon'),
                                    alpha = ifelse(input$pca_scoreplot_confidence_fillornot == 'noFill', 
                                                   input$pca_scoreplot_confidence_linealpha, input$pca_scoreplot_confidence_fillalpha),
                                    )
          }
          
          legend_position <- 'none'
          if (input$pca_scoreplot_legend_position == 'outside') {
            legend_position <- input$pca_scoreplot_legend_position_outside
          } else if (input$pca_scoreplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$pca_scoreplot_legend_position == 'inside') {
            legend_position <- c(input$pca_scoreplot_legend_position_inside_x, input$pca_scoreplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  # legend.background = element_blank(),
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$pca_scoreplot_panel_background_fill_color),
                  plot.title = element_text(family = input$pca_scoreplot_title_fontfamily, size = input$pca_scoreplot_title_fontsize, vjust = 1, hjust = input$pca_scoreplot_title_position),
                  axis.title.x = element_text(family = input$pca_scoreplot_xaxis_fontfamily, size = input$pca_scoreplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$pca_scoreplot_yaxis_fontfamily, size = input$pca_scoreplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$pca_scoreplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          pca_result_scoreplot <<- ggplot(pca.score, aes(x = p1, y = p2, color = group, fill = group)) +
            ellipse +
            scale_fill_manual(values = ggsciColorPalette(input$pca_scoreplot_confidence_fillcolor_multi, input$pca_scoreplot_confidence_fillalpha)) +
            geom_hline(yintercept = 0, linetype = 1, size = 0.5) +
            geom_vline(xintercept = 0, linetype = 1, size = 0.5) +
            geom_point(size = input$pca_scoreplot_datapoint_size, shape = as.numeric(input$pca_scoreplot_datapoint_shape), aes(fill = group)) +
            scale_color_manual(values = ggsciColorPalette(input$pca_scoreplot_datapoint_fillcolor, input$pca_scoreplot_datapoint_fillalpha)) +
            ggtitle(input$pca_scoreplot_title_text) +
            xlab(input$pca_scoreplot_xaxis_label) + ylab(input$pca_scoreplot_yaxis_label) +
            theme_settings
          
          return(pca_result_scoreplot)
        }, height = function() {
          pca_result_scoreplot_height <<- input$pca_result_scoreplot_height
          return(pca_result_scoreplot_height)
        }, width = function() {
          pca_result_scoreplot_width <<- input$pca_result_scoreplot_width
          return(pca_result_scoreplot_width)
        })
        
        # loading plot
        output$loading_plot <- renderPlot({
          
          if (!is.null(dataset_attachment_var_class)) {
            if (ncol(dataset_attachment_var_class) == 2 && nrow(dataset_attachment_var_class) > 0) {
              # prepare loading data
              loading_data = pca@loadingMN %>% as.data.frame() %>% 
                mutate('metabolite' = rownames(pca@loadingMN)) %>% 
                mutate("class" = dataset_attachment_var_class[[2]] %>% as.factor() )
              
              legend_position <- 'none'
              if (input$pca_loadingplot_legend_position == 'outside') {
                legend_position <- input$pca_loadingplot_legend_position_outside
              } else if (input$pca_loadingplot_legend_position == 'none') {
                legend_position <- 'none'
              } else if (input$pca_loadingplot_legend_position == 'inside') {
                legend_position <- c(input$pca_loadingplot_legend_position_inside_x, input$pca_loadingplot_legend_position_inside_y)
              }
              
              theme_settings <- theme_bw() +
                theme(legend.position = legend_position,
                      legend.justification = "center",
                      legend.box = "vertical",
                      # legend.background = element_blank(),
                      legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      panel.background = element_rect(fill = input$pca_loadingplot_panel_background_fill_color),
                      plot.title = element_text(family = input$pca_loadingplot_title_fontfamily, size = input$pca_loadingplot_title_fontsize, vjust = 1, hjust = input$pca_loadingplot_title_position),
                      axis.title.x = element_text(family = input$pca_loadingplot_xaxis_fontfamily, size = input$pca_loadingplot_xaxis_fontsize),
                      axis.title.y = element_text(family = input$pca_loadingplot_yaxis_fontfamily, size = input$pca_loadingplot_yaxis_fontsize),
                      axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                      axis.ticks = element_line(color = 'black')
                )
              
              if (input$pca_loadingplot_panel_settings_showgrid == FALSE) {
                theme_settings <- theme_settings + theme(panel.grid = element_blank())
              }
              
              # plot
              pca_result_loadingplot <<- ggplot(loading_data, aes(p1, p2, label = metabolite)) +
                geom_point(aes(color = class), size = 3) +
                scale_color_manual(values = sample(c(pal_aaas()(10), pal_igv()(50)),
                                                   length(levels(loading_data$class)))) +
                geom_hline(yintercept = 0, linetype = 2, size = .5, color = 'gray33') + 
                geom_vline(xintercept = 0, linetype = 2, size = .5, color = "gray33") +
                ggtitle(input$pca_loadingplot_title_text) + 
                xlab(input$pca_loadingplot_xaxis_label) + ylab(input$pca_loadingplot_yaxis_label) +
                theme_settings
              
              return(pca_result_loadingplot)
            } else {
              toastr_warning(message = "", title = "Please check the format of metabolite classification data")
              return(NULL)
            }
          } else {
            toastr_warning(message = "", title = "Please upload the metabolite classification data")
            return(NULL)
          }
          
        }, height = function() {
          pca_result_loadingplot_height <<- input$pca_result_loadingplot_height
          return(pca_result_loadingplot_height)
        }, width = function() {
          pca_result_loadingplot_width <<- input$pca_result_loadingplot_width
          return(pca_result_loadingplot_width)
        })
        
        # scree plot
        output$scree_plot <- renderPlot({
          # prepare data
          scree_data = data.frame(pca@pcaVarVn) %>% `colnames<-`("explaination") %>% 
            mutate(pca@modelDF) %>%  
            mutate('x' = rownames(pca@modelDF))
          
          legend_position <- 'none'
          if (input$pca_screeplot_legend_position == 'outside') {
            legend_position <- input$pca_screeplot_legend_position_outside
          } else if (input$pca_screeplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$pca_screeplot_legend_position == 'inside') {
            legend_position <- c(input$pca_screeplot_legend_position_inside_x, input$pca_screeplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$pca_screeplot_panel_background_fill_color),
                  plot.title = element_text(family = input$pca_screeplot_title_fontfamily, size = input$pca_screeplot_title_fontsize, vjust = 1, hjust = input$pca_screeplot_title_position),
                  axis.title.x = element_text(family = input$pca_screeplot_xaxis_fontfamily, size = input$pca_screeplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$pca_screeplot_yaxis_fontfamily, size = input$pca_screeplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$pca_screeplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          pca_result_screeplot <<- ggplot(scree_data, aes(x = factor(x))) +
            geom_col(aes(y=`R2X`, color = "R2X"), fill = '#5DB1DDFF',) +
            geom_line(aes(y=`R2X(cum)`, group = NA, color = "explaination"), linewidth = 1) +
            geom_point(aes(y=`R2X(cum)`, color = "explaination"), size = 2.75) +
            scale_y_continuous(sec.axis = sec_axis( ~.*1, name = "Cumulative R2X")) +
            scale_color_manual(values = c("#DC0000B2" ,"#8491B4B2" )) +
            geom_label_repel(aes(y=`R2X(cum)`,label = sprintf("%.2f", `R2X(cum)`)), nudge_y = 0.01) +
            theme_settings +
            ggtitle(input$pca_screeplot_title_text) +
            xlab(input$pca_screeplot_xaxis_label) + ylab(input$pca_screeplot_yaxis_label)
          
          return(pca_result_screeplot)
          
        }, height = function() {
          pca_result_screeplot_height <<- input$pca_result_screeplot_height
          return(pca_result_screeplot_height)
        }, width = function() {
          pca_result_screeplot_width <<- input$pca_result_screeplot_width
          return(pca_result_screeplot_width)
        })
        
        # bio plot
        output$bio_plot <- renderPlot({
          
          # prepare bio data
          c <- ncol(data_matrix)
          r <- nrow(data_matrix)
          bio <- data.frame(matrix(ncol = 2, nrow = c + r)) %>% 
            mutate('group' = c(rep("p(cor)", r), rep("t(cor)", c)) %>% as.factor()) %>% 
            mutate('metabolite/sample ID' = c(colnames(data_matrix), as.vector(selected_group_data[[1]]))) %>% 
            mutate("class/group" = c(dataset_attachment_var_class[[1]], selected_group_data$group))
          
          A1 <- c()
          A2 <- c()
          for (i in 1:r) {
            A1 <- append(A1, cor(unlist(data_matrix[i, ], use.names = FALSE), pca@loadingMN[, 1]))
            A2 <- append(A2, cor(unlist(data_matrix[i, ], use.names = FALSE), pca@loadingMN[, 2]))
          }
          for (i in 1:c) {
            A1 <- append(A1, cor(data_matrix[, i], pca@scoreMN[, 1]))
            A2 <- append(A2, cor(data_matrix[, i], pca@scoreMN[, 2]))
          }
          bio["A1"] <- A1
          bio["A2"] <- A2
          
          # ggplot
          legend_position <- 'none'
          if (input$pca_bioplot_legend_position == 'outside') {
            legend_position <- input$pca_bioplot_legend_position_outside
          } else if (input$pca_bioplot_legend_position == 'none') {
            legend_position <- 'none'
          } else if (input$pca_bioplot_legend_position == 'inside') {
            legend_position <- c(input$pca_bioplot_legend_position_inside_x, input$pca_bioplot_legend_position_inside_y)
          }
          
          theme_settings <- theme_bw() +
            theme(legend.position = legend_position,
                  legend.justification = "center",
                  legend.box = "vertical",
                  legend.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  panel.background = element_rect(fill = input$pca_bioplot_panel_background_fill_color),
                  plot.title = element_text(family = input$pca_bioplot_title_fontfamily, size = input$pca_bioplot_title_fontsize, vjust = 1, hjust = input$pca_bioplot_title_position),
                  axis.title.x = element_text(family = input$pca_bioplot_xaxis_fontfamily, size = input$pca_bioplot_xaxis_fontsize),
                  axis.title.y = element_text(family = input$pca_bioplot_yaxis_fontfamily, size = input$pca_bioplot_yaxis_fontsize),
                  axis.text = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.title = element_text(color = 'black', size = 12, family = 'sans', face = 'plain'),
                  axis.ticks = element_line(color = 'black')
            )
          
          if (input$pca_bioplot_panel_settings_showgrid == FALSE) {
            theme_settings <- theme_settings + theme(panel.grid = element_blank())
          }
          
          pca_result_bioplot <<- ggplot(bio, aes(A1, A2, label = `metabolite/sample ID`)) +
            geom_vline(xintercept = 0) +
            geom_hline(yintercept = 0) +
            geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = 'black') +
            geom_circle(aes(x0 = 0, y0 = 0, r = 0.75), color = 'black') +
            geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), color = 'black') +
            geom_point(aes(fill = group), color = 'gray34', size = 4, shape = 21) +
            scale_fill_manual(values = c("#D595A7FF", "#009966FF")) +
            theme_settings +
            ggtitle(input$pca_bioplot_title_text) +
            xlab(input$pca_bioplot_xaxis_label) + ylab(input$pca_bioplot_yaxis_label)
          
          return(pca_result_bioplot)
        }, height = function() {
          pca_result_bioplot_height <<- input$pca_result_bioplot_height
          return(pca_result_bioplot_height)
        }, width = function() {
          pca_result_bioplot_width <<- input$pca_result_bioplot_width
          return(pca_result_bioplot_width)
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
  observeEvent(input$pca_scoreplot_confidence_fillcolor_multi, {
    runjs(changeBgOfSelectedColorPalette('pca_scoreplot_confidence_fillcolor_multi', input$pca_scoreplot_confidence_fillcolor_multi))
  })
  
  # 改变所选择调色板的背景图片
  observeEvent(input$pca_scoreplot_datapoint_fillcolor, {
    runjs(changeBgOfSelectedColorPalette('pca_scoreplot_datapoint_fillcolor', input$pca_scoreplot_datapoint_fillcolor))
  })
  
  # 改变所选择线条类型的背景图片
  observeEvent(input$pca_scoreplot_confidence_linetype, {
    runjs(changeBgOfSelectedLineType(input$pca_scoreplot_confidence_linetype))
  })
  
  # 是否填充颜色
  observeEvent(input$pca_scoreplot_confidence_fillornot, {
    if (input$pca_scoreplot_confidence_fillornot == 'noFill') {
      shinyjs::hide('pca_scoreplot_confidence_fillcolor_single')
      shinyjs::hide('pca_scoreplot_confidence_fillcolor_multi')
      shinyjs::hide('pca_scoreplot_confidence_fillalpha')
    } else if (input$pca_scoreplot_confidence_fillornot == 'solidColor') {
      shinyjs::show('pca_scoreplot_confidence_fillalpha')
      if (input$pca_scoreplot_confidence_datacoverage == 'TRUE') {
        shinyjs::show('pca_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_single')
      } else {
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_multi')
        shinyjs::show('pca_scoreplot_confidence_fillcolor_single')
      }
    }
  })
  
  # 是否显示线条
  observeEvent(input$pca_scoreplot_confidence_withlineornot, {
    if (input$pca_scoreplot_confidence_withlineornot == 'solidLine') {
      shinyjs::show('pca_scoreplot_confidence_linetype')
      shinyjs::show('pca_scoreplot_confidence_lineweight')
      shinyjs::show('pca_scoreplot_confidence_linealpha')
    } else {
      shinyjs::hide('pca_scoreplot_confidence_linetype')
      shinyjs::hide('pca_scoreplot_confidence_lineweight')
      shinyjs::hide('pca_scoreplot_confidence_linealpha')
    }
  })
  
  # 选择for all data时，显示单个颜色选择器
  # 选择for group data时，显示分组颜色选择器
  observeEvent(input$pca_scoreplot_confidence_datacoverage, {
    if (input$pca_scoreplot_confidence_datacoverage == 'TRUE') {
      if (input$pca_scoreplot_confidence_fillornot == 'noFill') {
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_single')
        shinyjs::hide('pca_scoreplot_confidence_fillalpha')
      } else {
        shinyjs::show('pca_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_single')
        shinyjs::show('pca_scoreplot_confidence_fillalpha')
      }
    } else {
      if (input$pca_scoreplot_confidence_fillornot == 'noFill') {
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_multi')
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_single')
        shinyjs::hide('pca_scoreplot_confidence_fillalpha')
      } else {
        shinyjs::hide('pca_scoreplot_confidence_fillcolor_multi')
        shinyjs::show('pca_scoreplot_confidence_fillcolor_single')
        shinyjs::show('pca_scoreplot_confidence_fillalpha')
      }
    }
    
  })
  
  # scoreplot legend显示位置
  observeEvent(input$pca_scoreplot_legend_position, {
    if (input$pca_scoreplot_legend_position == 'none') {
      shinyjs::hide('pca_scoreplot_legend_position_outside')
      shinyjs::hide('pca_scoreplot_legend_position_inside_x')
      shinyjs::hide('pca_scoreplot_legend_position_inside_y')
    } else if (input$pca_scoreplot_legend_position == 'inside') {
      shinyjs::hide('pca_scoreplot_legend_position_outside')
      shinyjs::show('pca_scoreplot_legend_position_inside_x')
      shinyjs::show('pca_scoreplot_legend_position_inside_y')
    } else if (input$pca_scoreplot_legend_position == 'outside') {
      shinyjs::show('pca_scoreplot_legend_position_outside')
      shinyjs::hide('pca_scoreplot_legend_position_inside_x')
      shinyjs::hide('pca_scoreplot_legend_position_inside_y')
    }
  })
  
  # loadingplot legend显示位置
  observeEvent(input$pca_loadingplot_legend_position, {
    if (input$pca_loadingplot_legend_position == 'none') {
      shinyjs::hide('pca_loadingplot_legend_position_outside')
      shinyjs::hide('pca_loadingplot_legend_position_inside_x')
      shinyjs::hide('pca_loadingplot_legend_position_inside_y')
    } else if (input$pca_loadingplot_legend_position == 'inside') {
      shinyjs::hide('pca_loadingplot_legend_position_outside')
      shinyjs::show('pca_loadingplot_legend_position_inside_x')
      shinyjs::show('pca_loadingplot_legend_position_inside_y')
    } else if (input$pca_loadingplot_legend_position == 'outside') {
      shinyjs::show('pca_loadingplot_legend_position_outside')
      shinyjs::hide('pca_loadingplot_legend_position_inside_x')
      shinyjs::hide('pca_loadingplot_legend_position_inside_y')
    }
  })
  
  
  
  
  
  # 导出数据预处理结果
  observeEvent(input$export_preprocessed_data, {
    showModal(modalDialog(
      title = "Download data",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_pca_result_table_name"), label = "File name", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_pca_result_table_format"), label = "Choose format", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_pca_result_table_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  output$export_pca_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_pca_result_table_name) || input$export_pca_result_table_name == "") {
        paste("preprocessed-data-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_pca_result_table_format, sep="")
      } else {
        paste(input$export_pca_result_table_name, input$export_pca_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_pca_result_table_format == ".csv") {
        write.csv(preprocessed_data, file, row.names = FALSE)
      } else if (input$export_pca_result_table_format == ".xlsx") {
        write.xlsx(preprocessed_data, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  
  # 导出pca得分图
  observeEvent(input$export_pca_result_scoreplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_pca_result_scoreplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_pca_result_scoreplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_pca_result_scoreplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_pca_result_scoreplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_pca_result_scoreplot_name) || input$export_pca_result_scoreplot_name == "") {
        paste("scoreplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_pca_result_scoreplot_format, sep="")
      } else {
        paste(input$export_pca_result_scoreplot_name, input$export_pca_result_scoreplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = pca_result_scoreplot,
             width = pca_result_scoreplot_width / dpi,
             height = pca_result_scoreplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 导出 pca loading plot
  observeEvent(input$export_pca_result_loadingplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_pca_result_loadingplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_pca_result_loadingplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_pca_result_loadingplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_pca_result_loadingplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_pca_result_loadingplot_name) || input$export_pca_result_loadingplot_name == "") {
        paste("loadingplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_pca_result_loadingplot_format, sep="")
      } else {
        paste(input$export_pca_result_loadingplot_name, input$export_pca_result_loadingplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = pca_result_loadingplot,
             width = pca_result_loadingplot_width / dpi,
             height = pca_result_loadingplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 导出 pca scree plot
  observeEvent(input$export_pca_result_screeplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_pca_result_screeplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_pca_result_screeplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_pca_result_screeplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_pca_result_screeplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_pca_result_screeplot_name) || input$export_pca_result_screeplot_name == "") {
        paste("screeplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_pca_result_screeplot_format, sep="")
      } else {
        paste(input$export_pca_result_screeplot_name, input$export_pca_result_screeplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = pca_result_screeplot,
             width = pca_result_screeplot_width / dpi,
             height = pca_result_screeplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 导出 pca bio plot
  observeEvent(input$export_pca_result_bioplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        title = "Download",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_pca_result_bioplot_name"), label = "File name", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_pca_result_bioplot_format"), label = "Format", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_pca_result_bioplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_pca_result_bioplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_pca_result_bioplot_name) || input$export_pca_result_bioplot_name == "") {
        paste("bioplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_pca_result_bioplot_format, sep="")
      } else {
        paste(input$export_pca_result_bioplot_name, input$export_pca_result_bioplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, plot = pca_result_bioplot,
             width = pca_result_bioplot_width / dpi,
             height = pca_result_bioplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "上传数据",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/DimensionReduction_PCA_UploadData.md")),
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
    filename = "dimension_reduction_pca_matrix.csv",
    content = function(file) {
      matrixdata <- read.csv("help/data/dimension_reduction_pca_matrix.csv", check.names = FALSE)
      write.csv(matrixdata, file, row.names = FALSE)
    }
  )
  
  output$download_samplegrouping_attachment <- downloadHandler(
    filename = "dimension_reduction_pca_sample_group.csv",
    content = function(file) {
      sample_group_data <- read.csv("help/data/dimension_reduction_pca_sample_group.csv", check.names = FALSE)
      write.csv(sample_group_data, file, row.names = FALSE)
    }
  )
  
  output$download_variableclass_attachment <- downloadHandler(
    filename = "dimension_reduction_pca_metabolite_class.csv",
    content = function(file) {
      variable_class_data <- read.csv("help/data/dimension_reduction_pca_metabolite_class.csv", check.names = FALSE)
      write.csv(variable_class_data, file, row.names = FALSE)
    }
  )
  
}