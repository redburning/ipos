source("global.R", encoding = "UTF-8")
source('machine_learning/machine_learning.R', encoding = "UTF-8")
source('regression_analysis/regression_analysis.R', encoding = "UTF-8")
source('difference_analysis/difference_analysis.R', encoding = "UTF-8")
source('quality_control/quality_control.R', encoding = "UTF-8")
source('user_management/user_management.R', encoding = "UTF-8")
source('extended_toolbox/heatmap.R', encoding = "UTF-8")
source('biology_enrichment/ora_enrichment.R', encoding = "UTF-8")
source('biology_enrichment/gsea_enrichment.R', encoding = "UTF-8")
source('chemical_enrichment/chemical_enrichment.R', encoding = "UTF-8")
source('bioinfo_mining/bioinfo_mining.R', encoding = "UTF-8")
source('correlation_net/correlation_net.R', encoding = "UTF-8")
source('biomarker_selection/biomarker_selection.R', encoding = "UTF-8")
source('dimension_reduction/pca.R', encoding = "UTF-8")
source('dimension_reduction/opls.R', encoding = "UTF-8")
source('missing_value_imputation/missing_value_imputation.R', encoding = "UTF-8")
source('pathway_gallery/pathway_gallery.R', encoding = "UTF-8")

jscode <- "
shinyjs.showHomeBg = function() {
  document.querySelector('.content-wrapper').querySelectorAll('section')[0].style.backgroundImage = 'url(bg.png)';
}
shinyjs.hideHomeBg = function() {
  document.querySelector('.content-wrapper').querySelectorAll('section')[0].style.backgroundImage = 'none';
}
"

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Microsoft YaHei",
  appFontColor = "rgb(0, 0, 0)",
  primaryFontColor = "rgb(255, 255, 255)",
  infoFontColor = "rgb(255, 255, 255)",
  successFontColor = "rgb(255, 255, 255)",
  warningFontColor = "rgb(255, 255, 255)",
  dangerFontColor = "rgb(255, 255, 255)",
  bodyBackColor = "rgb(248, 248, 248)",
  
  ### header
  logoBackColor = "rgb(23, 103, 124)",
  
  # headerButtonBackColor = "rgb(238, 238, 238)",
  headerButtonBackColor = "rgb(20, 97, 117)",
  headerButtonIconColor = "rgb(75, 75, 75)",
  headerButtonBackColorHover = "rgb(56, 161, 187)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  
  # headerBackColor = "rgb(238,238,238)",
  headerBackColor = "rgb(20,97,117)",
  headerBoxShadowColor = "#aaaaaa",
  headerBoxShadowSize = "2px 2px 2px",
  
  ### sidebar
  sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(20,97,117)",
    colorMiddle = "rgb(56,161,187)",
    colorEnd = "rgb(3,22,56)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarPadding = 0,
  
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 0,
  sidebarMenuBorderRadius = 0,
  
  sidebarShadowRadius = "3px 5px 5px",
  sidebarShadowColor = "#aaaaaa",
  
  sidebarUserTextColor = "rgb(255, 255, 255)",
  sidebarSearchBackColor = "rgb(92, 172, 238)",
  # sidebarSearchBackColor = "rgb(0, 172, 238)",
  sidebarSearchIconColor = "rgb(255, 255, 255)",
  sidebarSearchBorderColor = "rgb(153, 153, 153)",
  
  sidebarTabTextColor = "rgb(255, 255, 255)",
  sidebarTabTextSize = 15,
  sidebarTabBorderStyle = "none none solid none",
  sidebarTabBorderColor = "rgb(35, 106, 135)",
  sidebarTabBorderWidth = 1,
  
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = "rgb(0,0,0)",
  sidebarTabRadiusSelected = "0px 10px 10px 0px",
  
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "rgb(50,50,50)",
  sidebarTabBorderStyleHover = "none none solid none",
  sidebarTabBorderColorHover = "rgb(75,126,151)",
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 10px 10px 0px",
  
  ### boxes
  boxBackColor = "rgb(255,255,255)",
  boxBorderRadius = 5,
  boxShadowSize = "0px 1px 1px",
  boxShadowColor = "rgba(0,0,0,.1)",
  boxTitleSize = 16,
  boxDefaultColor = "rgb(210,214,220)",
  boxPrimaryColor = "rgba(44, 222, 235, 1)",
  boxInfoColor = "rgb(60, 141, 188)",
  boxSuccessColor = "rgba(143, 217, 168, 1)",
  # boxWarningColor = "rgb(244, 156, 104)",
  # boxWarningColor = "rgb(253, 194, 58)",
  boxWarningColor = "rgb(253, 187, 113)",
  # boxDangerColor = "rgb(229, 71, 111)",
  # boxDangerColor = "rgb(233, 100, 73)",
  boxDangerColor = "rgb(251, 119, 86, 0.8)",
  
  tabBoxTabColor = "rgb(255, 255, 255)",
  tabBoxTabTextSize = 10,
  tabBoxTabTextColor = "rgb(0, 0, 0)",
  tabBoxTabTextColorSelected = "rgb(0, 0, 0)",
  tabBoxBackColor = "rgb(255, 255, 255)",
  tabBoxHighlightColor = "rgba(44, 222, 235, 1)",
  tabBoxBorderRadius = 5,
  
  ### inputs
  buttonBackColor = "rgb(245, 245, 245)",
  buttonTextColor = "rgb(0, 0, 0)",
  buttonBorderColor = "rgb(200, 200, 200)",
  buttonBorderRadius = 5,
  
  buttonBackColorHover = "rgb(235, 235, 235)",
  buttonTextColorHover = "rgb(100,100,100)",
  buttonBorderColorHover = "rgb(200,200,200)",
  
  textboxBackColor = "rgb(255, 255, 255)",
  textboxBorderColor = "rgb(200, 200, 200)",
  textboxBorderRadius = "5px 0px 0px 5px",
  textboxBackColorSelect = "rgb(245, 245, 245)",
  textboxBorderColorSelect = "rgb(200, 200, 200)",
  
  ### tables
  tableBackColor = "rgb(255, 255, 255)",
  tableBorderColor = "rgb(240, 240, 240)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
  
)


# header
header <- dashboardHeader(
  title = div(HTML(paste0('<a id="', "btn_home",'" href="#" class="action-button">
                            <div>
                              <div>
                                <img src="logo-IPOS-v3.8.0.png", height=35, align="center">
                              </div>
                            </div>
                          </a>'))
  ),
  tags$li(
    class = "dropdown",
    style = "padding: 8px;",
    actionButton(inputId = "setting", label = "", icon = icon("gear"), 
                 class = "",
                 style = "background-color: rgb(22, 199, 154, 0.5); border: none; color: #FFFFFF; display: none"),
  )
  
)

customLogo <- shinyDashboardLogoDIY(
  boldText = "iPOS",
  mainText = "",
  textSize = 18,
  badgeText = "v3.3.0",
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#40E0D0",
  badgeBorderRadius = 3
)
#header <- dashboardHeader(title = customLogo)

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem(text = "首页", tabName = "home"),
              HTML("<li>
                            <a href='#shiny-tab-user_management' data-toggle='tab' data-value='user_management'>
                              <i class='fa fa-cogs' role='presentation' aria-label='cogs icon'></i>
                              <span>用户管理</span>
                            </a>
                       </li>"),
              menuItem(text = "缺失值填充", tabName = "missing_value_imputation", icon = icon('cube')),
              HTML("<li>
                            <a href='#shiny-tab-quality_control' data-toggle='tab' data-value='quality_control'>
                              <i class='iconfont icon-zhiliangkongzhitu' role='presentation' aria-label='cogs icon'></i>
                              <span>质控矫正</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a href='#shiny-tab-difference_analysis' data-toggle='tab' data-value='difference_analysis'>
                              <i class='iconfont icon-haiguanjianguan-kucunchayifenxi' role='presentation' aria-label='stream icon'></i>
                              <span>差异分析</span>
                            </a>
                       </li>"),
              HTML("<li class='' classname='iconfont icon-jiqixuexi'>
                            <a href='#shiny-tab-machine_learning' data-toggle='tab' data-value='machine_learning' aria-expanded='false' tabindex='-1' aria-selected='false'>
                              <i class='iconfont icon-jiqixuexi' role='presentation' aria-label='dashboard icon'></i>
                              <span>机器学习</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a href='#shiny-tab-regression_analysis' data-toggle='tab' data-value='regression_analysis'>
                              <i class='iconfont icon-huiguimoxing' role='presentation' aria-label='chart-line icon'></i>
                              <span>回归估计</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a href='#shiny-tab-bioinfo_mining' data-toggle='tab' data-value='bioinfo_mining'>
                              <i class='iconfont icon-shujuwajue' role='presentation' aria-label='cogs icon'></i>
                              <span>生信挖掘</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a href='#shiny-tab-correlation_net' data-toggle='tab' data-value='correlation_net'>
                              <i class='iconfont icon-wajue' role='presentation' aria-label='cogs icon'></i>
                              <span>相关网络</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a href='#shiny-tab-biomarker_selection' data-toggle='tab' data-value='biomarker_selection'>
                              <i class='iconfont icon-shujuwajue' role='presentation' aria-label='cogs icon'></i>
                              <span>标志物分析</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a href='#shiny-tab-chemical_enrichment' data-toggle='tab' data-value='chemical_enrichment'>
                              <i class='iconfont icon-jiyinfuji' role='presentation' aria-label='cogs icon'></i>
                              <span>化学富集</span>
                            </a>
                       </li>"),
              menuItem(text = "代谢通路富集", startExpanded = FALSE, icon = icon("magnifying-glass-chart"),
                       menuSubItem(text = "ORA", tabName = "ora_enrichment", icon = NULL),
                       menuSubItem(text = "GSEA" , tabName = "gsea_enrichment", icon = NULL)
              ), 
              menuItem(text = "多元降维", startExpanded = FALSE, icon = icon("magnifying-glass-chart"),
                       menuSubItem(text = "PCA", tabName = "dimension_reduction", icon = NULL),
                       menuSubItem(text = "OPLS" , tabName = "dimension_reduction_opls", icon = NULL)
              ),
              menuItem(text = "知识图谱", tabName = "pathway_gallery", icon = icon('book')),
              menuItem(text = "扩展坞", startExpanded = FALSE, icon = icon("box"),
                       menuSubItem(text = "heatmap", tabName = "heatmap", icon = NULL)
              )
              
  )
)


# body
body <- dashboardBody(
  customTheme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "iconfont.css"),
    tags$script(src = "lib/typed.js"),
    tags$script(src = "lib/global.js")
  ),
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("showHomeBg", "hideHomeBg")),
  tabItems(
    tabItem(
      tabName = "home",
      class = "active",
      tabPanel(title = "",
               value = "home",
               HTML("
                    <div style='padding-top: 50px;' class='flex-center'>
                        <img src='./IntelligentCloudforMulti-OmicsScience.png' style='height: 80px;'>
                    </div>
                    <div class='cellular-container'>
                        <a id='btn_machine_learning' href='#' class='action-button'>
                          <div class='cell'>
                              <img src='./hexagon_bg.png' class='cell-big cell-img'>
                              <div class='icon-container'>
                                  <img src='./machinelearning_s_icon.png' style='width: 100%;'>
                              </div>
                              <div class='cell-text'>机器学习</div>
                          </div>
                        </a>
                        <a id='btn_quality_control' href='#' class='action-button'>
                          <div class='cell' style='position: absolute; left: calc(50% + 5vh); top: calc(50% + 3px);'>
                              <img src='./hexagon_bg.png' class='cell-big cell-img'>
                              <div class='icon-container'>
                                  <img src='./control_s_icon.png' style='width: 100%;'>
                              </div>
                              <div class='cell-text'>质控矫正</div>
                          </div>
                        </a>
                        <a id='btn_difference_analysis' href='#' class='action-button'>
                          <div class='cell' style='position: absolute; left: calc(50% - 8vh); top: calc(50% + 8vh);'>
                              <img src='./hexagon_bg.png' class='cell-big cell-img'>
                              <div class='icon-container'>
                                  <img src='./analysis_s_icon.png' style='width: 100%;'>
                              </div>
                              <div class='cell-text'>差异分析</div>
                          </div>
                        </a>
                        <a id='btn_regression_analysis' href='#' class='action-button'>
                          <div class='cell' style='position: absolute; left: calc(50% - 21vh); top: calc(50% + 3px);'>
                              <img src='./hexagon_bg.png' class='cell-big cell-img'>
                              <div class='icon-container'>
                                  <img src='./estimate_s_icon.png' style='width: 100%;'>
                              </div>
                              <div class='cell-text'>回归估计</div>
                          </div>
                        </a>
                        <div class='cell' style='position: absolute; left: calc(50% + 18vh); top: calc(50% + 8vh);'>
                            <img src='./hexagon_bg.png' class='cell-big cell-img'>
                            <div class='icon-container'>
                                <img src='./excavate_s_icon.png' style='width: 100%;'>
                            </div>
                            <div class='cell-text'>生信挖掘</div>
                        </div>
                        <a id='btn_chemical_enrichment' href='#' class='action-button'>
                          <div class='cell' style='position: absolute; left: calc(50% + 5vh); top: calc(50% + 16vh - 3px);'>
                              <img src='./hexagon_bg.png' class='cell-big cell-img'>
                              <div class='icon-container'>
                                  <img src='./chemistry_s_icon.png' style='width: 100%;'>
                              </div>
                              <div class='cell-text'>化学富集</div>
                          </div>
                        </a>
                        <div class='cell' style='position: absolute; left: calc(50% - 21vh); top: calc(50% + 16vh - 3px);'>
                            <img src='./hexagon_bg.png' class='cell-big cell-img'>
                            <div class='icon-container'>
                                <img src='./network_s_icon.png' style='width: 100%;'>
                            </div>
                            <div class='cell-text'>相关网络</div>
                        </div>
                        <a id='btn_ora_enrichment' href='#' class='action-button'>
                          <div class='cell' style='position: absolute; left: calc(50% - 34vh); top: calc(50% + 8vh);'>
                              <img src='./hexagon_bg.png' class='cell-big cell-img'>
                              <div class='icon-container'>
                                  <img src='./metabolize_s_icon.png' style='width: 100%;'>
                              </div>
                              <div class='cell-text'>代谢通路富集</div>
                          </div>
                        </a>
                        <div class='cell' style='position: absolute; left: calc(50% - 28vh); top: 50%;'>
                            <img src='./hexagon_middle_bg.png' class='cell-middle cell-img'>
                        </div>
                        <div class='cell' style='position: absolute; left: calc(50% - 27vh + 3px); top: calc(50% + 23vh)'>
                            <img src='./hexagon_small_bg.png' class='cell-small cell-img'>
                        </div>
                        <div class='cell' style='position: absolute; left: calc(50% - 40vh + 3px); top: calc(50% + 10vh - 3px)'>
                            <img src='./hexagon_small_bg.png' class='cell-small cell-img'>
                        </div>
                        <div class='cell' style='position: absolute; left: calc(50% + 20vh); top: 50%;'>
                            <img src='./hexagon_middle_bg.png' class='cell-middle cell-img'>
                        </div>
                        <div class='cell' style='position: absolute; left: calc(50% + 20vh); top: calc(50% + 23vh);'>
                            <img src='./hexagon_middle_bg.png' class='cell-middle cell-img'>
                        </div>
                        <div class='cell' style='position: absolute; left: calc(50% + 33vh + 6px); top: calc(50% + 15vh);'>
                            <img src='./hexagon_small_bg.png' class='cell-small cell-img'>
                        </div>
                        <div class='cell' style='position: absolute; left: calc(50% + 27vh); top: calc(50% - 3vh);'>
                            <img src='./hexagon_small_bg.png' class='cell-small cell-img'>
                        </div>
                    </div>
                    
                    <script>
                      $(document).ready(function() {
                       var targetLinks = document.querySelectorAll('a[href^=\"#shiny-tab-\"]');
                       targetLinks.forEach(function(link) {
                        // 添加点击事件监听器
                        link.addEventListener('click', handleClick);
                      });
                      
                      // 点击事件处理函数
                      function handleClick(event) {
                        // 获取被点击的链接
                        var clickedLink = event.target;
                        // 只有在home位置显示背景图片
                        console.log(clickedLink.getAttribute('href'));
                        if (clickedLink.getAttribute('href') === null || clickedLink.getAttribute('href').startsWith('#shiny-tab-')) {
                          document.querySelector('.content-wrapper').querySelectorAll('section')[0].style.backgroundImage = 'none';
                        }
                      }
                     });
                    </script>
                    "),
      )
    ),
    tabItem(
      tabName = "machine_learning",
      machineLearningUI(id = "machine_learning")
    ),
    tabItem(
      tabName = "regression_analysis",
      regressionAnalysisUI(id = "regression_analysis")
    ),
    tabItem(
      tabName = "difference_analysis",
      differenceAnalysisUI(id = "difference_analysis")
    ),
    tabItem(
      tabName = "quality_control",
      qualityControlUI(id = "quality_control")
    ),
    tabItem(
      tabName = "user_management",
      userManagementUI(id = "user_management")
    ),
    tabItem(
      tabName = "heatmap",
      heatmapUI(id = "heatmap")
    ),
    tabItem(
      tabName = "ora_enrichment",
      oraEnrichmentUI(id = "ora_enrichment")
    ),
    tabItem(
      tabName = "gsea_enrichment",
      gseaEnrichmentUI(id = "gsea_enrichment")
    ),
    tabItem(
      tabName = "chemical_enrichment",
      chemicalEnrichmentUI(id = "chemical_enrichment")
    ),
    tabItem(
      tabName = "bioinfo_mining",
      bioinfoMiningUI(id = "bioinfo_mining")
    ),
    tabItem(
      tabName = "correlation_net",
      correlationNetUI(id = "correlation_net")
    ),
    tabItem(
      tabName = "biomarker_selection",
      biomarkerSelectionUI(id = "biomarker_selection")
    ),
    tabItem(
      tabName = "dimension_reduction",
      dimensionReductionPCAUI(id = "dimension_reduction")
    ),
    tabItem(
      tabName = "dimension_reduction_opls",
      dimensionReductionOPLSUI(id = "dimension_reduction_opls")
    ),
    tabItem(
      tabName = "pathway_gallery",
      pathwayGalleryUI(id = "pathway_gallery")
    ),
    tabItem(
      tabName = "missing_value_imputation",
      missingValueImputationUI(id = "missing_value_imputation")
    )
  )
)

ui <- dashboardPage(header, sidebar, body, title = "IPOS")


# Define server logic
server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$btn_home, {
    js$showHomeBg()
    updateTabItems(session, "menu", "home")
  })
  observeEvent(input$btn_machine_learning, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "machine_learning")
  })
  observeEvent(input$btn_regression_analysis, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "regression_analysis")
  })
  observeEvent(input$btn_difference_analysis, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "difference_analysis")
  })
  observeEvent(input$btn_quality_control, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "quality_control")
  })
  observeEvent(input$btn_ora_enrichment, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "ora_enrichment")
  })
  observeEvent(input$btn_chemical_enrichment, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "chemical_enrichment")
  })
  observeEvent(input$setting, {
    js$hideHomeBg()
    updateTabItems(session, "menu", "user_management")
  })
  
  callModule(machineLearningServer, "machine_learning")
  callModule(regressionAnalysisServer, "regression_analysis")
  callModule(differenceAnalysisServer, "difference_analysis")
  callModule(qualityControlServer, "quality_control")
  callModule(userManagementServer, "user_management")
  callModule(heatmapServer, "heatmap")
  callModule(oraEnrichmentServer, "ora_enrichment")
  callModule(gseaEnrichmentServer, "gsea_enrichment")
  callModule(chemicalEnrichmentServer, "chemical_enrichment")
  callModule(bioinfoMiningServer, "bioinfo_mining")
  callModule(correlationNetServer, "correlation_net")
  callModule(biomarkerSelectionServer, "biomarker_selection")
  callModule(dimensionReductionPCAServer, "dimension_reduction")
  callModule(dimensionReductionOPLSServer, "dimension_reduction_opls")
  callModule(missingValueImputationServer, "missing_value_imputation")
  callModule(pathwayGalleryServer, "pathway_gallery")
}

shinyApp(ui, server)