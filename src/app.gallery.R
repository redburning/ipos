source("global.R", encoding = "UTF-8")
source('home/gallery.R', encoding = "UTF-8")
source('machine_learning/machine_learning.R', encoding = "UTF-8")
source('regression_analysis/regression_analysis.R', encoding = "UTF-8")
source('difference_analysis/difference_analysis.R', encoding = "UTF-8")
source('quality_control/quality_control.R', encoding = "UTF-8")
source('biology_enrichment/ora_enrichment.R', encoding = "UTF-8")
source('biology_enrichment/gsea_enrichment.R', encoding = "UTF-8")
source('chemical_enrichment/chemical_enrichment.R', encoding = "UTF-8")
source('bioinfo_mining/bioinfo_mining.R', encoding = "UTF-8")
source('correlation_net/correlation_net.R', encoding = "UTF-8")
source('biomarker_selection/biomarker_selection.R', encoding = "UTF-8")
source('dimension_reduction/pca.R', encoding = "UTF-8")

source('dimension_reduction/opls_loadingplot.R', encoding = "UTF-8")
source('dimension_reduction/opls_splot.R', encoding = "UTF-8")
source('dimension_reduction/opls_vplot.R', encoding = "UTF-8")

source('extended_toolbox/gallery_heatmap.R', encoding = "UTF-8")

source('pathway_gallery/pathway_gallery.R', encoding = "UTF-8")

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
  title = div(HTML(paste0('<a href="#shiny-tab-home" data-toggle="tab" data-value="home" aria-expanded="true" tabindex="0" aria-selected="true">
                            <div>
                              <div>
                                <img src="logo-IPOS-v3.3.0.png", height=35, align="center">
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
              HTML("<li class='' classname='iconfont icon-jiqixuexi'>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-jiqixuexi' role='presentation' aria-label='dashboard icon'></i>
                              <span>机器学习</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-huiguimoxing' role='presentation' aria-label='chart-line icon'></i>
                              <span>回归估计</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-haiguanjianguan-kucunchayifenxi' role='presentation' aria-label='stream icon'></i>
                              <span>差异分析</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-zhiliangkongzhitu' role='presentation' aria-label='cogs icon'></i>
                              <span>质控矫正</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-shujuwajue' role='presentation' aria-label='cogs icon'></i>
                              <span>生信挖掘</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-shujuwajue' role='presentation' aria-label='cogs icon'></i>
                              <span>代谢通路富集</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-jiyinfuji' role='presentation' aria-label='cogs icon'></i>
                              <span>化学富集</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-wajue' role='presentation' aria-label='cogs icon'></i>
                              <span>相关网络</span>
                            </a>
                       </li>"),
              HTML("<li>
                            <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                              <i class='iconfont icon-shujuwajue' role='presentation' aria-label='cogs icon'></i>
                              <span>标志物分析</span>
                            </a>
                       </li>"),
              menuItem(text = "多元降维", startExpanded = FALSE, icon = icon("magnifying-glass-chart"),
                       menuSubItem(text = "Loading Plot" , tabName = "dimension_reduction_opls_loadingplot", icon = NULL),
                       menuSubItem(text = "S-Plot" , tabName = "dimension_reduction_opls_splot", icon = NULL),
                       menuSubItem(text = "V-Plot" , tabName = "dimension_reduction_opls_vplot", icon = NULL)
              ),
              menuItem(text = "知识图谱", tabName = "pathway_gallery", icon = icon('book')),
              menuItem(text = "扩展坞", startExpanded = FALSE, icon = icon("box"),
                       menuSubItem(text = "heatmap", tabName = "extended_toolbox_heatmap", icon = NULL)
              )
  )
)


# body
body <- dashboardBody(
  customTheme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "gallery.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "iconfont.css"),
    tags$script(src = "lib/typed.js"),
    tags$script(src = "lib/global.js")
  ),
  useShinyjs(),
  HTML(
    "
      <div class='overlay' id='overlay'></div>
      <div class='msgBox' id='msgBox'>
        <div style='padding:10px 15px; background-color:#0077cc; border-radius:5px 5px 0px 0px;'>
          <div style='font-size:20px; color:white;'>Tips</div>
        </div>
        <div style='padding:30px 15px;'>
          <p>This module does not support trial, it is only available in the release version. Please visit <a href='http://82.157.20.231:3838/ipos'>iPOS cloud service</a> and contact the administrator for authorization.</p>
          <div style='display:flex; justify-content:flex-end; gap:20px; margin-top:20px;'>
            <div style='background-color:#0077cc; color: white; border-radius:5px; width:65px;'>
              <a style='display:flex; justify-content:center; color:white; padding: 5px 10px; cursor:pointer;'
                onclick='document.getElementById(\"overlay\").style.display=\"none\"; document.getElementById(\"msgBox\").style.display=\"none\";'>OK</a>
            </div>
          </div>
        </div>
      </div>
      "
  ),
  tabItems(
    tabItem(
      tabName = "home",
      galleryUI(id = "home")
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
      tabName = "extended_toolbox_heatmap",
      heatmapUI(id = "extended_toolbox_heatmap")
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
      tabName = "dimension_reduction_opls_loadingplot",
      dimensionReductionOPLSLoadingPlotUI(id = "dimension_reduction_opls_loadingplot")
    ),
    tabItem(
      tabName = "dimension_reduction_opls_splot",
      dimensionReductionOPLSSPlotUI(id = "dimension_reduction_opls_splot")
    ),
    tabItem(
      tabName = "dimension_reduction_opls_vplot",
      dimensionReductionOPLSVPlotUI(id = "dimension_reduction_opls_vplot")
    ),
    tabItem(
      tabName = "pathway_gallery",
      pathwayGalleryUI(id = "pathway_gallery")
    )
  )
)

ui <- dashboardPage(header, sidebar, body, title = "IPOS")


# Define server logic
server <- function(input, output, session) {
  ns <- session$ns
  observeEvent(input$btn_home, {
    updateTabItems(session, "menu", "home")
  })
  
  callModule(machineLearningServer, "machine_learning")
  callModule(regressionAnalysisServer, "regression_analysis")
  callModule(differenceAnalysisServer, "difference_analysis")
  callModule(qualityControlServer, "quality_control")
  callModule(heatmapServer, "extended_toolbox_heatmap")
  callModule(oraEnrichmentServer, "ora_enrichment")
  callModule(gseaEnrichmentServer, "gsea_enrichment")
  callModule(chemicalEnrichmentServer, "chemical_enrichment")
  callModule(bioinfoMiningServer, "bioinfo_mining")
  callModule(correlationNetServer, "correlation_net")
  callModule(biomarkerSelectionServer, "biomarker_selection")
  callModule(dimensionReductionOPLSLoadingPlotServer, "dimension_reduction_opls_loadingplot")
  callModule(dimensionReductionOPLSSPlotServer, "dimension_reduction_opls_splot")
  callModule(dimensionReductionOPLSVPlotServer, "dimension_reduction_opls_vplot")
  callModule(pathwayGalleryServer, "pathway_gallery")
}

shinyApp(ui, server)