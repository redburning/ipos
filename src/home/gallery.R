source("global.R", encoding = "UTF-8")

galleryUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    HTML("
          <div style='padding:0px 20px 20px 20px;'>
            <h3 id='dimension-reduction' class='gallery-module-head'>多元降维</h3>
            <div style='display:flex; gap:35px;'>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a href='#shiny-tab-dimension_reduction_opls_loadingplot' data-toggle='tab' data-value='dimension_reduction_opls_loadingplot' aria-expanded='true' tabindex='0' aria-selected='true'>
                    <img src='gallery/dimension_reduction-loadingplot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>Loading Plot</div>
                    <div class='gallery-chart-subtitle'>载荷图</div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>多元降维</div>
                    <div class='gallery-chart-tag2'>OPLS</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a href='#shiny-tab-dimension_reduction_opls_splot' data-toggle='tab' data-value='dimension_reduction_opls_splot' aria-expanded='true' tabindex='0' aria-selected='true'>
                    <img src='gallery/dimension_reduction-splot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>S-Plot</div>
                    <div class='gallery-chart-subtitle'></div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>多元降维</div>
                    <div class='gallery-chart-tag2'>OPLS</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a href='#shiny-tab-dimension_reduction_opls_vplot' data-toggle='tab' data-value='dimension_reduction_opls_vplot' aria-expanded='true' tabindex='0' aria-selected='true'>
                    <img src='gallery/dimension_reduction-vplot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>V-Plot</div>
                    <div class='gallery-chart-subtitle'></div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>多元降维</div>
                    <div class='gallery-chart-tag2'>OPLS</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                    <img src='gallery/dimension_reduction-vipbubbleplot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>VIP Bubble Plot</div>
                    <div class='gallery-chart-subtitle'>VIP气泡图</div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>多元降维</div>
                    <div class='gallery-chart-tag2'>OPLS</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                    <img src='gallery/dimension_reduction-scoreplot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>Score Plot</div>
                    <div class='gallery-chart-subtitle'>得分图</div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>多元降维</div>
                    <div class='gallery-chart-tag2'>OPLS</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                    <img src='gallery/dimension_reduction-bioplot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>Bio Plot</div>
                    <div class='gallery-chart-subtitle'></div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>多元降维</div>
                    <div class='gallery-chart-tag2'>OPLS</div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          
          <div style='padding:0px 20px 20px 20px;'>
            <h3 id='difference-analysis' class='gallery-module-head'>差异分析</h3>
            <div style='display:flex; gap:35px;'>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                    <img src='gallery/difference_analysis-boxplot.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>Box Plot</div>
                    <div class='gallery-chart-subtitle'>箱线图</div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>差异分析</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                    <img src='gallery/difference_analysis-windrose.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>WindRose Plot</div>
                    <div class='gallery-chart-subtitle'>玫瑰图</div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>差异分析</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a onclick='document.getElementById(\"overlay\").style.display=\"block\"; document.getElementById(\"msgBox\").style.display=\"block\";'>
                    <img src='gallery/difference_analysis-zscore.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>Z-Score Plot</div>
                    <div class='gallery-chart-subtitle'></div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>差异分析</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
              </div>
              <div style='width:16.66666667%'>
              </div>
              <div style='width:16.66666667%'>
              </div>
              
            </div>
          </div>
          
          <div style='padding:0px 20px 20px 20px;'>
            <h3 id='difference-analysis' class='gallery-module-head'>扩展坞</h3>
            <div style='display:flex; gap:35px;'>
              <div style='width:16.66666667%'>
                <div class='gallery-chart-container'>
                  <a href='#shiny-tab-extended_toolbox_heatmap' data-toggle='tab' data-value='extended_toolbox_heatmap' aria-expanded='true' tabindex='0' aria-selected='true'>
                    <img src='gallery/extend_toolbox-heatmap.png' class='gallery-chartarea'>
                  </a>
                </div>
                <div class='gallery-chart-info'>
                  <div>
                    <div class='gallery-chart-title'>Heatmap</div>
                    <div class='gallery-chart-subtitle'></div>
                  </div>
                  <div style='display:flex; gap:6px;'>
                    <div class='gallery-chart-tag'>扩展坞</div>
                  </div>
                </div>
              </div>
              <div style='width:16.66666667%'>
              </div>
              <div style='width:16.66666667%'>
              </div>
              <div style='width:16.66666667%'>
              </div>
              <div style='width:16.66666667%'>
              </div>
              <div style='width:16.66666667%'>
              </div>
              
            </div>
          </div>
          "
         ),
  )
}
