library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinytoastr)
library(dashboardthemes)
library(plotly)
library(randomForest)
library(DT)

library(xlsx)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(momr)
library(e1071)
library(glmnet)
library(psych)
library(EnhancedVolcano)

library(dplyr)
library(glue)
library(RSQLite)
library(DBI)
library(lubridate)

library(reactable)
library(scales)
library(stringr)

library(bslib)

# 显示中文, linux环境不需要
#library(sysfonts)
#library(showtextdb)
#library(showtext)

# 颜色选择器
library(colourpicker)

library(corrplot)
library(plotROC)
library(pROC)

library(clusterProfiler)
library(forcats)

library(ropls)
library(ggsci)
library(scales)
library(ggforce)

library(pheatmap)

# 缺失值填充
library(Amelia)

options(warn=-1)
options(digits = 4)
options(shiny.maxRequestSize=60*1024^2)

#显示中文
#showtext_auto()

STYLES <- list(
  height_hist = 280,
  # outliers
  height_outlier_hist = "180px",
  # time subsetting
  # not setting the box height make arrange multiple items easier.
  # height_hist_subset_box = "380px",
  height_hist_subset_output = "150px",
  # height_selected_loc_box = "480px"
  # height_selected_loc = 480
  page_action = "background-color: #FFEB3B;font-weight: 500;width:100%;",
  # using similar color with first box in each page.
  page_switch = "background-color: #7ad0f7;font-weight: 500;width:100%;",
  external_link = "background-color: #a7c1fc;font-weight: 500;width:100%;",
  execute_button = "background-color: #28b5b5; width:100%; margin-top: 5px; color: #ffffff; border: none",
  export_button = "background-color: #28b5b5; width:100%; margin-top: 5px; color: #ffffff; border: none",
  add_button = "background-color: #28b5b5; width:100%; margin-top: 25px; color: #ffffff; border: none",
  help_button = "background-color: #16c79a; width:80%; margin-top: 5px; color: #ffffff; border: none; float:right",
  help_download_sampledata_button = "background-color: #16c79a; width:120%; margin-top: 15px; color: #ffffff; border: none;",
  # vertical align checkbox, radio buttons to button in same line.
  align_down = "margin-top: 5px;",
  align_up = "margin-top: -5px;",
  align_up_group = "margin-top: -12px;",
  # info box blue #00c0ef
  # p_info = "text-align:justify; color:black; background-color:#8EE5EE; padding:15px; border-radius:5px"
  p_info = "text-align:justify; color:black; background-color:#8fd9a8; padding:15px; border-radius:5px"
  
)

COLUMNS <- list(
  feature = "feature",
  meanDecreaseInAccuracy = "Mean decrease in accuracy"
)

ColorBrewr <- list(
  seq = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", 
          "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens", "GnBu", "BuPu", 
          "BuGn", "Blues"),
  custom = c("#00468b", "DeepPink", "red", "OrangeRed", "gold", "orange", "springgreen", 
             "mediumseagreen", "blue", "royalblue", "skyblue", "purple", "pink", 
             "salmon", "NavajoWhite", "Chocolate", "BlueViolet", "Tomato", "steelblue1", 
             "dodgerblue", "springgreen4", "khaki1", "firebrick", "grey30", "forestgreen",  "red2", "red3", "black", 
             "darkslategray1", "turquoise4", "indianred1", "darkorange1", "#F6C241", "#953D38",
             brewer.pal(8, "Set2"),
             brewer.pal(12, "Set3")
             )
)

GradientColorPalette <- list(
  Palette1 = c("#f7ca64", "#46bac2", "#7e62a3"),
  Palette2 = c("#7e62a3", "#46bac2", "#f7ca64"),
  Palette3 = c("#5D3376", "#C95898", "#FFF2E4"),
  Palette4 = c("#EE0771", "#FFE34F"),
  Palette5 = c("#2D78C2", "#36C2A9"),
  Palette6 = c("#EC0D78", "#5139D7"),
  Palette7 = c("#FCC692", "#FF8A92"),
  Palette8 = c("#FFCB8E", "#EE64C2", "#6E35FF"),
  Palette9 = c("#FC685C", "#013DB5"),
  Palette10 = c("#0C7BB3", "#F2BAE8")
)

getColorPalette <- function(palettename, classnum) {
  colorpalette <- c()
  set.seed(10)
  if (palettename == 'Palette 1') {
    colorpalette <- c("orange", "yellow", "pink", "springgreen", "#00FFAAFF", "mediumseagreen",
                      "#00BBFFFF", "royalblue", "skyblue", "blue", "#9400D3", "#FF0099FF")
  } else if (palettename == 'Palette 2') {
    if (classnum <= 12) {
      colorpalette <- brewer.pal(classnum, "Set3")
    } else {
      colorpalette <- brewer.pal(12, "Set3")
    }
  } else if (palettename == 'Palette 3') {
    if (classnum <= 8) {
      colorpalette <- brewer.pal(classnum, "Set2")
    } else {
      colorpalette <- brewer.pal(8, "Set2")
    }
  } else if (palettename == 'Palette 4') {
    if (classnum <= 9) {
      colorpalette <- brewer.pal(classnum, "Set1")
    } else {
      colorpalette <- brewer.pal(9, "Set1")
    }
  } else if (palettename == 'Palette 5') {
    if (classnum <= 8) {
      colorpalette <- brewer.pal(classnum, "Pastel2")
    } else {
      colorpalette <- brewer.pal(8, "Pastel2")
    }
  } else if (palettename == 'Palette 6') {
    if (classnum <= 9) {
      colorpalette <- brewer.pal(classnum, "Pastel1")
    } else {
      colorpalette <- brewer.pal(9, "Pastel1")
    }
  } else if (palettename == 'Palette 7') {
    if (classnum <= 12) {
      colorpalette <- brewer.pal(classnum, "Paired")
    } else {
      colorpalette <- brewer.pal(12, "Paired")
    }
  } else if (palettename == 'Palette 8') {
    if (classnum <= 8) {
      colorpalette <- brewer.pal(classnum, "Dark2")
    } else {
      colorpalette <- brewer.pal(8, "Dark2")
    }
  } else if (palettename == 'Palette 9') {
    if (classnum <= 8) {
      colorpalette <- brewer.pal(classnum, "Accent")
    } else {
      colorpalette <- brewer.pal(8, "Accent")
    }
  }
  while (length(colorpalette) < classnum) {
    index <- ceiling(runif(1, min = 0, max = length(colorpalette)))
    colorpalette <- append(colorpalette, colorpalette[index])
  }
  return(colorpalette)
}

ggsciColorPalette <- function(palettename, alpha = 1, size = NULL) {
  color_palette <- c()
  if (palettename == 'd3') {
    color_palette = pal_d3("category10", alpha = alpha)(10)
  } else if (palettename == 'aaas') {
    color_palette = pal_aaas("default", alpha = alpha)(10)
  } else if (palettename == 'jama') {
    color_palette = pal_jama("default", alpha = alpha)(7)
  } else if (palettename == 'jco') {
    color_palette = pal_jco("default", alpha = alpha)(10)
  } else if (palettename == 'lancet') {
    color_palette = pal_lancet("lanonc", alpha = alpha)(9)
  } else if (palettename == 'locus' || palettename == 'locuszoom') {
    color_palette = pal_locuszoom("default", alpha = alpha)(7)
  } else if (palettename == 'nejm') {
    color_palette = pal_nejm("default", alpha = alpha)(8)
  } else if (palettename == 'npg') {
    color_palette = pal_npg("nrc", alpha = alpha)(10)
  } else if (palettename == 'rick') {
    color_palette = pal_rickandmorty("schwifty", alpha = alpha)(12)
  } else if (palettename == 'simpson' || palettename == 'simpsons') {
    color_palette = pal_simpsons("springfield", alpha = alpha)(16)
  } else if (palettename == 'startrek') {
    color_palette = pal_startrek("uniform", alpha = alpha)(7)
  } else if (palettename == 'tron') {
    color_palette = pal_tron("legacy", alpha = alpha)(7)
  } else if (palettename == 'dark') {
    color_palette = pal_uchicago("dark", alpha = alpha)(9)
  } else if (palettename == 'light') {
    color_palette = pal_uchicago("light", alpha = alpha)(9)
  } else if (palettename == 'random') {
    random_color_sets <- 
      c("#E64B35B2", "#3C5488B2", "#91D1C2B2", "#4DBBD5B2", "#F39B7FB2","#DC0000B2", 
        "#E7C76FFF", "#00A087B2", "#8491B4B2", "#7E6148B2", "#4CAF50", "#339900FF", "#ff8c3e",
        "#99CC00FF", "#8ec4cb","#5050FFFF", "#CE3D32FF", "#749B58FF", "#F0E685FF", "#466983FF",
        "#0099CCFF", "#BA6338FF", "#5DB1DDFF", "#802268FF", "#6BD76BFF", "#D595A7FF", "#924822FF",
        "#837B8DFF", "#C75127FF", "#D58F5CFF", "#7A65A5FF", "#E4AF69FF", "#3B1B53FF", 
        "#CDDEB7FF", "#612A79FF", "#18C7AD","#369AFF","#4781BF","#57728F","#70E0D0","#79B9FC","#85FFED",
        "#89C400","#9975E0","#9BA7B3","#A09DF5","#B4FF99","#C7B8E6","#C7E1FC",
        "#C9FFD6","#FC6C32","#FC7979","#FCB2A2","#FFC973","#FFE0C2","#FFF1B8",
        "#1B8F7D","#40C7B3","#5194DB","#69B4FF","#6BBF7F","#6DEDDA","#7DB4F0",
        "#7EE095","#92B1D1","#99E8DC","#9BA7B3","#A1C8F0","#AC82FF","#B0875D",
        "#B0BFCF","#BE9FFC","#C7B8E6","#C7E1FC","#C9FFD6","#CCBA74","#CCE8D3",
        "#CFFCF6","#D5AFDE","#D5DDE6","#E3F1FF","#E6C29E","#E8D482","#EBFFEF",
        "#F0E3AF","#F0E8FF","#F0EEE1","#FCB268","#FFD4A8","#FFE0C2","#FFF1B8")
    color_palette <- sample(random_color_sets, size = size)
  }
  
  # if too many colors
  if (!is.null(size) && length(color_palette) > size) {
    color_palette <- color_palette[1:size]
  }
  
  # if color is not enough, fill with pal_igv
  if (!is.null(size) && length(color_palette) < size) {
    color_palette <- append(color_palette, sample(pal_igv(alpha = alpha)(50), size = (size - length(color_palette))))
  }
  
  return(color_palette)
}

getGradientColorPalette <- function(palettename) {
  if (palettename == 'Palette 1') {
    return(colorRampPalette(color = c("red","white","blue"))(99))
  } else if (palettename == 'Palette 2') {
    return(colorRampPalette(colors = c("green","black","red"))(99))
  } else if (palettename == 'Palette 3') {
    return(colorRampPalette(brewer.pal(11,"Spectral"))(99))
  } else if (palettename == 'Palette 4') {
    return(colorRampPalette(brewer.pal(11,"RdYlGn"))(99))
  } else if (palettename == 'Palette 5') {
    return(colorRampPalette(brewer.pal(11,"RdYlBu"))(99))
  } else if (palettename == 'Palette 6') {
    return(colorRampPalette(brewer.pal(11,"RdGy"))(99))
  } else if (palettename == 'Palette 7') {
    return(colorRampPalette(brewer.pal(11,"RdBu"))(99))
  } else if (palettename == 'Palette 8') {
    return(colorRampPalette(brewer.pal(11,"PuOr"))(99))
  } else if (palettename == 'Palette 9') {
    return(colorRampPalette(brewer.pal(11,"PRGn"))(99))
  } else if (palettename == 'Palette 10') {
    return(colorRampPalette(brewer.pal(11,"PiYG"))(99))
  } else if (palettename == 'Palette 11') {
    return(colorRampPalette(brewer.pal(11,"BrBG"))(99))
  } 
}

jscode <- "
shinyjs.collapse = function(boxid) {
  if ($('#' + boxid).closest('.box').find('[data-widget=collapse]').children('i').attr('class').indexOf('fa-plus') !== -1) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
  }
}

shinyjs.execute = function(id) {
  $('#' + id)[0].click();
}

shinyjs.hideDiv = function(id) {
  document.getElementById(id).style.display = 'none';
}

shinyjs.showDiv = function(id) {
  document.getElementById(id).style.display = 'block';
}

shinyjs.updateElementInnerHTML = function(param) {
  document.getElementById(param.id).innerHTML = param.innerHTML;
}

shinyjs.updateStyle = function(param) {
    // 获取元素
    var element = document.getElementById(param.id);
    element.style[param.property] = param.style;
}
"

effective_digits <- 2
# 像素与毫米转换比例
plot_size_fold <- 3


dataTableOptions = list(bLengthChange = FALSE,
                        pageLength = 15,
                        initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().body()).css({'font-size': '12px'});",
                          "$(this.api().table().header()).css({'font-size': '12px'});",
                          "}"),
                        columnDefs = list(list(className='dt-left', targets="_all")),
                        scrollX = TRUE,
                        searching = FALSE,
                        paging = TRUE,
                        bInfo = TRUE,
                        language = list(paginate = list("next" = "<i class='fa fa-chevron-right'></i>",
                                                        "previous" = "<i class='fa fa-chevron-left'></i>"),
                                        sInfo = "当前显示 _START_ 到 _END_ 条，共计 _TOTAL_ 条记录。")
                        # bAutoWidth = TRUE
                        # columns = list(list(width = "100px"), list(width = "200px"))
                        )

dataTableOptions_pageLength25 = list(bLengthChange = FALSE,
                        pageLength = 25,
                        initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().body()).css({'font-size': '12px'});",
                          "$(this.api().table().header()).css({'font-size': '12px'});",
                          "}"),
                        columnDefs = list(list(className='dt-left', targets="_all")),
                        scrollX = TRUE,
                        searching = FALSE,
                        paging = TRUE,
                        bInfo = TRUE,
                        language = list(paginate = list("next" = "<i class='fa fa-chevron-right'></i>",
                                                        "previous" = "<i class='fa fa-chevron-left'></i>")
                                        )
                        # bAutoWidth = TRUE
                        # columns = list(list(width = "100px"), list(width = "200px"))
)

dataTableOptions_pageLength30 = list(bLengthChange = FALSE,
                                     pageLength = 31,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().body()).css({'font-size': '12px'});",
                                       "$(this.api().table().header()).css({'font-size': '12px'});",
                                       "}"),
                                     columnDefs = list(list(className='dt-left', targets="_all")),
                                     scrollX = TRUE,
                                     searching = FALSE,
                                     paging = TRUE,
                                     bInfo = FALSE,
                                     language = list(paginate = list("next" = "<i class='fa fa-chevron-right'></i>",
                                                                     "previous" = "<i class='fa fa-chevron-left'></i>")
                                     )
                                     # bAutoWidth = TRUE
                                     # columns = list(list(width = "100px"), list(width = "200px"))
)

dataTableOptions_mini = list(bLengthChange = FALSE,
                             pageLength = 25,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().body()).css({'font-size': '12px'});",
                               "$(this.api().table().header()).css({'font-size': '12px'});",
                               "}"),
                             columnDefs = list(list(className='dt-left', targets="_all")),
                             scrollX = TRUE,
                             searching = FALSE,
                             paging = FALSE,
                             bInfo = FALSE,
                             ordering = FALSE,
                             language = list(paginate = list("next" = "<i class='fa fa-chevron-right'></i>",
                                                             "previous" = "<i class='fa fa-chevron-left'></i>")
                             ),
                             bAutoWidth = TRUE
                             # columns = list(list(width = "100px"), list(width = "200px"))
)


#
# 化学富集
#
load.ChemRICH.Packages <- function() {
  if (!require("devtools"))
    install.packages('devtools', repos="http://cran.rstudio.com/")
  if (!require("RCurl"))
    install.packages('RCurl', repos="http://cran.rstudio.com/")
  if (!require("pacman"))
    install.packages('pacman', repos="http://cran.rstudio.com/")
  library(devtools)
  library(RCurl)
  library(pacman)
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  pacman::p_load(GGally)
  pacman::p_load(DT)
  pacman::p_load(RCurl)
  pacman::p_load(RJSONIO)
  pacman::p_load(ape)
  pacman::p_load(devEMF)
  pacman::p_load(dynamicTreeCut)
  pacman::p_load(extrafont)
  pacman::p_load(ggplot2)
  pacman::p_load(ggpubr)
  pacman::p_load(ggrepel)
  pacman::p_load(grid)
  pacman::p_load(htmlwidgets)
  pacman::p_load(igraph)
  pacman::p_load(magrittr)
  pacman::p_load(network)
  pacman::p_load(officer)
  pacman::p_load(openxlsx)
  pacman::p_load(phytools)
  pacman::p_load(plotly)
  pacman::p_load(plotrix)
  pacman::p_load(rcdk)
  pacman::p_load(readxl)
  pacman::p_load(rvg)
  pacman::p_load(sna)
  pacman::p_load(visNetwork)
}
load.ChemRICH.Packages()


buildAccordionItem <- function(id, title, collapsed = FALSE) {
  btn_down_style = ifelse(collapsed == FALSE, "display:block", "display:none")
  btn_right_style = ifelse(collapsed == FALSE, "display:none", "display:block")
  return(
    HTML(
      sprintf('
              <div class="collapse-item-header">
                  <button class="collapse-btn">
                      <div style="display:flex">
                          <svg class="collapse-btn-arrow-down" viewBox="0 0 24 24" width="18" height="18" style="%s">
                            <path d="M17,9.17a1,1,0,0,0-1.41,0L12,12.71,8.46,9.17a1,1,0,0,0-1.41,0,1,1,0,0,0,0,1.42l4.24,4.24a1,1,0,0,0,1.42,0L17,10.59A1,1,0,0,0,17,9.17Z"></path>
                          </svg>
                          <svg class="collapse-btn-arrow-right" viewBox="0 0 24 24" width="18" height="18" style="%s">
                            <path d="M14.83,11.29,10.59,7.05a1,1,0,0,0-1.42,0,1,1,0,0,0,0,1.41L12.71,12,9.17,15.54a1,1,0,0,0,0,1.41,1,1,0,0,0,.71.29,1,1,0,0,0,.71-.29l4.24-4.24A1,1,0,0,0,14.83,11.29Z"></path>
                          </svg>
                      </div>
                  </button>
                  <div>
                    <span class="collapse-item-title">%s</span>
                  </div>
              </div>
              ', btn_down_style, btn_right_style, title)
    )
  )
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
    var target = document.getElementById('dimension_reduction-opls_scoreplot_confidence_linetype');
    if (target !== null) {
      var element = target.nextElementSibling.firstChild;
      element.style.height = '30px';
      element.style.background = 'url(./linetype/linetype-%s.png) 10%% 50%% / 70%% 70%% no-repeat';
      element.style.backgroundColor = 'white';
    }", name)
  )
}

onInspectDataBtnClick <- function (drawer, overlay) {
  return(sprintf("
    (function () {
      var drawer = document.getElementById('%s');
      var overlay = document.getElementById('%s');
      drawer.classList.toggle('drawer-open');
      overlay.classList.toggle('overlay-active');
      document.body.style.overflow = 'scroll';
    })()
    ", drawer, overlay)
  )
}

onOverlayClick <- function (drawer, overlay) {
  return(sprintf("
    (function () {
      var drawer = document.getElementById('%s');
      var overlay = document.getElementById('%s');
      drawer.classList.toggle('drawer-open');
      overlay.classList.toggle('overlay-active');
      document.body.style.overflow = 'scroll';
    })()
    ", drawer, overlay)
  )
}

hideDivById <- function (id) {
  return(sprintf("
    (function () {
      document.getElementById('%s').style.display = 'none';
    })()
    ", id)
  )
}

showDivById <- function (id) {
  return(sprintf("
    (function () {
      document.getElementById('%s').style.display = 'block';
    })()
    ", id)
  )
}