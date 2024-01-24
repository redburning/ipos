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

ggsciColorPalette <- function(palettename, alpha = 1) {
  if (palettename == 'd3') {
    return(pal_d3("category10", alpha = alpha)(10))
  } else if (palettename == 'aaas') {
    return(pal_aaas("default", alpha = alpha)(10))
  } else if (palettename == 'jama') {
    return(pal_jama("default", alpha = alpha)(7))
  } else if (palettename == 'jco') {
    return(pal_jco("default", alpha = alpha)(10))
  } else if (palettename == 'lancet') {
    return(pal_lancet("lanonc", alpha = alpha)(9))
  } else if (palettename == 'locus') {
    return(pal_locuszoom("default", alpha = alpha)(7))
  } else if (palettename == 'nejm') {
    return(pal_nejm("default", alpha = alpha)(8))
  } else if (palettename == 'npg') {
    return(pal_npg("nrc", alpha = alpha)(10))
  } else if (palettename == 'rick') {
    return(pal_rickandmorty("schwifty", alpha = alpha)(12))
  } else if (palettename == 'simpson') {
    return(pal_simpsons("springfield", alpha = alpha)(16))
  } else if (palettename == 'startrek') {
    return(pal_startrek("uniform", alpha = alpha)(7))
  } else if (palettename == 'tron') {
    return(pal_tron("legacy", alpha = alpha)(7))
  } else if (palettename == 'dark') {
    return(pal_uchicago("dark", alpha = alpha)(9))
  } else if (palettename == 'light') {
    return(pal_uchicago("light", alpha = alpha)(9))
  }
}

jscode <- "
shinyjs.collapse = function(boxid) {
  if ($('#' + boxid).closest('.box').find('[data-widget=collapse]').children('i').attr('class').indexOf('fa-plus') !== -1) {
    $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
    console.log(boxid);
  }
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