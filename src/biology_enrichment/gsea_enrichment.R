source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


gsea_result_lollipopplot_width <- 600
gsea_result_lollipopplot_height <- 600
gsea_result_dotplot_width <- 800
gsea_result_dotplot_height <- 600


gseaEnrichmentUI <- function(id) {
  ns <- NS(id)
  useToastr()
  useShinyjs()
  fluidPage(
    fluidRow(
      tags$div(
        class = "gsea_enrichment",
        fluidRow(
          column(width = 12,
                 box(title = "上传数据", collapsible = TRUE, collapsed = FALSE, width = 12,
                     solidHeader = TRUE,
                     fluidRow(column(width = 2, offset = 10, actionButton(inputId = ns("help_upload"),
                                                                          label = "帮助文档",
                                                                          icon = icon("book"),
                                                                          class = "help-button")
                                     )
                     ),
                     fluidRow(
                       column(width = 7,
                              fileInput(
                                inputId = ns("dataloader_querylist"),
                                label = "",
                                buttonLabel = div(icon("folder-open"), " 上传 Query List... "),
                                placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                                accept = c(".xlsx", ".csv"))
                              ),
                       column(width = 5,
                              fileInput(
                                inputId = ns("dataloader_bglist"),
                                label = "",
                                buttonLabel = div(icon("folder-open"), " 上传 Background List... "),
                                placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                                accept = c(".xlsx", ".csv"))
                              )
                     ),
                 ),
          )
        ),
        fluidRow(
          HTML('
               <div id="gseaDetailDialog" class="dialog">
            			<div class="dialog-header">
            			  <h4 class="dialog-title">Current Set</h4>
            			</div>
            			<div class="dialog-body">
            			  <table class="reference">
                    	<tbody>
                    	  <tr>
                    	    <th style="width:15%">Set Name</th>
                    	    <th style="width:30%">Candidates</th>
                    	    <th style="width:55%">Description</th>
                    	  </tr>
                    		<tr class="content">
                    			<td><div id="gseaSetName"></div></td>
                    			<td><div id="gseaCandidates"></div></td>
                    			<td><div id="gseaDesc" style="white-space:break-spaces;"></div></td>
                    		</tr>
                    	</tbody>
                    </table>
            			</div>
            			<div class="dialog-footer">
            			  <button class="dialogOkBtn" onclick="(function() {
            				  document.getElementById(\'gseaDetailDialog\').style.display=\'none\';
            				  document.getElementById(\'gseaDialogMask\').style.display=\'none\'
            				}())">确定</button>
            			</div>
            	 </div>
            	 
            	 <div id="gseaDialogMask"></div>
               '),
          
          
          column(width = 12,
                 box(id = "box-gsea", title = "GSEA(Gene Set Enrichment Analysis)", collapsible = TRUE, collapsed = TRUE, width = 12,
                     solidHeader = TRUE,
                     fluidRow(column(width = 2, offset = 10, helpButton(ns("help_analysis")))),
                     fluidRow(
                       sidebarPanel(
                         width = 2,
                         fluid  = TRUE,
                         actionButton(ns("setting"),
                                      label = "参数设置",
                                      icon = icon("sliders"),
                                      class = "setting-button"),
                         actionButton(ns("execute"), 
                                      # label = "Execute", 
                                      label = "执行", 
                                      icon = icon("play"),
                                      class = "setting-button"),
                       ),
                       mainPanel(
                         width = 10,
                         tabsetPanel(
                           tabPanel(title = "参数",
                                    icon = icon("sliders"),
                                    hidden(div(id = ns("databaseDesc"),
                                               HTML('<div style="padding-top:10px"><h4>Database Description</h4></div>'),
                                               withSpinner(DT::DTOutput(outputId = ns("database_desc")))
                                               )
                                           ),
                                    hidden(div(id = ns("enrichParams"),
                                               HTML('<div style="padding-top:10px; margin-top:30px"><h4>Enrich Parameters</h4></div>'),
                                               withSpinner(DT::DTOutput(outputId = ns("enrich_params")))
                                               )
                                           ),
                                    hidden(div(id = ns("resultSummary"),
                                               HTML('<div style="padding-top:10px; margin-top:30px"><h4>Result Summary</h4></div>'),
                                               withSpinner(DT::DTOutput(outputId = ns("result_summary")))
                                               )
                                           ),
                           ),
                           tabPanel(title = "数据",
                                    icon = icon("table"),
                                    hidden(div(id = ns("resultTable"),
                                               withSpinner(DT::DTOutput(outputId = ns("result_table"))),
                                               fluidRow(
                                                 column(width = 2, offset = 10,
                                                        downloadButton(outputId = ns("download_ora_result_table"),
                                                                       label = "下载数据",
                                                                       class = "download-button",
                                                                       icon = icon("download")
                                                                       )
                                                        )
                                                 )
                                               )
                                           )
                                    ),
                           tabPanel(title = "棒棒糖图",
                                    # icon = icon(""),
                                    hidden(div(id = ns("oraResultLollipopPlot"),
                                               fluidRow(
                                                 br(),
                                                 column(width = 9,
                                                        withSpinner(plotOutput(outputId = ns("ora_result_lollipopplot"), 
                                                                               width = paste0(gsea_result_lollipopplot_width, "px"), 
                                                                               height = "auto")
                                                                    )
                                                        ),
                                                 column(width = 3,
                                                        class = "plot-setting-column",
                                                        tags$div(
                                                          class="plot-setting",
                                                          box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                              fluidRow(
                                                                column(width = 10,
                                                                       numericInput(inputId = ns("pvalue_topn_of_ora_lollipopplot"), label = "P-Value TopN", min = 0, value = 30)
                                                                ),
                                                                column(width = 2, 
                                                                       checkboxInput(inputId = ns("select_pvalue_topn_of_ora_lollipopplot"), label = "", value = TRUE)
                                                                )
                                                              ),
                                                              uiOutput(outputId = ns("ora_lollipopplot_pvalue")),
                                                              fluidRow(
                                                                column(width = 10,
                                                                       numericInput(inputId = ns("qvalue_topn_of_ora_lollipopplot"), label = "Q-Value TopN", min = 0, value = 30)
                                                                ),
                                                                column(width = 2, 
                                                                       checkboxInput(inputId = ns("select_qvalue_topn_of_ora_lollipopplot"), label = "", value = FALSE)
                                                                )
                                                              ),
                                                              uiOutput(outputId = ns("ora_lollipopplot_qvalue")),
                                                              fluidRow(
                                                                column(width = 6, 
                                                                       numericInput(inputId = ns("gsea_result_lollipopplot_width"),
                                                                                    label = "宽度",
                                                                                    value = gsea_result_lollipopplot_width)
                                                                       ),
                                                                column(width = 6, 
                                                                       numericInput(inputId = ns("gsea_result_lollipopplot_height"),
                                                                                    label = "高度",
                                                                                    value = gsea_result_lollipopplot_height)
                                                                       )
                                                                )
                                                              ),
                                                          ),
                                                        tags$div(
                                                          class = "plot-setting",
                                                          box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                              fluidRow(
                                                                column(width = 8,
                                                                       selectInput(inputId = ns("palette_of_ora_lollipopplot"),
                                                                                   label = "调色板",
                                                                                   selected = "Palette-1",
                                                                                   choices = c("Palette-1", "Palette-2", "Palette-3", "Palette-4", "Palette-5", "Palette-6", "Palette-7", "Palette-8", "Palette-9", "Palette-10")
                                                                                   )
                                                                       )
                                                                )
                                                              )
                                                          )
                                                        )
                                                 ),
                                               fluidRow(
                                                 column(width = 2, offset = 10,
                                                        actionButton(inputId = ns("download_ora_result_lollipopplot"),
                                                                     label = "下载图表",
                                                                     class = "download-button",
                                                                     icon = icon("download")
                                                                     )
                                                        )
                                                 )
                                               )
                                           )
                                    ),
                           tabPanel(title = "点图",
                                    # icon = icon(""),
                                    hidden(div(id = ns("oraResultDotPlot"),
                                               fluidRow(
                                                 br(),
                                                 column(width = 9,
                                                        withSpinner(plotOutput(outputId = ns("ora_result_dotplot"),
                                                                               width = paste0(gsea_result_dotplot_width, "px"),
                                                                               height = "auto")
                                                                    )
                                                        ),
                                                 column(width = 3,
                                                        class = "plot-setting-column",
                                                        tags$div(
                                                          class="plot-setting",
                                                          box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                              fluidRow(
                                                                column(width = 10,
                                                                       numericInput(inputId = ns("pvalue_topn_of_ora_dotplot"), label = "P-Value TopN", min = 0, value = 30)
                                                                ),
                                                                column(width = 2,
                                                                       checkboxInput(inputId = ns("select_pvalue_topn_of_ora_dotplot"), label = "", value = TRUE)
                                                                )
                                                              ),
                                                              uiOutput(outputId = ns("ora_dotplot_pvalue")),
                                                              fluidRow(
                                                                column(width = 10,
                                                                       numericInput(inputId = ns("qvalue_topn_of_ora_dotplot"), label = "Q-Value TopN", min = 0, value = 30)
                                                                ),
                                                                column(width = 2,
                                                                       checkboxInput(inputId = ns("select_qvalue_topn_of_ora_dotplot"), label = "", value = FALSE)
                                                                )
                                                              ),
                                                              uiOutput(outputId = ns("ora_dotplot_qvalue")),
                                                              fluidRow(
                                                                column(width = 6,
                                                                       numericInput(inputId = ns("gsea_result_dotplot_width"),
                                                                                    label = "宽度",
                                                                                    value = gsea_result_dotplot_width)
                                                                ),
                                                                column(width = 6,
                                                                       numericInput(inputId = ns("gsea_result_dotplot_height"),
                                                                                    label = "高度",
                                                                                    value = gsea_result_dotplot_height)
                                                                )
                                                              )
                                                          ),
                                                        ),
                                                        tags$div(
                                                          class = "plot-setting",
                                                          box(title = "颜色设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                              fluidRow(
                                                                column(width = 8,
                                                                       selectInput(inputId = ns("palette_of_ora_dotplot"),
                                                                                   label = "调色板",
                                                                                   selected = "Palette-1",
                                                                                   choices = c("Palette-1", "Palette-2", "Palette-3", "Palette-4", "Palette-5", "Palette-6", "Palette-7", "Palette-8", "Palette-9", "Palette-10")
                                                                       )
                                                                )
                                                              )
                                                          )
                                                        )
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 2, offset = 10,
                                                        actionButton(inputId = ns("download_ora_result_dotplot"),
                                                                     label = "下载图表",
                                                                     class = "download-button",
                                                                     icon = icon("download")
                                                                     )
                                                        )
                                                 )
                                               )
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
}






gseaEnrichmentServer <- function(input, output, session) {
  ns <- session$ns
  querylist <- NULL
  bglist <- NULL
  oraResult <- NULL
  downloadOraResult <- NULL
  pathwayDatabase <- c("KEGG", "HumanCyc", "Reactome", "SMPDB")
  minGSSize <- 3
  maxGSSize <- 1000
  multiTestCorrection <- "BH"
  pValueCutoff <- 0.05
  qValueCutoff <- 0.2
  
  ora_result_lollipopplot <- NULL
  ora_result_dotplot <- NULL
  
  pathwayDB <- read.csv("biology_enrichment/data/Pathway.Database.csv")
  idNameMapping <- read.csv("biology_enrichment/data/Pathway.Dictionary.csv", check.names = FALSE)
  idNameMappingDf <- data.frame(t(idNameMapping[c("Name")]))
  colnames(idNameMappingDf) <- idNameMapping[["HMDB"]]
  
  observeEvent(input$dataloader_querylist$datapath, {
    if (str_ends(input$dataloader_querylist$datapath, ".xlsx")) {
      querylist <<- data.frame(readxl::read_xlsx(path = input$dataloader_querylist$datapath, sheet = 1), stringsAsFactors = F)
    } else if (str_ends(input$dataloader_querylist$datapath, ".csv")) {
      querylist <<- read.csv(input$dataloader_querylist$datapath)
    }

    # 自动展开
    js$collapse("box-gsea")
  })
  
  observeEvent(input$dataloader_bglist$datapath, {
    if (str_ends(input$dataloader_bglist$datapath, ".xlsx")) {
      bglist <<- data.frame(readxl::read_xlsx(path = input$dataloader_bglist$datapath, sheet = 1), stringsAsFactors = F)
    } else if (str_ends(input$dataloader_bglist$datapath, ".csv")) {
      bglist <<- read.csv(input$dataloader_bglist$datapath)
    }
  })
  
  observeEvent(input$setting, {
    showModal(modalDialog(
      title = "参数设置",
      size = "m",
      fluidPage(
        fluidRow(
          column(width = 6, checkboxGroupInput(inputId = ns("pathwayDatabase"), 
                                               label = "Pathway Database", 
                                               choices = c("KEGG", "HumanCyc", "Reactome", "SMPDB"),
                                               selected = pathwayDatabase)
          ),
        ),
        fluidRow(
          column(width = 5, numericInput(inputId = ns("minGSSize"), label = "minGSSize", value = minGSSize, min = 1, max = 1000, step = 1)),
          column(width = 5, offset = 2, numericInput(inputId = ns("maxGSSize"), label = "maxGSSize", value = maxGSSize, min = 1, max = 1000, step = 1)),
        ),
        fluidRow(
          column(width = 5, selectInput(inputId = ns("multiTestCorrection"), 
                                        label = "Multiple Test Correction", 
                                        choices = c("BH", "holm", "bonferroni", "none"), 
                                        selected = multiTestCorrection)),
        ),
        fluidRow(
          column(width = 5, numericInput(inputId = ns("pValueCutoff"), label = "P-Value Cutoff", value = pValueCutoff, min = 0, step = 0.01)),
          column(width = 5, offset = 2, numericInput(inputId = ns("qValueCutoff"), label = "Q-Value Cutoff", value = qValueCutoff, min = 0, step = 0.1)),
        ),
        class = "setting-params"
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = ns("setting_ok"), label = "确认", icon = NULL, class = "dialogOkBtn"),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$setting_ok, {
    pathwayDatabase <<- input$pathwayDatabase
    minGSSize <<- input$minGSSize
    maxGSSize <<- input$maxGSSize
    multiTestCorrection <<- input$multiTestCorrection
    pValueCutoff <<- input$pValueCutoff
    qValueCutoff <<- input$qValueCutoff
    removeModal()
  })
  
  createDetailButton <- function(data) {
    button = list()
    for (i in 1:nrow(data)) {
      setName = data[["Set.Name"]][i]
      desc = data[["Description.y"]][i]
      candidates = gsub("/", ", ", data[["geneID"]][i])
      composition = data[["Set.Composition"]][i]
      
      # 根据id查找字典表, 找到name
      compositionIds = unlist(strsplit(data[["Set.Composition"]][i], split = ", "))
      compNameList = c()
      for (compId in compositionIds) {
        compName = idNameMappingDf[[compId]]
        if (!is.null(compName) && compName != "") {
          compNameList = append(compNameList, compName)
        }
      }
      
      # 找到hit基因的name
      geneIds = unlist(strsplit(data[["geneID"]][i], split = "/"))
      hits = ""
      hitNameList = c()
      for (hitId in geneIds) {
        hitName = idNameMappingDf[[hitId]]
        if (!is.null(hitName) && hitName != "") {
          hitNameList = append(hitNameList, hitName)
        }
      }
      
      compositionWithHits = ""
      for (name in compNameList) {
        span = ""
        if (name %in% hitNameList) {
          span = sprintf("<span style='color: mediumvioletred; font-weight: bold;'>%s; </span>", name);
        } else {
          span = sprintf("<span>%s; </span>", name);
        }
        if (compositionWithHits == "") {
          compositionWithHits = span;
        } else {
          compositionWithHits = paste0(compositionWithHits, span)
        }
      }
      
      btnHtml = sprintf('<button class="dataTables_button_details" onclick="(function() {
                document.getElementById(\'gseaDetailDialog\').style.display=\'block\';
                document.getElementById(\'gseaDialogMask\').style.display=\'block\';
                document.getElementById(\'gseaSetName\').innerHTML=`%s`;
                document.getElementById(\'gseaCandidates\').innerHTML=`%s`;
                document.getElementById(\'gseaDesc\').innerHTML=`%s`;
              }())">details</button>', setName, compositionWithHits, gsub('"', '“', desc))
      
      button = append(button, btnHtml)
    }
    return(button)
  }
  
  getHitsDetailContent <- function(data) {
    hits = c()
    comp = c()
    for (i in 1:nrow(data)) {
      setName = data[["Set.Name"]][i]
      desc = data[["Description.y"]][i]
      candidates = gsub("/", ", ", data[["geneID"]][i])
      composition = data[["Set.Composition"]][i]
      
      # 根据id查找字典表, 找到name
      compositionIds = unlist(strsplit(data[["Set.Composition"]][i], split = ", "))
      compNameList = c()
      for (compId in compositionIds) {
        compName = idNameMappingDf[[compId]]
        if (!is.null(compName) && compName != "") {
          compNameList = append(compNameList, compName)
        }
      }
      
      # 找到hit基因的name
      geneIds = unlist(strsplit(data[["geneID"]][i], split = "/"))
      hitNameList = c()
      for (hitId in geneIds) {
        hitName = idNameMappingDf[[hitId]]
        if (!is.null(hitName) && hitName != "") {
          hitNameList = append(hitNameList, hitName)
        }
      }
      
      currentHits = ""
      currentComp = ""
      for (name in compNameList) {
        span = ""
        if (name %in% hitNameList) {
          # span = sprintf("<span style='color: mediumvioletred; font-weight: bold;'>%s; </span>", name);
          span = sprintf("%s; ", name);
        }
        if (currentHits == "") {
          currentHits = span;
        } else {
          currentHits = paste0(currentHits, span)
        }
        
        if (currentComp == "") {
          currentComp = name;
        } else {
          currentComp = paste(currentComp, name, sep = "; ")
        }
      }
      hits = append(hits, currentHits)
      comp = append(comp, currentComp)
    }
    hitsDetail = data.frame(hits = hits, comp = comp)
    return(hitsDetail)
  }
  
  # 执行
  observeEvent(input$execute, {
    
    shinyjs::show("enrichParams")
    shinyjs::show("databaseDesc")
    shinyjs::show("resultSummary")
    shinyjs::show("resultTable")
    shinyjs::show("oraResultLollipopPlot")
    shinyjs::show("oraResultDotPlot")
    
    if (!is.null(querylist)) {
      
      # 过滤所选择数据集
      base = subset(pathwayDB, pathwayDB[["Source.Database"]] %in% pathwayDatabase)
      print(unique(base["Source.Database"]))
      
      base = base %>%
        dplyr::select(Set.ID, Set.Composition) %>%
        dplyr::mutate(Set.Composition = strsplit(Set.Composition, ', ')) %>%
        tidyr::unnest(cols = c(Set.Composition))
      
      if (is.null(bglist)) {
        enrichResult <- GSEA(querylist[[1]], 
                                 TERM2GENE = base, 
                                 minGSSize = minGSSize, 
                                 maxGSSize = maxGSSize, 
                                 pAdjustMethod = multiTestCorrection,
                                 pvalueCutoff = pValueCutoff,
                                 qvalueCutoff = qValueCutoff)
        
        oraResult <<- data.frame(enrichResult)
      } else {
        enrichResult <- GSEA(querylist[[1]], 
                                 universe = bglist[[1]],
                                 TERM2GENE = base, 
                                 minGSSize = minGSSize, 
                                 maxGSSize = maxGSSize, 
                                 pAdjustMethod = multiTestCorrection,
                                 pvalueCutoff = pValueCutoff,
                                 qvalueCutoff = qValueCutoff)
        
        oraResult <<- data.frame(enrichResult)
      }
      
      
      if (!is.null(oraResult) && nrow(oraResult) > 0) {
        # 关联详细信息
        oraResult <<- inner_join(oraResult, pathwayDB, by = c("ID" = "Set.ID"))
        
        temp_df_generatio = data.frame(name = oraResult[["GeneRatio"]])
        geneRatioFloat = apply(temp_df_generatio, 1, function(x) eval(parse(text=x)))
        
        temp_df_bgratio = data.frame(name = oraResult[["BgRatio"]])
        bgRatioFloat = apply(temp_df_bgratio, 1, function(x) eval(parse(text=x)))
        
        oraResult[["FoldEnrichment"]] = signif(geneRatioFloat / bgRatioFloat, digits = 0)
        
        # 保留有效数字
        oraResult[["Details"]] = createDetailButton(oraResult)
        oraResult[["pvalue"]] = signif(oraResult[["pvalue"]] , digits = 4)
        oraResult[["p.adjust"]] = signif(oraResult[["p.adjust"]] , digits = 4)
        oraResult[["qvalue"]] = signif(oraResult[["qvalue"]] , digits = 4)
        oraResult <- mutate(oraResult, RichFactor = Count / as.numeric(sub("/\\d+", "", BgRatio)))
        oraResult[["RichFactor"]] = signif(oraResult[["RichFactor"]] , digits = 4)
        hitsDetail = getHitsDetailContent(oraResult)
        
        oraResult[["HitsDetail"]] = hitsDetail[["hits"]]
        oraResult[["Set.Composition"]] = hitsDetail[["comp"]]
        
        displayOraResult = oraResult[, c("ID", "Set.Name", "Set.Size", "Source.Database", "GeneRatio", "BgRatio", "FoldEnrichment", "Count",
                                      "pvalue", "p.adjust", "qvalue", "RichFactor", "Set.Tag", "Details")]
        downloadOraResult <<- oraResult[, c("ID", "Set.Name", "Set.Size", "Source.Database", "GeneRatio", "BgRatio", "FoldEnrichment", "Count",
                                      "pvalue", "p.adjust", "qvalue", "RichFactor", "Set.Tag", "HitsDetail", "Set.Composition")]
        
        # 数据集描述Table
        dbset = unique(pathwayDB[["Source.Database"]])
        totalsets = c()
        minsize = c()
        maxsize = c()
        mediansize = c()
        version = c()
        for(db in dbset) {
          dbSubset = subset(pathwayDB, pathwayDB["Source.Database"] == db)
          totalsets = append(totalsets, nrow(dbSubset))
          minsize = append(minsize, min(dbSubset[["Set.Size"]]))
          maxsize = append(maxsize, max(dbSubset[["Set.Size"]]))
          mediansize = append(mediansize, median(dbSubset[["Set.Size"]]))
          if (length(dbSubset[["Version.information"]]) > 1) {
            version = append(version, dbSubset[["Version.information"]][1])
          } else {
            version = append(version, "")
          }
        }
        database_desc_df = data.frame(Database = unique(pathwayDB[["Source.Database"]]),
                                      TotalSets = totalsets,
                                      MinGSSize = minsize, 
                                      MaxGSSize = maxsize,
                                      MedianGSSize = mediansize,
                                      Version = version)
        
        
        output$database_desc <- DT::renderDT({
          DT::datatable({
            database_desc_df
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
          selection = 'none',
          style = 'bootstrap',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        
        # 设置参数Table
        enrich_params_df <- data.frame(param = c("Pathway Database", "minGSSize", "maxGSSize", "Multiple Test Correction", "P-Value Cutoff", "Q-Value Cutoff"),
                                       value = c(str_c(pathwayDatabase, sep = "", collapse = " + "), minGSSize, maxGSSize, multiTestCorrection, pValueCutoff, qValueCutoff))
        
        output$enrich_params <- DT::renderDT({
          DT::datatable({
            enrich_params_df
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
          selection = 'none',
          style = 'bootstrap',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        # resultSummary Table
        result_summary_df = data.frame(params = c("Uploaded query metabolites", 
                                                  "Upload background metabolites", 
                                                  "Overlapped metabolites",
                                                  "Overlapped pathway sets",
                                                  "Enriched significant pathway sets"),
                                       value = c(nrow(querylist), ifelse(is.null(bglist), 0, nrow(bglist)), "", "", "")
        )
        
        output$result_summary <- DT::renderDT({
          DT::datatable({
            result_summary_df
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
          selection = 'none',
          style = 'bootstrap',
          class = 'stripe hover cell-border compact datatable',
          rownames = FALSE
          )
        })
        
        # 计算结果Table
        output$result_table <- DT::renderDT({
          DT::datatable({
            displayOraResult
          },
          options = dataTableOptions,
          selection = 'none',
          style = 'bootstrap4',
          class = 'cell-border stripe compact datatable',
          rownames = FALSE
          )
        })
        
        # 棒棒糖图
        # 渲染p-value, q-value选择控件
        output$ora_lollipopplot_pvalue <- renderUI({
          fluidRow(
            column(width = 10,
                   sliderInput(inputId = ns("pvalue_of_ora_lollipopplot"), label = "P-Value",
                               min = min(oraResult[["pvalue"]]), 
                               max = max(oraResult[["pvalue"]]), 
                               value = c(min(oraResult[["pvalue"]]), max(oraResult[["pvalue"]]))
                   )
            ),
            column(width = 2,
                   checkboxInput(inputId = ns("select_pvalue_of_ora_lollipopplot"), label = "",
                                 value = FALSE)
            )
          )
        })
        output$ora_lollipopplot_qvalue <- renderUI({
          fluidRow(
            column(width = 10,
                   sliderInput(inputId = ns("qvalue_of_ora_lollipopplot"), label = "Q-Value",
                               min = min(oraResult[["qvalue"]]),
                               max = max(oraResult[["qvalue"]]),
                               value = c(min(oraResult[["qvalue"]]), max(oraResult[["qvalue"]]))
                   )
            ),
            column(width = 2,
                   checkboxInput(inputId = ns("select_qvalue_of_ora_lollipopplot"), label = "",
                                 value = FALSE)
            )
          )
        })
        
        output$ora_result_lollipopplot <- renderPlot({
          plotData <- oraResult
          if (input$select_pvalue_of_ora_lollipopplot == TRUE) {
            plotData = subset(plotData, plotData[["pvalue"]] > input$pvalue_of_ora_lollipopplot[1] & plotData[["pvalue"]] < input$pvalue_of_ora_lollipopplot[2])
          }
          if (input$select_qvalue_of_ora_lollipopplot == TRUE) {
            plotData = subset(plotData, plotData[["qvalue"]] > input$qvalue_of_ora_lollipopplot[1] & plotData[["qvalue"]] < input$qvalue_of_ora_lollipopplot[2])
          }
          if (input$select_pvalue_topn_of_ora_lollipopplot == TRUE) {
            plotData <- plotData[order(plotData["pvalue"]),]
            plotData <- head(plotData, input$pvalue_topn_of_ora_lollipopplot)
          }
          if (input$select_qvalue_topn_of_ora_lollipopplot == TRUE) {
            plotData <- plotData[order(plotData["qvalue"]),]
            plotData <- head(plotData, input$qvalue_topn_of_ora_lollipopplot)
          }
          
          if (nrow(plotData) > 0) {
            # 自适应更新ora-lollipop-plot的高度
            gsea_result_lollipopplot_height <<- nrow(plotData) * 20 + 40
            updateNumericInput(inputId = "gsea_result_lollipopplot_height", value = gsea_result_lollipopplot_height)
            
            palette = c()
            if (input$palette_of_ora_lollipopplot == "Palette-1") {
              palette = GradientColorPalette$Palette1
            } else if (input$palette_of_ora_lollipopplot == "Palette-2") {
              palette = GradientColorPalette$Palette2
            } else if (input$palette_of_ora_lollipopplot == "Palette-3") {
              palette = GradientColorPalette$Palette3
            } else if (input$palette_of_ora_lollipopplot == "Palette-4") {
              palette = GradientColorPalette$Palette4
            } else if (input$palette_of_ora_lollipopplot == "Palette-5") {
              palette = GradientColorPalette$Palette5
            } else if (input$palette_of_ora_lollipopplot == "Palette-6") {
              palette = GradientColorPalette$Palette6
            } else if (input$palette_of_ora_lollipopplot == "Palette-7") {
              palette = GradientColorPalette$Palette7
            } else if (input$palette_of_ora_lollipopplot == "Palette-8") {
              palette = GradientColorPalette$Palette8
            } else if (input$palette_of_ora_lollipopplot == "Palette-9") {
              palette = GradientColorPalette$Palette9
            } else if (input$palette_of_ora_lollipopplot == "Palette-10") {
              palette = GradientColorPalette$Palette10
            }
            
            ora_result_lollipopplot <<- ggplot(plotData, showCategory = 10, aes(FoldEnrichment, fct_reorder(Set.Name, FoldEnrichment))) +
              geom_segment(aes(xend=0, yend = Set.Name), size = 0.2) +
              geom_point(aes(color = pvalue, fill = pvalue, size = Count), shape = 21) +
              scale_size_continuous(range = c(2, 10)) +
              scale_colour_gradientn(colours = palette, trans = "log10", guide = NULL) +
              scale_fill_gradientn(colours = palette, trans = "log10", guide = guide_colorbar(reverse=TRUE, order=1)) +
              theme_bw() +
              theme(panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()) +
              theme(panel.grid.major.x = element_line(linetype = 2),
                    panel.grid.minor.x = element_line(linetype = 2)) +
              xlab("Fold Enrichment") +
              ylab("") +
              ggtitle("Biological Processes")
            return(ora_result_lollipopplot)
          } else {
            return(NULL)
          }
        }, height = function() {
          gsea_result_lollipopplot_height <<- input$gsea_result_lollipopplot_height
          return(gsea_result_lollipopplot_height)
        }, width = function() {
          gsea_result_lollipopplot_width <<- input$gsea_result_lollipopplot_width
          return(gsea_result_lollipopplot_width)
        })
        
        
        
        
        
        # dotplot
        # 渲染p-value, q-value选择控件
        output$ora_dotplot_pvalue <- renderUI({
          fluidRow(
            column(width = 10,
                   sliderInput(inputId = ns("pvalue_of_ora_dotplot"), label = "P-Value",
                               min = min(oraResult[["pvalue"]]), 
                               max = max(oraResult[["pvalue"]]), 
                               value = c(min(oraResult[["pvalue"]]), max(oraResult[["pvalue"]]))
                   )
            ),
            column(width = 2,
                   checkboxInput(inputId = ns("select_pvalue_of_ora_dotplot"), label = "",
                                 value = FALSE)
            )
          )
        })
        output$ora_dotplot_qvalue <- renderUI({
          fluidRow(
            column(width = 10,
                   sliderInput(inputId = ns("qvalue_of_ora_dotplot"), label = "Q-Value",
                               min = min(oraResult[["qvalue"]]),
                               max = max(oraResult[["qvalue"]]),
                               value = c(min(oraResult[["qvalue"]]), max(oraResult[["qvalue"]]))
                   )
            ),
            column(width = 2,
                   checkboxInput(inputId = ns("select_qvalue_of_ora_dotplot"), label = "",
                                 value = FALSE)
            )
          )
        })
        
        output$ora_result_dotplot <- renderPlot({
          plotData <- oraResult
          if (input$select_pvalue_of_ora_dotplot == TRUE) {
            plotData = subset(plotData, plotData[["pvalue"]] > input$pvalue_of_ora_dotplot[1] & plotData[["pvalue"]] < input$pvalue_of_ora_dotplot[2])
          }
          if (input$select_qvalue_of_ora_dotplot == TRUE) {
            plotData = subset(plotData, plotData[["qvalue"]] > input$qvalue_of_ora_dotplot[1] & plotData[["qvalue"]] < input$qvalue_of_ora_dotplot[2])
          }
          if (input$select_pvalue_topn_of_ora_dotplot == TRUE) {
            plotData <- plotData[order(plotData["pvalue"]),]
            plotData <- head(plotData, input$pvalue_topn_of_ora_dotplot)
          }
          if (input$select_qvalue_topn_of_ora_dotplot == TRUE) {
            plotData <- plotData[order(plotData["qvalue"]),]
            plotData <- head(plotData, input$qvalue_topn_of_ora_dotplot)
          }
          
          if (nrow(plotData) > 0) {
            # 自适应更新ora-lollipop-plot的高度
            gsea_result_dotplot_height <<- nrow(plotData) * 20 + 40
            updateNumericInput(inputId = "gsea_result_dotplot_height", value = gsea_result_dotplot_height)
            
            palette = c()
            if (input$palette_of_ora_dotplot == "Palette-1") {
              palette = GradientColorPalette$Palette1
            } else if (input$palette_of_ora_dotplot == "Palette-2") {
              palette = GradientColorPalette$Palette2
            } else if (input$palette_of_ora_dotplot == "Palette-3") {
              palette = GradientColorPalette$Palette3
            } else if (input$palette_of_ora_dotplot == "Palette-4") {
              palette = GradientColorPalette$Palette4
            } else if (input$palette_of_ora_dotplot == "Palette-5") {
              palette = GradientColorPalette$Palette5
            } else if (input$palette_of_ora_dotplot == "Palette-6") {
              palette = GradientColorPalette$Palette6
            } else if (input$palette_of_ora_dotplot == "Palette-7") {
              palette = GradientColorPalette$Palette7
            } else if (input$palette_of_ora_dotplot == "Palette-8") {
              palette = GradientColorPalette$Palette8
            } else if (input$palette_of_ora_dotplot == "Palette-9") {
              palette = GradientColorPalette$Palette9
            } else if (input$palette_of_ora_dotplot == "Palette-10") {
              palette = GradientColorPalette$Palette10
            }
            
            ora_result_dotplot <<- ggplot(plotData, showCategory = 10, aes(-log10(pvalue), fct_reorder(Set.Name, -log10(pvalue)))) +
              geom_point(aes(color = pvalue, fill = pvalue, size = FoldEnrichment), shape = 21) +
              scale_size_continuous(range = c(2, 10)) +
              scale_colour_gradientn(colours = palette, trans = "log10", guide = NULL) +
              scale_fill_gradientn(colours = palette, trans = "log10", guide = guide_colorbar(reverse=TRUE, order=1)) +
              theme_bw() +
              xlab("-log10(p-value)") +
              ylab("") +
              ggtitle("Overview of Enriched Metabolites Sets")
            return(ora_result_dotplot)
          } else {
            return(NULL)
          }
        }, height = function() {
          gsea_result_dotplot_height <<- input$gsea_result_dotplot_height
          return(gsea_result_dotplot_height)
        }, width = function() {
          gsea_result_dotplot_width <<- input$gsea_result_dotplot_width
          return(gsea_result_dotplot_width)
        })
      } else {
        toastr_warning(message = "根据您提供的background list未富集到任何结果", title = "0 enriched terms found")
        shinyjs::hide("enrichParams")
        shinyjs::hide("databaseDesc")
        shinyjs::hide("resultSummary")
        shinyjs::hide("resultTable")
        shinyjs::hide("oraResultLollipopPlot")
        shinyjs::hide("oraResultDotPlot")
      }
      
    }
  })
  
  # lollipop的p-value/q-value选择控制
  observeEvent(input$select_pvalue_of_ora_lollipopplot, {
    if(input$select_pvalue_of_ora_lollipopplot == FALSE) {
      shinyjs::disable("pvalue_of_ora_lollipopplot")
    } else {
      shinyjs::enable("pvalue_of_ora_lollipopplot")
    }
  })
  observeEvent(input$select_qvalue_of_ora_lollipopplot, {
    if(input$select_qvalue_of_ora_lollipopplot == FALSE) {
      shinyjs::disable("qvalue_of_ora_lollipopplot")
    } else {
      shinyjs::enable("qvalue_of_ora_lollipopplot")
    }
  })
  observeEvent(input$select_pvalue_topn_of_ora_lollipopplot, {
    if (input$select_pvalue_topn_of_ora_lollipopplot == FALSE) {
      shinyjs::disable("pvalue_topn_of_ora_lollipopplot")
    } else {
      shinyjs::enable("pvalue_topn_of_ora_lollipopplot")
    }
  })
  observeEvent(input$select_qvalue_topn_of_ora_lollipopplot, {
    if (input$select_qvalue_topn_of_ora_lollipopplot == FALSE) {
      shinyjs::disable("qvalue_topn_of_ora_lollipopplot")
    } else {
      shinyjs::enable("qvalue_topn_of_ora_lollipopplot")
    }
  })
  
  
  # dotplot的p-value/q-value选择控制
  observeEvent(input$select_pvalue_of_ora_dotplot, {
    if(input$select_pvalue_of_ora_dotplot == FALSE) {
      shinyjs::disable("pvalue_of_ora_dotplot")
    } else {
      shinyjs::enable("pvalue_of_ora_dotplot")
    }
  })
  observeEvent(input$select_qvalue_of_ora_dotplot, {
    if(input$select_qvalue_of_ora_dotplot == FALSE) {
      shinyjs::disable("qvalue_of_ora_dotplot")
    } else {
      shinyjs::enable("qvalue_of_ora_dotplot")
    }
  })
  observeEvent(input$select_pvalue_topn_of_ora_dotplot, {
    if (input$select_pvalue_topn_of_ora_dotplot == FALSE) {
      shinyjs::disable("pvalue_topn_of_ora_dotplot")
    } else {
      shinyjs::enable("pvalue_topn_of_ora_dotplot")
    }
  })
  observeEvent(input$select_qvalue_topn_of_ora_dotplot, {
    if (input$select_qvalue_topn_of_ora_dotplot == FALSE) {
      shinyjs::disable("qvalue_topn_of_ora_dotplot")
    } else {
      shinyjs::enable("qvalue_topn_of_ora_dotplot")
    }
  })
  
  
  # 下载数据
  output$download_ora_result_table <- downloadHandler(
    filename = function() {
      paste0("ora_enrich_results.xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(downloadOraResult, file, asTable = TRUE)
    }
  )
  
  
  # 导出棒棒糖图
  observeEvent(input$download_ora_result_lollipopplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("download_ora_result_lollipopplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("download_ora_result_lollipopplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("download_ora_result_lollipopplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$download_ora_result_lollipopplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$download_ora_result_lollipopplot_name) || input$download_ora_result_lollipopplot_name == "") {
        paste("lollipop-", format(Sys.time(), "%Y%m%d%H%M%S"), input$download_ora_result_lollipopplot_format, sep="")
      } else {
        paste(input$download_ora_result_lollipopplot_name, input$download_ora_result_lollipopplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = ora_result_lollipopplot,
             width = gsea_result_lollipopplot_width / plot_size_fold,
             height = gsea_result_lollipopplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  
  # 导出dotplot
  observeEvent(input$download_ora_result_dotplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("download_ora_result_dotplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("download_ora_result_dotplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("download_ora_result_dotplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$download_ora_result_dotplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$download_ora_result_dotplot_name) || input$download_ora_result_dotplot_name == "") {
        paste("dotplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$download_ora_result_dotplot_format, sep="")
      } else {
        paste(input$download_ora_result_dotplot_name, input$download_ora_result_dotplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = ora_result_dotplot,
             width = gsea_result_dotplot_width / plot_size_fold,
             height = gsea_result_dotplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "GSEA(Gene Set Enrichment Analysis)",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/BiologyEnrich_UploadData.md")),
        fluidRow(column(width = 2,
                        downloadButton(outputId = ns("download_sampledata_querylist_bioenrich"),
                                       label = "QueryList 样例", icon = icon("download"),
                                       class = "help-download-sampledata")),
                 column(width = 2,
                        downloadButton(outputId = ns("download_sampledata_backgroundlist_bioenrich"),
                                       label = "BackgroundList 样例", icon = icon("download"),
                                       class = "help-download-sampledata"))
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
  
  output$download_sampledata_querylist_bioenrich <- downloadHandler(
    filename = "querylist.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/bioenrich_querylist.csv")
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  output$download_sampledata_backgroundlist_bioenrich <- downloadHandler(
    filename = "backgroundlist.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/bioenrich_backgroundlist.csv")
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
}