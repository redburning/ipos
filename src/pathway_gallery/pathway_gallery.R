source("global.R", encoding = "UTF-8")

#
pathway_library <- NULL
# 记录检索历史
input_history <- c('乳腺癌', '胰腺癌', '肝癌', '脂肪酸', '巨噬细胞', '癌细胞')


buildImgElement <- function (src, desc = '', keywords_metabolite = '', keywords_pathway = '', 
                             keywords_disease = '', keywords_target = '', journal = '', doi = '') {
  img_desc = ifelse(desc == '', src, desc)
  img_keywords_metabolite = ifelse(keywords_metabolite == '', '', paste0(paste0('<div>', unlist(strsplit(keywords_metabolite, ";")), '</div>'), collapse = ""))
  img_keywords_pathway = ifelse(keywords_pathway == '', '', paste0(paste0('<div>', unlist(strsplit(keywords_pathway, ";")), '</div>'), collapse = ""))
  img_keywords_disease = ifelse(keywords_disease == '', '', paste0(paste0('<div>', unlist(strsplit(keywords_disease, ";")), '</div>'), collapse = ""))
  img_keywords_target = ifelse(keywords_target == '', '', paste0(paste0('<div>', unlist(strsplit(keywords_target, ";")), '</div>'), collapse = ""))
  img_desc = gsub("'", "&apos;", img_desc)
  img_keywords_metabolite = gsub("'", "&apos;", img_keywords_metabolite)
  img_keywords_pathway = gsub("'", "&apos;", img_keywords_pathway)
  img_keywords_disease = gsub("'", "&apos;", img_keywords_disease)
  img_keywords_target = gsub("'", "&apos;", img_keywords_target)
  return(
    sprintf('
        <div class="pathway_gallery_img_container" data-img-src="pathway-library/%s.png" 
          data-img-desc="%s" data-img-journal="%s" data-img-doi="%s" data-img-keywords-m="%s" data-img-keywords-p="%s" data-img-keywords-d="%s" data-img-keywords-t="%s" ">
          <img src="pathway-library-small/%s.png" class="pathway_gallery_search_result_img" style="width:100%%;">
          <div class="pathway_gallery_img_detail">
            <div class="pathway_gallery_img_desc">%s</div>
            <div style="display:flex; justify-content:flex-end; margin-top:8px;">
              <div class="pathway_gallery_img_download">Download</div>
            </div>
          </div>
        </div>
      ', src, img_desc, journal, doi, img_keywords_metabolite, img_keywords_pathway, img_keywords_disease, img_keywords_target, src, img_desc)
  )
}

buildColumn <- function(rowIndex) {
  column = c()
  for (i in 1:length(rowIndex)) {
    column <- append(column, buildImgElement(src = pathway_library[["name"]][rowIndex[i]],
                                             journal = pathway_library[["journal"]][rowIndex[i]],
                                             doi = pathway_library[["doi"]][rowIndex[i]],
                                             keywords_metabolite = pathway_library[["keywords_metabolite"]][rowIndex[i]],
                                             keywords_pathway = pathway_library[["keywords_pathway"]][rowIndex[i]],
                                             keywords_disease = pathway_library[["keywords_disease"]][rowIndex[i]],
                                             keywords_target = pathway_library[["keywords_target"]][rowIndex[i]]))
  }
  return(HTML(column))
}

# 根据搜索结果自动排布3列结果
buildGallery <- function (rowIndex1, rowIndex2, rowIndex3) {
  return(
    tags$div(
      id = "searched-gallery",
      style = "display:flex; justify-content:center; gap:16px;",
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; width:calc((100vw - 230px - 300px) / 3); max-width:350px;",
        buildColumn(rowIndex1)
      ),
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; width:calc((100vw - 230px - 300px) / 3); max-width:350px;",
        buildColumn(rowIndex2)
      ),
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; width:calc((100vw - 230px - 300px) / 3); max-width:350px;",
        buildColumn(rowIndex3)
      )
    )
  )
}


pathwayGalleryUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useToastr(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/pathway-gallery.css"),
      tags$script(src = "lib/pathway-gallery.js")
    ),
    extendShinyjs(text = jscode, functions = c("showDiv", "hideDiv", "updateElementInnerHTML")),
    
    tags$div(
      style = "display:flex; justify-content:center; margin-top:50px;",
      tags$div(
        tags$div(
          style = "display:flex; justify-content:center;",
          HTML('
               <img src="pathway-gallery-logo.png" style="height:80px; cursor:pointer;" id="pathway-gallery-logo">
               ')
        ),
        tags$div(
          style = "display:flex; margin-bottom:20px;",
          tags$div(
            id = 'pathway_gallery-search_container',
            tags$input(id = 'pathway_gallery-search_input', type = "text", style = "width:540px;", onfocus="showDropdown()",
                       placeholder = "Try searching for a metabolite, disease, pathway, or target."),
            tags$div(
              id = 'pathway_gallery-search_history',
              class = "search-history",
              HTML(
                paste0('<li>', input_history, '</li>', collapse = "")
              )
            )
          ),
          tags$div(
            actionButton(inputId = ns('search_btn'), label = "Search", icon = icon("search"), class = 'search-btn')
          )
        ),
        tags$div(
          id = "none-search-result",
          style = "color:#0b57d0; font-weight:600; font-size:16px; display:none;",
          "No results found based on your input"
        )
      )
    ),
    
    tags$div(
      id = "default-gallery",
      style = "display:flex; justify-content:center; gap:16px;",
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; width:calc((100vw - 230px - 300px) / 3); max-width:350px;",
        HTML(
          buildImgElement(src = "传统的瓦尔堡代谢体内肿瘤代谢的现代认识",
                          journal = "Nat Rev Urol. 2020 Apr;17(4):214-231.",
                          doi = "10.1038/s41585-020-0288-x",
                          keywords_metabolite = "glucose;glutamine;lactic acid;Pyruvic acid",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "肿瘤中的谷氨酰胺代谢",
                          journal = "Signal Transduct Target Ther. 2023 Sep 13;8(1):345.",
                          doi = "10.1038/s41392-023-01569-3",
                          keywords_metabolite = "glutamine;Glutathione;Cysteine",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "靶向核苷酸糖分子关键酶调节岩藻糖和唾液酸的代谢",
                          journal = "Biotechnol Adv. 2023 Oct:67:108184.",
                          doi = "10.1016/j.biotechadv.2023.108184",
                          keywords_metabolite = "glucose;glucose-6-phosphatase",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "TME中免疫细胞的脂肪酸代谢重编程",
                          journal = "Biochim Biophys Acta Rev Cancer. 2023 Sep;1878(5):188962",
                          doi = "10.1016/j.bbcan.2023.188962",
                          keywords_metabolite = "fatty acid",
                          keywords_pathway = "Fatty acid metabolism",
                          keywords_disease = "肿瘤;tumor",
                          keywords_target = "PPAR;过氧化物酶体增殖物激活受体;PPAR;IL-4;白细胞介素-4;Interleukin-4;IL-10;白细胞介素-10;Interleukin-10;"),
          buildImgElement(src = "SREBP活性的多水平调节",
                          journal = "Nat Rev Cancer. 2016 Nov;16(11):732-749.",
                          doi = "10.1038/nrc.2016.89",
                          keywords_metabolite = "glucose;Lysophosphatidylserine(LPS)",
                          keywords_target = "SREBP"),
          buildImgElement(src = "肿瘤相关巨噬细胞重编程",
                          journal = "Nature. 2023 Jul;619(7970):616-623.",
                          doi = "10.1038/s41586-023-06256-5",
                          keywords_disease = "乳腺癌;breast cancer;"),
          buildImgElement(src = "极低密度脂蛋白受体增强的胰腺星状细胞脂质代谢促进胰腺纤维化",
                          journal = "Very-low-density lipoprotein receptor-enhanced lipid metabolism in pancreatic stellate cells promotes pancreatic fibrosis",
                          doi = "10.1016/j.immuni.2022.06.001",
                          keywords_metabolite = "Linoleic acid(LA)",
                          keywords_pathway = "Tricarboxylic acid cycle",
                          keywords_disease = "慢性胰腺炎;chronic pancreatitis",
                          keywords_target = "IL-33;白细胞介素-33;Interleukin-33")
        )
      ),
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; width:calc((100vw - 230px - 300px) / 3); max-width:350px;",
        HTML(
          buildImgElement(src = "PAH代谢途径的改变",
                          journal = "Annu Rev Physiol. 2021 Feb 10;83:551-576.",
                          doi = "10.1146/annurev-physiol-031620-123956",
                          keywords_metabolite = "glucose;Arginine;Ornithine;lactic acid;Citrulline;Pyruvic acid",
                          keywords_pathway = "Tricarboxylic acid cycle",
                          keywords_disease = "肺动脉高压;Pulmonary arterial hypertension"),
          buildImgElement(src = "肿瘤免疫微环境影响单核-巨噬细胞代谢模式",
                          journal = "Nat Rev Immunol 2023 Feb;23(2):106-120.",
                          doi = "10.1038/s41577-022-00737-w",
                          keywords_metabolite = "glucose;glutamine;tryptophan",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "亚油酸通过代谢重编程来增强CD8+T细胞的抗肿瘤功能",
                          journal = "Cell Metab 2023 Apr 4;35(4):633-650.e9.",
                          doi = "10.1016/j.cmet.2023.02.013",
                          keywords_metabolite = "glucose;Linoleic acid(LA);Pyruvic acid;Triglyceride(TG)",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "p62在调节肿瘤葡萄糖和脂肪酸代谢中的作用",
                          journal = "Trends Endocrinol Metab. 2023 Aug;34(8):474-488.",
                          doi = "10.1016/j.tem.2023.05.004",
                          keywords_metabolite = "glucose;Glutathione",
                          keywords_pathway = "Glucose metabolism;Fatty acid metabolism",
                          keywords_disease = "肿瘤;tumor",
                          keywords_target = "PI3K;磷脂酰肌醇激酶;Phosphoinositide 3 kinase;Akt;汇丝氨酸/苏氨酸激酶;protein kinase B;NRF2;nuclear factor erythroid 2-related factor 2"),
          buildImgElement(src = "脂肪酸的合成与摄取",
                          journal = "Nat Rev Cancer. 2016 Nov;16(11):732-749.",
                          doi = "10.1038/nrc.2016.89",
                          keywords_metabolite = "Cholesterol;Palmitic acid",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "脂肪酸的β氧化途径",
                          journal = "Nat Rev Cancer. 2016 Nov;16(11):732-749.",
                          doi = "10.1038/nrc.2016.89",
                          keywords_metabolite = "Carnitine",
                          keywords_pathway = "Tricarboxylic acid cycle"),
        )
      ),
      tags$div(
        style = "display:flex; flex-direction:column; gap:8px; width:calc((100vw - 230px - 300px) / 3); max-width:350px;",
        HTML(
          buildImgElement(src = "增强雄性β细胞中 GLP-1 刺激的胰岛素胞吐作用的 AR 基因组和非基因组作用",
                          journal = "Xu, Weiwei et al. “Architecture of androgen receptor pathways amplifying glucagon-like peptide-1 insulinotropic action in male pancreatic β cells.” Cell reports vol. 42,5 (2023): 112529. doi:10.1016/j.celrep.2023.112529",
                          doi = "10.1016/j.celrep.2023.112529",
                          keywords_metabolite = "Testosterone",
                          keywords_pathway = "Tricarboxylic acid cycle",
                          keywords_target = "AR;"),
          buildImgElement(src = "岩藻糖GDP代谢的不同空间途径",
                          journal = "Biotechnol Adv. 2023 Oct:67:108184.",
                          doi = "10.1016/j.biotechadv.2023.108184",
                          keywords_metabolite = "Mannose;Fucose",
                          keywords_pathway = "Tricarboxylic acid cycle"),
          buildImgElement(src = "促进肿瘤细胞生长的代谢通路变化",
                          journal = "Pharmacol Ther.2021 Aug:224:107827",
                          doi = "10.1016/j.pharmthera.2021.107827",
                          keywords_metabolite = "glucose;glycine;glutamine;Serine;lactic acid;fructose;acetic acid;Aspartic acid;Methionine;Homocysteine;S-Adenosylhomocysteine",
                          keywords_pathway = "Glycolysis reprogramming",
                          keywords_disease = "糖尿病;diabetes mellitus",
                          keywords_target = "PKM2;丙酮酸激酶的M2异构体;pyruvate kinase"),
          buildImgElement(src = "AMPK对葡萄糖摄取和脂肪生成的控制",
                          journal = "Trends Endocrinol Metab. 2023Nov;34(11):704-717",
                          doi = "10.1016/j.tem.2023.08.011",
                          keywords_metabolite = "glucose;lactic acid;Citric acid;Pyruvic acid"),
          buildImgElement(src = "AMPK和PKA对脂肪分解的控制",
                          journal = "Trends Endocrinol Metab. 2023Nov;34(11):704-717",
                          doi = "10.1016/j.tem.2023.08.011",
                          keywords_metabolite = "fatty acid")
        )
      )
    ),
    
    tags$div(
      class="fullscreen", id="fullscreen", style="display:none;",
      tags$div(
        style = "display:flex; width:100vw; height:100vh;",
        tags$div(
          style = "display:flex; flex:1",
          tags$div(
            style = "width:80%; display:flex; justify-content:center; align-items:center;",
            tags$img(src = ""),
          ),
          tags$div(
            style = "width:20%; background-color:#1c1d22; padding:15px;",
            tags$div(
              style = "display:flex; flex-direction:column; height:100%; justify-content:space-between;",
              tags$div(
                tags$div(
                  style = "display:flex; justify-content:flex-end; margin-bottom:20px;",
                  tags$span(icon("times"), class = "fullscreen_close_btn")
                ),
                tags$div(
                  style = "display:flex; flex-direction:column;",
                  tags$div(
                    class = "fullscreen-keywords-container",
                    tags$div(class = "fullscreen-keywords-container-border2"),
                    tags$div(id = "fullscreen-desc", class = "fullscreen-img-desc", ""),
                    tags$div(id = "fullscreen-journal", class = "fullscreen-img-desc", style = "color:#4D8055", ""),
                    tags$div(id = "fullscreen-doi", class = "fullscreen-img-desc", style = "color:#3c8dbc", "")
                  ),
                  tags$div(
                    style = "margin-top:40px;",
                    tags$div(
                      class = "fullscreen-keywords-container",
                      tags$div(class = "fullscreen-keywords-container-border1"),
                      tags$div(class = "fullscreen-keywords-category", "Metabolite"),
                      tags$div(id = "fullscreen-keywords-metabolite", "", class = "fullscreen-keywords")
                    ),
                    tags$div(
                      class = "fullscreen-keywords-container",
                      tags$div(class = "fullscreen-keywords-container-border1"),
                      tags$div(class = "fullscreen-keywords-category", "Pathway"),
                      tags$div(id = "fullscreen-keywords-pathway", "", class = "fullscreen-keywords")
                    ),
                    tags$div(
                      class = "fullscreen-keywords-container",
                      tags$div(class = "fullscreen-keywords-container-border1"),
                      tags$div(class = "fullscreen-keywords-category", "Disease"),
                      tags$div(id = "fullscreen-keywords-disease", "", class = "fullscreen-keywords")
                    ),
                    tags$div(
                      class = "fullscreen-keywords-container",
                      tags$div(class = "fullscreen-keywords-container-border1"),
                      tags$div(class = "fullscreen-keywords-category", "Target"),
                      tags$div(id = "fullscreen-keywords-target", "", class = "fullscreen-keywords")
                    )
                  )
                )
              ),
              tags$div(
                id = "fullscreen-download",
                class = "pathway_gallery_img_download_bigbtn",
                tags$span(icon("download"), style = "margin-right:5px;"),
                "Download"
              )
              
            )
            
          )
        )
      )
    ),
    
    hidden(div(id = ns("search-result"),
               withSpinner(uiOutput(outputId = ns("search_pathway_result")))
               )
           )
  )
  
}


pathwayGalleryServer <- function(input, output, session) {
  
  pathway_library <<- xlsx::read.xlsx("pathway_gallery/data/PathwayLibrary.xlsx", sheetIndex = 1)
  
  observeEvent(input$search_btn, {
    tryCatch({
      js$hideDiv("default-gallery")
      js$hideDiv("none-search-result")
      shinyjs::show(id = "search-result")
      
      search_input = str_trim(input$search_input)
      
      if (search_input != "") {
        input_history <<- c(search_input, input_history)
        # 去重
        input_history <<- unique(input_history)
        # 只保留最多10个
        if (length(input_history) > 10) {
          input_history <<- input_history[1:10]
        }
      }
      
      js$updateElementInnerHTML(id = 'pathway_gallery-search_history', 
                                innerHTML = paste0('<li>', input_history, '</li>', collapse = ""))
      
      inputs = unlist(str_split(search_input, pattern = "[;,\\s+]"))
      search_result_index = c()
      for (i in 1:nrow(pathway_library)) {
        keywords = unlist(str_split(pathway_library[["keywords"]][i], pattern = "[;,\\s+]"))
        find = FALSE
        # 全词匹配/字符串包含
        for (k in 1:length(inputs)) {
          if (inputs[k] %in% keywords || (TRUE %in% grepl(inputs[k], keywords)) || str_detect(pathway_library[["name"]][i], inputs[k])) {
            find = TRUE
          }
        }
        if (find == TRUE) {
          # print(pathway_library[["name"]][i])
          search_result_index = append(search_result_index, i)
        }
      }
      
      # 将搜索结果组装为3列格式
      if (length(search_result_index) > 0) {
        search_result_column1 = c()
        search_result_column2 = c()
        search_result_column3 = c()
        for (i in 1:length(search_result_index)) {
          if (i %% 3 == 1) {
            search_result_column1 = append(search_result_column1, search_result_index[i])
          } else if (i %% 3 == 2) {
            search_result_column2 = append(search_result_column2, search_result_index[i])
          } else {
            search_result_column3 = append(search_result_column3, search_result_index[i])
          }
        }
        
        output$search_pathway_result <- renderUI({
          buildGallery(search_result_column1, search_result_column2, search_result_column3)
        })
      } else {
        js$hideDiv("pathway_gallery-search-result")
        js$showDiv("none-search-result")
      }
    }, warning = function(w) {
      toastr_warning(title = "搜索引擎遇到错误", message = paste(e))
    }, error = function(e) {
      toastr_error(title = "搜索引擎遇到错误", message = paste(e))
    })
    
  })
  
}