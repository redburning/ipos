source("global.R", encoding = "UTF-8")
source("help/help.R", encoding = "UTF-8")


# 箱线图尺寸
ttest_result_boxplot_width <- 600
ttest_result_boxplot_height <- 500
# 火山图尺寸
ttest_result_volcanoplot_width <- 600
ttest_result_volcanoplot_height <- 500
# 种类富集图尺寸
ttest_result_classenrichplot_width <- 800
ttest_result_classenrichplot_autoheight <- "auto"
# 环形玫瑰图尺寸
ttest_result_circularbarplot_width <- 1000
ttest_result_circularbarplot_height <- 1000
# z-score尺寸
ttest_result_zscoreplot_width <- 800
ttest_result_zscoreplot_height <- 1000
# anova boxplot尺寸
anova_result_boxplot_width <- 600
anova_result_boxplot_height <- 500


insertClassBoundary <- function(data) {
  colClass <- data[[3]]
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

drawCircularBarPlot <- function(dataset, ymin = -10, ymax = 10, pvalueFilter = FALSE, pmin = 0, pmax = 1, 
                                qvalueFilter = FALSE, qmin = 0, qmax = 1, fontsize = 3, 
                                showbaseline = TRUE, showclassboundary = FALSE, palettename, showlegend = FALSE) {
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
  
  plot <- ggplot(dataset, aes(x = as.factor(.data[["id"]]), y = .data[["value"]]))
  if (showbaseline) {
    plot <- plot + geom_hline(yintercept = 1, linetype = 2, color = "red")
  }
  plot <- plot + geom_bar(stat="identity", aes(fill = .data[["class"]])) +
    coord_polar() +
    geom_text(aes(x = .data[["id"]], 
                  y = ifelse(.data[["value"]] < 1.05, 1.1, .data[["value"]] + 0.1),
                  label = .data[["variable"]], 
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
  if (!is.null(ymin) & !is.null(ymax)) {
    plot <- plot + ylim(ymin, ymax)
  }
  return(plot)
}

drawClassEnrichPlot <- function(dataset, variable_class_fulllist, pValueFilter = TRUE, qValueFilter = FALSE, 
                                pmin = 0, pmax = 1, qmin = 0, qmax = 1, fontsize = 10, palette, showZeroCount = TRUE) {
  
  tryCatch({
    
    classenrich_df <- dataset
    if (!is.null(pValueFilter) && pValueFilter == TRUE) {
      classenrich_df <- subset(dataset, (dataset["P-value"] >= pmin) & (dataset["P-value"] <= pmax))
    }
    if (!is.null(qValueFilter) && qValueFilter == TRUE) {
      classenrich_df <- subset(dataset, (dataset["Q-value"] >= qmin) & (dataset["Q-value"] <= qmax))
    }
    # count for each class of full class set
    count <- c()
    classenrich_df_count <- NULL
    if (showZeroCount == TRUE) {
      for (c in as.character(unique(variable_class_fulllist))) {
        num <- 0
        for (i in (1:nrow(classenrich_df))) {
          if (as.character(classenrich_df[["class"]][i]) == c) {
            num = num + 1
          }
        }
        count <- append(count, num)
      }
      classenrich_df_count <- data.frame(class = as.character(unique(variable_class_fulllist)), count = count)
    } else {
      for (c in as.character(unique(classenrich_df[["class"]]))) {
        num <- 0
        for (i in (1:nrow(classenrich_df))) {
          if (as.character(classenrich_df[["class"]][i]) == c) {
            num = num + 1
          }
        }
        count <- append(count, num)
      }
      classenrich_df_count <- data.frame(class = as.character(unique(classenrich_df[["class"]])), count = count)
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
        for (i in (1:nrow(dataset))) {
          if (as.character(dataset[["class"]][i]) == c) {
            num = num + 1
          }
        }
        count_all <- append(count_all, num)
      }
      classenrich_df_ratio <- data.frame(class = as.character(unique(variable_class_fulllist)),
                                         ratio = count / count_all)
    } else {
      for (c in as.character(unique(classenrich_df[["class"]]))) {
        # 计算原来数据集每个类有多少个变量
        num <- 0
        for (i in (1:nrow(dataset))) {
          if (as.character(dataset[["class"]][i]) == c) {
            num = num + 1
          }
        }
        count_all <- append(count_all, num)
      }
      classenrich_df_ratio <- data.frame(class = as.character(unique(classenrich_df[["class"]])),
                                         ratio = count / count_all)
    }
    
    # sort desc
    classenrich_df_ratio <- classenrich_df_ratio[order(classenrich_df_ratio["ratio"]),]
    # variable column as factor to keep order
    classenrich_df_ratio[["class"]] <- factor(classenrich_df_ratio[["class"]], levels = as.character(classenrich_df_ratio[["class"]]))
    
    # plot
    plot_count <- ggplot(classenrich_df_count, aes(x = class, y = count)) + 
      geom_bar(stat="identity", fill = palette) + 
      coord_flip() +
      theme_classic() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = 'none') +
      labs(title = "number of differential metabolites", x = "", y = "number") +
      theme(axis.line.x = element_line(linetype = 1, color=palette, size = 1),
            axis.line.y = element_line(linetype = 1, color=palette, size = 1),
            axis.ticks.x = element_line(color=palette, size=1),
            axis.ticks.y = element_line(color=palette, size=1),
            axis.text.x = element_text(size = fontsize),
            axis.text.y = element_text(size = fontsize),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
      scale_y_continuous(expand = c(0.01, 0.01))
    
    plot_ratio <- ggplot(classenrich_df_ratio, aes(x = class, y = ratio)) + 
      geom_bar(stat="identity", fill=palette) + 
      coord_flip() +
      theme_classic() + 
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = 'none') +
      labs(title = "change ratio of differential metabolites", x = "", y = "changed metabolites/total of this category(%)") +
      theme(axis.line.x = element_line(linetype = 1, color=palette, size = 1),
            axis.line.y = element_line(linetype = 1, color=palette, size = 1),
            axis.ticks.x = element_line(color=palette, size=1),
            axis.ticks.y = element_line(color=palette, size=1),
            axis.text.x = element_text(size = fontsize), 
            axis.text.y = element_text(size = fontsize),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
      scale_y_continuous(limits = c(0, 1), expand = c(0.0001, 0.01), labels = percent)
    
    plot_matrix <- plot_grid(plot_count, plot_ratio) + theme(plot.margin = unit(c(0, 1, 1, 0), "cm"))
    return(plot_matrix)
  }, warning = function(w) {
    toastr_warning(title = "绘制种类统计图失败", message = paste(e))
  }, error = function(e) {
    toastr_error(title = "绘制种类统计图失败", message = paste(e))
  })
}

drawTtestBoxPlot <- function(data, group_test, group_control, logtransform = FALSE, variable, palettename, withpoint = FALSE) {
  tryCatch({
    # 是否log变换
    if (logtransform == TRUE) {
      for (i in 2:length(colnames(data))) {
        varname <- colnames(data)[i]
        for (j in 1:length(data[[varname]])) {
          data[[varname]][j] <- log2(data[[varname]][j])
        }
      }
    }
    
    # class 列转换为factor
    data[[1]] <- factor(data[[1]], levels = c(group_control, group_test))
    
    # 计算变量的ttest.pvalue, 作为图的副标题, 要先log变换, 后计算subtitle
    data <- subset(data, (data[1] == group_control) | (data[1] == group_test))
    data_test <- subset(data, data[1] == group_test)
    data_control <- subset(data, data[1] == group_control)
    ttest <- t.test(data_test[[variable]], data_control[[variable]], alternative = "two.sided", paired = FALSE, var.equal = TRUE)
    subtitle <- paste0(group_test, " v.s. ", group_control, ": ", format(ttest$p.value, scientific = TRUE))
    
    palette <- NULL
    n <- length(unique(data[[1]]))
    palette <- brewer.pal(ifelse(n < 3, 3, n), palettename)
    palette <- palette[1:n]
    
    plot <- ggplot(data, aes(x = data[[1]], y = .data[[variable]])) +
      stat_boxplot(geom = "errorbar", width = 0.2, colour = palette, lwd = 0.7)
    
    if (withpoint) {
      # setting color of scatter point
      points_color <- c()
      for (index in 1:length(data[[1]])) {
        if (data[[1]][index] == group_control) {
          points_color <- append(points_color, palette[1])
        } else if (data[[1]][index] == group_test) {
          points_color <- append(points_color, palette[2])
        }
      }
      # setting color of outlier white to hide
      plot <- plot + geom_boxplot(colour = palette, lwd = 0.7, width = 0.3, outlier.color = "white") +
        geom_point(size = 1, position = position_jitter(0.1), colour = points_color)
    } else {
      plot <- plot + geom_boxplot(colour = palette, lwd = 0.7, width = 0.3)
    }
    plot <- plot + labs(title = variable, subtitle = subtitle, x = "", y = "") +
      theme_classic() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(linetype = 1, color="black", size = 0.5),
            axis.line.y = element_line(linetype = 1, color="black", size = 0.5),
            axis.ticks.x = element_line(color="black", size = 0.5),
            axis.ticks.y = element_line(color="black", size = 0.5),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.margin = unit(c(1, 1, 1, 1), "cm"))
    return(plot)
  }, warning = function(w) {
    toastr_warning(title = "绘制箱线图失败", message = paste(e))
  }, error = function(e) {
    toastr_error(title = "绘制箱线图失败", message = paste(e))
  })
}


drawAnovaBoxPlot <- function(data, logtransform = FALSE, variable, palettename, withpoint = FALSE) {
  
  # 是否log变换
  if (logtransform == TRUE) {
    for (i in 2:length(colnames(data))) {
      varname <- colnames(data)[i]
      for (j in 1:length(data[[varname]])) {
        data[[varname]][j] <- log2(data[[varname]][j])
      }
    }
  }
  
  # 计算变量的anova.pvalue, 作为图的副标题, 先log变换, 后计算subtitle
  classcol <- colnames(data)[1]
  aov <- aov(as.formula(paste0("`", variable, "`",  "~", classcol)), data)
  df <- data.frame(unclass(summary(aov)), check.names = FALSE, stringsAsFactors = FALSE)
  pvalue <- df[["Pr(>F)"]][1]
  subtitle <- paste0("p-value: ", format(pvalue, scientific = TRUE))
  
  palette <- NULL
  n <- length(unique(data[[1]]))
  palette <- brewer.pal(ifelse(n < 3, 3, n), palettename)
  palette <- palette[1:n]
  
  plot <- ggplot(data, aes(x = data[[1]], y = .data[[variable]])) +
    stat_boxplot(geom = "errorbar", width = 0.2, colour = palette, lwd = 0.7)
  
  if (withpoint) {
    # setting color of scatter point
    points_color <- c()
    index = 1
    for (class in unique(data[[1]])) {
      points_color <- append(points_color, rep(palette[index], sum(data[[1]] == class)))
      index = index + 1
    }
    # setting color of outlier white to hide
    plot <- plot + geom_boxplot(colour = palette, lwd = 0.7, width = 0.3, outlier.color = "white") +
      geom_point(size = 1, position = position_jitter(0.1), colour = points_color)
  } else {
    plot <- plot + geom_boxplot(colour = palette, lwd = 0.7, width = 0.3)
  }
  plot <- plot + labs(title = variable, subtitle = subtitle, x = "", y = "") +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(linetype = 1, color="black", size = 0.5),
          axis.line.y = element_line(linetype = 1, color="black", size = 0.5),
          axis.ticks.x = element_line(color="black", size = 0.5),
          axis.ticks.y = element_line(color="black", size = 0.5),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = unit(c(1, 1, 1, 1), "cm"))
  return(plot)
}


drawVolcanoPlot <- function(data, setting) {
  
  # normalcolor <- "gray"
  # upcolor <- NULL
  # downcolor <- NULL
  # greencolor <- NULL
  # if (palette == "Set1") {
  #   upcolor = rgb(249, 64, 64, alpha = 30, max = 255)
  #   downcolor = rgb(85, 160, 251, alpha = 30, max = 255)
  #   greencolor = rgb(34, 139, 34, alpha = 30, max = 255)
  # } else if (palette == "Set2") {
  #   upcolor = rgb(220, 20, 60, alpha = 30, max = 255)
  #   downcolor = rgb(0, 255, 127, alpha = 30, max = 255)
  # } else if (palette == "Set3") {
  #   upcolor = rgb(255, 0, 0, alpha = 30, max = 255)
  #   downcolor = rgb(0, 191, 255, alpha = 30, max = 255)
  # }
  # 
  # ##对满足不同条件的数据给不同的标记，放入condition列，颜色放入color列
  # data$condition = ifelse(log2(data[[xFieldName]]) > 0.02 & data[[yFieldName]] < 0.05, "up", 
  #                         ifelse(log2(data[[xFieldName]]) < -0.02 & data[[yFieldName]] < 0.05, "up", 
  #                                ifelse(log2(data[[xFieldName]]) >= -0.02 & log2(data[[xFieldName]] <= 0.02) & data[[yFieldName]] < 0.05, "down",
  #                                       ifelse(log2(data[[xFieldName]]) < -0.02 & data[[yFieldName]] > 0.05, "green",
  #                                              ifelse(log2(data[[xFieldName]]) > 0.02 & data[[yFieldName]] > 0.05, "green", 
  #                                                     "normal")))))
  # 
  # xmin <- min(log2(data[[xFieldName]]))
  # xmax <- max(log2(data[[xFieldName]]))
  # ymin <- min(-log10(data[[yFieldName]]))
  # ymax <- max(-log10(data[[yFieldName]]))
  # 
  # plot <- ggplot(data = data, aes(x = log2(.data[[xFieldName]]), y = -log10(.data[[yFieldName]]), colour = condition)) + 
  #   geom_hline(yintercept = -log10(0.05), linetype = 2) + 
  #   geom_vline(xintercept = 0, linetype = 2) +
  #   geom_point(alpha = 0.8, size = 1.5) +
  #   scale_color_manual(values=c('up' = upcolor, 'down' = downcolor, 'normal' = normalcolor, 'green' = greencolor)) +
  #   theme_classic() + 
  #   theme(panel.grid.major.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.major.y = element_blank(),
  #         panel.grid.minor.y = element_blank(),
  #         panel.border = element_rect(fill = NA, color = "black"),
  #         axis.ticks.x = element_line(color = "black", size = 0.5),
  #         axis.ticks.y = element_line(color = "black", size = 0.5),
  #         plot.title = element_text(hjust = 0.5),
  #         plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  #   labs(x="Log2(Fold Change)", y=paste0("-Log10(", yFieldName, ")")) +
  #   scale_y_continuous(expand = c(0, 0)) +
  #   xlim(-max(abs(xmin), abs(xmax)) - (xmax - xmin) / 20, max(abs(xmin), abs(xmax)) + (xmax - xmin) / 20) +
  #   ylim(ymin - (ymax - ymin) / 20, ymax + (ymax - ymin) / 20)
  # 
  # # remove legend
  # if (showlegend == FALSE) {
  #   plot <- plot + theme(legend.position = 'none')
  # }
  
  
  xFieldName <- 'Log2(Fold Change)'
  yFieldName <- setting$yfieldname
  
  xmin <- min(data[[xFieldName]])
  xmax <- max(data[[xFieldName]])
  ymin <- min(-log10(data[[yFieldName]]))
  ymax <- max(-log10(data[[yFieldName]]))
  
  selectlab <- c(as.character(setting$selectlab))
  if (length(selectlab) > 0) {
    selectlab <- gsub("`", "", selectlab)
  } else {
    selectlab <- NULL
  }
  
  if (setting$legendposition == "none") {
    plot <- EnhancedVolcano(data,
                            title = setting$title,
                            subtitle = NULL,
                            caption = NULL,
                            lab = data[["variable"]],
                            x = xFieldName,
                            y = yFieldName,
                            pCutoff = setting$pcutoff, 
                            FCcutoff = log2(setting$fccutoff), 
                            border = "full", 
                            legendPosition = setting$legendposition,
                            legendIconSize = -1,
                            legendLabels = NULL,
                            pointSize = setting$pointsize,
                            colAlpha = setting$colalpha,
                            gridlines.major = FALSE,
                            gridlines.minor = FALSE,
                            labSize = setting$labelsize,
                            col = setting$colpalette,
                            drawConnectors = as.logical(setting$drawconnector),
                            widthConnectors = setting$connectorwidth,
                            selectLab = selectlab,
                            boxedLabels = as.logical(setting$boxedlabels),
                            ylab = paste0("-Log10(", yFieldName, ")"),
                            xlim = c(-max(abs(xmin), abs(xmax)) - (xmax - xmin) / 20, max(abs(xmin), abs(xmax)) + (xmax - xmin) / 20),
                            ylim = c(ymin - (ymax - ymin) / 20, ymax + (ymax - ymin) / 20))
  } else {
    plot <- EnhancedVolcano(data,
                            title = setting$title,
                            subtitle = NULL,
                            caption = NULL,
                            lab = data[["variable"]],
                            x = xFieldName,
                            y = yFieldName,
                            pCutoff = setting$pcutoff, 
                            FCcutoff = log2(setting$fccutoff), 
                            border = "full", 
                            legendPosition = setting$legendposition,
                            pointSize = setting$pointsize,
                            colAlpha = setting$colalpha,
                            gridlines.major = FALSE,
                            gridlines.minor = FALSE,
                            labSize = setting$labelsize,
                            col = setting$colpalette,
                            drawConnectors = as.logical(setting$drawconnector),
                            widthConnectors = setting$connectorwidth,
                            selectLab = selectlab,
                            boxedLabels = as.logical(setting$boxedlabels),
                            ylab = paste0("-Log10(", yFieldName, ")"),
                            xlim = c(-max(abs(xmin), abs(xmax)) - (xmax - xmin) / 20, max(abs(xmin), abs(xmax)) + (xmax - xmin) / 20),
                            ylim = c(ymin - (ymax - ymin) / 20, ymax + (ymax - ymin) / 20))
  }
  
  return(plot)
}


drawTtestZScorePlot <- function(data, ttest_result, group_control, group_test, logtransform = FALSE, pValueFilter, qValueFilter, 
                                pmin = 0, pmax = 1, qmin = 0, qmax = 1, palette = c("mediumseagreen", "firebrick"), plot_width, plot_height) {
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
    # 自适应更新zscore-plot的高度
    if (is.na(plot_height)) {
      ttest_result_zscoreplot_height <<- nrow(ttest_result) * 14 + 40
      updateNumericInput(inputId = "ttest_result_zscoreplot_height", value = ttest_result_zscoreplot_height)
    } else {
      ttest_result_zscoreplot_height <<- plot_height
    }
    if (is.na(plot_width)) {
      updateNumericInput(inputId = "ttest_result_zscoreplot_width", value = ttest_result_zscoreplot_width)
    } else {
      ttest_result_zscoreplot_width <<- plot_width
    }
    
    # data 根据所选特征筛选子集
    data = data[1:nrow(data), c(colnames(data)[1], ttest_result[["variable"]])]
    
    # 筛选出控制组和测试组
    data_control = subset(data, data[[1]] == group_control)
    data_test = subset(data, data[[1]] == group_test)
    
    # zscore
    for (i in 2:length(colnames(data))) {
      varname = colnames(data)[i]
      data_control_new = (data_control[[varname]] - mean(data_control[[varname]])) / sd(data_control[[varname]])
      data_test_new = (data_test[[varname]] - mean(data_control[[varname]])) / sd(data_control[[varname]])
      data_control[[varname]] = data_control_new
      data_test[[varname]] = data_test_new
    }
    
    # 将zscore结果组合
    data_control_test <- data.frame(group = c(data_control[[1]], data_test[[1]]))
    for (i in 2:length(colnames(data))) {
      varname = colnames(data)[i]
      data_control_test[[varname]] = c(data_control[[varname]], data_test[[varname]])
    }
    
    # 中位数
    m = c()
    for (i in 2:ncol(data_control_test)) { m = append(m, median(data_control_test[[i]])) }
    # 中位数的秩
    r = rank(m)
    
    variable = c()
    value = c()
    group = c()
    count = 0
    
    for (i in 2:ncol(data_control_test)) {
      for (j in 1:length(r)) {
        # 因为两个数据相差一列group
        if (r[j] == i - 1) {
          varname = colnames(data_control_test)[j + 1]
          variable = append(variable, rep(varname, nrow(data_control_test)))
          value = append(value, data_control_test[[varname]])
          group = append(group, data_control_test[[1]])
          break;
        }
      }
    }
    df <- data.frame(variable = variable, value = value, group = group)
    df[["variable"]] <- factor(df[["variable"]], levels = rev(unique(df[["variable"]])))
    
    group_control_name = paste0("`", group_control, "`")
    group_test_name = paste0("`", group_test, "`")
    plot <- ggplot(data = df, aes(x = variable, y = value, color = group)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = 2) +
      coord_flip() +
      scale_color_manual(values=palette) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(x = "", y = "Z-Score")
    return (plot)
  }
}


ttest <- function(data, mapping, classA, classB, logtransform) {
  # 保留一份原始数据备份, foldchange计算需要用到
  dataorig <- data
  if (logtransform == TRUE) {
    for (i in 2:length(colnames(data))) {
      varname <- colnames(data)[i]
      for (j in 1:length(data[[varname]])) {
        data[[varname]][j] <- log2(data[[varname]][j])
      }
    }
  }
  variable <- colnames(data)[2: length(data)]
  data_a <- subset(data, data[1] == classA)
  data_b <- subset(data, data[1] == classB)
  dataorig_a <- subset(dataorig, dataorig[1] == classA)
  dataorig_b <- subset(dataorig, dataorig[1] == classB)
  pvalue <- c()
  foldchange <- c()
  for(i in 2:length(colnames(data))) {
    varname <- colnames(data)[i]
    ttest <- t.test(data_a[[varname]], data_b[[varname]], alternative = "two.sided", paired = FALSE, var.equal = TRUE)
    pvalue <- append(pvalue, ttest$p.value)
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
  colnames(df) <- c("variable", "P-value", "Q-value", "Log10(P-value)", "Log10(Q-value)", "Fold Change", "Log2(Fold Change)")
  
  # replace with original feature name
  for (i in 1:length(df[["variable"]])) {
    df[["variable"]][i] <- as.character(mapping[[df[["variable"]][i]]][1])
  }
  
  return(df)
}


anova <- function(data, mapping) {
  variable <- c()
  pvalue <- c()
  class <- colnames(data)[1]
  for(i in 2:length(colnames(data))) {
    # replace with original feature name
    var <- colnames(data)[i]
    aov <- aov(as.formula(paste0(var, "~", class)), data)
    df <- data.frame(unclass(summary(aov)), check.names = FALSE, stringsAsFactors = FALSE)
    variable <- append(variable, as.character(mapping[[var]][1]))
    pvalue <- append(pvalue, df[["Pr(>F)"]][1])
  }
  res <- data.frame(variable = variable, 
                    pvalue = signif(pvalue, digits = 4), 
                    qvalue = signif(p.adjust(pvalue, method = "BH"), digits = 4))
  
  return(res)
}


differenceAnalysisUI <- function(id) {
  ns <- NS(id)
  
  # uploaded schedule or not
  uploadschedule <- FALSE
  
  fluidPage(
    useToastr(),
    extendShinyjs(text = jscode, functions = c("collapse")),
    
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico"),
              tags$style(type="text/css", ".checkbox {margin-top: 20px;"),
              tags$base(target = "_blank")
    ),
    
    
    fluidRow(
      box(
        # title = "Upload Data",
        title = "上传数据",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        # fluidRow(column(width = 2, offset = 10, helpButton(ns("help_upload")))),
        fluidRow(column(width = 2, offset = 10, actionButton(inputId = ns("help_upload"),
                                                             label = "帮助文档",
                                                             icon = icon("book"),
                                                             class = "help-button")
        )
        ),
        fluidRow(
          column(width = 7,
                 fileInput(
                   inputId = ns("dataloader"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " 上传数据... "),
                   # placeholder = "Click the button to select a file, or directly drag and drop the file here.",
                   placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                   accept = ".csv"
                 )
          ),
          column(width = 5,
                 fileInput(
                   inputId = ns("attachmentloader"),
                   label = "",
                   buttonLabel = div(icon("folder-open"), " 上传附表数据... "),
                   # placeholder = "Click the button to select a file, or directly drag and drop the file here.",
                   placeholder = "点击按钮选择文件, 或拖拽文件至此。",
                   accept = ".csv"
                 )
          )
        ),
        fluidRow(
          column(width = 12, uiOutput(ns("data_summary")))
        ),
        # fluidRow(
        #   column(width = 12,
        #          fileInput(
        #            inputId = ns("attachmentloader"),
        #            label = "",
        #            buttonLabel = div(icon("folder-open"), " 上传附表数据... "),
        #            # placeholder = "Click the button to select a file, or directly drag and drop the file here.",
        #            placeholder = "点击按钮选择文件, 或拖拽文件至此。",
        #            accept = ".csv"
        #          )
        #   )
        # ),
      ),
      box(
        id = "box-da",
        # title = "Difference Analysis",
        title = "差异分析",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        fluidRow(column(width = 2, offset = 10, helpButton(ns("help_analysis")))),
        fluidRow(
          sidebarPanel(
            width = 2,
            fluid  = TRUE,
            selectInput(inputId = ns("select_difference_model"),
                        # label = "Difference model",
                        label = "差异分析方法",
                        choices = c("t-test" = "ttest", "ANOVA" = "anova")),
            br(),
            conditionalPanel(
              condition = "input.select_difference_model == 'ttest'",
              actionButton(ns("ttest_setting"),
                           label = "参数设置",
                           icon = icon("sliders"),
                           class = "setting-button"),
              ns = ns
            ),
            actionButton(ns("execute"),
                         # label = "Execute",
                         label = "执行",
                         icon = icon("play"),
                         class = "setting-button"),
          ),
          conditionalPanel(
            condition = "input.select_difference_model == 'ttest'",
            mainPanel(
              useShinyjs(),
              width = 10,
              tabsetPanel(
                tabPanel(title = "参数",
                         icon = icon("sliders"),
                         hidden(div(id = ns("ttestResultParams"),
                                    withSpinner(DT::DTOutput(outputId = ns("ttest_result_params"), height = "400px"))
                         ))
                ),
                tabPanel(title = "数据",
                         icon = icon("table"),
                         hidden(div(id = ns("ttestResultTable"),
                                    withSpinner(DT::DTOutput(outputId = ns("ttest_result_table"))),
                                    column(width = 2, offset = 10, actionButton(inputId = ns("export_ttest_result_table"),
                                                                                class = "download-button",
                                                                                # label = "download data",
                                                                                label = "下载数据",
                                                                                icon = icon("download")))
                         ))
                ),
                tabPanel(title = "箱线图",
                         icon = tags$i(class = "iconfont icon-boxplot", role="presentation"),
                         hidden(div(id = ns("ttestResultBoxplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("ttest_result_boxplot"), width = "auto", height = "auto"))
                                      ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 12, uiOutput(outputId = ns("feature_of_ttest_boxplot"))),
                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId = ns("ttest_result_boxplot_width"),
                                                                         label = "宽度",
                                                                         value = ttest_result_boxplot_width)
                                                     ),
                                                     column(width = 6,
                                                            numericInput(inputId = ns("ttest_result_boxplot_height"),
                                                                         label = "高度",
                                                                         value = ttest_result_boxplot_height)
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 12, checkboxInput(inputId = ns("withpoint_of_ttest_boxplot"), label = "Scatter point", value = FALSE))
                                                   )
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 12, selectInput(inputId = ns("palette_of_ttest_boxplot"),
                                                                                    label = "palette",
                                                                                    choices = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent"))),
                                                     
                                                   ),
                                               )
                                             )
                                      )
                                    ),
                                    
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_ttest_result_boxplot"),
                                                                                  class = "download-button",
                                                                                  # label = "download chart",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download"))
                                      ),
                                    )
                         ))
                ),
                tabPanel(title = "火山图",
                         icon = tags$i(class = "iconfont icon-volcano", role="presentation"),
                         hidden(div(id = ns("ttestResultVolcanoplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("ttest_result_volcanoplot"), width = "auto", height = "auto")),
                                             # withSpinner(plotlyOutput(outputId = ns("ttest_result_volcanoplotly"), width = "100%", height = "100%")),
                                      ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 12, textInput(inputId = ns("title_of_volcanoplot"),
                                                                                  label = "标题",
                                                                                  value = "Volcano plot")
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 12, selectInput(inputId = ns("yfieldname_of_volcanoplot"),
                                                                                    label = "y field",
                                                                                    choices = c("P-value", "Q-value"),
                                                                                    selected = "P-value")
                                                     ),
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, numericInput(inputId = ns("pcutoff_of_volcanoplot"),
                                                                                    label = "P-Value cutoff",
                                                                                    value = 0.05)
                                                     ),
                                                     column(width = 6, numericInput(inputId = ns("fccutoff_of_volcanoplot"),
                                                                                    label = "Fold-Change cutoff",
                                                                                    value = 1)
                                                     ),
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, numericInput(inputId = ns("pointsize_of_volcanoplot"),
                                                                                    label = "Point 大小",
                                                                                    value = 3,
                                                                                    min = 1)
                                                     ),
                                                     column(width = 6, selectInput(inputId = ns("legendposition_of_volcanoplot"),
                                                                                   label = "图例位置",
                                                                                   choices = c("bottom", "top", "right", "left", "none"),
                                                                                   selected = "bottom")
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, numericInput(inputId = ns("ttest_result_volcanoplot_width"),
                                                                                    label = "宽度",
                                                                                    value = ttest_result_volcanoplot_width)
                                                     ),
                                                     column(width = 6, numericInput(inputId = ns("ttest_result_volcanoplot_height"),
                                                                                    label = "高度",
                                                                                    value = ttest_result_volcanoplot_height)
                                                     )
                                                   )
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 6,
                                                            colourInput(inputId = ns("palette1_of_volcanoplot"),
                                                                        label = "调色板-1",
                                                                        value = "grey30",
                                                                        returnName = TRUE,
                                                                        palette = "limited",
                                                                        closeOnClick = TRUE,
                                                                        allowedCols = c(ColorBrewr$custom)
                                                            ),
                                                     ),
                                                     column(width = 6,
                                                            colourInput(inputId = ns("palette2_of_volcanoplot"),
                                                                        label = "调色板-2",
                                                                        value = "forestgreen",
                                                                        returnName = TRUE,
                                                                        palette = "limited",
                                                                        closeOnClick = TRUE,
                                                                        allowedCols = c(ColorBrewr$custom)
                                                            ),
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            colourInput(inputId = ns("palette3_of_volcanoplot"),
                                                                        label = "调色板-3",
                                                                        value = "royalblue",
                                                                        returnName = TRUE,
                                                                        palette = "limited",
                                                                        closeOnClick = TRUE,
                                                                        allowedCols = c(ColorBrewr$custom)
                                                            ),
                                                     ),
                                                     column(width = 6,
                                                            colourInput(inputId = ns("palette4_of_volcanoplot"),
                                                                        label = "调色板-4",
                                                                        value = "red2",
                                                                        returnName = TRUE,
                                                                        palette = "limited",
                                                                        closeOnClick = TRUE,
                                                                        allowedCols = c(ColorBrewr$custom)
                                                            ),
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, numericInput(inputId = ns("colalpha_of_volcanoplot"),
                                                                                    label = "颜色透明度",
                                                                                    value = 1,
                                                                                    max = 1,
                                                                                    min = 0,
                                                                                    step = 0.1))
                                                   )
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "Label 设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 6, numericInput(inputId = ns("labelsize_of_volcanoplot"),
                                                                                    label = "Label 字体大小",
                                                                                    value = 5,
                                                                                    min = 1)
                                                     ),
                                                     column(width = 6, selectInput(inputId = ns("boxedlabels_of_volcanoplot"),
                                                                                   label = "Label 边框",
                                                                                   choices = c("否" = FALSE, "是" = TRUE))
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 6, selectInput(inputId = ns("drawconnector_of_volcanoplot"),
                                                                                   label = "显示连接线",
                                                                                   choices = c("否" = FALSE, "是" = TRUE))
                                                     ),
                                                     column(width = 6, numericInput(inputId = ns("connectorwidth_of_volcanoplot"),
                                                                                    label = "连接线宽",
                                                                                    value = 1,
                                                                                    max = 1,
                                                                                    min = 0,
                                                                                    step = 0.1)
                                                     ),
                                                   ),
                                                   fluidRow(
                                                     column(width = 12, uiOutput(outputId = ns("varselect_of_volcanoplot"))),
                                                   )
                                               )
                                             )
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_ttest_result_volcanoplot"),
                                                                                  class = "download-button",
                                                                                  # label = "download chart",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")))
                                    )
                         ))
                ),
                tabPanel(title = "环形玫瑰图",
                         icon = tags$i(class = "iconfont icon-nandinggeermeiguitu", role="presentation"),
                         hidden(div(id = ns("ttestResultCircularbarplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("ttest_result_circularbarplot"),
                                                                    width = paste0(ttest_result_circularbarplot_width, "px"),
                                                                    height = paste0(ttest_result_circularbarplot_height, "px")))),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 10,
                                                            sliderInput(inputId = ns("pvalue_of_circularbarplot"), label = "P-value",
                                                                        min = 0, max = 0.1, step = 0.01, value = c(0, 0.1))),
                                                     column(width = 2,
                                                            checkboxInput(inputId = ns("select_pvalue_of_circularbarplot"), label = "",
                                                                          value = TRUE)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 10,
                                                            sliderInput(inputId = ns("qvalue_of_circularbarplot"), label = "Q-value",
                                                                        min = 0, max = 0.1, step = 0.01, value = c(0, 0.1))),
                                                     column(width = 2,
                                                            checkboxInput(inputId = ns("select_qvalue_of_circularbarplot"), label = "",
                                                                          value = FALSE)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 8,
                                                            numericInput(inputId = ns("ymin_of_circularbarplot"), label = "min ylimit",
                                                                         value = -10, step = 0.1)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 8,
                                                            numericInput(inputId = ns("ymax_of_circularbarplot"), label = "max ylimit",
                                                                         value = 10, step = 0.1)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 8,
                                                            numericInput(inputId = ns("fontsize_of_circularbarplot"),
                                                                         label = "字号",
                                                                         value = 3)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 8,
                                                            checkboxInput(inputId = ns("showlegend_of_circularbarplot"),
                                                                          label = "显示图例",
                                                                          value = FALSE)),
                                                   )
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 8,
                                                            selectInput(inputId = ns("palette_of_circularbarplot"),
                                                                        label = "调色板",
                                                                        choices = c("Palette 1", "Palette 2", "Palette 3", "Palette 4", "Palette 5",
                                                                                    "Palette 6", "Palette 7", "Palette 8", "Palette 9"))),
                                                   )
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "高级设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 8,
                                                            checkboxInput(inputId = ns("showclassboundary_of_circularbarplot"),
                                                                          label = "显示类别边界",
                                                                          value = FALSE)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 8,
                                                            checkboxInput(inputId = ns("showbaseline_of_circularbarplot"),
                                                                          label = "显示基线",
                                                                          value = TRUE)),
                                                   ),
                                               )
                                             ),
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_ttest_result_circularbarplot"),
                                                                                  class = "download-button",
                                                                                  # label = "download chart",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")))
                                    ),
                         ))
                ),
                tabPanel(title = "种类统计图",
                         icon = tags$i(class = "iconfont icon-jiyinfuji", role="presentation"),
                         hidden(div(id = ns("ttestResultClassenrichplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("ttest_result_classenrichplot"),
                                                                    width = paste0(ttest_result_classenrichplot_width, "px"),
                                                                    height = ttest_result_classenrichplot_autoheight)),
                                             # withSpinner(plotlyOutput(outputId = ns("ttest_result_classenrichplotly"), width = "100%", height = "100%")),
                                      ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class="plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   uiOutput(outputId = ns("ui_pvalue_of_classenrichplot")),
                                                   uiOutput(outputId = ns("ui_qvalue_of_classenrichplot")),
                                                   fluidRow(
                                                     column(width = 8,
                                                            numericInput(inputId = ns("fontsize_of_classenrichplot"),
                                                                         label = "font size",
                                                                         value = 10)),
                                                   ),
                                                   fluidRow(
                                                     column(width = 8,
                                                            checkboxInput(inputId = ns("show_zerocount_of_classenrichplot"), label = "show all classes", value = FALSE))
                                                   )
                                               ),
                                               box(title = "颜色设置",collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 8,
                                                            # selectInput(inputId = ns("palette_of_classenrichplot"),
                                                            #             label = "调色板",
                                                            #             choices = ColorBrewr$custom),
                                                            colourInput(inputId = ns("palette_of_classenrichplot"),
                                                                        label = "调色板",
                                                                        value = "steelblue1",
                                                                        returnName = TRUE,
                                                                        palette = "limited",
                                                                        closeOnClick = TRUE,
                                                                        allowedCols = c(ColorBrewr$custom)
                                                            )
                                                     ),
                                                   )
                                               )
                                             )
                                      ),
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_ttest_result_classenrichplot"),
                                                                                  class = "download-button",
                                                                                  # label = "download chart",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")))
                                    ),
                         ))
                ),
                tabPanel(title = "Z-Score图",
                         icon = tags$i(class = "iconfont icon-a-geometricscaling", role="presentation"),
                         hidden(div(id = ns("ttestResultZScoreplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotlyOutput(outputId = ns("ttest_result_zscoreplot"), height = "100%", width = "100%")),
                                      ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class="plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   uiOutput(outputId = ns("ui_pvalue_of_zscoreplot")),
                                                   uiOutput(outputId = ns("ui_qvalue_of_zscoreplot")),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId = ns("ttest_result_zscoreplot_width"),
                                                                         label = "宽度",
                                                                         value = NA)
                                                     ),
                                                     column(width = 6,
                                                            numericInput(inputId = ns("ttest_result_zscoreplot_height"),
                                                                         label = "高度",
                                                                         value = NA)
                                                     )
                                                   )
                                               ),
                                             ),
                                             tags$div(
                                               class="plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 6,
                                                            colourInput(inputId = ns("palette1_of_zscoreplot"),
                                                                        label = "调色板",
                                                                        value = "mediumseagreen",
                                                                        returnName = TRUE,
                                                                        palette = "limited",
                                                                        closeOnClick = TRUE,
                                                                        allowedCols = c(ColorBrewr$custom)
                                                            )
                                                     ),
                                                     column(width = 6,
                                                            div(class="color-input-without-label",
                                                                colourInput(inputId = ns("palette2_of_zscoreplot"),
                                                                            label = "",
                                                                            value = "firebrick",
                                                                            returnName = TRUE,
                                                                            palette = "limited",
                                                                            closeOnClick = TRUE,
                                                                            allowedCols = c(ColorBrewr$custom)
                                                                            )
                                                                )
                                                            )
                                                   )
                                               )
                                             )
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_ttest_result_zscoreplot"),
                                                                                  class = "download-button",
                                                                                  # label = "download chart",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download")))
                                    )
                         ))
                )
              )
            ),
            ns = ns
          ),
          conditionalPanel(
            condition = "input.select_difference_model == 'anova'",
            mainPanel(
              useShinyjs(),
              width = 10,
              tabsetPanel(
                tabPanel(title = "数据", icon = icon("table"),
                         hidden(div(id = ns("anovaResultTable"),
                                    fluidRow(
                                      withSpinner(DT::DTOutput(outputId = ns("anova_result_table"))),
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_anova_result_table"),
                                                                                  class = "download-button",
                                                                                  # label = "download data",
                                                                                  label = "下载数据",
                                                                                  icon = icon("download")))
                                    )
                         )
                         )
                ),
                tabPanel(title = "箱线图",
                         hidden(div(id = ns("anovaResultBoxplot"),
                                    fluidRow(
                                      br(),
                                      column(width = 9,
                                             withSpinner(plotOutput(outputId = ns("anova_result_boxplot"), width = "auto", height = "auto")),
                                      ),
                                      column(width = 3,
                                             class = "plot-setting-column",
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "基础设置", collapsible = TRUE, collapsed = TRUE, width = 12,
                                                   fluidRow(
                                                     column(width = 12,
                                                            uiOutput(outputId = ns("feature_of_anova_boxplot"))
                                                     ),
                                                   ),
                                                   fluidRow(
                                                     column(width = 6,
                                                            numericInput(inputId = ns("anova_result_boxplot_width"),
                                                                         label = "宽度",
                                                                         value = anova_result_boxplot_width)
                                                     ),
                                                     column(width = 6,
                                                            numericInput(inputId = ns("anova_result_boxplot_height"),
                                                                         label = "高度",
                                                                         value = anova_result_boxplot_height)
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(width = 12,
                                                            checkboxInput(inputId = ns("withpoint_of_anova_boxplot"), label = "Scatter point", value = FALSE)
                                                     )
                                                   ),
                                               )
                                             ),
                                             tags$div(
                                               class = "plot-setting",
                                               box(title = "颜色设置", collapsible = TRUE, collapsed = FALSE, width = 12,
                                                   fluidRow(
                                                     column(width = 12, selectInput(inputId = ns("palette_of_anova_boxplot"),
                                                                                    label = "palette",
                                                                                    choices = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent")))
                                                   )
                                               )
                                             ),
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 2, offset = 10, actionButton(inputId = ns("export_anova_result_boxplot"),
                                                                                  class = "download-button",
                                                                                  label = "下载图表",
                                                                                  icon = icon("download"))),
                                    )
                         ))
                )
              )
            ),
            ns = ns
          )
        )
      )
    )
  )
}


differenceAnalysisServer <- function(input, output, session) {
  
  ns <- session$ns
  dataset <- NULL
  origdataset <- NULL
  dataset_attachment <- NULL
  dataset_stringasfactors_false <- NULL
  mapping <- NULL
  classes <- c()
  ttest_result_params <- NULL
  ttest_result_table <- NULL
  ttest_group_test <- NULL
  ttest_group_control <- NULL
  ttest_result_boxplot <- NULL
  ttest_result_volcanoplot <- NULL
  ttest_result_circularbarplot <- NULL
  ttest_result_classenrichplot <- NULL
  anova_result_table <- NULL
  anova_result_boxplot <- NULL
  ttest_result_zscoreplot <- NULL
  logtransform <- FALSE
  classenrich_data <- NULL
  
  observeEvent(input$dataloader$datapath, {
    # dataset with original column names
    origdataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
    dataset <<- read.csv(input$dataloader$datapath, stringsAsFactors = TRUE)
    dataset_stringasfactors_false <<- read.csv(input$dataloader$datapath, stringsAsFactors = FALSE)
    
    mapping <<- data.frame(matrix(ncol = ncol(dataset), nrow = 0), check.names = FALSE)
    mapping <<- rbind(mapping, colnames(origdataset))
    colnames(mapping) <<- colnames(dataset)
    classes <<- levels(unique(dataset[[1]]))
    ttest_group_control <<- classes[1]
    ttest_group_test <<- classes[2]
    
    output$data_summary <- renderUI({
      p("样本数: ", nrow(origdataset),
        "变量数: ", ncol(origdataset) - 1,
        # style = STYLES$p_info
      )
    })
    
    # 自动展开
    js$collapse("box-da")
    
    output$feature_of_ttest_boxplot <- renderUI({
      selectInput(inputId = ns("feature_to_draw_boxplot"), label = "select feature", 
                  choices = colnames(origdataset)[2:ncol(origdataset)],
                  selected = colnames(origdataset)[2])
    })
    
    output$feature_of_anova_boxplot <- renderUI({
      selectInput(inputId = ns("feature_to_draw_anova_boxplot"), label = "select feature", 
                  choices = colnames(origdataset)[2:ncol(origdataset)],
                  selected = colnames(origdataset)[2])
    })
    
  })
  
  observeEvent(input$attachmentloader$datapath, {
    # dataset with original column names
    tryCatch({
      attachment <- read.csv(input$attachmentloader$datapath, stringsAsFactors = TRUE, check.names = FALSE)
      if (nrow(attachment) > 0 && ncol(attachment) == 2) {
        dataset_attachment <<- attachment
      } else {
        toastr_warning(message = paste0("附表数据应该仅有两列，第一列为变量名称，第二列为变量所属类别，您上传的数据有", ncol(attachment), "列，请检查！"), 
                       title = "请检查附表数据格式")
      }
    }, error = function(e) {
      print(paste(e))
    })
  })
  
  observeEvent(input$ttest_setting, {
    showModal(modalDialog(
      title = "分组设置",
      size = "l",
      fluidPage(
        div(id = ns(paste("ttest_group_setting")),
            fluidRow(
              column(width = 5,
                     selectInput(inputId = ns("ttest_group_control"), label = "control group", choices = classes, selected = ttest_group_control)),
              column(width = 5,
                     selectInput(inputId = ns("ttest_group_test"), label = "test group", choices = classes, selected = ttest_group_test))
            )
        ),
        checkboxInput(inputId = ns("logtransform"), label = "Log2 transform", value = logtransform),
      ),
      easyClose = FALSE,
      fade = FALSE,
      footer = tagList(
        actionButton(inputId = ns("ttest_setting_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  observeEvent(input$ttest_setting_ok, {
    ttest_group_test <<- input$ttest_group_test
    ttest_group_control <<- input$ttest_group_control
    logtransform <<- input$logtransform
    removeModal()
  })
  
  # 环形玫瑰图的p-value/q-value选择控制
  observeEvent(input$select_pvalue_of_circularbarplot, {
    if(input$select_pvalue_of_circularbarplot == FALSE) {
      shinyjs::disable("pvalue_of_circularbarplot")
    } else {
      shinyjs::enable("pvalue_of_circularbarplot")
    }
  })
  
  observeEvent(input$select_qvalue_of_circularbarplot, {
    if(input$select_qvalue_of_circularbarplot == FALSE) {
      shinyjs::disable("qvalue_of_circularbarplot")
    } else {
      shinyjs::enable("qvalue_of_circularbarplot")
    }
  })
  
  # 种类富集图的p-value/q-value选择控制
  observeEvent(input$select_pvalue_of_classenrichplot, {
    if(input$select_pvalue_of_classenrichplot == FALSE) {
      shinyjs::disable("pvalue_of_classenrichplot")
    } else {
      shinyjs::enable("pvalue_of_classenrichplot")
    }
  })
  
  observeEvent(input$select_qvalue_of_classenrichplot, {
    if(input$select_qvalue_of_classenrichplot == FALSE) {
      shinyjs::disable("qvalue_of_classenrichplot")
    } else {
      shinyjs::enable("qvalue_of_classenrichplot")
    }
  })
  
  # z-score的p-value/q-value选择控制
  observeEvent(input$select_pvalue_of_zscoreplot, {
    if(input$select_pvalue_of_zscoreplot == FALSE) {
      shinyjs::disable("pvalue_of_zscoreplot")
    } else {
      shinyjs::enable("pvalue_of_zscoreplot")
    }
  })
  
  observeEvent(input$select_qvalue_of_zscoreplot, {
    if(input$select_qvalue_of_zscoreplot == FALSE) {
      shinyjs::disable("qvalue_of_zscoreplot")
    } else {
      shinyjs::enable("qvalue_of_zscoreplot")
    }
  })
  
  observeEvent(input$execute, {
    tryCatch({
      if (is.null(dataset)) {
        toastr_warning(title = "请上传数据")
      } else if (is.null(ttest_group_control) || trimws(ttest_group_control) == '') {
        toastr_warning(title = "参数设置有误", message = "未选择控制组")
      } else if (is.null(ttest_group_test) || trimws(ttest_group_test) == '') {
        toastr_warning(title = "参数设置有误", message = "未选择测试组")
      } else {
        if (input$select_difference_model == "ttest") {
          shinyjs::show("ttestResultParams")
          shinyjs::show("ttestResultTable")
          shinyjs::show("ttestResultBoxplot")
          shinyjs::show("ttestResultVolcanoplot")
          shinyjs::show("ttestResultZScoreplot")
          
          # t-test
          ttest_result <- ttest(dataset, mapping, ttest_group_test, ttest_group_control, logtransform)
          ttest_result_table <<- ttest_result
          
          ttest_result_params <<- data.frame(param = c("Group setting", "Log2 transform"), 
                                             value = c(paste0(ttest_group_test, "  v.s.  ", ttest_group_control), logtransform),
                                             stringsAsFactors = FALSE)
          # 将原始数据集(stringasfactor=FALSE)和ttest处理结果rbind组成最终结果
          # ttest_result_table <<- dataset_stringasfactors_false
          # ttest_result_table <<- rbind(ttest_result_table, 
          #                              c("P-value", ttest_result[["P-value"]]), 
          #                              c("Q-value", ttest_result[["Q-value"]]),
          #                              c("-Log10(P-value)", ttest_result[["-Log10(P-value)"]]),
          #                              c("-Log10(Q-value)", ttest_result[["-Log10(Q-value)"]]),
          #                              c("Fold Change", ttest_result[["Fold Change"]]),
          #                              c("Log2(Fold Change)", ttest_result[["Log2(Fold Change)"]]))
          
          # t-test reuslt params
          output$ttest_result_params <- DT::renderDT({
            DT::datatable({
              ttest_result_params
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
            selection = 'multiple',
            style = 'bootstrap',
            class = 'cell-border stripe compact',
            rownames = FALSE
            )
          })
          
          # t-test result table
          output$ttest_result_table <- DT::renderDT({
            DT::datatable({
              ttest_result_table
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
                           scrollY = "400px",
                           searching = FALSE,
                           paging = FALSE
            ),
            selection = 'multiple',
            style = 'bootstrap',
            class = 'cell-border stripe compact',
            rownames = FALSE
            )
          })
          
          # t-test result boxplot
          output$ttest_result_boxplot <- renderPlot({
            if (!is.null(input$feature_to_draw_boxplot) && !is.null(ttest_group_test) && !is.null(ttest_group_control)) {
              ttest_result_boxplot <<- drawTtestBoxPlot(origdataset, ttest_group_test, ttest_group_control, logtransform,
                                                        input$feature_to_draw_boxplot, input$palette_of_ttest_boxplot, input$withpoint_of_ttest_boxplot)
            }
            return(ttest_result_boxplot)
          }, height = function() {
            ttest_result_boxplot_height <<- input$ttest_result_boxplot_height
            return(ttest_result_boxplot_height)
          }, width = function() {
            ttest_result_boxplot_width <<- input$ttest_result_boxplot_width
            return(ttest_result_boxplot_width)
          })
          
          
          # t-test result volcanoplot
          df_varselect_choice <- as.data.frame(t(ttest_result[["variable"]]))
          colnames(df_varselect_choice) <- ttest_result[["variable"]]
          output$varselect_of_volcanoplot <- renderUI({
            varSelectInput(inputId = ns("vardrawconnector_of_volcanoplot"),
                           label = "选择显示label的变量",
                           data = df_varselect_choice,
                           multiple = TRUE)
          })
          
          output$ttest_result_volcanoplot <- renderPlot({
            setting <- list(
              'title' = input$title_of_volcanoplot,
              'pcutoff' = input$pcutoff_of_volcanoplot,
              'yfieldname' = input$yfieldname_of_volcanoplot,
              'fccutoff' = input$fccutoff_of_volcanoplot,
              'pointsize' = input$pointsize_of_volcanoplot,
              'legendposition' = input$legendposition_of_volcanoplot,
              'labelsize' = input$labelsize_of_volcanoplot,
              'colalpha' = input$colalpha_of_volcanoplot,
              'colpalette' = c(input$palette1_of_volcanoplot, input$palette2_of_volcanoplot, input$palette3_of_volcanoplot, input$palette4_of_volcanoplot),
              'drawconnector' = input$drawconnector_of_volcanoplot,
              'connectorwidth' = input$connectorwidth_of_volcanoplot,
              'selectlab' = input$vardrawconnector_of_volcanoplot,
              'boxedlabels' = input$boxedlabels_of_volcanoplot
            )
            ttest_result_volcanoplot <<- drawVolcanoPlot(ttest_result, setting)
            return(ttest_result_volcanoplot)
          }, width = function() {
            ttest_result_volcanoplot_width <<- input$ttest_result_volcanoplot_width
            return(ttest_result_volcanoplot_width)
          }, height = function() {
            ttest_result_volcanoplot_height <<- input$ttest_result_volcanoplot_height
            return(ttest_result_volcanoplot_height)
          })
          
          # EnhancedVolcano目前还不支持绘制plotly
          # output$ttest_result_volcanoplotly <- renderPlotly({
          #   setting <- list(
          #     'title' = input$title_of_volcanoplot,
          #     'pcutoff' = input$pcutoff_of_volcanoplot,
          #     'yfieldname' = input$yfieldname_of_volcanoplot,
          #     'fccutoff' = input$fccutoff_of_volcanoplot,
          #     'pointsize' = input$pointsize_of_volcanoplot,
          #     'legendposition' = input$legendposition_of_volcanoplot,
          #     'labelsize' = input$labelsize_of_volcanoplot,
          #     'colalpha' = input$colalpha_of_volcanoplot,
          #     'colpalette' = c(input$palette1_of_volcanoplot, input$palette2_of_volcanoplot, input$palette3_of_volcanoplot, input$palette4_of_volcanoplot),
          #     'drawconnector' = input$drawconnector_of_volcanoplot,
          #     'connectorwidth' = input$connectorwidth_of_volcanoplot,
          #     'selectlab' = input$vardrawconnector_of_volcanoplot,
          #     'boxedlabels' = input$boxedlabels_of_volcanoplot
          #   )
          #   ttest_result_volcanoplot <<- drawVolcanoPlot(ttest_result, setting)
          #   return(ggplotly(ttest_result_volcanoplot))
          # })
          
          
          # t-test result circular barplot
          if (!is.null((dataset_attachment))) {
            # output$ylimit_of_circularbarplot <- renderUI(
            #   sliderInput(inputId = ns("ylimit_of_circularbarplot"), label = "ylimit", min = -10, max = 10, step = 0.2, value = c(-10, 10))
            # )
            # output$pvalue_of_circularbarplot <- renderUI(
            #   sliderInput(inputId = ns("pvalue_of_circularbarplot"), label = "P-value", 
            #               min = min(ttest_result["P-value"]), max = max(ttest_result["Q-value"]), 
            #               step = 0.01, value = c(min(ttest_result["P-value"]), max(ttest_result["Q-value"])))
            # )
            # output$qvalue_of_circularbarplot <- renderUI(
            #   sliderInput(inputId = ns("qvalue_of_circularbarplot"), label = "Q-value", 
            #               min = min(ttest_result["Q-value"]), max = max(ttest_result["Q-value"]), 
            #               step = 0.01, value = c(min(ttest_result["Q-value"]), max = max(ttest_result["Q-value"])))
            # )
            
            data <- ttest_result
            # if (!is.null(input$pvalue_of_circularbarplot[1]) && !is.null(input$pvalue_of_circularbarplot[2])) {
            #   data <- subset(data, (data["P-value"] >= input$pvalue_of_circularbarplot[1]) & (data["P-value"] <= input$pvalue_of_circularbarplot[2]))
            # }
            # if (!is.null(input$qvalue_of_circularbarplot[1]) && !is.null(input$qvalue_of_circularbarplot[2])) {
            #   data <- subset(data, (data["Q-value"] >= input$qvalue_of_circularbarplot[1]) & (data["Q-value"] <= input$qvalue_of_circularbarplot[2]))
            # }
            data <- data[, c("variable", "Fold Change", "P-value", "Q-value")]
            
            variable_class_mapping <- data.frame(matrix(ncol = nrow(data), nrow = 0), check.names = FALSE)
            variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment[[2]]))
            colnames(variable_class_mapping) <- dataset_attachment[[1]]
            variable_class <- c()
            # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
            for (v in data[["variable"]]) {
              if (v %in% colnames(variable_class_mapping)) {
                variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
              } else {
                toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
                variable_class <- append(variable_class, "unknown")
              }
            }
            data["class"] = variable_class
            
            # 将变量类别列组合到结果中
            colnames(data) <- c("variable", "value", "P-value", "Q-value", "class")
            
            shinyjs::show("ttestResultCircularbarplot")
            output$ttest_result_circularbarplot <- renderPlot({
              ttest_result_circularbarplot <<- drawCircularBarPlot(dataset = data, 
                                                                   ymin = input$ymin_of_circularbarplot,
                                                                   ymax = input$ymax_of_circularbarplot,
                                                                   pvalueFilter = input$select_pvalue_of_circularbarplot,
                                                                   qvalueFilter = input$select_qvalue_of_circularbarplot,
                                                                   pmin = input$pvalue_of_circularbarplot[1],
                                                                   pmax = input$pvalue_of_circularbarplot[2],
                                                                   qmin = input$qvalue_of_circularbarplot[1],
                                                                   qmax = input$qvalue_of_circularbarplot[2],
                                                                   fontsize = input$fontsize_of_circularbarplot, 
                                                                   showbaseline = input$showbaseline_of_circularbarplot,
                                                                   showclassboundary = input$showclassboundary_of_circularbarplot,
                                                                   palettename = input$palette_of_circularbarplot,
                                                                   showlegend = input$showlegend_of_circularbarplot)
              return(ttest_result_circularbarplot)
            })
          } else {
            toastr_warning(message = "环形玫瑰图需要上传附表数据", title = "请上传附表")
          }
          
          # 种类富集图
          if (!is.null((dataset_attachment))) {
            # prepare data
            classenrich_data <- ttest_result[, c("variable", "P-value", "Q-value")]
            variable_class_mapping <- data.frame(matrix(ncol = nrow(classenrich_data), nrow = 0), check.names = FALSE)
            variable_class_mapping <- rbind(variable_class_mapping, as.character(dataset_attachment[[2]]))
            colnames(variable_class_mapping) <- dataset_attachment[[1]]
            # 变量种类全集
            variable_class_fulllist <- dataset_attachment[[2]]
            variable_class <- c()
            # 对每个变量, 找到其相应类别, 如果未找到统一设置为unknown
            for (v in classenrich_data[["variable"]]) {
              if (v %in% colnames(variable_class_mapping)) {
                variable_class <- append(variable_class, as.character(variable_class_mapping[[v]][1]))
              } else {
                toastr_warning(message = paste0("变量 ", v, " 类别信息未指定"), title = "变量类别未指定")
                variable_class <- append(variable_class, "unknown")
              }
            }
            classenrich_data["class"] = variable_class
            
            # 将变量类别列组合到结果中
            colnames(classenrich_data) <- c("variable", "P-value", "Q-value", "class")
            
            # 渲染p-value, q-value选择控件
            shinyjs::show("ttestResultClassenrichplot")
            # 自动更新pvalue的min, max
            output$ui_pvalue_of_classenrichplot <- renderUI({
              min_pvalue <- floor(min(classenrich_data[["P-value"]]) * 100) / 100
              max_pvalue <- ceiling(max(classenrich_data[["P-value"]]) * 100) / 100
              fluidRow(
                column(width = 10,
                       sliderInput(inputId = ns("pvalue_of_classenrichplot"), label = "P-value",
                                   min = min_pvalue, max = max_pvalue, step = 0.01, value = c(min_pvalue, max_pvalue))
                ),
                column(width = 2,
                       checkboxInput(inputId = ns("select_pvalue_of_classenrichplot"), label = "",
                                     value = FALSE)
                ),
              )
            })
            output$ui_qvalue_of_classenrichplot <- renderUI({
              min_qvalue <- floor(min(classenrich_data[["Q-value"]]) * 100) / 100
              max_qvalue <- ceiling(max(classenrich_data[["Q-value"]]) * 100) / 100
              fluidRow(
                column(width = 10,
                       sliderInput(inputId = ns("qvalue_of_classenrichplot"), label = "Q-value",
                                   min = min_qvalue, max = max_qvalue, step = 0.01, value = c(min_qvalue, max_qvalue))
                ),
                column(width = 2,
                       checkboxInput(inputId = ns("select_qvalue_of_classenrichplot"), label = "",
                                     value = FALSE)
                ),
              )
            })
            output$ttest_result_classenrichplot <- renderPlot({
              ttest_result_classenrichplot <<- drawClassEnrichPlot(dataset = classenrich_data,
                                                                   variable_class_fulllist,
                                                                   pValueFilter = input$select_pvalue_of_classenrichplot,
                                                                   qValueFilter = input$select_qvalue_of_classenrichplot,
                                                                   pmin = input$pvalue_of_classenrichplot[1],
                                                                   pmax = input$pvalue_of_classenrichplot[2],
                                                                   qmin = input$qvalue_of_classenrichplot[1],
                                                                   qmax = input$qvalue_of_classenrichplot[2],
                                                                   fontsize = input$fontsize_of_classenrichplot,
                                                                   palette = input$palette_of_classenrichplot,
                                                                   showZeroCount = input$show_zerocount_of_classenrichplot)
              return(ttest_result_classenrichplot)
            }, height = function() {
                if (input$show_zerocount_of_classenrichplot == TRUE) {
                  ttest_result_classenrichplot_autoheight <<- 18 * length(unique(variable_class_fulllist)) + 100
                  return(ttest_result_classenrichplot_autoheight)
                } else {
                  if (input$select_pvalue_of_classenrichplot == TRUE && input$select_qvalue_of_classenrichplot == FALSE) {
                    temp_df <- subset(classenrich_data, (classenrich_data["P-value"] >= input$pvalue_of_classenrichplot[1])
                                      & (classenrich_data["P-value"] <= input$pvalue_of_classenrichplot[2]))
                    ttest_result_classenrichplot_autoheight <<- 18 * length(unique(temp_df[["class"]])) + 100
                  } else if (input$select_pvalue_of_classenrichplot == FALSE && input$select_qvalue_of_classenrichplot == TRUE) {
                    temp_df <- subset(classenrich_data, (classenrich_data["Q-value"] >= input$qvalue_of_classenrichplot[1])
                                      & (classenrich_data["Q-value"] <= input$qvalue_of_classenrichplot[2]))
                    ttest_result_classenrichplot_autoheight <<- 18 * length(unique(temp_df[["class"]])) + 100
                  } else if (input$select_pvalue_of_classenrichplot == TRUE && input$select_qvalue_of_classenrichplot == TRUE) {
                    temp_df <- subset(classenrich_data, (classenrich_data["P-value"] >= input$pvalue_of_classenrichplot[1])
                                      & (classenrich_data["P-value"] <= input$pvalue_of_classenrichplot[2])
                                      & (classenrich_data["Q-value"] >= input$qvalue_of_classenrichplot[1])
                                      & (classenrich_data["Q-value"] <= input$qvalue_of_classenrichplot[2])
                    )
                    ttest_result_classenrichplot_autoheight <<- 18 * length(unique(temp_df[["class"]])) + 100
                  } else if (input$select_pvalue_of_classenrichplot == FALSE && input$select_qvalue_of_classenrichplot == FALSE) {
                    ttest_result_classenrichplot_autoheight <<- 18 * length(unique(variable_class_fulllist)) + 100
                  }
                  return(ttest_result_classenrichplot_autoheight)
                }
              }
            )
          } else {
            toastr_warning(message = "种类富集图需要上传附表数据", title = "请上传附表")
          }
          
          # z-score图
          # 自动更新pvalue的min, max
          output$ui_pvalue_of_zscoreplot <- renderUI({
            min_pvalue <- floor(min(ttest_result[["P-value"]]) * 100) / 100
            max_pvalue <- ceiling(max(ttest_result[["P-value"]]) * 100) / 100
            fluidRow(
              column(width = 10,
                     sliderInput(inputId = ns("pvalue_of_zscoreplot"), label = "P-value",
                                 min = min_pvalue, max = max_pvalue, step = 0.01, value = c(min_pvalue, max_pvalue))
              ),
              column(width = 2,
                     checkboxInput(inputId = ns("select_pvalue_of_zscoreplot"), label = "",
                                   value = FALSE)
              ),
            )
          })
          
          output$ui_qvalue_of_zscoreplot <- renderUI({
            min_qvalue <- floor(min(ttest_result[["Q-value"]]) * 100) / 100
            max_qvalue <- ceiling(max(ttest_result[["Q-value"]]) * 100) / 100
            fluidRow(
              column(width = 10,
                     sliderInput(inputId = ns("qvalue_of_zscoreplot"), label = "Q-value",
                                 min = min_qvalue, max = max_qvalue, step = 0.01, value = c(min_qvalue, max_qvalue))
              ),
              column(width = 2,
                     checkboxInput(inputId = ns("select_qvalue_of_zscoreplot"), label = "",
                                   value = FALSE)
              )
            )
          })
          
          output$ttest_result_zscoreplot <- renderPlotly({
            if (!is.null(ttest_group_test) && !is.null(ttest_group_control)) {
              ttest_result_zscoreplot <<- drawTtestZScorePlot(origdataset, 
                                                              ttest_result,
                                                              ttest_group_control, 
                                                              ttest_group_test, 
                                                              logtransform, 
                                                              pValueFilter = input$select_pvalue_of_zscoreplot,
                                                              qValueFilter = input$select_qvalue_of_zscoreplot,
                                                              pmin = input$pvalue_of_zscoreplot[1],
                                                              pmax = input$pvalue_of_zscoreplot[2],
                                                              qmin = input$qvalue_of_zscoreplot[1],
                                                              qmax = input$qvalue_of_zscoreplot[2],
                                                              palette = c(input$palette1_of_zscoreplot, input$palette2_of_zscoreplot),
                                                              plot_height = input$ttest_result_zscoreplot_height,
                                                              plot_width = input$ttest_result_zscoreplot_width)
            }
            ttest_result_zscoreplotly <- config(ggplotly(ttest_result_zscoreplot), displayModeBar = FALSE) %>% layout(width = ttest_result_zscoreplot_width, height = ttest_result_zscoreplot_height)
            return(ttest_result_zscoreplotly)
          })
          
        } else if (input$select_difference_model == "anova") {
          shinyjs::show("anovaResultTable")
          shinyjs::show("anovaResultBoxplot")
          
          anova_result_table <<- anova(dataset, mapping)
          
          # anova result table
          output$anova_result_table <- DT::renderDT({
            DT::datatable({
              anova_result_table
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
                           scrollY = "400px",
                           searching = FALSE,
                           paging = FALSE
            ),
            selection = 'multiple',
            style = 'bootstrap',
            class = 'cell-border stripe compact',
            rownames = FALSE
            )
          })
          
          # anova box plot
          output$anova_result_boxplot <- renderPlot({
            if (!is.null(input$feature_to_draw_anova_boxplot)) {
              anova_result_boxplot <<- drawAnovaBoxPlot(origdataset, logtransform, input$feature_to_draw_anova_boxplot, 
                                                        input$palette_of_anova_boxplot, input$withpoint_of_anova_boxplot)
            }
            return(anova_result_boxplot)
          }, width = function() {
            anova_result_boxplot_width <<- input$anova_result_boxplot_width
            return(anova_result_boxplot_width)
          }, height = function() {
            anova_result_boxplot_height <<- input$anova_result_boxplot_height
            return(anova_result_boxplot_height)
          })
        }
      }
    }, error = function(e) {
      print(paste(e))
      toastr_error(title = "运行时遇到错误")
    })
  })
  
  
  observeEvent(input$export_ttest_result_table, {
    showModal(modalDialog(
      title = "下载数据",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_ttest_result_table_name"), label = "文件名称", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_ttest_result_table_format"), label = "选择格式", choices = c(".csv", ".xlsx")),
        checkboxInput(inputId = ns("export_ttest_result_table_with_originaldata"), label = "附加原始数据", value = FALSE)
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_ttest_result_table_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  output$export_ttest_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_ttest_result_table_name) || input$export_ttest_result_table_name == "") {
        paste("ttest-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_ttest_result_table_format, sep="")
      } else {
        paste(input$export_ttest_result_table_name, input$export_ttest_result_table_format, sep = "")
      }
    },
    content = function(file) {
      result_table <- rbind(dataset_stringasfactors_false,
                            c("P-value", ttest_result_table[["P-value"]]),
                            c("Q-value", ttest_result_table[["Q-value"]]),
                            c("Log10(P-value)", ttest_result_table[["Log10(P-value)"]]),
                            c("Log10(Q-value)", ttest_result_table[["Log10(Q-value)"]]),
                            c("Fold Change", ttest_result_table[["Fold Change"]]),
                            c("Log2(Fold Change)", ttest_result_table[["Log2(Fold Change)"]]))
      if (input$export_ttest_result_table_format == ".csv") {
        if (input$export_ttest_result_table_with_originaldata == TRUE) {
          write.csv(result_table, file, row.names = FALSE)
        } else {
          write.csv(ttest_result_table, file, row.names = FALSE)
        }
      } else if (input$export_ttest_result_table_format == ".xlsx") {
        if (input$export_ttest_result_table_with_originaldata == TRUE) {
          write.xlsx(result_table, file, row.names = FALSE)
        } else {
          write.xlsx(ttest_result_table, file, row.names = FALSE)
        }
      }
      removeModal()
    }
  )
  
  observeEvent(input$export_ttest_result_boxplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_ttest_result_boxplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_ttest_result_boxplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_ttest_result_boxplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_ttest_result_boxplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_ttest_result_boxplot_name) || input$export_ttest_result_boxplot_name == "") {
        paste("boxplot-", input$feature_to_draw_boxplot, "-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_ttest_result_boxplot_format, sep="")
      } else {
        paste(input$export_ttest_result_boxplot_name, input$export_ttest_result_boxplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, 
             plot = ttest_result_boxplot,
             width = input$ttest_result_boxplot_width / dpi,
             height = input$ttest_result_boxplot_height / dpi,
             dpi = dpi)
      removeModal()
    },
    contentType = "application/zip"
  )
  
  
  # 导出火山图
  observeEvent(input$export_ttest_result_volcanoplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_ttest_result_volcanoplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_ttest_result_volcanoplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_ttest_result_volcanoplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_ttest_result_volcanoplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_ttest_result_volcanoplot_name) || input$export_ttest_result_volcanoplot_name == "") {
        paste("volcanoplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_ttest_result_volcanoplot_format, sep="")
      } else {
        paste(input$export_ttest_result_volcanoplot_name, input$export_ttest_result_volcanoplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = ttest_result_volcanoplot, 
             width = ttest_result_volcanoplot_width / plot_size_fold, 
             height = ttest_result_volcanoplot_height / plot_size_fold, 
             units = "mm")
      removeModal()
    }
  )
  
  # 导出环形柱状图
  observeEvent(input$export_ttest_result_circularbarplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_ttest_result_circularbarplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_ttest_result_circularbarplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_ttest_result_circularbarplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_ttest_result_circularbarplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_ttest_result_circularbarplot_name) || input$export_ttest_result_circularbarplot_name == "") {
        paste("windrose-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_ttest_result_circularbarplot_format, sep="")
      } else {
        paste(input$export_ttest_result_circularbarplot_name, input$export_ttest_result_circularbarplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, plot = ttest_result_circularbarplot,
             width = ttest_result_circularbarplot_width / plot_size_fold,
             height = ttest_result_circularbarplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  # 导出种类富集图
  observeEvent(input$export_ttest_result_classenrichplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_ttest_result_classenrichplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_ttest_result_classenrichplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_ttest_result_classenrichplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_ttest_result_classenrichplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_ttest_result_classenrichplot_name) || input$export_ttest_result_classenrichplot_name == "") {
        paste("classenrich-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_ttest_result_classenrichplot_format, sep="")
      } else {
        paste(input$export_ttest_result_classenrichplot_name, input$export_ttest_result_classenrichplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = ttest_result_classenrichplot,
             width = ttest_result_classenrichplot_width / plot_size_fold,
             height = ttest_result_classenrichplot_autoheight / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  # zscore plotly的宽高度设置
  observe({
    output$ttest_result_zscoreplot <- renderPlotly({
      ttest_result_zscoreplotly <- ggplotly(ttest_result_zscoreplot) %>% layout(width = input$ttest_result_zscoreplot_width, height = input$ttest_result_zscoreplot_height) %>% config(list(displayModeBar = FALSE))
      ttest_result_zscoreplot_height <<- input$ttest_result_zscoreplot_height
      ttest_result_zscoreplot_width <<- input$ttest_result_zscoreplot_width
      return(ttest_result_zscoreplotly)
    })
  })
  
  # 导出zscore图
  observeEvent(input$export_ttest_result_zscoreplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_ttest_result_zscoreplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_ttest_result_zscoreplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_ttest_result_zscoreplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_ttest_result_zscoreplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_ttest_result_zscoreplot_name) || input$export_ttest_result_zscoreplot_name == "") {
        paste("zscore-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_ttest_result_zscoreplot_format, sep="")
      } else {
        paste(input$export_ttest_result_zscoreplot_name, input$export_ttest_result_zscoreplot_format, sep = "")
      }
    },
    content = function(file) {
      dpi <- 96
      ggsave(filename = file, 
             plot = ttest_result_zscoreplot,
             width = input$ttest_result_zscoreplot_width / dpi,
             height = input$ttest_result_zscoreplot_height / dpi,
             dpi = dpi)
      removeModal()
    }
  )
  
  # 导出anova计算结果
  observeEvent(input$export_anova_result_table, {
    showModal(modalDialog(
      title = "下载数据",
      size = "m",
      fluidPage(
        textInput(inputId = ns("export_anova_result_table_name"), label = "文件名称", 
                  placeholder = "Enter the file name may help you find the file easily...",
                  width = "400px"),
        selectInput(inputId = ns("export_anova_result_table_format"), label = "选择格式", choices = c(".csv", ".xlsx"))
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        downloadButton(outputId = ns("export_anova_result_table_ok"), label = "确认", icon = NULL),
        modalButton(label = "取消")
      )
    ))
  })
  
  output$export_anova_result_table_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_anova_result_table_name) || input$export_anova_result_table_name == "") {
        paste("anova-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_anova_result_table_format, sep="")
      } else {
        paste(input$export_anova_result_table_name, input$export_anova_result_table_format, sep = "")
      }
    },
    content = function(file) {
      if (input$export_anova_result_table_format == ".csv") {
        write.csv(anova_result_table, file, row.names = FALSE)
      } else if (input$export_anova_result_table_format == ".xlsx") {
        write.xlsx(anova_result_table, file, row.names = FALSE)
      }
      removeModal()
    }
  )
  
  # 导出anova boxplot
  observeEvent(input$export_anova_result_boxplot, {
    showModal(tags$div(
      id = "1", modalDialog(
        # title = "download chart",
        title = "下载图表",
        size = "m",
        fluidPage(
          textInput(inputId = ns("export_anova_result_boxplot_name"), label = "文件名称", 
                    placeholder = "Enter the file name may help you find the file easily...",
                    width = "400px"),
          selectInput(inputId = ns("export_anova_result_boxplot_format"), label = "选择格式", choices = c(".jpg", ".png", ".tiff", ".pdf")),
        ),
        easyClose = TRUE,
        fade = FALSE,
        footer = tagList(
          downloadButton(outputId = ns("export_anova_result_boxplot_ok"), label = "确认", icon = NULL),
          modalButton(label = "取消")
        )
      ))
    )
  })
  
  output$export_anova_result_boxplot_ok <- downloadHandler(
    filename = function() {
      if (is.null(input$export_anova_result_boxplot_name) || input$export_anova_result_boxplot_name == "") {
        paste("boxplot-", format(Sys.time(), "%Y%m%d%H%M%S"), input$export_anova_result_boxplot_format, sep="")
      } else {
        paste(input$export_anova_result_boxplot_name, input$export_anova_result_boxplot_format, sep = "")
      }
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = anova_result_boxplot,
             width = anova_result_boxplot_width / plot_size_fold,
             height = anova_result_boxplot_height / plot_size_fold,
             units = "mm")
      removeModal()
    }
  )
  
  
  # 帮助文档
  observeEvent(input$help_upload, {
    showModal(modalDialog(
      title = "上传数据",
      size = "l",
      fluidPage(
        includeMarkdown(file.path(getwd(), "help/DifferenceAnalysis_UploadData.md")),
        fluidRow(column(width = 2,
                        downloadButton(outputId = ns("download_sampledata_main"), 
                                       label = "下载分类数据", icon = icon("download"), 
                                       style = STYLES$help_download_sampledata_button)),
                 column(width = 2,
                        downloadButton(outputId = ns("download_sampledata_attachment"), 
                                       label = "下载附表数据", icon = icon("download"), 
                                       style = STYLES$help_download_sampledata_button))
        ),
      ),
      easyClose = TRUE,
      fade = FALSE,
      footer = tagList(
        modalButton(label = "确认")
      ),
      style="max-height:500px; overflow:auto;"
    ))
  })
  
  output$download_sampledata_main <- downloadHandler(
    filename = "differenceanalysis_main.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_main.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  output$download_sampledata_attachment <- downloadHandler(
    filename = "differenceanalysis_attachment.csv",
    content = function(file) {
      sampledata <- read.csv("help/data/da_attachment.csv", check.names = FALSE)
      write.csv(sampledata, file, row.names = FALSE)
    }
  )
  
  callModule(helpServer, "help_analysis", title = "差异分析", size = "l", file = "help/DifferenceAnalysis.md")
  
}

shinyApp(ui = differenceAnalysisUI(id = "difference_analysis"), server = differenceAnalysisServer)