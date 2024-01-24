# IPOS——生物信息数据分析系统

## 机器学习

功能点概述：

1. 随机森林算法特征选择，支持二分类及多分类，Y为连续性变量或分类变量；     
2. SVM算法特征选择，支持二分类及多分类，Y为连续性变量或分类变量；
3. 系统支持算法常用参数调节；
4. 算法完成特征权重计算后，权重分布以可视化图表方式展现，图表可导出；
5. 支持将特征选择结果以 excel 等形式导出；

## 回归拟合

功能点概述：

1. LASSO回归进行变量压缩；
2. Elastic-Net进行变量压缩；
3. 系统支持算法常用参数调节；
4. 算法完成变量压缩后，所选变量的权重系数以可视化图表方式展现，且图表可导出；
5. 支持将变量压缩结果以 excel 等形式导出；

## 差异分析

功能点概述：

1. T-test (p-value, FDR adjusted q-value, log2(Fold  change注意：数据是log变换后，因此FC为mean值之差), 0-log10(q value) 
2. ANOVA (p-value, FDR adjusted q-value)
3. 设置分组的选项卡
4. 差异标志物的 boxplot 图
5. 差异标志物的 windrose 图

## 质量控制

功能点概述：

1. 主成分分析（PCA）
2. 使用MetNormalizer校准（基于支持向量回归算法）
3. 校准后的数据继续进行主成分分析

4. ParScaling 等多种数据尺度化方式可选；
5. PCA 得分图 QC 样本与 Subject 样本以不同颜色标记出；
6. 支持将 PCA 得分图导出；
7. Log2 Transformed 可选；
8. PCA 得分图 
9. 主成分的均值±3标准差图
10. RSD 分布图
11. first QC + last QC 样本的 Spearman 相关系数分布图
12. BoxPlot / ViolinPlot