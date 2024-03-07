![](./www/ipos-logo.png)

腾讯云试用版地址：http://82.157.20.231:3838/ipos

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

## 多元降维

### OPLS

模块 | 功能 | 详细
-- | -- | --
多元降维分析-OPLS | Data Preprocessing | 1. Log2   transformation    <br> 2. Log10 transformation    <br> 3. Centering scaling    <br> 4. Unit Variance scaling    <br> 5. Pareto scaling    <br> 6. Range scaling    <br> 7. Vast scaling    <br> 8. Level scaling    <br> 9. 支持数据预处理结果的查看与下载
多元降维分析-OPLS | Score Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   Score Plot Data，支持数据导出**     <br>    **支持图中数据点的填充颜色组合、填充颜色透明度、边框颜色、大小、形状的设置**    <br> 2. Confidence settings     <br>    **支持为 所有样本/每类样本 生成置信区间椭圆**     <br>    **支持置信区间椭圆的显示/隐藏、线型、粗细、线条颜色、填充颜色、填充透明度**     <br>    支持置信度自由设定，默认 0.95 置信度    <br> 3. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 4. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 5. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 6. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 7. Size settings     <br>    支持图片宽度、高度的设置    <br> 8. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | Loading Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   Loading Plot Data，支持数据导出**     <br>    **支持图中数据点的颜色组合、颜色透明度、大小、形状的设置**     <br>    支持图中   h-line、v-line 的显示/隐藏、基准值、线型、粗细、颜色的设置    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | Bio Plot | 1. Data   settings     <br>    **以 Drawer 方式展示 Bio Plot Data，支持数据导出**     <br>    **支持图中不同组数据点的填充颜色、颜色透明度、大小、边框颜色的设置**     <br>    支持图中 h-line、v-line 的显示/隐藏、基准值、线型、粗细、颜色的设置    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | VIP-Loilipop Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   VIP-Loilipop Plot Data，支持数据导出**     <br>    **支持设置   VIP 阈值，过滤显示图中的数据**     <br>    **支持依据代谢物名称、VIP   值升序/降序排列图中代谢物顺序**     <br>    **支持为图中不同类别代谢物对应的数据点设置不同颜色组合**     <br>    **支持为图中所有代谢物对应的数据点设置为统一颜色、支持填充颜色、透明度，边框颜色、透明度的自由设定**     <br>    支持设置图中数据点的大小     <br>    **支持为图中不同类别代谢物对应的线段设置不同颜色组合**     <br>    **支持为图中所有代谢物对应的线段设置为统一颜色、支持线条颜色、透明度的自由设定**     <br>    支持设置图中线段的宽度    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | VIP-Bubble Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   VIP-Bubble PlotData，支持数据导出**     <br>    **支持设置 VIP 阈值，过滤显示图中的数据**     <br>    **x 轴基准值为 vip 值， y 轴基准值支持   P-Value/Q-Value 的切换，数据点大小基准值为 abs(Log2(Fold change))**     <br>    **P-Value 的计算方式支持   T-test/Wilcox-test 的切换**     <br>    **支持为图中不同类别代谢物对应的数据点设置不同颜色组合**     <br>    支持图中数据点大小、填充色、透明度的设置     <br>    **支持显示/隐藏代谢物名称**     <br>    支持图中   h-line、v-line 的显示/隐藏、基准值、线型、粗细、颜色的设置    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | S-Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   S-Plot Data，支持数据导出**     <br>    **支持为图中不同类别代谢物对应的数据点设置不同填充颜色组合**     <br>    **支持根据 VIP 阈值区间设置代谢物的不同填充颜色**     <br>    **预置 9 种颜色组合，并支持自定义RGB颜色组合**     <br>    支持数据点大小、填充透明度的设置     <br>    支持图中   h-line、v-line 的显示/隐藏、基准值、线型、粗细、颜色的设置    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | V-Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   V-Plot Data，支持数据导出**     <br>    **支持根据 VIP 阈值区间设置代谢物的不同填充颜色**     <br>    支持数据点大小、填充透明度的设置     <br>    **支持代谢物名称的显示/隐藏**     <br>    支持图中   h-line、v-line 的显示/隐藏、基准值、线型、粗细、颜色的设置    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
多元降维分析-OPLS | Permutation Plot | 1. Data   settings     <br>    **以 Drawer 方式展示   Permutation Plot Data，支持数据导出**     <br>    支持图中所有数据点的填充颜色、填充透明度、大小、形状的设置     <br>    支持图中所有线的线型、粗细、颜色的设置    <br> 2. Title settings     <br>    支持 title 的文本、字体、字号、显示位置的设置    <br> 3. Panel settings     <br>    支持 Panel 的背景色、透明度、网格线的设置    <br> 4. Axis settings     <br>    支持x轴/y轴的文本、字体、字号的设置    <br> 5. Legend settings     <br>    支持 legend 的显示/隐藏、显示位置的设置    <br> 6. Size settings     <br>    支持图片宽度、高度的设置    <br> 7. Download     <br>    支持图片导出为 png、tiff、jpg、pdf 格式
