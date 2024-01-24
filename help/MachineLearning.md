##### 模型参数说明

1. 随机森林

   `Number of trees to grow`：随机森林中决策树的数量

2. svm-rfe

   `Fold of cross-validation`：交叉验证的折数

   svm-rfe 在计算特征权重时内嵌了 `交叉验证`，最终给出的特征排序即是特征在交叉验证过程中的平均 rank；

   `Kernel` 核函数类型

   `Cost` linear 型核函数的惩罚因子

   

##### 参考文献

- svm-rfe 的源码来自 [https://github.com/johncolby/SVM-RFE](https://github.com/johncolby/SVM-RFE)
- R 数据可视化：水平渐变色柱状图：[https://www.jianshu.com/p/97323f7e05fd](https://www.jianshu.com/p/97323f7e05fd)

