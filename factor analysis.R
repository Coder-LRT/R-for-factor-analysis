
###################################因子分析#####################################
#准备封装包
install.packages("corrplot")#相关系数矩阵可视化
install.packages("psych")#因子分析,本次实验使用其平行分析的能力
#另外需要下载mvstat程序包，安装到library的路径下

#读取数据
airquality <- read.csv("C:/Users/LRT/Desktop/graduate/paper/data/Airdata.csv", 
                       sep=",", header = TRUE,stringsAsFactors = FALSE)
#简化名称
colnames(airquality) <- c("city", "SO2", "NO2","PM10", "CO","O3","PM2.5","days","rain")

#查看数据结构
summary(airquality)
str(airquality)

#相关系数矩阵及具有双色表正负相关
airquality_cor <- cor(airquality[2:9], method = "spearman")#spearman法求相关系数矩阵
library(corrplot)#加载corrplot包
corrplot.mixed(airquality_cor, number.cex = .9)#绘制混合型相关系数矩阵图

#巴雷特球形检验&KMO检验
library(psych)#加载检验用到的包
cortest.bartlett(airquality_cor)#巴雷特球形检验
KMO(airquality_cor)#kmo检验

#判断需要提取的公因子个数
library(psych)#加载检验用到的包
fa.parallel(airquality_cor, n.obs = 320, 
            fa = "fa", n.iter = 100)#制作碎石图判断个数

#主成分法因子分析,并计算按方差最大旋转因子，使用回归的方法计算因子得分并排名
library(mvstats)
airquality_score <- factpc(airquality[2:9],2,rotation="varimax",scores = "regression")
airquality_score$Vars#输出因子方差、方差贡献率以及累计方差贡献率
airquality_score$loadings#输出因子载荷矩阵
airquality_score$scores#输出因子得分
airquality_score$Rank#输出排名
factor.plot(airquality_score,labels = rownames(airquality_score$loadings),
            title = "Factor Analysis",pos = "3")



