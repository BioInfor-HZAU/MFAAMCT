#读取两个文件
#install.packages('R.matlab')
#library("R.matlab")
#path<-('F:/')
#pathname1<-file.path(path,'AMD.mat')
#pathname2<-file.path(path,'AMD_Name.mat')
#testdata1<-readMat(pathname1)
#testdata2<-readMat(pathname2)

#手动选取两个文件
setwd("./comparison")
#install.packages('R.matlab')
library("R.matlab")
print("Please select the AMD data:") 
testdata1<-readMat(file.choose())
print("Please select the AMD_name data:") 
testdata2<-readMat(file.choose())
#print(testdata1)
#print(testdata2)

#合并数据
options(max.print = 1000000)
x<-testdata1[[1]]
y<-testdata1[[2]]
z<-t(y)
dataset1<-cbind(x,z)  # 成功将其加至后面
a<-(testdata2[[1]][1,])
b<-t(a)  
c<-append(b,"AMD")
dataset<-rbind(c,dataset1)

#去掉缺失值
a<-c()
for (i in (1:(ncol(dataset)-1))){
xx<-dataset[,i]
yy<-sum(xx=="0")
if (yy>0)  # 缺失值去掉
{
a<-c(a,-i)
}
}
zzz<-dataset[,a]

#去掉RS为0的情况
a<-c()
for (i in (1:(ncol(zzz)-1))){
xx<-zzz[1,i][[1]]
if (xx=="0") 
{
a<-c(a,-i)
}
}
zz<-zzz[,a]


#生成显性模型数据
title1<-zz[1,]  # title
a<-length(zz[,1])
snppre<-zz[(2:a),]
b<-length(zz[1,])-1
snp<-snppre[,(1:b)]
disease<-snppre[,(b+1)]
snp[snp=="1"]=0
snp[snp=="2"]=1
snp[snp=="3"]=1
disease[disease=="2"]=0
datasetdom1<-cbind(snp,disease)  # 按列合并
title<-list()    # 拆成单个的字符串
for (i in 1:length(title1)){
title<-append(title,title1[[i]][1])
}
datasetdom<-rbind(title,datasetdom1)  # 添加标题

#整体写入文件  # 合计72018个SNP
write.table(datasetdom,file = "./result/amdcs72018.csv",sep = ",",row.names = FALSE,col.names = FALSE)

#生成多值集数据
#title<-zz[1,]  # title
a<-length(zz[,1])
snppre<-zz[(2:a),]
b<-length(zz[1,])-1
snp<-snppre[,(1:b)]
disease<-snppre[,(b+1)]
snp[snp=="1"]=1
snp[snp=="2"]=2
snp[snp=="3"]=3
disease[disease=="2"]=2
datasetdom2<-cbind(snp,disease)  # 按列合并
datasetdom<-rbind(title,datasetdom2)  # 添加标题

#整体写入文件  # 合计72018个SNP
write.table(datasetdom,file = "./result/amdmv72018.csv",sep = ",",row.names = FALSE,col.names = FALSE)

#生成显性模型数据
setwd("./comparison")
library(isscc)
print("Please select the mv data:") 
mvsccsamples<-read.csv(file.choose(),header=F)  # choose the mv data
zz<-mvsccsamples
title1<-zz[1,]  # title
a<-length(zz[,1])
snppre<-zz[(2:a),]
b<-length(zz[1,])-1
snp<-snppre[,(1:b)]
disease<-snppre[,(b+1)]
snp[snp=="1"]=0
snp[snp=="2"]=1
snp[snp=="3"]=1
disease[disease=="2"]=0
datasetdom1<-cbind(snp,disease)  # 按列合并
title<-list()    # 拆成单个的字符串
for (i in 1:length(title1)){
title<-append(title,title1[[i]][1])
}
datasetdom<-rbind(title,datasetdom1)  # 添加标题

#整体写入文件
write.table(datasetdom,file = "./result/csx.csv",sep = ",",row.names = FALSE,col.names = FALSE)


setwd("./comparison")
library(isscc)
print("Please select the mv data:") 
mvsccsamples<-read.csv(file.choose(),header=F)  # choose the mv data
zz<-mvsccsamples
title1<-zz[1,]  # title
a<-length(zz[,1])
snppre<-zz[(2:a),]
b<-length(zz[1,])-1
snp<-snppre[,(1:b)]
disease<-snppre[,(b+1)]
snp[snp=="1"]=0
snp[snp=="2"]=1
snp[snp=="3"]=2
disease[disease=="2"]=0
datasetdom1<-cbind(snp,disease)  # 按列合并
title<-list()    # 拆成单个的字符串
for (i in 1:length(title1)){
title<-append(title,title1[[i]][1])
}
datasetdom<-rbind(title,datasetdom1)  # 添加标题

#整体写入文件
write.table(datasetdom,file = "./result/mvx.csv",sep = ",",row.names = FALSE,col.names = FALSE)


