#作業前の処理

library(nimble)
library(coda)
library(tidyverse)

setwd("/Users/wildlife/Desktop/R")
#作ったデータセットの読み込み
df <- read.csv("jokou.csv")
#年を文字列にしておく
df$year <- as.character(df$year)

#使うデータ選択
use_data<-select(df, c(4,5,9:13,27))

#d_useの列を作成
use_data$d_use <- apply(use_data,1,function(x){!(1 %in% is.na(x))}) & c(rep(FALSE,times=103),rep(TRUE,times=947-103))

use_data$d_use <- use_data$d_use %>% as.integer()

tail(df,100)







#adj&num作成
#id_useを作成

id_use<-data.frame(matrix(nrow = nrow(df)/9)[, 1])
colnames(id_use)<-c("id_use")


for(i in 1:947){
  if(use_data$d_use[i]==0){
    id_use[i,]<-NA
  }else{
    id_use[i,]<-sum(use_data$d_use[1:i])
  }
}
#head(id_use,200)
#nrow(id_use)


adjall<-NULL
numall<-NULL


for(i in 1:947){
  if(is.na(id_use[i,])!=1){
    tmp_adj<-c(id_use[i-1,],id_use[i+1,])%>%na.omit
    adjall<-c(adjall,tmp_adj)
    numall<-c(numall,length(tmp_adj))
  }
}


#使うデータセット作成
use_datas<-use_data[use_data$d_use>0,]
nrow(use_datas)

cols <- use_datas$cols
pop <- use_datas$pop
veg_1 <- use_datas$veg_1_100m
veg_2 <- use_datas$veg_2_100m
veg_3 <- use_datas$veg_3_100m
veg_4 <- use_datas$veg_4_100m
veg_5 <- use_datas$veg_5_100m
dem <- use_datas$dem_100m



sum(numall)

head(numall)
head(adjall)
length(adjall)
length(numall)
