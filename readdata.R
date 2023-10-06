library(jsonlite)
library(tidyverse)
library(formattable)

# 元データの読み込み
dat <- read.csv("test.csv", header =T)

# 必要な列の抜き出し
js_dat <- dat %>% 
  dplyr::select(datajs)

# データを入れるためのデータフレーム
js_data <- tibble()

# 被験者ごとにデータ変換
for(i in 3:nrow(js_dat)){
  #Qualtricsから出力されたもののうちjsPsychのデータを抽出
  tidy_df01 <- js_dat[i, 1]
  #txtとして書き出し
  write.table(tidy_df01, "output.txt", quote = F, col.names=F, row.names = F, append=F)
  #jsonとして読み込み整理
  tidy_df02 <- fromJSON("output.txt") %>% 
    as_tibble() %>% 
    #被験者番号を追加
    mutate(code = i-2)
  #他の参加者と結合する
  if(i == 3){
    js_data <- tidy_df02
  }else{
    js_data <- union(js_data, tidy_df02)
  }
}

# 左右条件と角度条件の列を作る
js_data$lr<-NA
js_data$ang<-NA
js_data$lr[str_detect(js_data$stimu,'left')]<-'left'
js_data$lr[str_detect(js_data$stimu,'right')]<-'right'
js_data$ang[str_detect(js_data$stimu,'0')]<-0
js_data$ang[str_detect(js_data$stimu,'45')]<-360-45
js_data$ang[str_detect(js_data$stimu,'90')]<-360-90
js_data$ang[str_detect(js_data$stimu,'135')]<-360-135
js_data$ang[str_detect(js_data$stimu,'180')]<-360-180
js_data$ang[str_detect(js_data$stimu,'225')]<-360-225
js_data$ang[str_detect(js_data$stimu,'270')]<-360-270
js_data$ang[str_detect(js_data$stimu,'315')]<-360-315

# lr indexとtoward indexを計算
js_data$lridx<-sin(js_data$ang*pi/180)*js_data$rt
js_data$tbidx<--cos(js_data$ang*pi/180)*js_data$rt

lrsummary<-data.frame()
tbsummary<-data.frame()
# 被験者ごと、左右ごとにindexの平均を計算してデータフレームに代入
for (i in 1:10){
  lrsummary<-rbind(lrsummary,c(mean(js_data$lridx[js_data$lr=='right' & js_data$code==i]), mean(js_data$lridx[js_data$lr=='left' & js_data$code==i])))
  tbsummary<-rbind(tbsummary,c(mean(js_data$tbidx[js_data$lr=='right' & js_data$code==i]), mean(js_data$tbidx[js_data$lr=='left' & js_data$code==i])))
}

# データフレームの列の名前を変更
colnames(lrsummary)<-c('right','left')
colnames(tbsummary)<-c('right','left')

# まとめの表を出力
print(lrsummary)
print(tbsummary)

# 箱ひげ図を出力
boxplot(lrsummary)
boxplot(tbsummary)

# せっかくなのでviolin plotも
library(ggplot2)
library(tidyr)
longlr<-pivot_longer(lrsummary, cols=c('right','left'))
longtb<-pivot_longer(tbsummary, cols=c('right','left'))
lrg<-ggplot(longlr, aes(x=name, y=value))+geom_violin()+geom_point()
tbg<-ggplot(longtb, aes(x=name, y=value))+geom_violin()+geom_point()

plot(lrg)
plot(tbg)
