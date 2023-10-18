library(jsonlite)
library(tidyverse)
library(formattable)
library(pracma)

cri<-3
chit<-0.9
minrt<-300
maxrt<-2500

# 元データの読み込み
options(encoding = 'UTF-8')
fl <- file.choose("*.csv")
dat <- read.csv(fl, header =T)

# 必要な列の抜き出し
js_dat <- dat %>% 
  dplyr::select(datajs)

# データを入れるためのデータフレーム
js_data <- tibble()

# 被験者ごとにデータ変換
for(i in 4:nrow(js_dat)){
  if (js_dat[i,]!=''){
  #Qualtricsから出力されたもののうちjsPsychのデータを抽出
  tidy_df01 <- js_dat[i, 1]
  #txtとして書き出し
  write.table(tidy_df01, "output.txt", quote = F, col.names=F, row.names = F, append=F)
  #jsonとして読み込み整理
  tidy_df02 <- fromJSON("output.txt") %>% 
    as_tibble() %>% 
    #被験者番号を追加
    mutate(code = i-3)
  #他の参加者と結合する
  if(i == 5){
    js_data <- tidy_df02
  }else{
    js_data <- union(js_data, tidy_df02)
  }
  mrt<-mean(js_data[js_data$code==i-3,]$rt)
  sdrt<-sd(js_data[js_data$code==i-3,]$rt)
  js_data[js_data$code==i-3,]$rt[js_data[js_data$code==i-3,]$rt<minrt]<-NA
  js_data[js_data$code==i-3,]$rt[js_data[js_data$code==i-3,]$rt>maxrt]<-NA
  js_data[js_data$code==i-3,]$rt[js_data[js_data$code==i-3,]$rt>mrt+sdrt*cri]<-NA
  js_data[js_data$code==i-3,]$rt[js_data[js_data$code==i-3,]$rt<mrt-sdrt*cri]<-NA
  js_data[js_data$code==i-3,]$rt[js_data[js_data$code==i-3,]$correct==FALSE]<-NA
  }
}


                        
# 左右条件と角度条件の列を作る
js_data$lr<-NA
js_data$ang<-NA
js_data$lr[str_detect(js_data$stimu,'left')]<-'left'
js_data$lr[str_detect(js_data$stimu,'right')]<-'right'
js_data$cnd[str_detect(js_data$stimu,'chair')]<-'chair'
js_data$cnd[str_detect(js_data$stimu,'human')]<-'human'
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

lr_c<-data.frame()
tb_c<-data.frame()
lr_h<-data.frame()
tb_h<-data.frame()



# 被験者ごと、左右ごとにindexの平均を計算してデータフレームに代入
for (i in unique(js_data$code)){
  hit<-sum(js_data[js_data$code==i,]$correct)/64
  if (hit>=chit){
  
  if (js_data[js_data$code==i,]$cnd[1]=='chair'){
    lr_c<-rbind(lr_c,c(mean(js_data$lridx[js_data$lr=='right' & js_data$code==i], na.rm=TRUE), mean(js_data$lridx[js_data$lr=='left' & js_data$code==i], na.rm=TRUE), sum(as.integer(js_data$correct==TRUE & js_data$code==i))/64*100))
    tb_c<-rbind(tb_c,c(mean(js_data$tbidx[js_data$lr=='right' & js_data$code==i], na.rm=TRUE), mean(js_data$tbidx[js_data$lr=='left' & js_data$code==i], na.rm=TRUE)))
  }
  else{
    lr_h<-rbind(lr_h,c(mean(js_data$lridx[js_data$lr=='right' & js_data$code==i], na.rm=TRUE), mean(js_data$lridx[js_data$lr=='left' & js_data$code==i], na.rm=TRUE), sum(as.integer(js_data$correct==TRUE & js_data$code==i))/64*100))
    tb_h<-rbind(tb_h,c(mean(js_data$tbidx[js_data$lr=='right' & js_data$code==i], na.rm=TRUE), mean(js_data$tbidx[js_data$lr=='left' & js_data$code==i], na.rm=TRUE)))
  }
  }
  
}

# データフレームの列の名前を変更
colnames(lr_c)<-c('right','left','error')
colnames(tb_c)<-c('right','left')
colnames(lr_h)<-c('right','left','error')
colnames(tb_h)<-c('right','left')

# まとめの表を出力
print(lr_c)
print(tb_c)
print(lr_h)
print(tb_h)

# 箱ひげ図を出力
par(mfrow=c(1,2))
boxplot(lr_c)
boxplot(lr_h)
par(mfrow=c(1,2))
boxplot(tb_c)
boxplot(tb_h)

# せっかくなのでviolin plotも
library(ggplot2)
library(tidyr)
glr<-ggplot(js_data, aes(x=interaction(lr,cnd), y=lridx, color=code))+geom_violin()+geom_point()
gtb<-ggplot(js_data, aes(x=interaction(lr,cnd), y=tbidx, color=code))+geom_violin()+geom_point()
plot(glr)
plot(gtb)

