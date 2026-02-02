library(jsonlite)
library(tidyverse)
library(formattable)
library(ggplot2)
library(tidyr)
library(patchwork)
library(psych)

rm(list=ls())

cri <- 3
chit <- 0.9
minrt <- 300
maxrt <- 2500

cndname <- c('chair', 'human')

# 元データの読み込み
fl <- file.choose("*.csv")
dat <- read.csv(fl, header = T)

# 必要な列の抜き出し
js_dat <- dat %>%
  dplyr::select(datajs)

# 評価データの抽出
evls <- select(dat, starts_with('Q29'))

# データを入れるためのデータフレーム
js_data <- tibble()

# 評価データを入れるためのデータフレーム
eval_frm <- data.frame()

# 被験者ごとにデータ変換
for (i in 3:nrow(js_dat)) {
  if (js_dat[i, ] != '') {
    # 評価データを行列に代入
    for (j in 1:29) {
      eval_frm <-
        rbind(eval_frm, c(i - 2, j, evls[i, j]))
    }
    #Qualtricsから出力されたもののうちjsPsychのデータを抽出
    tidy_df01 <- js_dat[i, 1]
    #txtとして書き出し
    write.table(
      tidy_df01,
      "output.txt",
      quote = F,
      col.names = F,
      row.names = F,
      append = F
    )
    #jsonとして読み込み整理
    tidy_df02 <- fromJSON("output.txt") %>%
      as_tibble() %>%
      #被験者番号を追加
      mutate(code = i - 2)
    #他の参加者と結合する
    if (i == 3) {
      js_data <- tidy_df02
    } else{
      js_data <- union(js_data, tidy_df02)
    }
    mrt <- mean(js_data[js_data$code == i - 2, ]$rt)
    sdrt <- sd(js_data[js_data$code == i - 2, ]$rt)
    js_data[js_data$code==i-2,]$rt[js_data[js_data$code==i-2,]$rt>maxrt]<-NA
    js_data[js_data$code == i - 2, ]$rt[js_data[js_data$code == i - 2, ]$rt <
                                          minrt] <- NA
    js_data[js_data$code == i - 2, ]$rt[js_data[js_data$code == i - 2, ]$rt >
                                          mrt + sdrt * cri] <- NA
    js_data[js_data$code == i - 2, ]$rt[js_data[js_data$code == i - 2, ]$rt <
                                          mrt - sdrt * cri] <- NA
    js_data[js_data$code == i - 2, ]$rt[js_data[js_data$code == i - 2, ]$correct ==
                                          FALSE] <- NA
  }
}

# 評価データに列名をつける
colnames(eval_frm) <- c('id', 'Q', 'eval')
eval_frm$eval<-as.numeric(eval_frm$eval)

#逆転項目の処理　　　　　　　　　　　　　　　　！！！！！！！！！！！！！！！！ここ変える！！！！！！！！！！！！！！！！
eval_frm$eval[eval_frm$Q == "3"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 3])
eval_frm$eval[eval_frm$Q == "4"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 4])
eval_frm$eval[eval_frm$Q == "7"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 7])
eval_frm$eval[eval_frm$Q == "12"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 12])    
eval_frm$eval[eval_frm$Q == "13"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 13])
eval_frm$eval[eval_frm$Q == "14"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 14])
eval_frm$eval[eval_frm$Q == "15"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 15])
eval_frm$eval[eval_frm$Q == "18"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 18])
eval_frm$eval[eval_frm$Q == "19"] <- 6 - as.numeric(eval_frm$eval[eval_frm$Q == 19])
eval_frm[eval_frm$Q=="20",]$eval<-NA

sumeval<-data.frame()

for (i in unique(eval_frm$id)){
  
  PDd<-eval_frm %>% filter(Q==6 | Q==10 | Q==17 | Q==25 | Q==28 | Q==13 | Q==19) %>%
    pivot_wider(names_from=Q, values_from=eval)
  ECd<-eval_frm %>% filter(Q==2 | Q==9 | Q==21 | Q==23 | Q==4 | Q==14 | Q==18) %>%
    pivot_wider(names_from=Q, values_from=eval)
  PTd<-eval_frm %>% filter(Q==8 | Q==11 | Q==22 | Q==26 | Q==29 | Q==3 | Q==15) %>%
    pivot_wider(names_from=Q, values_from=eval)
  FSd<-eval_frm %>% filter(Q==1 | Q==5 | Q==16 | Q==24 | Q==27 | Q==7 | Q==12) %>%
    pivot_wider(names_from=Q, values_from=eval)
  
  total<-eval_frm %>% filter(id==i) %>% 
    select(eval) %>% sum(na.rm = T)
  PD<-eval_frm %>% filter(id==i, Q==6 | Q==10 | Q==17 | Q==25 | Q==28 | Q==13 | Q==19) %>% 
    select(eval) %>% sum(na.rm = T)
  EC<-eval_frm %>% filter(id==i, Q==2 | Q==9 | Q==21 | Q==23 | Q==4 | Q==14 | Q==18) %>% 
    select(eval) %>% sum(na.rm = T)
  PT<-eval_frm %>% filter(id==i, Q==8 | Q==11 | Q==22 | Q==26 | Q==29 | Q==3 | Q==15) %>% 
    select(eval) %>% sum(na.rm = T)
  FS<-eval_frm %>% filter(id==i, Q==1 | Q==5 | Q==16 | Q==24 | Q==27 | Q==7 | Q==12) %>% 
    select(eval) %>% sum(na.rm = T)
  sumeval<-rbind(sumeval,c(total,PD,EC,PT,FS))
}
colnames(sumeval)<-c('total','PD','EC','PT','FS')

#print(alpha(PDd[,-1]))
#print(alpha(ECd[,-1]))
#print(alpha(PTd[,-1]))
#print(alpha(FSd[,-1]))


# 左右条件と角度条件の列を作る
js_data$lr <- NA
js_data$ang <- NA
js_data$cnd <- NA
js_data$lr[str_detect(js_data$stimu, 'left')] <- 'left'
js_data$lr[str_detect(js_data$stimu, 'right')] <- 'right'
js_data$ang[str_detect(js_data$stimu, '0')] <- 360-0
js_data$ang[str_detect(js_data$stimu, '45')] <- 360-45
js_data$ang[str_detect(js_data$stimu, '90')] <- 360-90
js_data$ang[str_detect(js_data$stimu, '135')] <- 360-135
js_data$ang[str_detect(js_data$stimu, '180')] <- 360-180
js_data$ang[str_detect(js_data$stimu, '225')] <- 360-225
js_data$ang[str_detect(js_data$stimu, '270')] <- 360-270
js_data$ang[str_detect(js_data$stimu, '315')] <- 360-315

# 関心角度はR225-R135, L135-L225

# lr indexとtoward indexを計算
js_data$lridx <- sin(js_data$ang * pi / 180) * js_data$rt
js_data$tbidx <- -cos(js_data$ang * pi / 180) * js_data$rt

lrdata <- data.frame()
tbdata <- data.frame()
aoi<-data.frame()

# 被験者ごと、左右ごとにindexの平均を計算してデータフレームに代入
for (i in unique(js_data$code)){
  hit <- sum(js_data[js_data$code == i, ]$correct) / 64
  if (hit >= chit) {
      lrdata <-
        rbind(lrdata, c(
          mean(js_data$lridx[js_data$lr == 'right' &
                               js_data$code == i], na.rm = TRUE),
          mean(js_data$lridx[js_data$lr == 'left' &
                               js_data$code == i], na.rm = TRUE)
        ))
      tbdata <-
        rbind(tbdata, c(
          mean(js_data$tbidx[js_data$lr == 'right' &
                               js_data$code == i], na.rm = TRUE),
          mean(js_data$tbidx[js_data$lr == 'left' &
                               js_data$code == i], na.rm = TRUE)
        ))
      aoi<-rbind(aoi,
                 c(
                   mean(js_data[js_data$code==i & js_data$lr == 'left' & js_data$ang=="135",]$rt, na.rm = T),
                   mean(js_data[js_data$code==i & js_data$lr == 'left' & js_data$ang=="225",]$rt,na.rm = T),
                   mean(js_data[js_data$code==i & js_data$lr == 'right' & js_data$ang=="135",]$rt,na.rm = T),
                   mean(js_data[js_data$code==i & js_data$lr == 'right' & js_data$ang=="225",]$rt,na.rm = T)
                 ))
  }
  else{
    sumeval[i,]<-NA
    PDd[i,]<-NA
    ECd[i,]<-NA
    PTd[i,]<-NA
    FSd[i,]<-NA
  }
}

source("anovakun_489.txt")
anovakun(aoi, 'sAB', 2, 2, peta = TRUE)


sumeval<-na.omit(sumeval)
PDd<-na.omit(PDd)
ECd<-na.omit(ECd)
PTd<-na.omit(PTd)
FSd<-na.omit(FSd)

TOTALd<-cbind(PDd,ECd[,-1],PTd[,-1],FSd[,-1])

describe(sumeval)

# 信頼性係数
print(alpha(PDd[,-1]))
print(alpha(ECd[,-1]))
print(alpha(PTd[,-1]))
print(alpha(FSd[,-1]))
print(alpha(TOTALd[,-1]))

# データフレームの列の名前を変更
colnames(lrdata) <- c('right', 'left')
colnames(tbdata) <- c('right', 'left')
colnames(aoi)<-c('left.135','left.225','right.135','right.225')

laoi<-pivot_longer(aoi,cols = c('left.135','left.225','right.135','right.225'),names_sep='\\.',names_to = c('LR','ang'))


# まとめの表を出力
print(lrdata)
print(tbdata)


# 評価データ表示
# print(eval_frm)

# 箱ひげ図を出力
par(mfrow = c(1, 2))
boxplot(lrdata)
boxplot(tbdata)

# せっかくなのでviolin plotも


gang<-ggplot(laoi, aes(x=LR, y=value, color=ang, fill=ang))+geom_violin()+ylab("Reaction Time(ms)")
gang<-gang+stat_summary(fun=mean, geom='point', color='white',position=position_dodge(width=0.9), size=2)
gang<-gang+scale_fill_grey()+scale_color_grey()+theme_bw()
gang<-gang+theme(text =element_text(size=18))
plot(gang)
ggsave('fig5.png')

source('anovakun_489.txt')

anovakun(aoi,'sAB',2,2,peta=T)


# トータル
cordat<-data.frame(cbind(aoi$left.135-aoi$left.225, aoi$right.225-aoi$right.135))
colnames(cordat)<-c('left','right')
cor(sumeval$total,cordat$right)
plot(sumeval$total, cordat$left, xlab='IRI Score', ylab='AOI RT difference (135deg-225deg)', main="Avater at Left")
plot(sumeval$total, cordat$right, xlab='IRI Score', ylab='AOI RT difference (225deg-135deg)', main ="Avater at Right")

# PD
cordat<-data.frame(cbind(aoi$left.135-aoi$left.225, aoi$right.225-aoi$right.135))
colnames(cordat)<-c('left','right')
cor(sumeval$PD,cordat$left)
cor(sumeval$PD,cordat$right)
#plot(sumeval$PD, cordat$left, xlab='IRI Score(PD)', ylab='AOI RT difference (135deg-225deg)', main="Avater at Left")
#plot(sumeval$PD, cordat$right, xlab='IRI Score(PD)', ylab='AOI RT difference (225deg-135deg)', main ="Avater at Right")

cor(sumeval$EC,cordat$left)
cor(sumeval$EC,cordat$right)
cor(sumeval$PT,cordat$left)
cor(sumeval$PT,cordat$right)
cor(sumeval$FS,cordat$left)
cor(sumeval$FS,cordat$right)
cor(sumeval$total,cordat$left)
cor(sumeval$total,cordat$right)


lcordat<-cbind(cordat,sumeval$PT)
colnames(lcordat)<-c('left','right','eval')
gleft<-ggplot(lcordat, aes(x=eval,y=left))+geom_point(size=3)+ylab('RT difference(msec)')+xlab('IRI Score')+theme_bw()
gleft<-gleft+theme(text =element_text(size=18))+ylim(c(-800,600))+ggtitle('Left')

gright<-ggplot(lcordat, aes(x=eval,y=right))+geom_point(size=3)+ylab('RT difference(msec)')+xlab('IRI Score(PT)')+theme_bw()
gright<-gright+theme(text =element_text(size=18))+ylim(c(-800,600))+ggtitle('Right')
gleft+gright
ggsave('fig6.png')
