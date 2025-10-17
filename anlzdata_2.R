library(jsonlite)
library(tidyverse)
library(formattable)

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
  sumeval<-rbind(sumeval, 
                 sum(eval_frm[eval_frm$id==i,]$eval, na.rm=T))  
}

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
}

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
library(ggplot2)
library(tidyr)

gang<-ggplot(laoi, aes(x=ang, y=value, color=LR, fill=LR))+geom_violin()+ylab("Reaction Time(ms)")
gang<-gang+stat_summary(fun=mean, geom='point', color='white',position=position_dodge(width=0.9), size=2)
#glr<-glr+geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1,position=position_dodge(width=0.9))
gang<-gang+scale_fill_grey()+scale_color_grey()+theme_bw()
gang<-gang+theme(text =element_text(size=18))
plot(gang)
ggsave('fig5.png')

source('anovakun_489.txt')

data_chair<-cbind('chair',lr_c)
data_human<-cbind('human',lr_h)

colnames(data_chair)<-c('condition','right','left')
colnames(data_human)<-c('condition','right','left')
anovadata<-rbind(data_chair, data_human)

anovakun(anovadata,'AsB',2,2,peta=T)
