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

# 左右条件と角度条件の列を作る
js_data$lr <- NA
js_data$ang <- NA
js_data$cnd <- NA
js_data$lr[str_detect(js_data$stimu, 'left')] <- 'left'
js_data$lr[str_detect(js_data$stimu, 'right')] <- 'right'
js_data$ang[str_detect(js_data$stimu, '0')] <- 0
js_data$ang[str_detect(js_data$stimu, '45')] <- 45
js_data$ang[str_detect(js_data$stimu, '90')] <- 90
js_data$ang[str_detect(js_data$stimu, '135')] <- 135
js_data$ang[str_detect(js_data$stimu, '180')] <- 180
js_data$ang[str_detect(js_data$stimu, '225')] <- 225
js_data$ang[str_detect(js_data$stimu, '270')] <- 270
js_data$ang[str_detect(js_data$stimu, '315')] <- 315



# lr indexとtoward indexを計算
js_data$lridx <- sin(js_data$ang * pi / 180) * js_data$rt
js_data$tbidx <- -cos(js_data$ang * pi / 180) * js_data$rt

lrdata <- data.frame()
tbdata <- data.frame()

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
  }
}

# データフレームの列の名前を変更
colnames(lrdata) <- c('right', 'left')
colnames(tbdata) <- c('right', 'left')


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

lr_c_temp<-cbind(1:nrow(lr_c),cbind('Chair',lr_c))
colnames(lr_c_temp)<-c('id','Avater','Right','Left')

lr_h_temp<-cbind(1:nrow(lr_h),cbind('Person',lr_h))
colnames(lr_h_temp)<-c('id','Avater','Right','Left')

long_lr<-rbind(pivot_longer(lr_c_temp, cols = c('Left','Right')),pivot_longer(lr_h_temp, cols = c('Left','Right')))
colnames(long_lr)<-c('id','Avater','Condition','LR_bias')
glr<-ggplot(long_lr, aes(x=Avater, y=LR_bias, color=Condition, fill=Condition))+geom_violin()+ylab("L/R bias")
glr<-glr+stat_summary(fun=mean, geom='point', color='white',position=position_dodge(width=0.9), size=2)
#glr<-glr+geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1,position=position_dodge(width=0.9))
glr<-glr+scale_fill_grey()+scale_color_grey()+theme_bw()
glr<-glr+theme(text =element_text(size=18))
plot(glr)
ggsave('fig4.png')

source('anovakun_489.txt')

data_chair<-cbind('chair',lr_c)
data_human<-cbind('human',lr_h)

colnames(data_chair)<-c('condition','right','left')
colnames(data_human)<-c('condition','right','left')
anovadata<-rbind(data_chair, data_human)

anovakun(anovadata,'AsB',2,2,peta=T)
