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

#性別、年齢の抽出
gender <- select(dat, starts_with('Q4'))
sum(gender==1)#男性
sum(gender==2)#女性
sum(gender==3)#無回答

age <- select(dat, starts_with('Q2'))
age$Q2 <- as.numeric(age$Q2)
mean(age$Q2,na.rm=TRUE)

# 評価データの抽出
evlshuman <- select(dat, starts_with('Q16'))
evlschair <- select(dat, starts_with('Q27'))
evls <- cbind(evlshuman,evlschair)

# データを入れるためのデータフレーム
js_data <- tibble()

# 評価データを入れるためのデータフレーム
eval_frm <- data.frame()

# 被験者ごとにデータ変換
for (i in 3:nrow(js_dat)) {
  if (js_dat[i, ] != '') {
    # 評価データを行列に代入
    for (j in 1:21) {
      eval_frm <-
        rbind(eval_frm, c(i - 2, cndname[as.integer(evls[i, j] > evls[i, j + 21]) +
                                           1], j, max(evls[i, j], evls[i, j + 21])))
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
colnames(eval_frm) <- c('id', 'cnd', 'Q', 'eval')
eval_frm$eval<-as.numeric(eval_frm$eval)


# 左右条件と角度条件の列を作る
js_data$ang <- NA
js_data$bunseki_ang <- NA
js_data$cnd <- NA
js_data$cnd[str_detect(js_data$stimulus, 'chair')] <- 'chair'
js_data$cnd[str_detect(js_data$stimulus, 'human')] <- 'human'
js_data$ang[str_detect(js_data$stimu, '45')] <- 45
js_data$ang[str_detect(js_data$stimu, '90')] <- 90
js_data$ang[str_detect(js_data$stimu, '135')] <- 135
js_data$ang[str_detect(js_data$stimu, '225')] <- 225
js_data$ang[str_detect(js_data$stimu, '270')] <- 270
js_data$ang[str_detect(js_data$stimu, '315')] <- 315

js_data$bunseki_ang [str_detect(js_data$stimu, '45')] <- 45
js_data$bunseki_ang [str_detect(js_data$stimu, '90')] <- 90
js_data$bunseki_ang [str_detect(js_data$stimu, '135')] <- 135
js_data$bunseki_ang [str_detect(js_data$stimu, '225')] <- 360-225
js_data$bunseki_ang [str_detect(js_data$stimu, '270')] <- 360-270
js_data$bunseki_ang [str_detect(js_data$stimu, '315')] <- 360-315

anovadat<-data.frame(matrix(data=NA, nrow=length(unique(js_data$code)), ncol=4))
idx<-1

#90パーセント以下の参加者のRTにNAを入れる
for (i in unique(js_data$code)){
  hit <- sum(js_data[js_data$code == i, ]$correct) / 72
  if (hit < chit | eval_frm[eval_frm$id == i&eval_frm$Q == 21, ]$eval != 6) {
    js_data[js_data$code == i, ]$rt  <- NA
    eval_frm[eval_frm$id==i,]$eval<-NA
  }
  else{
    anovadat[idx,1]<-js_data[js_data$code==i,]$cnd[1]
    anovadat[idx,2]<-mean(js_data[js_data$code==i & js_data$bunseki_ang==45,]$rt, na.rm=T)
    anovadat[idx,3]<-mean(js_data[js_data$code==i & js_data$bunseki_ang==90,]$rt, na.rm=T)
    anovadat[idx,4]<-mean(js_data[js_data$code==i & js_data$bunseki_ang==135,]$rt, na.rm = T)
  }
  idx<-idx+1;
}

anovadat<-na.exclude(anovadat)
colnames(anovadat)<-c('condition','45','90','135')

#フィラー項目の処理
for (i in unique(eval_frm$id)){
  if (eval_frm[eval_frm$id == i&eval_frm$Q == 21, ]$eval != 6){
    eval_frm[eval_frm$id == i, ]$eval<- NA
  }}
#フィラー項目の列を消去する
eval_frm$eval[eval_frm$Q == "21"] <- NA


#t検定
sublist <- unique(eval_frm$id)
qlist <- c(8,9,11,15,17)
res <- matrix(nrow=length(sublist), ncol=length(qlist))
condition <- matrix(ncol=length(sublist))
eval_frm$eval <- as.numeric(eval_frm$eval)
eval_frm$eval[eval_frm$Q == 9] <- 8 - eval_frm$eval[eval_frm$Q == 9]
eval_frm$eval[eval_frm$Q == 17] <- 8 - eval_frm$eval[eval_frm$Q == 17]

# 評価点反転
eval_frm$eval<-8-eval_frm$eval

evaldat<-data.frame()

for (i in 1:length(sublist)) {
  temp<-eval_frm[eval_frm$id==sublist[i],]
  evaldat<-rbind(evaldat,c(sublist[i],eval_frm[eval_frm$id==sublist[i],]$cnd[1],mean(temp[temp$Q==8 | temp$Q==9 | temp$Q==11 | temp$Q==15 | temp$Q==17,]$eval)))
  condition[i] <- eval_frm[eval_frm$id==sublist[i],]$cnd[1]
  for (j in 1:length(qlist)){
    ia <- sublist[i]
    ja <- qlist[j]
    res[i,j] <- as.numeric(eval_frm[which(eval_frm$id==ia & eval_frm$Q==ja),]$eval)
    #res[,2] <- 8-res[,2]
    
  }
}

evaldat<-na.exclude(evaldat)

colnames(evaldat)<-c('id','condition','eval')
evaldat$condition<-as.factor(evaldat$condition)
evaldat$eval<-as.numeric(evaldat$eval)

summary(evaldat)
mean(evaldat[evaldat$condition=='human',]$eval)
sd(evaldat[evaldat$condition=='human',]$eval)
mean(evaldat[evaldat$condition=='chair',]$eval)
sd(evaldat[evaldat$condition=='chair',]$eval)
t.test(evaldat$eval~evaldat$condition)
effectsize::hedges_g(evaldat$eval~evaldat$condition)

source('anovakun_489.txt')
anovakun(anovadat, 'AsB', 2, 3, peta=T)

rod=data.frame()


ldat<-pivot_longer(anovadat, cols=c('45', '90', '135'))
g<-ggplot(ldat, aes(x=name, y=value, fill=condition))+geom_violin()+xlab("Angle(degree)")+ylab("Reaction Time(msec.)")+scale_x_discrete(limit=c('45','90','135'))
# g<-ggplot(ldat, aes(x=name, y=value, color=condition))+geom_jitter()+xlab("Angle(degree)")+ylab("Reaction Time(msec.)")
g<-g+stat_summary(fun=mean, geom='point', color='white',position=position_dodge(width=0.9), size=2)
g<-g+scale_fill_grey()+scale_color_grey()+theme_bw()
g<-g+theme(text =element_text(size=18))
plot(g)


ldat2<-pivot_longer(anovadat, cols=c('45', '90', '135'))
ldat2$name<-as.numeric(ldat2$name)
g2<-ggplot(ldat2, aes(x=name, y=value, color=condition))
g2<-g2+geom_smooth(method='lm', formula='y~x', aes(color=condition))
# g<-ggplot(ldat, aes(x=name, y=value, color=condition))+geom_jitter()+xlab("Angle(degree)")+ylab("Reaction Time(msec.)")
g2<-g2+stat_summary(fun=mean, geom='point', color='white',position=position_dodge(width=0.9), size=2)
g2<-g2+scale_fill_grey()+scale_color_grey()+theme_bw()
g2<-g2+theme(text =element_text(size=18))
plot(g2)


