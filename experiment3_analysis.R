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
     }
}

# 評価データに列名をつける
colnames(eval_frm) <- c('id', 'cnd', 'Q', 'eval')

#外れ値等を抜く
mrt <- mean(js_data$rt)
sdrt <- sd(js_data$rt)


for (i in 1:nrow(js_data)){
  #mean+3SD
  if(js_data$rt[i]>mrt+sdrt*cri){
    js_data$rt[i]<-NA
  }
  #mean-3SD
  else if(js_data$rt[i]<mrt-sdrt*cri){
    js_data$rt[i]<-NA
  }
  #MAX
  else if(js_data$rt[i]>maxrt){
    js_data$rt[i]<-NA
  }
  #MIN
  else if(js_data$rt[i]<minrt){
    js_data$rt[i]<-NA
  }
  #FALSE
  else if(js_data$correct[i]==FALSE){
    js_data$rt[i]<-NA
  }
  else
    js_data$rt[i]<-js_data$rt[i]
}

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


#90パーセント以下の参加者のRTにNAを入れる
for (i in unique(js_data$code)){
  hit <- sum(js_data[js_data$code == i, ]$correct) / 72
  if (hit < chit) {
    js_data[js_data$code == i, ]$rt  <- NA
  }}

#フィラー項目の処理
for (i in unique(eval_frm$id)){
  if (eval_frm[eval_frm$id == i&eval_frm$Q == 21, ]$eval != 6){
    eval_frm[eval_frm$id == i, ]$eval<- NA
  }}
#フィラー項目の列を消去する
eval_frm$eval[eval_frm$Q == "21"] <- NA

#評価データを参加者ごとに平均する
eval <- data.frame()

eval_frm$eval <- as.numeric(eval_frm$eval)

eval_frm$eval[eval_frm$Q == 9] <- 8 - eval_frm$eval[eval_frm$Q == 9]

for (i in unique(eval_frm$id)){
  
  eval <- rbind(eval, c(
    mean(eval_frm[eval_frm$id == i&eval_frm$Q == 8, ]$eval,eval_frm[eval_frm$id == 1&eval_frm$Q == 9, ]$eval
        ,eval_frm[eval_frm$id == 1&eval_frm$Q == 11, ]$eval,eval_frm[eval_frm$id == 1&eval_frm$Q == 15, ]$eval),
        eval_frm[eval_frm$id == i&eval_frm$Q == 1,]$cnd)
  )}

colnames(eval) <- c('eval','cnd')



#評価データのグラフを書く
eval$cnd <- as.factor(eval$cnd)
eval$eval <- as.numeric(eval$eval)

g <- ggplot(eval, aes(x=cnd,y=eval))
g <- g + geom_boxplot()
plot(g)

bo <- ggplot(eval, aes(x = cnd, y = eval)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1)

plot(bo)


#t検定
ttest <- t.test(eval$eval~eval$cnd)
t <- ttest$statistic
df <- ttest$parameter
t.effect <- sqrt(t^2/(t^2+df))

ttest
t.effect

chair_data <- eval[eval$cnd=="chair",]
mean(chair_data$eval,na.rm=TRUE)
sd(chair_data$eval,na.rm=TRUE)

human_data <- eval[eval$cnd=="human",]
mean(human_data$eval,na.rm=TRUE)
sd(human_data$eval,na.rm=TRUE)

#anova君の処理
ac <- data.frame()
anovahuman <- data.frame()
anovachair <- data.frame()

for (i in unique(js_data$code)){
  
  anovachair <- rbind(anovachair, c( "chair",
    mean(js_data[js_data$code == i & js_data$ang == 45 & js_data$cnd == "chair",]$rt, 
         js_data[js_data$code == i & js_data$ang == 315& js_data$cnd == "chair",]$rt,na.rm = TRUE, trim = 0),
    mean(js_data[js_data$code == i & js_data$ang == 90 & js_data$cnd == "chair",]$rt, 
         js_data[js_data$code == i & js_data$ang == 270& js_data$cnd == "chair",]$rt,na.rm = TRUE, trim = 0),
    mean(js_data[js_data$code == i & js_data$ang == 135 & js_data$cnd == "chair",]$rt, 
         js_data[js_data$code == i & js_data$ang == 225& js_data$cnd == "chair",]$rt,na.rm = TRUE, trim = 0)
  ))

anovahuman <- rbind(anovahuman, c("human",
mean(js_data[js_data$code == i & js_data$ang == 45 & js_data$cnd == "human",]$rt, 
     js_data[js_data$code == i & js_data$ang == 315& js_data$cnd == "human",]$rt,na.rm = TRUE, trim = 0),
mean(js_data[js_data$code == i & js_data$ang == 90 & js_data$cnd == "human",]$rt, 
     js_data[js_data$code == i & js_data$ang == 270& js_data$cnd == "human",]$rt,na.rm = TRUE, trim = 0),
mean(js_data[js_data$code == i & js_data$ang == 135 & js_data$cnd == "human",]$rt, 
     js_data[js_data$code == i & js_data$ang == 225& js_data$cnd == "human",]$rt,na.rm = TRUE, trim = 0)
  ))
}

colnames(anovahuman) <- c('cnd','45','90','135')
colnames(anovachair) <- c('cnd','45','90','135')

anovadata <- rbind(anovahuman,anovachair)

#分散分析
source('anovakun_489.txt')
anovakun(anovadata,'AsB',2,3,peta = T)

#分散分析のグラフ作り
box <- data.frame(cbind(js_data$code, js_data$cnd,js_data$bunseki_ang,js_data$rt))
colnames(box) <- c('id','cnd','ang','rt')

box$ang<- as.character(box$ang)
box$cnd<- as.character(box$cnd)
box$rt<- as.numeric(box$rt)

                                   
g <- ggplot(box, aes(x=ang,y=rt, fill=cnd))
g <- g + scale_x_discrete(limits = c("45", "90", "135"))
g <- g + geom_boxplot()
plot(g)


#信頼係数を出す
library(psych)

  eval_frm2 <- data.frame(cbind(eval_frm$id,eval_frm$Q, eval_frm$eval))
  colnames(eval_frm2) <- c('id', 'Q', 'eval')
  eval_frm2$id <- as.numeric(eval_frm2$id)
  eval_frm2$Q <- as.numeric(eval_frm2$Q)
  eval_frm2$eval <- as.numeric(eval_frm2$eval)
  
  
  # 回答者ごとに質問番号（QuestionID）を列として整形
  psyeval <- reshape(eval_frm2, idvar = "id", timevar = "Q", direction = "wide")
  
  psyeval <- psyeval %>% select(-eval.1,-eval.2,-eval.3,-eval.4,-eval.5,-eval.6,-eval.7,-eval.10,-eval.12,-eval.13,
                                -eval.14,-eval.16,-eval.17,-eval.18,-eval.19,-eval.20,-eval.21)
  
  
  psyeval <- psyeval %>% mutate(eval.9 = (8 - eval.9))
  
  # 整形後のデータを確認
  #head(df_wide)
  # psyeval <- psyeval[, !(colnames(psyeval) == "eval.20")]  # "eval.20"を除外
  alpha(psyeval[, -1],na.rm = TRUE)  # 再度分析を実行
  
  cor(psyeval$eval.8, psyeval$eval.9, use="complete.obs")
  cor(psyeval$eval.8, psyeval$eval.11, use="complete.obs")
  cor(psyeval$eval.8, psyeval$eval.9, use="complete.obs")   
  
  
#社会的望ましさの平均など
  mean(eval$eval,na.rm = TRUE)
  max(eval$eval,na.rm = TRUE)
  min(eval$eval,na.rm = TRUE)
  boxplot(eval$eval)
  
  sya <- data_frame()
  for (i in unique(eval_frm$id)){
    sya <- rbind(sya,c(mean(eval_frm$eval[eval_frm$id==i],na.rm=TRUE)))
  }
hist(sya$X1.3,xlim = c(1, 7))  
