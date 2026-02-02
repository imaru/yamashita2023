library(jsonlite)
library(tidyverse)
library(formattable)

cri <- 3
chit <- 0.9
minrt <- 300
maxrt <- 2500

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
        rbind(eval_frm, c(i - 2, j, max(evls[i, j], evls[i, j + 29])))
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
colnames(eval_frm) <- c('id', 'Q', 'eval')

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
  else if(js_data$response[i]!=js_data$correct_response[i]){
    js_data$rt[i]<-NA
  }
  else
    js_data$rt[i]<-js_data$rt[i]
}


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

#フィラー項目の処理  　　　　　　　　　　　　　！！！！！！！！！！！！！！！！ここ変える！！！！！！！！！！！！！！！！
for (i in unique(eval_frm$id)){
  if (eval_frm[eval_frm$id == i&eval_frm$Q == 20, ]$eval != 5){
    eval_frm[eval_frm$id == i, ]$eval<- NA
  }}

#フィラー項目の列を除外する                   ！！！！！！！！！！！！！！！！ここ変える！！！！！！！！！！！！！！！！
eval_frm$eval[eval_frm$Q == "20"] <- NA

#評価データを参加者ごとに平均する

eval <- data.frame()

eval_frm$eval <- as.numeric(eval_frm$eval)

for (i in unique(eval_frm$id)){
  
  eval <- rbind(eval, c(
    sum(eval_frm$eval[eval_frm$id == i], na.rm = TRUE)
  ))}

#評価データを参加者ごとに平均に名前を付ける
colnames(eval) <- c('eval')

# 左右条件と角度条件の列を作る
js_data$ang <- NA
js_data$bunseki_ang <- NA

js_data$ang[str_detect(js_data$stimulus, '45')] <- 45
#js_data$ang[str_detect(js_data$stimu, '90')] <- 90
js_data$ang[str_detect(js_data$stimulus, '135')] <- 135
js_data$ang[str_detect(js_data$stimulus, '225')] <- 225
#js_data$ang[str_detect(js_data$stimu, '270')] <- 270
js_data$ang[str_detect(js_data$stimulus, '315')] <- 315

js_data$bunseki_ang [str_detect(js_data$stimulus, '45')] <- 45
#js_data$bunseki_ang [str_detect(js_data$stimu, '90')] <- 90
js_data$bunseki_ang [str_detect(js_data$stimulus, '135')] <- 135
js_data$bunseki_ang [str_detect(js_data$stimulus, '225')] <- 360-225
#js_data$bunseki_ang [str_detect(js_data$stimu, '270')] <- 360-270
js_data$bunseki_ang [str_detect(js_data$stimulus, '315')] <- 360-315



#correctを追加
js_data$correct <- NA
for (i in 1:nrow(js_data)){
 if(js_data$response[i]==js_data$correct_response[i]){
    js_data$correct[i]<-"CORRECT"
  }
  else
    js_data$correct[i]<-"FALSE"
}


#90パーセント以下の参加者のRTにNAを入れる
for (i in unique(js_data$code)){
  hit <- sum(js_data[js_data$code == i, ]$correct=="CORRECT") / 48
  if (hit < chit) {
    js_data[js_data$code == i, ]$rt  <- NA
    
    
  }}

#分散分析

anovadata <- data.frame()

for (i in unique(js_data$code)){
  
  anovadata <- rbind(anovadata, c(mean(js_data[js_data$code == i & js_data$ang == 45,]$rt, 
                                          js_data[js_data$code == i & js_data$ang == 315,]$rt,na.rm = TRUE, trim = 0),
                                  mean(js_data[js_data$code == i & js_data$ang == 135,]$rt, 
                                          js_data[js_data$code == i & js_data$ang == 225,]$rt,na.rm = TRUE, trim = 0)
  ))
  
 }

colnames(anovadata) <- c('45','135')

source('anovakun_489.txt')
anovakun(anovadata,'sA',2,peta = T)

#分散分析じゃなくて、t検定では。
library(tidyr)
library(dplyr)

tdata <- anovadata %>%
  pivot_longer(cols = c(`45`, `135`), names_to = "Direction", values_to = "Value") %>%
  filter(!is.na(Value)) %>% # NAを削除
  select(Value, Direction) # 列の順序を調整

ttest <- t.test(tdata$Value~tdata$Direction)

t <- ttest$statistic
df <- ttest$parameter
t.effect <- sqrt(t^2/(t^2+df))

ttest
t.effect


#分散分析のグラフ作り
box <- data.frame(cbind(js_data$code,js_data$bunseki_ang,js_data$rt))
colnames(box) <- c('id','ang','rt')

box$ang<- as.character(box$ang)
box$rt<- as.numeric(box$rt)


g <- ggplot(box, aes(x=ang,y=rt))
g <- g + scale_x_discrete(limits = c("45", "135"))
g <- g + geom_boxplot()
plot(g)

#相関分析
soukan1 <- data.frame()

for (i in unique(js_data$code)){
  soukan1 <- rbind(soukan1,c(mean(js_data$rt[js_data$code == i],na.rm=TRUE, trim = 0)))
  
}

colnames(soukan1) <- c('rt')

soukan1　<- data.frame(cbind(soukan1$rt, eval$eval))

colnames(soukan1) <- c('rt','eval')

soukan1 <- soukan1[-36, ]

#[1相関]相関係数
cor(soukan1$rt, soukan1$eval, use="complete.obs")

#[1相関]グラフを書く
plot(soukan1$rt, soukan1$eval)

#IRIのPTだけで見たら相関があるのではないか。
PD <- data.frame()
EC <-data.frame()
PT　<- data.frame()
FS <- data.frame()

for (i in unique(eval_frm$id)){
  PD <- rbind(PD, c(i,
                    sum(eval_frm$eval[eval_frm$id == i&eval_frm$Q==6],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==10],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==17],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==25],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==28],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==13],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==19])
  ))}

for (i in unique(eval_frm$id)){
  EC <- rbind(EC, c(i,
                    sum(eval_frm$eval[eval_frm$id == i&eval_frm$Q==2],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==9],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==21],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==23],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==4],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==14],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==18])
  ))}

for (i in unique(eval_frm$id)){
  PT <- rbind(PT, c(i,
                    sum(eval_frm$eval[eval_frm$id == i&eval_frm$Q==8],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==11],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==22],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==26],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==29],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==3],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==15])
  ))}

for (i in unique(eval_frm$id)){
  FS <- rbind(FS, c(i,
                    sum(eval_frm$eval[eval_frm$id == i&eval_frm$Q==1],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==5],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==16],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==24],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==27],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==7],
                        eval_frm$eval[eval_frm$id == i&eval_frm$Q==12])
  ))}

colnames(PD) <- c("id","eval")
colnames(EC) <- c("id","eval")
colnames(PT) <- c("id","eval")
colnames(FS) <- c("id","eval")
PD$eval <- as.numeric(PD$eval)
EC$eval <- as.numeric(EC$eval)
PT$eval <- as.numeric(PT$eval)
FS$eval <- as.numeric(FS$eval)

PD <- PD[-36, ]
EC <- EC[-36, ]
PT <- PT[-36, ]
FS <- FS[-36, ]

#PD相関係数
cor(soukan1$rt, PD$eval, use="complete.obs")
#PDグラフを書く
plot(soukan1$rt, PD$eval,ylim=c(7,35))

#EC相関係数
cor(soukan1$rt, EC$eval, use="complete.obs")
#ECグラフを書く
plot(soukan1$rt, EC$eval,ylim=c(7,35))

#PT相関係数
cor(soukan1$rt, PT$eval, use="complete.obs")
#PTグラフを書く
plot(soukan1$rt, PT$eval,ylim=c(7,35))

#FS相関係数
cor(soukan1$rt, FS$eval, use="complete.obs")
#FSグラフを書く
plot(soukan1$rt, FS$eval,ylim=c(7,35))

#IRIのデータをグラフ化
boxplot(soukan1$eval)

#信頼係数を出す
  library(psych)

  # 回答者ごとに質問番号（QuestionID）を列として整形
  psyeval <- reshape(eval_frm, idvar = "id", timevar = "Q", direction = "wide")
  
  
  # 整形後のデータを確認
  #head(df_wide)
  psyeval <- psyeval[, !(colnames(psyeval) == "eval.20")]  # "eval.20"を除外
  alpha(psyeval[, -1], na.rm = TRUE)  # 再度分析を実行

#IRIの平均値など
  mean(eval_frm$eval,na.rm = TRUE)
  max(eval_frm$eval,na.rm = TRUE)
  min(eval_frm$eval,na.rm = TRUE)
  boxplot(eval_frm$eval)
  
  hist_eval <- data.frame()
  
  for (i in unique(eval_frm$id)) {
    hist_eval <- rbind(hist_eval,c(i,mean(eval_frm$eval[eval_frm$id==i],na.rm=TRUE)))
    
  }
  
  colnames(hist_eval) <- c("id","eval")
  hist_eval$eval <- as.numeric(hist_eval$eval)
  hist_eval$id <- as.numeric(hist_eval$id)  
  hist(hist_eval$eval,xlim = c(1, 5))  
  hist(hist_eval$id)
  