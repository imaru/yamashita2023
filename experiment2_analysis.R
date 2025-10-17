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

# 評価データに列名をつける
colnames(eval_frm) <- c('id', 'Q', 'eval')

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
js_data$lr <- NA
js_data$ang <- NA
js_data$lr[str_detect(js_data$stimu, 'left')] <- 'left'
js_data$lr[str_detect(js_data$stimu, 'right')] <- 'right'
js_data$ang[str_detect(js_data$stimu, '0')] <- 0
js_data$ang[str_detect(js_data$stimu, '45')] <- 45
js_data$ang[str_detect(js_data$stimu, '90')] <-  90
js_data$ang[str_detect(js_data$stimu, '135')] <-  135
js_data$ang[str_detect(js_data$stimu, '180')] <- 180
js_data$ang[str_detect(js_data$stimu, '225')] <- 225
js_data$ang[str_detect(js_data$stimu, '270')] <- 270
js_data$ang[str_detect(js_data$stimu, '315')] <- 315

#90パーセント以下の参加者のRTにNAを入れる
#for (i in unique(js_data$code)){
#  hit <- sum(js_data[js_data$code == i, ]$correct) / 64
#  if (hit < chit) {
#    js_data[js_data$code == i, ]$rt  <- NA
      
      
#  }}



#[1相関]Left条件の225度と、Right条件の135度の平均とEPT得点

#Left条件の225度と、Right条件の135度を参加者ごとに平均

R135R225 <- data.frame()
soukan1 <- data.frame()

for (i in unique(js_data$code)){
  R135R225 <- mean((js_data$rt[js_data$lr == 'right' & js_data$ang == '135' & js_data$code == i]), na.rm = TRUE)-
  mean((js_data$rt[js_data$lr == 'right' & js_data$ang == '225' &  js_data$code == i]), na.rm = TRUE)
 
      soukan1 <- rbind(soukan1, R135R225)
       }

# [1相関]データフレームの列の名前を変更
colnames(soukan1) <- c('R135-R225')

#[1相関]まとめの表を出力
print(soukan1)

#[1相関]評価データと統合する
soukan1　<- data.frame(cbind(soukan1$`R135-R225`, eval$eval))

colnames(soukan1) <- c('R135-R225','eval')

#[1相関]相関係数
cor(soukan1$`R135-R225`, soukan1$eval, use="complete.obs")

#[1相関]グラフを書く
plot(soukan1$`R135-R225`, soukan1$eval,ylim=c(28,140))

#L225-L135
L225L135 <- data.frame()
soukan2 <-data.frame()
for (i in unique(js_data$code)){
  L225L135 <- mean((js_data$rt[js_data$lr == 'left' & js_data$ang == '225' & js_data$code == i]),na.rm=TRUE)-
    mean((js_data$rt[js_data$lr == 'left' & js_data$ang == '135' &  js_data$code == i]),na.rm=TRUE)
  
  soukan2 <- rbind(soukan2, L225L135)
}

# [1相関]データフレームの列の名前を変更
colnames(soukan2) <- c('L225-L135')

#[1相関]まとめの表を出力
print(soukan2)

#[1相関]評価データと統合する
soukan2　<- data.frame(cbind(soukan2$`L225-L135`, eval$eval))

colnames(soukan2) <- c('L225-L135','eval')

#[1相関]相関係数
cor(soukan2$`L225-L135`, soukan2$eval, use="complete.obs")

#[1相関]グラフを書く
plot(soukan2$`L225-L135`, soukan2$eval,ylim=c(28,140))

#全体との相関を見る
zentai <- data.frame()
for (i in unique(js_data$code)){
  zentai <- rbind(zentai, mean(js_data$rt[js_data$code == i], na.rm = TRUE))
}
colnames(zentai) <- c('rt')
zentai　<- data.frame(cbind(zentai$rt, eval$eval))

colnames(zentai) <- c('meanrt','eval')
cor(zentai$meanrt, zentai$eval, use="complete.obs")
plot(zentai$meanrt, zentai$eval)

#[2分散]Left条件の135-225と、Right条件の225-135の差
#[2分散]参加者ごとのLeft条件の135-225
bunsan2 <- data.frame()

for (i in unique(js_data$code)){
 
    bunsan2 <- rbind(bunsan2, c(
     mean ((js_data$rt[js_data$lr == 'left' &
                        js_data$ang == '135' &
                        js_data$code == i ])-
                                              (js_data$rt[js_data$lr == 'left' &
                                                           js_data$ang == '225' &
                                                           js_data$code == i ]),na.rm=TRUE),
     mean ((js_data$rt[js_data$lr == 'right' &
                     js_data$ang == '225' &
                     js_data$code == i ])-
         (js_data$rt[js_data$lr == 'right' &
                       js_data$ang == '135' &
                       js_data$code == i ]),na.rm=TRUE)
      
      ))}

#[2分散]データフレームの列の名前を変更
colnames(bunsan2) <- c('L135-225','R225-135')

#[2分散]まとめの表を出力
print(bunsan2)

#[2分散]グラフ
boxplot(bunsan2)

#L135,L225,R135,R225の表を作る
bunsan <- cbind(js_data$rt[js_data$ang == "135" & js_data$lr == "left"], 
                js_data$rt[js_data$ang == "225" & js_data$lr == "left"],
                js_data$rt[js_data$ang == "135" & js_data$lr == "right"],
                js_data$rt[js_data$ang == "225" & js_data$lr == "right"]
                )

colnames(bunsan) <- c('L135', 'L225', 'R135', 'R225')

#[2分散]グラフ
boxplot(bunsan)
# せっかくなのでviolin plotも
violin <- data.frame(rbind(cbind(js_data$code[js_data$ang == "135" & js_data$lr == "left"],'Left','135',js_data$rt[js_data$ang == "135" & js_data$lr == "left"]),
                cbind(js_data$code[js_data$ang == "225" & js_data$lr == "left"],'Left','225',js_data$rt[js_data$ang == "225" & js_data$lr == "left"]),
                cbind(js_data$code[js_data$ang == "135" & js_data$lr == "right"],'Right','135',js_data$rt[js_data$ang == "135" & js_data$lr == "right"]),
                cbind(js_data$code[js_data$ang == "225" & js_data$lr == "right"],'Right','225',js_data$rt[js_data$ang == "225" & js_data$lr == "right"])))
colnames(violin) <- c('code','lr','cnd','rt')
cleaned_violin <- violin %>%
  filter(!is.na(rt))

cleaned_violin$code <- as.numeric(cleaned_violin$code)
cleaned_violin$rt <- as.numeric(cleaned_violin$rt)

library(ggplot2)
library(tidyr)
glr <-
  ggplot(cleaned_violin, aes(
    x = interaction(lr, cnd),
    y = rt,
    color = code
  )) + geom_violin() + geom_point()
plot(glr)

#[2分散]分散分析
source('anovakun_489.txt')
anovakun(bunsan,'sAB',2,2,peta = T)


# [3分散lr]lr indexとtoward indexを計算
js_data$lridx <- sin(js_data$ang * pi / 180) * js_data$rt

lr_c <- data.frame()

# [3分散lr]被験者ごと、左右ごとにindexの平均を計算してデータフレームに代入
for (i in unique(js_data$code)){
  
       lr_c <-
        rbind(lr_c, c(
          mean(js_data$lridx[js_data$lr == 'right' &
                               js_data$code == i], na.rm = TRUE),
          mean(js_data$lridx[js_data$lr == 'left' &
                               js_data$code == i], na.rm = TRUE)
        ))
          }

# [3分散lr]データフレームの列の名前を変更
colnames(lr_c) <- c('right', 'left')

# [3分散lr]まとめの表を出力
print(lr_c)

#[3分散]グラフ
boxplot(lr_c)


#[3分散]t検定
ttest <- t.test(lr_c$`right`,lr_c$`left`,use="complete.obs")
ttest

#角度と反応時間のグラフ
boxplot(data=js_data, rt~ang)
#right条件での角度と反応時間のグラフ
lr_right <- js_data[js_data$lr=='right',]
boxplot(data=lr_right, rt~ang)
#left条件での角度と反応時間のグラフ
lr_left <- js_data[js_data$lr=='left',]
boxplot(data=lr_left, rt~ang)

#csvファイルで読み込む
#write.csv(js_data, "js_data.csv")

#hit1 <- data.frame()
 #for (i in unique(js_data$code)){
  # hit1 <- rbind(hit1,sum(js_data[js_data$code == i, ]$correct) / 64)}

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

#PD相関係数
cor(zentai$meanrt, PD$eval, use="complete.obs")
#PDグラフを書く
plot(zentai$meanrt, PD$eval,ylim=c(7,35))

#EC相関係数
cor(zentai$meanrt, EC$eval, use="complete.obs")
#ECグラフを書く
plot(zentai$meanrt, EC$eval,ylim=c(7,35))

#PT相関係数
cor(zentai$meanrt, PT$eval, use="complete.obs")
#PTグラフを書く
plot(zentai$meanrt, PT$eval,ylim=c(7,35))

#FS相関係数
cor(zentai$meanrt, FS$eval, use="complete.obs")
#FSグラフを書く
plot(zentai$meanrt, FS$eval,ylim=c(7,35))

#Rightとやってみたら？
#PD相関係数
cor(soukan1$`R135-R225`, PD$eval, use="complete.obs")

#EC相関係数
cor(soukan1$`R135-R225`, EC$eval, use="complete.obs")

#PT相関係数
cor(soukan1$`R135-R225`, PT$eval, use="complete.obs")

#FS相関係数
cor(soukan1$`R135-R225`, FS$eval, use="complete.obs")

#leftとやってみたら？
#PD相関係数
cor(soukan2$`L225-L135`, PD$eval, use="complete.obs")

#EC相関係数
cor(soukan2$`L225-L135`, EC$eval, use="complete.obs")

#PT相関係数
cor(soukan2$`L225-L135`, PT$eval, use="complete.obs")

#FS相関係数
cor(soukan2$`L225-L135`, FS$eval, use="complete.obs")

#信頼係数を出す
  library(psych)

  # 回答者ごとに質問番号（QuestionID）を列として整形
  psyeval <- reshape(eval_frm, idvar = "id", timevar = "Q", direction = "wide")


  # 整形後のデータを確認
  #head(df_wide)
  psyeval <- psyeval[, !(colnames(psyeval) == "eval.20")]  # "eval.20"を除外
  alpha(psyeval[, -1], na.rm = TRUE)  # 再度分析を実行
  
#IRI得点の平均など
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
  hist(hist_eval$eval,xlim = c(1, 5))  
  