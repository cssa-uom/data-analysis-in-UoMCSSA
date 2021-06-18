library(showtext)
library(MASS)
library(lattice)
library(ggplot2)
library(scales)
library(forcats)
library(png)
library(RColorBrewer)
library(plyr)
library(gridExtra)
showtext_auto()

mydata = read.csv("final.csv")
mydata

gender <- mydata[, 1]
department.type <- mydata[, 6]
play.gok <- mydata[, 30]
gok.level <- mydata[, 31]
play.fps <- mydata[, 32]
fps.level <- mydata[, 33]
play.ballgame <- mydata[, 34]
ballgame.type <- mydata[, 35]
watch.TVseries <- mydata[, 36]
TVseries.type <- mydata[, 37]
music <- mydata[, 38]
music.type <- mydata[, 39]
sleep.time <- mydata[, 28]
major <- mydata[, 8]
location <- mydata[, 5]
relationship <- mydata[, 9]
course.level <- mydata[, 7]
pressure.report <- mydata[, 21]
lib <- mydata[, 25]
shortsight <- mydata[, 11]

table(gender)
table(department.type)
table(play.gok)
table(gok.level)
table(play.fps)
table(fps.level)
table(play.ballgame)
table(ballgame.type)
table(watch.TVseries)
table(TVseries.type)
table(music)
table(music.type)
table(sleep.time)
table(major)

## gender vs sleep time
plot1 <- ggplot(data = mydata, aes(x = gender ,  fill = sleep.time)) +
  geom_bar(width=0.25, position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="black")+
  labs(fill = "入睡时间")+
  xlab("性别")+
  ylab("人数占比")+
  theme_minimal()
plot1

## gender pie chart 
slices <- c(1, 87, 53) 
pct <- round(slices / sum(slices)*100)
lbls <- paste(pct, "%", sep ="") # ad % to labels 
colors <- brewer.pal(3, "Pastel2") 
pie(slices, labels = lbls, col = colors, cex = 0.8)
legend("topright", c("其他", "女", "男"), cex = 0.7, fill = colors)

## major vs sleep time
plot2 <- ggplot(data = mydata, aes(x = major ,  fill = sleep.time)) +
  geom_bar(width=0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_classic()
plot2

## course level vs sleep time
plot5 <- ggplot(data = mydata, aes(x = course.level ,  fill = sleep.time)) +
  geom_bar(width=0.25) +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black")+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("")+
  ylab("人数")+
  theme_classic()
plot5

## pressure vs sleep time
plot6 <- ggplot(data = mydata, aes(x = pressure.report ,  fill = sleep.time)) +
  geom_bar(width=0.35) +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black")+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("学习压力评分")+
  ylab("人数")+
  theme_classic()
plot6

## relationship status vs sleep time 
plot4 <- ggplot(data = mydata, aes(x = relationship ,  fill = sleep.time)) +
  geom_bar(width=0.25) +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black")+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("情感状态")+
  ylab("人数")+
  theme_classic()
plot4

## department type vs sleep time 
plot7 <- ggplot(data = mydata, aes(x = department.type  ,  fill = sleep.time )) +
  geom_bar(width=0.25) +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_classic()
plot7

## ball game type vs sleep after 12
type <- c("足球", "篮球", "羽毛球","网球","乒乓球","澳式足球","棒球","排球", "高尔夫","多人运动","保龄球","桌球")
count <- c(13, 30, 44, 10, 17, 2, 2, 2, 1, 1, 2, 1)

ballgame.df <- data.frame(type, count)
plot8 <- ggplot(ballgame.df, aes(reorder(type, count),count)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.55)+
  geom_text(aes(label=count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
plot8

## anime/TV series type vs sleep after 12
TVseries.type <- c("动漫","韩剧","美剧", "国产剧","日剧","都看")
TVseries.count <- c(31, 4, 7, 5, 3, 13)
TVseries.df <- data.frame(TVseries.type, TVseries.count)
plot9 <- ggplot(TVseries.df, aes(reorder(TVseries.type, TVseries.count),TVseries.count)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.35)+
  geom_text(aes(label=TVseries.count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
plot9

## music type vs sleep after 12
music.type <- c("摇滚", "流行", "纯音乐","国风","说唱", "民谣", "R&b", "古典", "爵士", "日漫oped")
music.count <- c(4, 17, 3, 1, 6, 5, 5, 3, 1, 3) 
music.df <- data.frame(music.type, music.count)
plot10 <- ggplot(music.df, aes(reorder(music.type,music.count),music.count )) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=music.count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
plot10

## like music vs sleep after 12
plot11 <- ggplot(data = mydata, aes(x = music  ,  fill = sleep.time )) +
  geom_bar(width=0.25) +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("是否喜欢听音乐")+
  ylab("人数")+
  theme_classic()
plot11

## like sport vs sleep after 12
plot12 <- ggplot(data = mydata, aes(x = play.ballgame  ,  fill = sleep.time )) +
  geom_bar(width=0.25) +
  scale_fill_brewer(palette = "Set2") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black")+
  labs(fill = "入睡时间")+
  xlab("是否喜欢球类运动")+
  ylab("人数")+
  theme_classic()
plot12

## like watch anime/TV series vs sleep after 12
plot13 <- ggplot(data = mydata, aes(x = watch.TVseries  ,  fill = sleep.time )) +
  geom_bar(width=0.25) +
  scale_fill_brewer(palette = "Set2") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black")+
  labs(fill = "入睡时间")+
  xlab("是否喜欢看番剧")+
  ylab("人数")+
  theme_classic()
plot13

## like play gok vs sleep after 12
plot14 <- ggplot(data = mydata, aes(x = play.gok  ,  fill = sleep.time )) +
  geom_bar(width=0.25) +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black")+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "入睡时间")+
  xlab("是否喜欢玩王者荣耀")+
  ylab("人数")+
  theme_classic()
plot14

