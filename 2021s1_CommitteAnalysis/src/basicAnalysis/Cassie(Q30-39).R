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

## play gok or not (gender based)
plot1 <- ggplot(data = mydata, aes(x = play.gok, fill = gender)) +
  geom_bar(stat="count", width = 0.25) +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", size = 2.5)+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "性别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
  ##ggtitle("是否玩王者荣耀")+
  ##theme(plot.title = element_text(hjust = 0.5, size = 10))
plot1

## play gok or not (type of department based)
plot11 <- ggplot(data = mydata, aes(x = play.gok,  fill = department.type)) +
  geom_bar(width=0.8, position = "dodge") +
  labs(fill = "部门类别")+
  scale_fill_brewer(palette = "Set3") +
  xlab("")+
  ylab("人数")+
  theme_minimal()
plot11

## game level of gok (dotnut chart)
goklevel.df <- as.data.frame(table(gok.level))
goklevel.df <- goklevel.df[-1,]
goklevel.df$fraction = goklevel.df$Freq / sum(goklevel.df$Freq)
goklevel.df$ymax = cumsum(goklevel.df$fraction)
goklevel.df$ymin = c(0, head(goklevel.df$ymax, n=-1))
goklevel.df$labelPosition <- (goklevel.df$ymax + goklevel.df$ymin) / 2
goklevel.df
goklevel.df$label <- paste0(goklevel.df$gok.level, "\n 人数: ", goklevel.df$Freq)
plot2 <- ggplot(goklevel.df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gok.level)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=2.2) +
  scale_fill_brewer(palette="Pastel1") +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ##ggtitle("王者荣耀段位统计")+
  theme(plot.title = element_text(hjust = 0.5))
plot2

##grid.arrange(plot1, plot11, plot2, nrow = 3)
##grid.arrange(plot2, arrangeGrob(plot1, plot11, nrow=2), nrow = 1) change font size

## play fps or not
plot3 <- ggplot(data = mydata, aes(x = play.fps ,  fill = gender)) +
  geom_bar(width=0.3) +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", size = 2.5)+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "性别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
  
#ggtitle("是否玩王者荣耀")+
#theme(plot.title = element_text(hjust = 0.5))
plot3

## game level of fps (pie chart)
myPalette <- brewer.pal(5,"Pastel1") 
slices <- c(10, 29, 8) 
pct <- round(slices / sum(slices)*100)
lbls <- paste(pct, "%", sep ="") # ad % to labels 
#colors <- brewer.pal(3, "Pastel1") 
pie(slices, labels = lbls, col = myPalette, main = "", cex = 0.7)
legend("topright", c("我是PFS大神", "穷吃鸡富快递", "落地成盒"), cex = 0.55, fill = myPalette)

## play ball game (type of department based)
plot4 <- ggplot(data = mydata, aes(x = play.ballgame,  fill = department.type)) +
  geom_bar(width=0.7, position = "dodge") +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "部门类别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
  
#ggtitle("是否喜欢球类运动")+
#theme(plot.title = element_text(hjust = 0.5))
plot4

## play ball game (gender based)
plot5 <- ggplot(data = mydata, aes(x = play.ballgame,  fill = gender)) +
  geom_bar(width=0.3) +
  scale_fill_brewer(palette = "Set2") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", size = 2.5)+
  labs(fill = "性别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
#ggtitle("是否喜欢球类运动")+
#theme(plot.title = element_text(hjust = 0.5))
plot5

## favorite ball game type
type <- c("足球", "篮球", "羽毛球","网球","乒乓球","澳式足球","棒球","排球", "高尔夫","多人运动","保龄球","桌球")
count <- c(16, 33, 51, 16, 22, 3, 2, 3, 1, 1, 2, 1)

ballgame.df <- data.frame(type, count)
plot6 <- ggplot(ballgame.df, aes(reorder(type, count),count)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.7)+
  geom_text(aes(label=count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
  ##theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ##geom_text(aes(label=count), vjust=-0.3, size=3.5)+
  ##theme_minimal()
plot6

## watch TV series (gender based)
plot7 <- ggplot(data = mydata, aes(x = watch.TVseries,  fill = gender)) +
  geom_bar(width=0.3) +
  scale_fill_brewer(palette = "Set2") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", size = 2.5)+
  
  labs(fill = "性别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
#ggtitle("是否喜欢球类运动")+
#theme(plot.title = element_text(hjust = 0.5))
plot7

## watch TV series (type of department based)
plot8 <- ggplot(data = mydata, aes(x = watch.TVseries,  fill = department.type)) +
  geom_bar(width=0.8, position = "dodge") +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "部门类别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
#ggtitle("是否喜欢球类运动")+
#theme(plot.title = element_text(hjust = 0.5))
plot8

## TV series type (need to reorder)
TVseries.type <- c("动漫","韩剧","美剧", "国产剧","英剧","日剧","都看")
TVseries.count <- c(32, 4, 8, 5, 1, 3, 16)
TVseries.df <- data.frame(TVseries.type, TVseries.count)
plot9 <- ggplot(TVseries.df, aes(reorder(TVseries.type, TVseries.count),TVseries.count)) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=TVseries.count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
plot9

##
anime.type <- c("热血番", "恋爱番", "悬疑番","泡面番","运动番","推理番","里番")
anime.count <- c(10, 2, 3, 5, 1, 3, 1)
anime.df <- data.frame(anime.type, anime.count)
plot10 <- ggplot(anime.df, aes(reorder(anime.type,anime.count),anime.count )) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=anime.count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
plot10

## like music
plot12 <- ggplot(data = mydata, aes(x = music ,  fill = gender)) +
  geom_bar(width=0.3) +
  scale_fill_brewer(palette = "Set2") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", size = 2.5)+
  labs(fill = "性别")+
  xlab("")+
  ylab("人数")+
  theme_minimal()
plot12


## music type
#music.type <- c("英文", "中文", "Jpop","Kpop","都可")
music.count <- c(3, 5, 3, 6, 33)
myPalette <- brewer.pal(5, "Set2") 
slices <- c(3, 5, 3, 6, 33) 
pct <- round(slices / sum(slices)*100)
lbls <- paste(pct, "%", sep ="") # ad % to labels 
pie(slices, labels = lbls, col = myPalette, main = "", cex = 0.7)
legend("topright", c("英文", "中文", "Jpop","Kpop","都可"), cex = 0.7, fill = myPalette)

## specific type
music.type <- c("摇滚", "流行", "纯音乐","国风","说唱", "民谣", "R&b", "古典", "爵士", "日漫oped")
music.count <- c(5, 23, 3, 1, 6, 6, 6, 3, 2, 4) 
music.df <- data.frame(music.type, music.count)
plot13 <- ggplot(music.df, aes(reorder(music.type,music.count),music.count )) +
  geom_bar(stat="identity", fill="steelblue", width = 0.5)+
  geom_text(aes(label=music.count), vjust=0.5, hjust = -0.2, size = 3) +
  xlab("")+
  ylab("人数")+
  coord_flip()+
  theme_minimal()
plot13

