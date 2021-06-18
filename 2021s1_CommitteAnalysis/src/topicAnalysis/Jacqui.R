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


archi.df <- mydata[major=="Architecture, Building and planning", ]
archi.df
archi.relationship <- archi.df$你目前的感情状态是.........必填.
archi.major <- aechi.df$你的专业是.必填.
plot15 <- ggplot(data = archi.df, aes(x = archi.major,  fill = archi.relationship )) +
  geom_bar(width=0.25, position = position_dodge2(preserve = "single")) +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "感情状态")+
  xlab("")+
  ylab("人数")+
  theme_classic()
plot15


plot16 <- ggplot(data = mydata, aes(x = major,  fill = relationship)) +
  geom_bar(width=0.25, position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "感情状态")+
  theme(axis.text = element_text(size = 3))+
  xlab("")+
  ylab("人数占比")+
  coord_flip()+
  theme_classic()
plot16

report.df <- mydata[pressure.report=="5", ]
report.df
report.major <- report.df$你的专业是.必填.
report.major
plot17 <- ggplot(data = report.df, aes(report.major)) +
  geom_bar(width=0.25, fill="steelblue") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", vjust=0.5, hjust = -0.2, size = 3)+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "")+
  theme(axis.text = element_text(size = 3))+
  xlab("")+
  ylab("自评“学习压力极高”的人数")+
  coord_flip()+
  theme_classic()
plot17


eng.df <- mydata[major=="Engineering", ]
eng.df
eng.gender <- eng.df$你的性别是.必填.
eng.relationship <- eng.df$你目前的感情状态是.........必填.
eng.pressure <- eng.df$你会给自己目前感到的学习压力打几分..必填.
eng.fail <- eng.df$你在墨大有Fail过的经历吗..必填.
plot18 <- ggplot(data = eng.df, aes(x = eng.gender )) +
  geom_bar(width=0.2, fill="steelblue") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", vjust=-0.5, hjust = 0.5, size = 3)+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "")+
  xlab("")+
  ylab("人数")+
  ggtitle("工程专业性别分布")+
  theme_classic()
plot18

plot19 <- ggplot(data = eng.df, aes(x = eng.relationship )) +
  geom_bar(width=0.2, fill="steelblue") +
  stat_count(geom = "text", 
             aes(label = stat(count)), colour="black", vjust=-0.5, hjust = 0.5, size = 3)+
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "")+
  xlab("")+
  ylab("人数")+
  ggtitle("工程专业情感状态分布")+
  theme_classic()
plot19

slices2 <- c(2, 4) 
pct2 <- round(slices2 / sum(slices2)*100)
lbls2 <- paste(pct2, "%", sep ="") 
colors2 <- brewer.pal(3, "Set3")
pie(slices2,labels = lbls2, col = colors2, main = "工程专业学习压力自评分布", cex = 0.8)
legend(0.67, 1.05, c("3分","5分"), cex = 0.7, fill = colors2)


slices3 <- c(2, 4) 
pct3 <- round(slices2 / sum(slices2)*100)
lbls3 <- paste(pct2, "%", sep ="") 
colors2 <- brewer.pal(3, "Set3")
pie(slices2,labels = lbls2, col = colors2, main = "工程专业挂科人数分布", cex = 0.8)
legend(0.67, 1.05, c("有","没有"), cex = 0.7, fill = colors2)


