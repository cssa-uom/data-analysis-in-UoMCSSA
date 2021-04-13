##data analysis by Cassie

#install.packages("ggplot2")
library(MASS)
library(lattice)
library(ggplot2)
library(scales)
library(forcats)
library(png)
library(RColorBrewer)
library(plyr)

my_data <- read.csv("data.csv")
my_data

course_level <- my_data[, 1]
gender <- my_data[, 5]
major <- my_data[, 2]
stayed.year <- my_data[, 7]
languages <- my_data[, 10]
birthdate <- my_data[, 4]
relationship <- my_data[, 9]
game <- my_data[, 13]
stayed.year <- my_data[, 7]

table(game)
table(relationship)
table(birthdate)
table(languages)
table(stayed.year)
table(gender)
table(course_level)
table(major)
table(stayed.year)

##course level pie chart 
slices <- c(20, 13) 
pct <- round(slices / sum(slices)*100)
lbls <- paste(pct, "%", sep ="") # ad % to labels 
colors <- brewer.pal(3, "Pastel1") 
pie(slices, labels = lbls, col = colors, main = "Pie Chart of course level", cex = 0.8)
legend("topright", c("undergraduate", "master"), cex = 0.7, fill = colors)

##relationship status pie chart 
slices2 <- c(21, 11, 1) 
pct2 <- round(slices2 / sum(slices2)*100)
lbls2 <- paste(pct2, "%", sep ="") 
colors2 <- brewer.pal(3, "Set3")
pie(slices2,labels = lbls2, col = colors2, main = "Pie Chart of relationship", cex = 0.8)
legend(0.67, 1.05, c("guguagugua","bugubugu","something's going"), cex = 0.7, fill = colors2)

##gender vs course level
plot1 <- ggplot(my_data, aes(x = gender, fill = course_level))+
  geom_bar(width = 0.3)+
  theme_bw()+
  labs(y = "Number of members", title = "Gender vs Course level")
plot1

##major vs course level
new.table <- sort(table(major),decreasing = TRUE)
major.df <- as.data.frame(new.table)
plot2 <- ggplot(major.df,  aes(x = major, y = Freq, fill = major))+
  #geom_histogram(aes(y = Freq), binwidth = 0.6, stat="identity")+
  geom_bar(stat="identity")+
  scale_fill_manual( values = c("BSc-Data Science"="#9EBCDA","BSc-Computing and Software Systems"="#9EBCDA","Bachelor of Design - Computing"="#9EBCDA","M-IT-Computing"="#8C6BB1","Master of Electrical Engineering"="#8C6BB1","MSc-Mathematics and Statistics"="#8C6BB1","M-IT-Cyber Security"="#8C6BB1", "M-IT-Artificial Intelligence"="#8C6BB1", "M Arts and cultural  management"="#8C6BB1", "M-IT-Distributed Computing" = "#8C6BB1", "MC-CS-Computer Science"="#8C6BB1", "MC-ENG-Software"="#8C6BB1", "other"="#9EBCDA"), guide = FALSE)+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
  labs(title = "Major course")+
  xlab("")+ylab("count")
plot2

##stayed years vs frequency
mu <- ddply(my_data, "gender", summarise, grp.mean = mean(stayed.year))
plot3 <-ggplot(my_data, aes(x = stayed.year,  fill = gender, color = gender)) +
  geom_histogram(position = "identity", alpha = 0.2, bins = 15)+
  theme(legend.position = "top")+
  geom_vline(data = mu, aes(xintercept = grp.mean),linetype = "dashed",size = 1, color = c("red","blue"))+
  labs(title = "year stayed histogram plot",x = "Number of years stayed in Melbourne", y = "Count")+
  geom_text(aes(x = 4.4, label = "mean line", y = 5.5), colour = "black", angle = 0) +
  theme_classic()
plot3

## stayed year vs gender
gy.df <- my_data[c(5,7)]
names(gy.df)[1] <- "sex"
names(gy.df)[2] <- "year"
#gy.df
mu.group <- ddply(gy.df, "sex", summarise, grp.mean = mean(year))
plot4 <-ggplot(gy.df, aes(x = year)) +
  geom_histogram(color = "black", fill = "lightblue",binwidth = 0.5)+
  facet_grid(sex~ .)+
  geom_vline(data = mu.group, aes(xintercept = grp.mean), color = "red", linetype = "dashed", size = 1)+
  geom_text(aes(x = 3.65, label = "mean line", y = 3), colour = "red", angle = 0) +
  labs(title = "year stayed histogram plot",x = "Number of years stayed in Melbourne", y = "Count")
plot4

##game vs frequency
game.table <- sort(table(game),decreasing = FALSE)
game.df <- as.data.frame(game.table)
#game.df
plot5 <- ggplot(game.df, aes(x = game, y = Freq)) + 
  geom_bar(color = "black",fill = "lightblue", alpha = 0.8, stat ="identity", width = 0.65) +
  theme_minimal() +
  theme(plot.title = element_text( hjust = 0.5, vjust = 0.7, face='bold'))+
  labs(0.3,title ="Popular type of game", y = "")+
  geom_text(aes(x = game, y = Freq, label = percent(Freq/25), hjust = 0.45, vjust = -0.27), position = position_dodge(width=0.5))+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(axis.text.x=element_text(angle=70,hjust=1)) 
plot5

