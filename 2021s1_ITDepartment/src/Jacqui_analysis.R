# data analysis by Jacqui

# install.packages('showtext') first 
library(showtext)
library(RColorBrewer)

# 提取数据
mydata = read.csv(file = "data.csv", header = TRUE);

showtext_auto()
time_abroad = mydata[1:33,6];
time_mel = mydata[1:33,7];
time_mel[8] = 3; time_mel[21] = 5.5;
no_language = mydata[1:33,10];
self_evaluation = mydata[1:33,12];
birthday_wish = mydata[1:33,23];
dob = mydata[1:33,4];
yob = substr(dob,1,4);

# 业务能力 vs 编程数量
plot(no_language,self_evaluation,pch = 19, xlab = "学习过的编程语言数量", ylab = "业务能力自评");

time_abroad = factor(time_abroad);
yob = factor(yob);
birthday_wish = factor(birthday_wish)
no_language = factor(no_language)
self_evaluation = factor(self_evaluation)
clean_data = data.frame(time_abroad = time_abroad, time_mel= time_mel, no_language = no_language, self_evaluation = self_evaluation, birthday_wish = birthday_wish, yob = yob);

# 海外念书 vs 人数
plot(clean_data$time_abroad)
title("2021 sem1 信息部开始海外念书时间",xlab = "开始海外念书时间", ylab = "人数")

# 出生年份 vs 人数
plot(clean_data$yob)
title("2021 sem1 信息部出生年份分布",xlab = "出生年份", ylab = "人数")

#自我评价 vs 小可爱祝福
count = table(birthday_wish, factor(self_evaluation))
barplot(count,col=c("darkblue","red"),legend = rownames(count))
title(ylab = "让小可爱送祝福人数", xlab = "自我评价")
yes = c(2,3,6,2,0);
no = c(4,5,6,4,1);
yes_prop = yes/(yes+no); no_prop = no/(yes+no);

# 业务能力自评 vs 人数比例
plot(levels(self_evaluation), yes_prop,pch = 19,col="red",xlab = "业务能力自评", ylab = "人数比例",ylim = c(0,1))
points(levels(self_evaluation), no_prop,pch=18, col="blue")
legend(1, 1, legend=c("是","否"),col=c("red", "blue"), pch =c(19,18),cex=0.8)
title("让小可爱送祝福人数比例与业务能力自评关系")

# 编程数量 vs 人数
count1 = table(self_evaluation,no_language)
barplot(count1,col=brewer.pal(5, "Set2") ,legend= rownames(count1),xlab = "会的编程语言数量", ylab = "人数")
title("能力自评与会的编程语言数量关系")

