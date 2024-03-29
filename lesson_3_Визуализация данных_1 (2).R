# ��� ����������� ��������� ��������� ����������,
# ���� �� ����������� �����, �� ������� ��������� ������ � ������� install.packages("��� ������")
library(rio)
library(dplyr)
# install.packages("rafalib")
library(rafalib)
# ��������� ������� � R � ������� import() �� ������ rio
dat<-import("cardio_train.csv")
head(dat)
# ����� ������� �������������� �������� � ������ ������ � ������� dim() � str()
dim(dat)

str(dat)

#� ������� age  ������� ����������� � ����, ��� �� ��������. 
#�������, ��� �������� �������������, ������� ����� �������, � ������� ������� �������� ����� � �����
?mutate

dat$age/365
trunc(dat$age/365) # �������� ������� �����
dat<-dat %>% mutate(age_years=(trunc(age/365))) # �-���  trunc() �������� ������� �����

#�������� ������� "age_years"
head(dat)
#� ���� �������� ����� ����������� ��� ���������� ��  ������
#�Cardiovascular Disease�: ������ � ������� �������� ��������
# �� ����� 70 000 ��������� ��� ������ �������� � ������� �������,
#��� ��� ������� ����������� �������������
mypar(1,2)
hist(dat$ap_hi)
hist(dat$ap_lo) # ������ ������� ��� ��, ���� �� �� ������� �������!
# ����� ������������� � ����� ������ ����� ���� ��� ������ �������������� � �����.����������
# �������� ����� ��� ������� ���� ��,���� �� ������������� ��������� �����������, ��� �� �����������
mean(dat$ap_hi)
sd(dat$ap_hi)

mean(dat$ap_lo)
sd(dat$ap_lo) # �� ��������� ������� ����������� ����������,�������������,�  ������� ���������

# boxplot �������� ����� �� ����� ����������� ������� ������������� ������
#������� ��������� �� ��� �������� ��������� ��������

box_lo<-boxplot(dat$ap_lo)
box_hi<-boxplot(dat$ap_hi) # ����� �������, ������� ������ �� sd � mean
# ��������� ������� � ���,��� ������ � ���������


# �������� ��������� ��� ����� ��������,������� ���� ���� ������� �����
box_lo<-boxplot(dat$ap_lo[dat$ap_lo<200 & dat$ap_lo>20])
title("������ ��������")
box_hi<-boxplot(dat$ap_hi[dat$ap_hi<300 & dat$ap_hi>40])
title("������� ��������")



min(dat$ap_hi[dat$ap_hi<300&dat$ap_hi>40]) #��� �������� �������� ������� ������� �/����� 70 �� 140

min(dat$ap_hi)
# ����� � ����� �������� ������������ �� �������� ��������
#��������, � ��� ���� ������ � ������������� ������, ������� ������ �� ����� � ��� ��������
dat$ap_hi[dat$ap_hi<0]

# �������� �������� ����������, ������� ����� ��������
#��� ��������
# ����� ������� � ��� ��������

median(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])
quantile(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20],0.25)

# ��� �������� ��������
sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])
length(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])

68994*25/100
ind_p25<-trunc(68994*25/100)+1
ind_p25

sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[ind_p25]

#����������, ������ 1� �������� ��������� � ��������
68994/2
sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[c(34497,34498)]

sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[17249:34498]

tail(sort(dat$ap_lo[dat$ap_lo<200&dat$ap_lo>20])[17249:34498], 20)

#������������� ��������  filter(), %>%  �� ������ dplyr ,����� ����������� ������� ��� ��������� ������ �����

tidy_set <-dat %>% filter((ap_lo<200&ap_lo>20) & (ap_hi<300&ap_hi>40))

head(tidy_set)
#������� ����� �������� ���������� � 1-� � ����� ��������
dim(dat)
dim(tidy_set)
nrow(dat)
ncol(dat)
#������� ������ � ����� ����������� ���������� � ������� ��������������
# ������� ��������
mean(tidy_set$ap_hi)
mean(dat$ap_hi)
sd(tidy_set$ap_hi)
sd(dat$ap_hi)
#������ ��������
mean(tidy_set$ap_lo)
mean(dat$ap_lo)
sd(tidy_set$ap_lo)
sd(dat$ap_lo)

#����� ��������� ��������� ������ �������� �� ������ � ������� �����������
mypar(2,2)

hist(dat$ap_lo, main="������ ��������", xlab = "aplo_dat", ylab="�������")
hist(tidy_set$ap_lo, main="������ ��������.new", xlab = "aplo_tidy", ylab="�������")

hist(tidy_set$ap_hi, main="������� ��������.new", xlab = "aphi_tidy", ylab="�������")
hist(dat$ap_hi, main="������� ��������", xlab = "aphi_dat", ylab="�������")


# QQ-������ ��������� ��������� ������ �� ������������
#� ������ ����� ���� �������� ������������� �������� � ���������� ��������� ��������
mypar(1,2)
qqnorm(tidy_set$ap_lo, main="������ ��������.tidy")
qqline(tidy_set$ap_lo, col="red", lwd=2)

qqnorm(tidy_set$ap_hi, main="������� ��������.tidy")
qqline(tidy_set$ap_hi, col="red", lwd=4)
abline(h=160, col="green")
#�����: �� ������� �������� :������� � ������ �������� ����� ������� 
# ������ � ����� ��������������,��� �������������� ���������� ��������������
#�� �������� ��������

#������� ������� � ������ �������� �� ����
mypar(1,2)
head(tidy_set)
groupss_lo<-split(tidy_set$ap_lo,tidy_set$gender)
str(groupss_lo)
boxplot(groupss_lo)
title("������ ��������")

groupss_hi<-split(tidy_set$ap_hi,tidy_set$gender)
str(groupss_hi)
boxplot(groupss_hi)
title("������� ��������")

#�������� � ������������ ������� "������� ��������" � "������ ��������" ���
#����� ���� ��������

plot(density(tidy_set$ap_lo), col=1, lwd=2, main="������ ��������")
plot(density(tidy_set$ap_hi), col=3, lwd=2, main="������� ��������")

#���������� � �������� �����
mypar(1,1)
plot(density(tidy_set$ap_lo), col="1", lwd=2, main="������ ��������")
abline(v=70,col="red")
abline(v=60,col="red")
abline(v=80,col="red")
sort_lo<-sort(tidy_set$ap_lo)
cut_1<-sort_lo[sort_lo>65&sort_lo<75]
cut_1
tail(cut_1,20)

#��� ���������� ���������� �������������?
plot(density(tidy_set$ap_lo, adjust = 100),col=1, lwd=2, main="������ ��������")

#��� ������� �� �����
plot(density(tidy_set$ap_lo, adjust = 10),col=1, lwd=2, main="������ ������������ ��������")
lines(density(tidy_set$ap_hi,adjust = 10),col=3, lwd=2, lty=2)
legend("topright",c("ap_lo","ap_hi"), col=c(1,3), lty = c(1,2))
#�����������
plot(tidy_set$ap_hi,tidy_set$ap_lo)

#������� ���������� �� ������

plot(tidy_set$ap_hi,tidy_set$ap_lo, pch=21,
     bg= as.numeric(factor(tidy_set$gender)), xlab = "������� ��������", ylab= "������ ��������")
legend("topright", levels(factor(tidy_set$gender)),col=seq(along=levels(factor(tidy_set$gender))), pch=19,cex=1.1)
# ���������� �����

plot(tidy_set$weight,tidy_set$height, pch=21,
     bg= as.numeric(factor(tidy_set$gender)), xlab = "���", ylab= "����")
legend("topright", levels(factor(tidy_set$gender)),col=seq(along=levels(factor(tidy_set$gender))), pch=19,cex=1.1)
cor()

# ���������� �����
head(tidy_set)
miniset<-tidy_set[,3:5]
head(miniset)
nrow(miniset)
plot(miniset,pch=21, bg=miniset$gender)
plot(miniset$gender,miniset$weight, pch=21, bg=miniset$gender)

colnames(tidy_set)
mini_set1<-tidy_set[,c(3,6,7)]
head(mini_set1)
plot(mini_set1,pch=21, bg=miniset$gender)

########
plot(miniset$gender,miniset$height,pch=21,bg=miniset$gender)
######The End


