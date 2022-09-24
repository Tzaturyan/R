#
options(download.file.method="libcurl")

#install.packages("cluster")
library(cluster)
library(dplyr)


data()
?votes.repub
head(votes.repub)

str(votes.repub)


dim(votes.repub)

votes.repub[1:6,1:10]

length(colMeans(votes.repub))

colMeans(votes.repub, na.rm=TRUE)

# â 30-ì ñòîëáöå íåò ïðîïóùåííûõ çíà÷åíèé

votes.repub[,30]

if(mean(votes.repub[,30])>60){
  print("ðåñïóáëèêàíöû íàáðàëè âûñîêèé ïðîöåíò ãîëîñîâ")
}else{
  print("Ðåñïóáëèêàíöû íàáðàëè ìåíåå 60% ãîëîñîâ")
}


# â 7-ì ñòîëáöå åñòü ïðîïóùåííûå çíà÷åíèÿ

if(mean(votes.repub[,7], na.rm = TRUE)>60){
  print("ðåñïóáëèêàíöû íàáðàëè âûñîêèé ïîöåíò ãîëîñîâ")
} else{
  print("Ðåñïóáëèêàíöû íàáðàëè ìåíåå 60% ãîëîñîâ")
}

?ifelse()

x<-c(3,0,0,0,1,0)
x

ifelse(x!=0,"Yes","No")

#Â òîì ñëó÷àå ,åñëè ñðåäíåå çíà÷åíèå äîëè ãîëîñîâ, îòäàííûõ çà ðåñïóáëèêàíöåâ,
#ïî øòàòàì áîëåå 60, áóäåò âûâîäèòüñÿ ñîîáùåíèå : "Ðåñïóáëèêàíöû íàáðàëè ìåíåå 60% ãîëîñîâ",
#â ïðîòèâíîì ñëó÷àå "Ðåñïóáëèêàíöû íàáðàëè ìåíåå 60% ãîëîñîâ"

ifelse(colMeans(votes.repub,na.rm = TRUE)>60, 
       "ðåñïóáëèêàíöû íàáðàëè âûñîêèé ïðîöåíò ãîëîñîâ","Ðåñïóáëèêàíöû íàáðàëè ìåíåå 60% ãîëîñîâ")

# Ïîñòàâèì ïåðåä ñîáîé çàäà÷ó íàïèñàòü ôóíêöèþ,êîòîðàÿ áû ñ÷èòàëà, íà êàêóþ ñóììó ïðîäàíî ìÿ÷åé çà îïðåäåëåííûé äåíü
#Çäåñü æå ïîçíàêîìèìñÿ ñ ïàêåòîì lubridate, ïðåäíàçíà÷åííîãî äëÿ ðàáîòû ñ äàòàìè
# Âîñïîëüçóåìñÿ íàáîðîì «dat» ,ñîäåðæàùåãî äàííûå î ïðîäàæàõ â ïåðâûå äíè 2018 ãîäà 
# ÍÅ ÇÀÁÓÄÜÒÅ ÓÑÒÀÍÎÂÈÒÜ ÐÀÁÎ×ÓÞ ÄÈÐÅÊÒÎÐÈÞ,ÃÄÅ ËÅÆÈÒ ÍÓÆÍÛÉ ÔÀÉË sESSION->SET WORKING DIRECTORY->CHOOSE DIRECTORY

getwd()

dat<-read.csv("datn.csv")
#X -  íîìåð ïîêóïêè
# d.date – ãîä-ìåñÿö-äåíü
#ball – òåííèñíûå ìÿ÷è ïðîäàâàëèñü ïî îäíîìó, à òàêæå ðàñïàêîâàííûìè ïî 3 è 5 ìÿ÷åé
#price – öåíû

head(dat)

dim(dat)
str(dat)

dat$d.date

unique(dat$ball)

unique(dat$price)

dim(dat)
datn<-dat[,-1]
datn
head(datn,10)
str(datn)
unique(datn$ball)
unique(dat$price)

#install.packages("lubridate")
library(lubridate)

?lubridate

class(datn$d.date)
datn$d.date

dayn<-ymd(datn$d.date)
class(dayn)
dayn

year(dayn)
month(dayn)
day(dayn)

datn$price

datn$price[day(ymd(datn$d.date))==1]

sum(datn$price[day(ymd(datn$d.date))==1])

f.1<-function(d){
  sum(datn$price[day(ymd(datn$d.date))==d])
}

f.1(1)

f.1<-function(d){
  t<-sum(datn$price[day(ymd(datn$d.date))==d]) #çäåñü íåîáõîäèìî äîáàâèòü return(t)
  return(t)
   }
  
f.1(3)

sum(f.1(1),f.1(2),f.1(3))
sum(datn$price)

#íàïèøåì ôóíêöèþ, êîòîðàÿ áû ñóììèðîâàëà ïðîäàæè, íî òóò áóäóò åùå óêàçàí ìåñÿö

f.2<-function(m,d){sum(datn$price[month(ymd(datn$d.date))==m & day(ymd(datn$d.date))==d])}
f.2(1,1)
f.2(1,3)
#âòîðîãî ìåñÿöà â íàáîðå äàííûõ íåò
f.2(2,1)

# Ïîñ÷èòàåì ïðèáûëü ìàãàçèíà ïî äíÿì. Äëÿ ýòîãî ïîíàäîáèòñÿ öèêë for
f.1(1)
f.1(2)

for(i in 1:3){
  print(f.1(i))
}
# ðàññìîòðèì ôóíêöèè lapply() , sapply()

head(votes.repub)
?apply
class(votes.repub)

apply(votes.repub,1,mean)

?lapply

class(lapply(votes.repub,sum))

unlist(lapply(votes.repub,sum))

as.numeric(unlist(lapply(votes.repub,sum)))

sapply(votes.repub,sum)

class(sapply(votes.repub,sum))


## ôóíêöèÿ tapply() ïèìåíÿåò íóæíóþ ôóíêöèþ ê ãðóïïàì
#Êàê îíà pàáîòàåò áóäåò ïîêàçàíî íà ïðèìåðå äàòà ñýòà "Orange"
data()
head(Orange,17) # îêðóæíîñòü èçìåðÿëàñü â îäèíàêîâîì âîçðàñòå
?Orange
Orange
str(Orange)
#Íàáîð ñîäåðæèò òðè ïåðåìåííûå:
# - íîìåð äåðåâà;
# - âîçðàñò äåðåâà ñ 31.12.1968, â äíÿõ;
# - äëèíà îêðóæíîñòè ñòâîëà.

#Íàïèøåì ôóíêöèþ, êîòîðàÿ áóäåò ñ÷èòàòü íà ñêîëüêî èçìåíèëàñü îêóæíîñòü äåðåâà ñ 
# ïåðâîé äàòû çàìåðà(118-ûé äåíü) äî ïîñëåäíåé äàòû (1582 äåíü)
# ïðåæäå, ââåäåì ôóíêöèè range(), diff()

a<- c(1,2,3,5,8)
a
diff(a)

range(a)
min(a)
max(a)

diff(range(a))

l<-function(x){diff(range(x))} 

l(a)

?tapply

tapply(Orange$circumference,Orange$Tree,l) # 1,2 àðãóìåíòû ôóíêöèè äîëæíû áûòü îäèíàêîâîé äëèíû,
                                           #1-é -ýòî òî,÷òî ðàçáèâàåì ïî ïîäãðóïïà,2-é- ôàêòîð
###
search()



z<-c(1,2,3,4)

v<-c(0,82,2,8)

z %in% v

v%in%z

#Ñîõðàíÿåì íàáîð äàííûõ â ôîðìàòå csv

a<-c(1,2,3)
b<-c(0,0,0)

data.frame(a,b)

nm<-paste0(seq(1,3),"_","row")
nm
?data.frame
df.1<-data.frame(a,b, row.names = nm)

dim(df.1)

write.csv(df.1,file = "november.csv")

getwd()

df<-read.csv("november.csv")
class(df)

df
?read.csv

read.csv("cardio_train.csv")
head(read.csv("cardio_train.csv"))

head(read.csv("cardio_train.csv",sep=";"))

read.csv("cardio_train.csv",sep=";",nrow=5,row.names = paste0("row",seq(1,5))) #ñêîëüêî ñòðîê õîòèì ïðî÷èòàòü çàäàòü èìÿ ñòðîêè

read.csv("cardio_train.csv",sep=";",nrow=5,row.names =1)# â êà÷åñòâå èìåí ñòðîê ìîæíî è ñòîëáåö âÿçòü

read.csv("cardio_train.csv",sep=";",nrow=5,row.names = paste0("row",seq(1,5)), header= FALSE)

# Çàãðóçèì ïàêåò äëÿ ÷òåíèÿ äàííûõ â ôîìàòå excel

#install.packages("readxls")
library(readxl)

read_xls("book_1.xls")
?read_xls
#install.packages("rio")

library(rio)
?rio
#Ñðàâíèì áàçîâûå ôóíêöèè è ôóíêöèè ïàêåòà "ðèî"

head(import("cardio_train.csv"))
import("book_1.xls")
read.csv("book_1.xls") # îøèáêà!  ýòà ôóíêöèÿ íå ïðî÷èòàåò äðóãîé ôîðìàò
?tolower

version

