
rm(list = ls())

# R oturumunu UTF-8'e ayarla
Sys.setlocale("LC_CTYPE", "tr_TR.UTF-8")


library(readxl)  
veri<- read_excel("C:/Users/HANDENUR/Downloads/zamanserisi-veri.xlsx")
View(veri)

install.packages("fpp")
install.packages("forecast")

library(fpp)
library(forecast)

#zaman serisi olarak tanimlayalim:
veri_ts <- ts(veri)
#zaman serisi grafigi cizelim.
ts.plot(veri_ts,gpars=list(xlab="Zaman", ylab="Basel hava durumu"))
#ACF ve PACF grafiklerini cizdirelim:
#ggAcf(veri_ts, lag.max = 76)
Acf(veri_ts, lag.max = 76,  ylim=c(-1,1), lwd=1)
Pacf(veri_ts, lag.max = 72, ylim=c(-1,1), lwd=1)



#trent mevcut. s??cakl??k serisinin trent bileseni regresyon analizi yardimi ile olusturulur.
veri_trent<-tslm(veri_ts~trend)
#fitted values=orijinal serinin trent bileseni
periyot<- veri_ts-veri_trent[["fitted.values"]]  #serinin periyoduna sahip mevsimsel bilesen serisi
#veya
#periyot<- veri_ts-veri_trent$fitted.values
#serinin periyoduna sahip mevsimsel bilesen serisinin ACF grafiginden periyodu bulalim:
Acf(periyot,lag.max = 96,  ylim=c(-1,1), lwd=1)

#1.fark alarak periyodu g??relim.
acf(diff(veri_ts),lag.max = 42,ylim=c(-1,1),lwd=2)  #periyot:12
#veri_ts periyot bilgiside girilsin
veri_ts <- ts(veri,frequency = 12)

#duraganlastirma-fark alma 
fark1<-(diff(veri_ts))
acf(fark1,lag.max = 64 ,ylim=c(-1,1),lwd=2) #1.derece fark


#AYRISTIRMA YONTEMLERI - #TOPLAMSAL
#merkezsel hareketli ortalama serisi 
veri1_ts<- ma(veri_ts, order =12, centre = TRUE)  
#order: germe sayisidir. periyot degeDeri yaz1labilir
#germe sayisi arttikca seri duzlesir.
#Mevsimsel bilesenin bulunusu (hata terimi de mevcut)
mevsim<- veri_ts-veri1_ts

#Mevsim serisinin ortalamalari
donem_ort<-t(matrix(data=mevsim, nrow = 12))
colMeans(donem_ort, na.rm = T)
sum(colMeans(donem_ort, na.rm = T))
mean(colMeans(donem_ort, na.rm = T))

#mevsimsel endeks degerlerinin bulunusu
endeks<- colMeans(donem_ort, na.rm = T)-mean(colMeans(donem_ort, na.rm = T))
#endeks degerlerini seri boyunca yazdirma islemi
indeks<-  matrix(data = endeks, nrow = 96 , byrow = TRUE)


#extra olarak indeks degerlerini seri boyunca yazdirmak istersek,
#indeks_alternatif<- decompose(Tuketim_ts, "additive") kodu kullanilir.

#trent bileseni bulal1m (hata terimi de mevcut)
trenthata<- veri_ts-indeks

#seriyi hatadan arindirmak icin trenthata serisine dogrusal regresyon islemi uygulanir.
trent<-tslm(trenthata~trend)
#ciktida yer alan fitted values orijinal serinin trent bilesenidir. 
#tahmin serisini bulalim: (mevsimsel endeks+saf trent serisi)
tahmin<- indeks+trent[["fitted.values"]]
#hata serisini bulalim:
hata<- veri_ts-indeks-trent[["fitted.values"]]

######Modelin Guvenilirligi######
#Toplamsal modelin ele alinan seri uzerinde gecerli bir model olup olmadigini kontrol edelim
#(yani tahminleri guvenilir mi???)

#orijinal seri ile tahmin serisinin uyumu
plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2, ylim=c(19,320))
lines( window(tahmin) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(veri )),
                   expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c(4,2))

#hatalar akgurultu mu?
Acf(hata, main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2)
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2)

#Ho:hatalar aras??nda iliski yoktur. 
Box.test(hata, lag = 42, type = "Ljung") #p<0.05 H0 red.

#CARPiMSAL AYRI??TIRMA Y??NTEM??###

#mevsimsel bileseni bulunmasi (Zt/MHO) (hata terimi de mevcut)
mevsim2 <- veri_ts/veri1_ts
#her bir periyot icin ortalama degerlerinin hesabi
donemort2<-t(matrix(data=mevsim2, nrow = 12))
colMeans(donemort2, na.rm = T)
sum(colMeans(donemort2, na.rm = T))
#ortalamalarin ortalamasi
mean(colMeans(donemort2, na.rm = T))
#mevsimsel endeks degerlerinin bulunusu
endeks2<- colMeans(donemort2, na.rm = T)/mean(colMeans(donemort2, na.rm = T))
endeks2
#endeks degerlerini seri boyunca yazdirma islemi
indeks2<-  matrix(data = endeks2, nrow = 96)
indeks2
#extra olarak indeks degerlerini seri boyunca yazdirmak istersek,
#indeks2_alternatif1<- decompose(veri_ts, type="multiplicative") kodu kullanilir.

#trent serisi (hata da mevcut) (orijinal seri/mevsimsel endeks serisi)
trenthata2<- veri_ts/indeks2

#hatadan arindirma islemi
trent2<- tslm(trenthata2~trend)
tahmin2<- indeks2*trent2[["fitted.values"]] #tahmin=endeks*orijinal serinin trent bileseni

#hata serisi
hata2<- veri_ts-tahmin2


#orijinal seri ile tahmin serisinin uyumu
plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin2) ,lty=3,col=2 ,lwd=2)
legend("topleft",c(expression(paste(veri )),
                   expression(paste(tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c("blue","pink"))

#hatalar akgurultu mu?
Acf(hata2,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2)
Pacf(hata2,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2)

#Ho:hatalar aras??nda iliski yoktur. 
Box.test(hata2, lag = 42, type = "Ljung") #p<0.05 H0 red.

###REGRESYON ANAL??Z??####
install.packages("FitAR")
library(fpp)
library(stats)
library(forecast)
library(FitAR)

#toplamsal
#96 gOzlemim var.
t<-1:1:96  #t terimini olusturalim.1'den 96'ya

sin1<-sin(2*3.1416*t/12)
cos1<-cos(2*3.1416*t/12)

veri_df<-as.data.frame(cbind(veri, t, sin1, cos1))

names(veri_df)<- c("y", "t", "sin1", "cos1")
attach(veri_df)

regresyon.model1<-lm(y~t+sin1+cos1) 
summary(regresyon.model1)

##durbin-watson testi
dwtest(y~t+sin1+cos1)

#katsayilar anlamsiz. modele devam edilmez.

#carpimsal

s1<-t*sin(2*3.1416*t/12)
c1<-t*cos(2*3.1416*t/12)


veri_df2<-as.data.frame(cbind(veri, t, s1, c1))

names(veri_df2)<- c("y", "t", "s1", "c1")
attach(veri_df2)

regresyon.model2<-lm(y~t+s1+c1)
summary(regresyon.model2)
##durbin-watson testi
dwtest(y~t+s1+c1)


#USTEL DUZLEST??RME
#TOPLAMSAL WINTERS MODEL?? 

Winters1<- ets(veri_ts, model = "AAA")
summary(Winters1)

#l=ortalama duzeyin baslangic degeri
#b=egimin bas.degeri
#mevsimsel terimin baslangD1c degerleri s

tahmin_winter<- Winters1[["fitted"]]

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_winter) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Orjinalseri)),
                   expression(paste(Winters1Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

hata_winters<-Winters1[["residuals"]]

Acf(hata_winters,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2)
Pacf(hata_winters,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2)

Box.test(hata_winters, lag = 52, type = "Ljung")


#CARPIMSAL WINTERS
Winters2<- ets(veri_ts, model ="MAM")
summary(Winters2)

tahmin_winter2<- Winters2[["fitted"]]

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_winter2) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Orjinalseri)),
                   expression(paste(Winters2Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

hata_winters2<-Winters2[["residuals"]]

Acf(hata_winters2,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2)
Pacf(hata_winters2,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2)

Box.test(hata_winters2, lag = 52, type = "Ljung")

### box-jenkins
fark1<-(diff(veri_ts))

acf(fark1,lag.max = 64 ,ylim=c(-1,1),lwd=2) #1.derece mevsimsel ve tret fark
pacf(fark1,lag.max = 64 ,ylim=c(-1,1),lwd=2)

veri_ts<-ts(veri,frequency = 12)

#P=0, Q=0
arima1<-Arima(veri_ts, order = c(1,1,0), seasonal= c(0,0,0), include.constant=TRUE)
coeftest(arima1)  #anlaml??
summary(arima1)
#P=1,Q=0
arima2<-Arima(veri_ts, order = c(1,1,0), seasonal= c(1,0,0), include.constant=TRUE)
coeftest(arima2)
summary(arima2)   #anlams??z
#P=0,Q=1
arima3<-Arima(veri_ts, order = c(1,1,0), seasonal= c(0,0,1), include.constant=TRUE)
coeftest(arima3)
summary(arima3)
#P=0,Q=2
arima4<-Arima(veri_ts, order = c(1,1,0), seasonal= c(0,0,2), include.constant=TRUE)
coeftest(arima4)
summary(arima4)
#P=0,Q=3
arima5<-Arima(veri_ts, order = c(1,1,0), seasonal= c(0,0,3), include.constant=TRUE)
coeftest(arima5)
summary(arima5)  #anlams??z
#P=1,Q=1
arima6<-Arima(veri_ts, order = c(1,1,0), seasonal= c(1,0,1), include.constant=TRUE)
coeftest(arima6)
summary(arima6)
#P=1,Q=2
arima7<-Arima(veri_ts, order = c(1,1,0), seasonal= c(1,0,2), include.constant=TRUE)
coeftest(arima7)
summary(arima7)
#P=1,Q=3
arima8<-Arima(veri_ts, order = c(1,1,0), seasonal= c(1,0,3), include.constant=TRUE)
coeftest(arima8)
summary(arima8)

#ACF grafi??inin daha h??zl?? azald?????? durum

arima9<-Arima(veri_ts, order = c(0,1,1), seasonal= c(0,0,0), include.constant=TRUE)
coeftest(arima9)  #ANLAMLI
summary(arima9)

arima10<-Arima(veri_ts, order = c(0,1,1), seasonal= c(1,0,0), include.constant=TRUE)
coeftest(arima10)
summary(arima10)

arima11<-Arima(veri_ts, order = c(0,1,1), seasonal= c(0,0,2), include.constant=TRUE)
coeftest(arima11)
summary(arima11)


arima12<-Arima(veri_ts, order = c(0,1,1), seasonal= c(0,0,3), include.constant=TRUE)
coeftest(arima12)
summary(arima12)


arima13<-Arima(veri_ts, order = c(0,1,1), seasonal= c(1,0,1), include.constant=TRUE)
coeftest(arima13)
summary(arima13)

arima14<-Arima(veri_ts, order = c(0,1,1), seasonal= c(1,0,2), include.constant=TRUE)
coeftest(arima14)
summary(arima14)

arima15<-Arima(veri_ts, order = c(0,1,1), seasonal= c(1,0,3), include.constant=TRUE)
coeftest(arima15)
summary(arima15)

#hem PACF hem de ACF nin h??zl?? azald?????? durum
arima16<-Arima(veri_ts, order = c(1,1,3), seasonal= c(0,0,0), include.constant=TRUE)
coeftest(arima16)
summary(arima16)

arima17<-Arima(veri_ts, order = c(1,1,3), seasonal= c(1,0,0), include.constant=TRUE)
coeftest(arima17)
summary(arima17)

arima18<-Arima(veri_ts, order = c(1,1,3), seasonal= c(0,0,1), include.constant=TRUE)
coeftest(arima18)
summary(arima18)

arima19<-Arima(veri_ts, order = c(1,1,3), seasonal= c(0,0,2), include.constant=TRUE)
coeftest(arima19)
summary(arima19)

arima20<-Arima(veri_ts, order = c(1,1,3), seasonal= c(0,0,3), include.constant=TRUE)
coeftest(arima20)
summary(arima20)

arima21<-Arima(veri_ts, order = c(1,1,3), seasonal= c(1,0,1), include.constant=TRUE)
coeftest(arima21)

arima22<-Arima(veri_ts, order = c(1,1,3), seasonal= c(1,0,2), include.constant=TRUE)
coeftest(arima22)

arima23<-Arima(veri_ts, order = c(1,1,3), seasonal= c(1,0,3), include.constant=TRUE)
coeftest(arima23)

tahmin_arima1<-arima1[["fitted"]]
hata_arima1<- arima1[["residuals"]]

tahmin_arima9<-arima9[["fitted"]]
hata_arima9<- arima9[["residuals"]]

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_arima1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(veri_ts)),
                   expression(paste(Tahmin1))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

plot( window(veri_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin_arima9) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(veri_ts)),
                   expression(paste(Tahmin9))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

Acf(hata_arima9,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2)
Pacf(hata_arima9,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2)
Box.test(hata_arima9, lag = 42, type = "Ljung")

Acf(hata_arima1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2)
Pacf(hata_arima1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2)
Box.test(hata_arima1, lag = 42, type = "Ljung")


#veri_ts <- ts(veri, frequency = 12) 
ongoru<-forecast(arima1,h=6)
ongoru[["mean"]]

son_veri<- as.data.frame(ongoru[["mean"]])
son_veri<- cbind(c("00:00:00","01:00:00","02:00:00",
                   "03:00:00","04:00:00","05:00:00"),son_veri)
names(son_veri)<-c("saat","ongoru")
son_veri




