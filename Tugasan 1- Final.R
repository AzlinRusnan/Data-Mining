##Tugasan satu

#1. Bina data untuk setiap lajur merujuk kepada pembolehubah yang berasingan.

#Pembolehubah Kuantitatif
#P/Ubah Diskrit
Bil.Pekerja <- c(4,3,1,1,2,1974,5,100,4,4,2,2,200,7)
#P/Ubah Selanjar
Pend.Bulanan <- c(15214.32, 3126.60, 5211.10, 3000.00, 12431.11, 290000.00, 16321.00, 176342.11, 6251.99,
                  6011.23, 4321.67, 6743.28, 167223.90, 26590.12)
Mod.Bulanan <- c(5000.00, 1530.00, 3211.00, 1444.00, 4372.11, 100000.00, 6421.66, 54320.00, 4421.00,
                 5432.31, 1500.00, 2000.00, 48761.00, 6000.00)

#Pembolehubah Kualitatif
#P/Ubah Nominal

#Large=1 (TRUE), Small=0 (FALSE)
Status.Sykt <- c(F,F,F,F,F,T,F,T,F,F,F,F,T,F)
Status.Sykt <- ifelse(Status.Sykt==T, "Large","Small")
Status.Sykt <- factor(Status.Sykt)

Nama.Sykt <- c("ABC","Bookstore Timah","Prasa","Delta","Alfa","Gama","Bakeri Hiasan","Shel","DV",
               "Viva","Kedai Sate Ali","Kedai Runcit Abu","Kilang Apel","DM")

Kategori.Per <- c("P","P","S","P","P","P","S","B","S","S","S","S","B","P")
Kategori.Per <- as.factor(Kategori.Per)
library(plyr)
Kategori.Per2 <- revalue(Kategori.Per,c("P"="Company", "B"="Limited", "S"="Private"))

Status.Penarafan <- c("A","B","B-","B","A+","A+","A","A+","C","C-","B-","B","A+","A")
Status.Penarafan <- as.factor(Status.Penarafan)

#2. Berdasarkan data dalam (1), gabungkan semua pembolehubah dalam bentuk bingkai data.
#Data Frame

data <- data.frame(Nama.Sykt,Pend.Bulanan,Mod.Bulanan,Bil.Pekerja,Kategori.Per2,Status.Sykt,Status.Penarafan)

#3. Berdasarkan data dalam (2), susun data untuk semua pembolehubah berdasarkan 
#tingkat pendapatan (daripada pendapatan rendah kepada tinggi).

data2 <- data[order(Pend.Bulanan),]

#4. Berdasarkan data dalam (3), bina pembolehubah baharu yang menunjukkan 
#syarikat yang mempunyai pendapatan melebihi RM10000.00 sebulan.

Sykt.Kaya <- data2$Pend.Bulanan>10000.00
data3 <- data2[Sykt.Kaya, ]

#5. Berdasarkan data dalam (3), huraikan ringkasan statistik tentang data tersebut.

summary(data2)

#6. Bina plot data yang sesuai untuk memaparkan maklumat data dalam (3).

library(ggplot2)
library(ggpubr)
options(scipen = 999) #to remove scientific notation
ggbarplot(data=data2, x="Nama.Sykt", y="Pend.Bulanan",
          fill="Nama.Sykt")+xlab("Nama Syarikat") + ylab("Pendapatan Bulanan (RM)")+
  ggtitle("Pendapatan Bulanan Syarikat")+
  #Mengkoordinasi title di tengah
  theme(plot.title = element_text(hjust = 0.5))+
  
  #Mengubah x-axis title ke nilai 11
  theme(axis.text.x = element_text(size=11))+
  #Mengubah y-axis title ke nilai 11
  theme(axis.text.y = element_text(size=11))+
  #Mengubah axis title
  theme(axis.title = element_text(size = 15))+
  #scale untuk menambah koma pada nombor di y-axis
  scale_y_continuous(label = scales::comma)+
  
  #theme untuk tukar position legend ke-kanan
  theme(legend.position = "right")+
  #legend font size and key size
  theme(legend.title = element_text(size=11), legend.text=element_text(size=10))+
  theme(legend.key.size=unit(0.35,"cm"))+
  
  #grid line
  theme(panel.grid.major.y=element_line(color="red",size=0.5,linetype=2))+
  #background color
  theme(panel.background = element_rect(fill="grey",color="black"))



