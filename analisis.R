library(readxl)
dat <- read_excel("D:\\Adi\\Kuliah\\PKM_RE_Iklan.csv\\asli.xlsx")
dat <- as.data.frame(dat)
head(dat,1)
str(dat)
dat <- as.data.frame(dat)
dat[,-2] <- lapply(dat[,-2], function(x) as.factor(x))
str(dat)
colnames(dat)
colnames(dat) <- c(colnames(dat[,1:13]),"Price","Discount","Rating","Review",
                   "Uncomplained","Description","Appearance","Best Seller","Top","Advertisement",
                   "Rating","Complete Choice","Location","Lots of Followers","Fast Respon",
                   "Free Shipping","Lots of Discount","Payment Method","Long Standing",
                   "Detailed Adress","Special Marked","Contact","Advertisement Site",
                   "Other Advertisement Site")
colnames(dat)

reor <- function(x){
  factor(x, c("Tidak pernah","Jarang","Sering","Selalu"))
}
dat[,14:37] <- lapply(dat[,14:37], function(x) reor(x))
str(dat)
ganti <- function(x){
  revalue(x, c("Tidak pernah" = "0",
               "Jarang" = "1",
               "Sering" = "2",
               "Selalu" = "3"))
}
str(dat)
library(plyr)
dat[,14:37] <- lapply(dat[,14:37], function(x) ganti(x))
dat[,14:37] <- lapply(dat[,14:37], function(x) as.numeric(as.character(x)))
q <- t(dat[,14:37])
q <- as.data.frame(q)
as.numeric(which(apply(q, 2, var)==0))
dat[c(as.numeric(which(apply(q, 2, var)==0))),14:37]
dat <- dat[-c(as.numeric(which(apply(q, 2, var)==0))),]
rownames(dat) <- NULL
nrow(dat)
colnames(dat)
colnames(dat[,14:23])
produk <- dat[,14:23]
colnames(dat[,24:37])
toko <- dat[,24:37]
library(ggplot2)
library(tidyr)
freq_prod <- produk %>% gather(pernyataan, intensitas, Price:Advertisement)
head(freq_prod)
str(freq_prod)
library(dplyr)
#frekuensi jawaban produk
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
bak <- function(x){
  revalue(x, c( "0" = "Never",
                "1" = "Seldom",
                "2" = "Often",
                "3" = "Always"))
}
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Always", "Often", "Seldom", "Never"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating":"Other Advertisement Site")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Always", "Often", "Seldom", "Never"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensity", "n")
colnames(freq_toko) <- c("pernyataan", "Intensity", "n")

#Percentage Stacked Bar Chart
nrow(dat)
ggplot(freq_prod, aes(fill=Intensity, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Category")+ylab("Percentage")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensity, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Category")+ylab("Percentage")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
#
colnames(dat)
jenis <- dat[,6:10]
ganti2 <- function(x){
  revalue(x, c("Tidak pernah" = "0",
               "Kadang-kadang" = "1",
               "Sering" = "2"))
}
library(plyr)
colnames(jenis)
jenis[,] <- lapply(jenis[,], function(x) ganti2(x))
jenis[,] <- lapply(jenis[,], function(x) as.numeric(as.character(x)))
str(jenis)
freq_jenis <- jenis %>% gather(pernyataan, intensitas, "Shopee":"Bukalapak")
freq_jenis <- freq_jenis %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_jenis <- as.data.frame(freq_jenis)

freq_jenis <- as.data.frame(freq_jenis)
str(freq_jenis)
freq_jenis$intensitas <- as.factor(freq_jenis$intensitas)
str(freq_jenis)
bak2 <- function(x){
  revalue(x, c( "0" = "Tidak pernah",
                "1" = "Jarang",
                "2" = "Sering"))
}
freq_jenis$intensitas <- bak2(freq_jenis$intensitas)
str(freq_jenis)
levels(freq_jenis$intensitas)
str(freq_jenis)
freq_jenis$intensitas <- factor(freq_jenis$intensitas, c("Sering", "Jarang", "Tidak pernah"))
ggplot(freq_jenis, aes(fill=intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
library(plyr)
levels(dat$Pendidikan)
table(dat$Pendidikan)
dat$Pendidikan <- revalue(dat$Pendidikan, c("Diploma/Sarjana" = ">SMA/Sederajat",
                                            "S2/S3" = ">SMA/Sederajat",
                                            "SD/sederajat" = "<SMA/Sederajat",
                                            "SMP/Sederajat" = "<SMA/Sederajat"))
prop.table(table(dat$`Jenis kelamin`))
prop.table(table(dat$Pendidikan))
prop.table(table(dat$Pendapatan))
prop.table(table(dat$lama_menggunakan))
prop.table(table(dat$intensitas))
#multinom
colnames(dat)
mul <- dat[,c(1,2,4,5,11,12,13,23)]
colnames(mul)
str(mul)
ggplot(mul, aes(x=as.factor(Produk_tersering), fill=as.factor(Produk_tersering) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") + xlab("Produk")+ylab("Jumlah")+
  theme(legend.position="none")
#analisis
str(mul)
mul$Advertisement <- as.factor(mul$Advertisement)
bak <- function(x){
  revalue(x, c( "0" = "Never",
                "1" = "Seldom",
                "2" = "Often",
                "3" = "Always"))
}
head(mul)
mul$Advertisement <- bak(mul$Advertisement)
str(mul)
table(mul$Iklan)
levels(mul$Iklan)
levels(mul$Pendidikan)
mul$Edu <- factor(mul$Pendidikan, c("Low","Middle","High"))
levels(mul$Pendapatan)
mul$Income <- factor(mul$Pendapatan, c("Less than 500.000","500.000-2.000.000","More than 2.000.000"))
levels(mul$lama_menggunakan)
mul$lama_menggunakan <- factor(mul$lama_menggunakan, c("kurang dari 1 tahun","sekitar 1-2 tahun","lebih dari 2 tahun"))

levels(mul$intensitas)

mul$intensitas <- factor(mul$intensitas, c("kurang dari 2 kali dalam sebulan",
                                                           "2-5 kali dalam sebulan",
                                                           "6-10 kali dalam sebulan",
                                                           "lebih dari 10 kali dalam sebulan"))
levels(mul$intensitas)
str(mul)
library(VGAM)

#Gender dan Umur
colnames(mul)
mul <- mul[,-7]
colnames(mul) <- c("Gender", "Age", colnames(mul[,-c(1,2)]))
colnames(mul)
#
#Umur
colnames(mul)
fit <- vglm(Iklan~Umur, family=multinomial(refLevel = "Tidak pernah"), data=mul)
summary(fit)
min(mul$Umur)
max(mul$Umur)
curve(expr = 1/(1 + exp(-0.81+0.12*x) + exp(-0.52-0.08*x)+exp(-2.48+0.12*x)), ylab =
        expression(hat(pi)), xlab = "Umur", xlim = c(10,60), col = "black", 
      lty = "solid", ylim=c(0,1),lwd = 2, n = 1000, type = "n",panel.first = grid(col = "gray",                                                                           lty = "dotted"))
curve(expr = 1/(1 + exp(-0.81+0.12*x) + exp(-0.52-0.08*x)+exp(-2.48+0.12*x)),col = "black", 
      lty = "longdash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60))
curve(expr = exp(-0.81+0.12*x)/(1 + exp(-0.81+0.12*x) + exp(-0.52-0.08*x)+exp(-2.48+0.12*x)),
      col = "Green", lty = "solid", 
      lwd = 2, n = 1000, add = TRUE, xlim = c(10,60), ylim = c(0,1))
curve(expr = exp(-0.52-0.08*x)/(1 + exp(-0.81+0.12*x) + exp(-0.52-0.08*x)+exp(-2.48+0.12*x)),col = "blue", 
      lty = "dotdash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60), ylim=c(0,1))
curve(expr = exp(-2.48+0.12*x)/(1 + exp(-0.81+0.12*x) + exp(-0.52-0.08*x)+exp(-2.48+0.12*x)),col = "red", 
      lty = "twodash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60))
legend("right", legend=c("Tidak pernah", "Jarang", "Sering", "Selalu"),
       lty=c("longdash","solid", "dotdash", "twodash"),
       col=c("black","green","blue","red"), bty="n", lwd = c(2,2,2,2), 
       seg.len = 4, cex = 0.5)
#Gender dan Umur
colnames(mul)
fit <- vglm(Advertisement~Gender+Age, family=multinomial(refLevel = "Never"), data=mul)
summary(fit)
#Laki-laki
curve(expr = 1/(1 + exp(-1.71+0.14*x) + exp(-1.36+0.1*x)+exp(-4.10+0.14*x)), ylab =
        expression(hat(pi)), xlab = "Age", xlim = c(10,60), col = "black", 
      lty = "solid", ylim=c(0,1),lwd = 2, n = 1000, type = "n",panel.first = grid(col = "gray",                                                                           lty = "dotted"))
curve(expr = 1/(1 + exp(-1.71+0.14*x) + exp(-1.36+0.1*x)+exp(-4.10+0.14*x)),col = "black", 
      lty = "longdash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60))
curve(expr = exp(-1.71+0.14*x)/(1 + exp(-1.71+0.14*x) + exp(-1.36+0.1*x)+exp(-4.10+0.14*x)),
      col = "Green", lty = "solid", 
      lwd = 2, n = 1000, add = TRUE, xlim = c(10,60), ylim = c(0,1))
curve(expr = exp(-1.36+0.1*x)/(1 + exp(-1.71+0.14*x) + exp(-1.36+0.1*x)+exp(-4.10+0.14*x)),col = "blue", 
      lty = "dotdash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60), ylim=c(0,1))
curve(expr = exp(-4.10+0.14*x)/(1 + exp(-1.71+0.14*x) + exp(-1.36+0.1*x)+exp(-4.10+0.14*x)),col = "red", 
      lty = "twodash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60))
legend("right", legend=c("Never", "Seldom", "Often", "Always"),
       lty=c("longdash","solid", "dotdash", "twodash"),
       col=c("black","green","blue","red"), bty="n", lwd = c(2,2,2,2), 
       seg.len = 4, cex = 0.5)
#Perempuan
-1.71+0.65
-1.36+0.60
-4.10+1.34
curve(expr = 1/(1 + exp(-1.06+0.14*x) + exp(-0.76+0.1*x)+exp(-2.76+0.14*x)), ylab =
        expression(hat(pi)), xlab = "Age", xlim = c(10,60), col = "black", 
      lty = "solid", ylim=c(0,1),lwd = 2, n = 1000, type = "n",panel.first = grid(col = "gray",                                                                           lty = "dotted"))
curve(expr = 1/(1 + exp(-1.06+0.14*x) + exp(-0.76+0.1*x)+exp(-2.76+0.14*x)),col = "black", 
      lty = "longdash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60))
curve(expr = exp(-1.06+0.14*x)/(1 + exp(-1.06+0.14*x) + exp(-0.76+0.1*x)+exp(-2.76+0.14*x)),
      col = "Green", lty = "solid", 
      lwd = 2, n = 1000, add = TRUE, xlim = c(10,60), ylim = c(0,1))
curve(expr = exp(-0.76+0.1*x)/(1 + exp(-1.06+0.14*x) + exp(-0.76+0.1*x)+exp(-2.76+0.14*x)),col = "blue", 
      lty = "dotdash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60), ylim=c(0,1))
curve(expr = exp(-2.76+0.14*x)/(1 + exp(-1.06+0.14*x) + exp(-0.76+0.1*x)+exp(-2.76+0.14*x)),col = "red", 
      lty = "twodash", lwd = 2, n = 1000, add = TRUE, xlim = c(10,60))
legend("right", legend=c("Never", "Seldom", "Often", "Always"),
       lty=c("longdash","solid", "dotdash", "twodash"),
       col=c("black","green","blue","red"), bty="n", lwd = c(2,2,2,2), 
       seg.len = 4, cex = 0.5)

fit <- vglm(Iklan~Gender+Pendapatan, family=multinomial(refLevel = "Tidak pernah"), data=mul)
summary(fit)
coef(fit, matrix = T)
0.13748/0.06429
2*(1-pnorm(0.13748/0.06429))
#Perhitungan
b10 <- 0.92
b20 <- 0.67
b30 <- -1.63
b11g <- 0.57
b21g <- 0.52
b31g <- 1.40
b12s <- 0.69
b22s <- 0.30
b32s <- 0.72
b12t <- 0.76
b22t <- 0.41
b32t <- 1.60

#Tidak pernah
#1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
#exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
#exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
#exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
######################
#Laki-laki dan rendah#
######################
g <- 0
s <- 0
t <- 0
#Tidak pernah
1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))


######################
#Laki-laki dan sedang#
######################
g <- 0
s <- 1
t <- 0
#Tidak pernah
1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
######################
#Laki-laki dan tinggi#
######################
g <- 0
s <- 0
t <- 1
#Tidak pernah
1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))

######################
#Perempuan dan rendah#
######################
g <- 1
s <- 0
t <- 0
#Tidak pernah
1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))

######################
#Perempuan dan sedang#
######################
g <- 1
s <- 1
t <- 0
#Tidak pernah
1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))

######################
#Perempuan dan tinggi#
######################
g <- 1
s <- 0
t <- 1
#Tidak pernah
1/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Jarang
exp(0.92+0.57*g+0.69*s+0.76*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Sering
exp(0.67+0.52*g+0.30*s+0.41*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))
#Selalu
exp(-1.63+1.40*g+0.72*s+1.60*t)/(1+exp(0.92+0.57*g+0.69*s+0.76*t)+exp(0.67+0.52*g+0.30*s+0.41*t)+exp(-1.63+1.40*g+0.72*s+1.60*t))

###########
# Heatmap #
###########
library('MASS')
colnames(klas)
str(klas)
map <- klas
map[,] <- lapply(map[,], function(x) bak(as.factor(x)))
str(map)
levels(map$Rating) <- c("Never", levels(map$Rating))
levels(map$Appearance) <- c("Never", levels(map$Appearance))
library('rcompanion')
colnames(map)
map_prod <- map[,1:10]
map_toko <- map[,11:21]
head(map_prod)
a1 <- vector()
for(i in 1:10){
  a1[i] <- cramerV(map_prod[,1], map_prod[,i], bias.correct = T)
}
a1
a2 <- vector()
for(i in 1:10){
  a2[i] <- cramerV(map_prod[,2], map_prod[,i], bias.correct = T)
}
a2
a3 <- vector()
for(i in 1:10){
  a3[i] <- cramerV(map_prod[,3], map_prod[,i], bias.correct = T)
}
a3
a4 <- vector()
for(i in 1:10){
  a4[i] <- cramerV(map_prod[,4], map_prod[,i], bias.correct = T)
}
a4
a5 <- vector()
for(i in 1:10){
  a5[i] <- cramerV(map_prod[,5], map_prod[,i], bias.correct = T)
}
a5
a6 <- vector()
for(i in 1:10){
  a6[i] <- cramerV(map_prod[,6], map_prod[,i], bias.correct = T)
}
a6
a7 <- vector()
for(i in 1:10){
  a7[i] <- cramerV(map_prod[,7], map_prod[,i], bias.correct = T)
}
a7
a8 <- vector()
for(i in 1:10){
  a8[i] <- cramerV(map_prod[,8], map_prod[,i], bias.correct = T)
}
a8
a9 <- vector()
for(i in 1:10){
  a9[i] <- cramerV(map_prod[,9], map_prod[,i], bias.correct = T)
}
a9
a10 <- vector()
for(i in 1:10){
  a10[i] <- cramerV(map_prod[,10], map_prod[,i], bias.correct = T)
}
a10
dmap_prod <- data.frame(var1 = rep(colnames(map_prod),10),
                        var2 = c(rep(colnames(map_prod)[1],10),
                                 rep(colnames(map_prod)[2],10),
                                 rep(colnames(map_prod)[3],10),
                                 rep(colnames(map_prod)[4],10),
                                 rep(colnames(map_prod)[5],10),
                                 rep(colnames(map_prod)[6],10),
                                 rep(colnames(map_prod)[7],10),
                                 rep(colnames(map_prod)[8],10),
                                 rep(colnames(map_prod)[9],10),
                                 rep(colnames(map_prod)[10],10)),
                        value = round(c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10),2))
head(dmap_prod)
#Produk
ggplot(dmap_prod, aes(var1, var2, fill= value)) + 
  geom_raster()+theme_bw()+scale_fill_viridis_c()+xlab("Category")+ylab("Category")

# Create a ggheatmap
ggheatmap <- ggplot(dmap_prod, aes(var2, var1, fill = value))+xlab("Category")+
  ylab("Category")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Cramer's V") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(var2, var1, label = value), color = "black", size = 4)

#
#Toko
#
a1 <- vector()
for(i in 1:11){
  a1[i] <- cramerV(map_toko[,1], map_toko[,i], bias.correct = T)
}
a1
a2 <- vector()
for(i in 1:11){
  a2[i] <- cramerV(map_toko[,2], map_toko[,i], bias.correct = T)
}
a2
a3 <- vector()
for(i in 1:11){
  a3[i] <- cramerV(map_toko[,3], map_toko[,i], bias.correct = T)
}
a3
a4 <- vector()
for(i in 1:11){
  a4[i] <- cramerV(map_toko[,4], map_toko[,i], bias.correct = T)
}
a4
a5 <- vector()
for(i in 1:11){
  a5[i] <- cramerV(map_toko[,5], map_toko[,i], bias.correct = T)
}
a5
a6 <- vector()
for(i in 1:11){
  a6[i] <- cramerV(map_toko[,6], map_toko[,i], bias.correct = T)
}
a6
a7 <- vector()
for(i in 1:11){
  a7[i] <- cramerV(map_toko[,7], map_toko[,i], bias.correct = T)
}
a7
a8 <- vector()
for(i in 1:11){
  a8[i] <- cramerV(map_toko[,8], map_toko[,i], bias.correct = T)
}
a8
a9 <- vector()
for(i in 1:11){
  a9[i] <- cramerV(map_toko[,9], map_toko[,i], bias.correct = T)
}
a9
a10 <- vector()
for(i in 1:11){
  a10[i] <- cramerV(map_toko[,10], map_toko[,i], bias.correct = T)
}
a10
a11 <- vector()
for(i in 1:11){
  a11[i] <- cramerV(map_toko[,11], map_toko[,i], bias.correct = T)
}
a11
colnames(map_toko) <- c("Rating", colnames(map_toko[,-1]))
colnames(map_toko)
dmap_toko <- data.frame(var1 = rep(colnames(map_toko),11),
                        var2 = c(rep(colnames(map_toko)[1],11),
                                 rep(colnames(map_toko)[2],11),
                                 rep(colnames(map_toko)[3],11),
                                 rep(colnames(map_toko)[4],11),
                                 rep(colnames(map_toko)[5],11),
                                 rep(colnames(map_toko)[6],11),
                                 rep(colnames(map_toko)[7],11),
                                 rep(colnames(map_toko)[8],11),
                                 rep(colnames(map_toko)[9],11),
                                 rep(colnames(map_toko)[10],11),
                                 rep(colnames(map_toko)[11],11)),
                        value = round(c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11),2))
head(dmap_toko)
# Create a ggheatmap
ggheatmap <- ggplot(dmap_toko, aes(var2, var1, fill = value))+xlab("Category")+ylab("Category")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Cramer's V") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(var2, var1, label = value), color = "black", size = 4)


###########
# Cluster #
###########
colnames(dat)
klas <- dat[,14:37]
library(factoextra)
fviz_nbclust(klas, kmeans, method = "silhouette", k.max = 8)
set.seed(123) # for reproducibility
km.res <- kmeans(klas, 2, nstart = 25)
klas$clus <- km.res$cluster
klas1 <- klas[klas$clus == 1,]
klas2 <- klas[klas$clus == 2,]
colnames(klas1)
produk <- klas1[,1:10]
toko <- klas1[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
produk <- klas2[,1:10]
toko <- klas2[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
nrow(klas1)
nrow(klas2)
nrow(dat[dat$`Jenis kelamin` == "Laki-laki",])
table(dat$Pendapatan)
#dat$Pendapatan <- revalue(dat$Pendapatan,c("500.000-2.000.000 per bulan" =
#                                             ">= 500.000 per bulan",
#                                           "lebih dari 2.000.000 per bulan"=
#                                             ">= 500.000 per bulan"))
##################
### Pendapatan ###
##################
nrow(dat)
colnames(dat)
levels(dat$Pendapatan)
inc1 <- dat[dat$Pendapatan == "kurang dari 500.000 per bulan",14:37]
inc2 <- dat[dat$Pendapatan == "500.000-2.000.000 per bulan",14:37]
inc3 <- dat[dat$Pendapatan == "lebih dari 2.000.000 per bulan",14:37]

###
# Kurang dari 500rb/bulan
###
library(factoextra)
fviz_nbclust(inc1, kmeans, method = "silhouette", k.max = 8)
set.seed(123) # for reproducibility
km.res <- kmeans(inc1, 2, nstart = 25)
inc1$clus <- km.res$cluster
inc11 <- inc1[inc1$clus == 1,]
inc12 <- inc1[inc1$clus == 2,]
produk <- inc11[,1:10]
toko <- inc11[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
produk <- inc12[,1:10]
toko <- inc12[,11:24]
#
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
nrow(inc11)
nrow(inc12)
###
# 500rb-2jt/bulan
###
fviz_nbclust(inc2, kmeans, method = "silhouette", k.max = 8)
set.seed(123) # for reproducibility
km.res <- kmeans(inc2, 2, nstart = 25)
inc2$clus <- km.res$cluster
inc21 <- inc2[inc2$clus == 1,]
inc22 <- inc2[inc2$clus == 2,]
produk <- inc21[,1:10]
toko <- inc21[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
produk <- inc22[,1:10]
toko <- inc22[,11:24]
#
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
nrow(inc21)
nrow(inc22)
###
# >2jt/bulan
###
fviz_nbclust(inc3, kmeans, method = "silhouette", k.max = 8)
set.seed(123) # for reproducibility
km.res <- kmeans(inc3, 2, nstart = 25)
inc3$clus <- km.res$cluster
inc31 <- inc3[inc3$clus == 1,]
inc32 <- inc3[inc3$clus == 2,]
produk <- inc31[,1:10]
toko <- inc31[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
produk <- inc32[,1:10]
toko <- inc32[,11:24]
#
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))

##################
### Gender ###
##################
nrow(dat)
colnames(dat)
levels(dat$`Jenis kelamin`)
inc1 <- dat[dat$`Jenis kelamin` == "Laki-laki",14:37]
inc2 <- dat[dat$`Jenis kelamin` == "Perempuan",14:37]
nrow(inc1)
nrow(inc2)
###
# Laki-laki
###
library(factoextra)
fviz_nbclust(inc1, kmeans, method = "silhouette", k.max = 8)
set.seed(123) # for reproducibility
km.res <- kmeans(inc1, 2, nstart = 25)
inc1$clus <- km.res$cluster
inc11 <- inc1[inc1$clus == 1,]
inc12 <- inc1[inc1$clus == 2,]
produk <- inc11[,1:10]
toko <- inc11[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
produk <- inc12[,1:10]
toko <- inc12[,11:24]
#
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
nrow(inc11)
nrow(inc12)
###
# Perempuan
###
fviz_nbclust(inc2, kmeans, method = "silhouette", k.max = 8)
set.seed(123) # for reproducibility
km.res <- kmeans(inc2, 2, nstart = 25)
inc2$clus <- km.res$cluster
inc21 <- inc2[inc2$clus == 1,]
inc22 <- inc2[inc2$clus == 2,]
produk <- inc21[,1:10]
toko <- inc21[,11:24]
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
###
produk <- inc22[,1:10]
toko <- inc22[,11:24]
#
#frekuensi jawaban produk
freq_prod <- produk %>% gather(pernyataan, intensitas, Harga:Iklan)
freq_prod <- freq_prod %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_prod <- as.data.frame(freq_prod)
str(freq_prod)
freq_prod$intensitas <- as.factor(freq_prod$intensitas)
str(freq_prod)
freq_prod <- as.data.frame(freq_prod)
freq_prod$intensitas <- bak(freq_prod$intensitas)
str(freq_prod)
head(freq_prod)
levels(freq_prod$intensitas)
freq_prod$intensitas <- factor(freq_prod$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_prod)
str(freq_prod)
#frekuensi jawaban toko
colnames(toko)
freq_toko <- toko %>% gather(pernyataan, intensitas, "Rating.1":"Iklan situs lain")
freq_toko <- freq_toko %>% group_by(pernyataan,intensitas) %>% count(intensitas)
freq_toko <- as.data.frame(freq_toko)
str(freq_toko)
freq_toko$intensitas <- as.factor(freq_toko$intensitas)
freq_toko$intensitas <- bak(freq_toko$intensitas)
head(freq_toko)
levels(freq_toko$intensitas)
freq_toko$intensitas <- factor(freq_toko$intensitas, levels = c("Selalu", "Sering", "Jarang", "Tidak pernah"))
head(freq_toko)
str(freq_toko)
colnames(freq_prod) <- c("pernyataan", "Intensitas", "n")
colnames(freq_toko) <- c("pernyataan", "Intensitas", "n")

#Percentage Stacked Bar Chart
ggplot(freq_prod, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
ggplot(freq_toko, aes(fill=Intensitas, y=n, x=pernyataan)) + 
  geom_bar(position="fill", stat="identity") + xlab("Kategori")+ylab("Persentase")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
nrow(inc21)
nrow(inc22)


#Perhitungan pendapatan
b10 <- 1.33
b20 <- 1.12
b30 <- -0.33
b11s <- 0.69
b21s <- 0.33
b31s <- 0.55
b11t <- 0.46
b21t <- 0.67
b31t <- 2.12
exp(b30)/(1+exp(b10)+exp(b20)+exp(b30))
exp(b20)/(1+exp(b10)+exp(b20)+exp(b30))
exp(b10)/(1+exp(b10)+exp(b20)+exp(b30))
1/(1+exp(b10)+exp(b20)+exp(b30))
#
exp(b30+b31s)/(1+exp(b30+b31s)+exp(b20+b21s)+exp(b10+b11s))
exp(b20+b21s)/(1+exp(b30+b31s)+exp(b20+b21s)+exp(b10+b11s))
exp(b10+b11s)/(1+exp(b30+b31s)+exp(b20+b21s)+exp(b10+b11s))
1/(1+exp(b30+b31s)+exp(b20+b21s)+exp(b10+b11s))
#
exp(b30+b31t)/(1+exp(b30+b31t)+exp(b20+b21t)+exp(b10+b11t))
exp(b20+b21t)/(1+exp(b30+b31t)+exp(b20+b21t)+exp(b10+b11t))
exp(b10+b11t)/(1+exp(b30+b31t)+exp(b20+b21t)+exp(b10+b11t))
1/(1+exp(b30+b31t)+exp(b20+b21t)+exp(b10+b11t))

