#1. Jika rata-rata pelanggan datang ke toko adalah 3 orang per jam, 
#modelkan dengan Poisson dan hitung P(X≥5)
lambda <- 3
x <- 0:15
pmf <- dpois(x, lambda)
#Membuat grafik PMF Poisson
#Setiap garis menunjukkan peluang jumlah pelanggan tertentu dalam satu jam
plot(x, pmf, type = "h", lwd = 3,
     main = "Poisson(λ = 3)",
     xlab = "Jumlah pelanggan (k)",
     ylab = "P(X = k)")
#Menghitung P(X≥5)
Poisson_pelanggan <- 1 - ppois(4, lambda)
Poisson_pelanggan

#2. Dari 100 bola (20 berwarna merah), 
#diambil 10 tanpa pengembalian. 
#Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat.
#Sebaran Hipergeometrik
N <- 100    #ukuran populasi
K <- 20    #jumlah sukses di populasi (bola merah = sukses)
n <- 10    #ukuran sampel
#Distribusi jumlah bola merah
x <- 0:n
pmf <- dhyper(x, K, N - K, n)
#Menampilkan PMF
data.frame(x, pmf)
#Membuat grafik
plot(x, pmf,type = "h",lwd = 3,
     main = "Distribusi Hipergeometrik",
     xlab = "Jumlah bola merah (X)",
     ylab = "P(X = x)")

#3. Simulasikan 1.000 percobaan Binomial (n=15,p=0.4)
#dan bandingkan histogram hasil simulasi dengan PMF teoretis.
n <- 15
p <- 0.4
x <- 0:n
#simulasi 1000 percobaan
m <- 1000
samp <- rbinom(m,size = n,prob = p)
#PMF teoritis
pmf_binom <- dbinom(x, size = n, prob = p)
# Grafik PMF teoritis
plot(x, pmf_binom, type = "h", lwd = 3,
     main = "PMF Binomial",
     xlab = "Jumlah sukses (k)",
     ylab = "Probabilitas (P(X = k))")
#Histogram hasil simulasi
hist(samp,
     breaks = seq(-0.5, 15.5, by = 1),
     probability = TRUE,
     main = "Simulasi Binomial (n = 15, p = 0.4)",
     xlab = "Jumlah keberhasilan (X)",
     ylab = "Probabilitas")
#Menambahkan PMF teoretis ke histogram
points(x, pmf_binom, pch = 19, col = "red")
lines(x, pmf_binom, lwd = 2, col = "red")