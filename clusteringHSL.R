# Clustering de imágenes en espacio HSL con R
# www.overfitting.net

library(tiff)

# Leemos imagen
imagen=readTIFF("marmenor.tif", native=F, convert=F)

# CONVERSIÓN RGB -> HSL VECTORIZADA
R=imagen[,,1]
G=imagen[,,2]
B=imagen[,,3]

Cmax=pmax(R,G,B)  # 0..1
Cmin=pmin(R,G,B)  # 0..1
Delta=Cmax-Cmin  # 0..1 

# L: 0..1
L=(Cmax + Cmin) / 2

# S: 0..1
S=Delta/(1-abs(2*L-1))
S[which(Delta==0)]=0

# H: 0..360
H=Cmax*0
indices=which(Cmax==R)
H[indices]=60*(((G[indices]-B[indices])/Delta[indices]) %% 6)
indices=which(Cmax==G)
H[which(Cmax==G)]=60*((B[indices]-R[indices])/Delta[indices] +  2)
indices=which(Cmax==B)
H[which(Cmax==B)]=60*((R[indices]-G[indices])/Delta[indices] +  4)
H[which(Delta==0)]=0


# CLUSTERING K-MEANS
dim(R)=length(R)
dim(G)=length(G)
dim(B)=length(B)
dim(H)=length(H)
dim(S)=length(S)
dim(L)=length(L)

M=array(0,c(length(R),6))  # Matriz RGBHSL
colnames(M)=c("R","G","B","H","S","L")
M[,1]=R
M[,2]=G
M[,3]=B
M[,4]=H/360  # Normalizamos H a rango 0..1
M[,5]=S
M[,6]=L

kmeansfit=kmeans(subset(M, select = c("H")), 5, nstart=10, iter.max=10)
clustering=kmeansfit$cluster
dim(clustering)=dim(imagen[,,1])
clustering=clustering-min(clustering)
clustering=clustering/max(clustering)
writeTIFF(clustering, "clustering_5_H.tif", bits.per.sample=8, compression="LZW")


# HISTOGRAMA H CON CENTROIDES
histoH=hist(as.integer(subset(M, select = c("H"))*360), breaks=c(0:360), right=F, plot=F)
xaxis=0:359
ylimits=c(0,max(histoH$counts))
plot(xaxis, histoH$counts, type='h', col='blue' ,
     main="Histograma H", xlab="Hue [0..359]", ylab="Núm. píxeles", ylim=ylimits)
abline(v=round(kmeansfit$centers*360), col='red')  # Centroides

