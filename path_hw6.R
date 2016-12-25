# Software Tools - Term Project
# Hazel Kavili

# upload packages you think you'll need
library(dplyr)
library(tidyr)
library(ggplot2)

# load csv files from your working directory
path <- "/Users/hazelkavili/Desktop/termProject"
load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv, header = FALSE)
  do.call(rbind, tables)
}
DataClean <- load_data("/Users/hazelkavili/Desktop/termProject")


# clean data
DataClean$V1 <- as.character(DataClean$V1)
DataClean$V1 <- gsub("^\\s+|\\s+$", "", DataClean$V1) # drop white space
DataClean <- DataClean[-which(DataClean$V1==""),]

byValue <- 38 # hard coded :(
OrmanSuIsleri <- DataClean[seq(2,nrow(DataClean),by = byValue),1]
Meteoroloji   <- DataClean[seq(3,nrow(DataClean),by = byValue),1]
OrtSicaklik   <- DataClean[seq(4,nrow(DataClean),by = byValue),1]
IstasyonAdi   <- DataClean[seq(5,nrow(DataClean),by = byValue),1]
Yil <- DataClean[seq(6,nrow(DataClean),by = byValue),1]
GunAy <- DataClean[seq(7,nrow(DataClean),by = byValue),1]


IDMerge <- paste(rep(IstasyonAdi,each = byValue),rep(Yil,each = byValue),sep="-")
DataClean <- data.frame(IDMerge,DataClean)
UniqueIDMerge <- unique(IDMerge)

# make an empty data frame 
DataMaster <- data.frame(IDMerge = as.character(0),
                         GunAy = as.character(0),
                         Ay1 = as.numeric(0),
                         Ay2 = as.numeric(0),
                         Ay3 = as.numeric(0),
                         Ay4 = as.numeric(0),
                         Ay5 = as.numeric(0),
                         Ay6 = as.numeric(0),
                         Ay7 = as.numeric(0),
                         Ay8 = as.numeric(0),
                         Ay9 = as.numeric(0),
                         Ay10 = as.numeric(0),
                         Ay11 = as.numeric(0),
                         Ay12 = as.numeric(0))

for(i in 1:length(UniqueIDMerge)){
  IDMerge <- UniqueIDMerge[i]
  GunAy <- paste(1:31)
  tmpData <- DataClean[which(DataClean$IDMerge==IDMerge & DataClean$V1%in%GunAy),
                       colnames(DataClean)%in%paste("V",2:13,sep="")]
  
  colnames(tmpData) <- paste("Ay",1:12,sep="")
  tmpData <- data.frame(IDMerge = IDMerge, GunAy = GunAy, tmpData)
  DataMaster <- rbind(DataMaster,tmpData)
  
}

# separate station_no, city and year
DataMaster <- DataMaster[-1,]

DataMaster <- DataMaster %>% separate(IDMerge, c("istasyon_adi", "yil" ), sep = "-") %>% 
  separate(istasyon_adi, c("gereksiz","yeni_istasyon"), sep = ":") %>% 
  separate(yil, c("gereksiz2", "yil"), sep = ":")

DataMaster <- DataMaster[,-1]
DataMaster <- DataMaster[,-2]
DataMaster <- DataMaster %>% separate(yeni_istasyon, c("sehir", "istasyon_no"), sep = "/")

DataMaster$sehir <- gsub("^\\s+|\\s+$", "", DataMaster$sehir)
View(DataMaster) #just to see the table

# calculate monthly means
ortalamalar <- DataMaster %>% group_by(istasyon_no) %>% 
  summarise(ocak = mean(Ay1, na.rm = TRUE), 
            subat = mean(Ay2, na.rm = TRUE),
            mart = mean(Ay3, na.rm = TRUE),
            nisan = mean(Ay4, na.rm = TRUE),
            mayis = mean(Ay5, na.rm = TRUE),
            haziran = mean(Ay6, na.rm = TRUE),
            temmuz = mean(Ay7, na.rm = TRUE),
            agustos = mean(Ay8, na.rm = TRUE),
            eylul = mean(Ay9, na.rm = TRUE),
            ekim = mean(Ay10, na.rm = TRUE),
            kasim = mean(Ay11, na.rm = TRUE),
            aralik = mean(Ay12, na.rm = TRUE))



# make plots for each station
ortalamalar <- as.matrix(ortalamalar[,-1])
sehirler <- unique(DataMaster$sehir)
par(mfrow=c(3,2))
par(mar=c(5.1,5.1,4.1,2.1))

for (k in seq(ortalamalar)) { 
  plot(ortalamalar[k,], xlab = "Months", ylab = "Temperature", type = "l", col = k) 
  axis(1, at=1:12)
  title(main = paste0("Ortalama Sicaklik: ", sehirler[k]))  
  }

# save image as pdf
dev.copy(png,'ortalamasicaklik')
dev.off()
