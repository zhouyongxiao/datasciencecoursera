library(dplyr)
setwd("D:/Users/Yongxiao/datasciencecoursera")
data<-read.csv("getdata_data_ss06hid.csv")
data<-mutate(data,agricultureLogical=(ACR==3 & AGS>2))
which(data$agricultureLogical)

library(jpeg)
pic<-readJPEG("getdata_jeff (1).jpg",native=TRUE)
quantile(pic,na.rm=TRUE,probs=c(0.3,0.8))


gdp<-read.csv("getdata_data_GDP.csv")
edu<-read.csv("getdata_data_EDSTATS_Country.csv")
edu<-rename(edu,cd=CountryCode)
ngdp<-merge(gdp,edu,by.x="cd",by.y="cd",all=FALSE)
ngdp<-ngdp[order(as.numeric(gsub(",", "",as.character(ngdp$amount)))),]
highoecd<-filter(ngdp,Income.Group=="High income: OECD")
mean(as.numeric(as.character(highoecd$Ranking)))
highnonoecd<-filter(ngdp,Income.Group=="High income: nonOECD")
mean(as.numeric(as.character(highnonoecd$Ranking)),na.rm=TRUE)

lowmiddle<-filter(ngdp,Income.Group=="Lower middle income")
target<-lowmiddle[as.numeric(as.character(lowmiddle$Ranking))<=38,]