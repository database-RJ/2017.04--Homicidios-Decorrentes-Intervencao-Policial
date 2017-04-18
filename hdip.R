## 
# Esse script visualiza a evolucao temporal do numero de 
#  Homicidios decorrentes de intervencao policial no Rio de Janeiro

# autor: Joao Meirelles
##

#carregas as bibliotecas necessarias
library("readxl")
library(stringr)
library(reshape2)
library(ggplot2)
library(zoo)

#######################
#####Load Data#########
#######################
setwd("/home/jm/DATABASE-RJ/2017.04--Homicidios-Decorrentes-Intervencao-Policial/")

file_list <- list.files(path = "/home/jm/DATABASE-RJ/2017.04--Homicidios-Decorrentes-Intervencao-Policial/data", pattern = ".xls")



for (file in file_list) {
 print(file)
   df <- read_excel(paste("./data/",file, sep=""))
  df <- df[c(6:145),]
  colnames(df) = df[1, ]
  df$`UNIDADES POLICIAIS` <- gsub("-", ".", df$`UNIDADES POLICIAIS`)
  df$UP <- substr(df$`UNIDADES POLICIAIS`, 0, 3)
  df = df[-1, ] 
  
  ano <- substr(file, 30, 33)
  
  colnames(df)[1] <- paste("UOP", ano, sep="-")
  colnames(df)[2] <- paste("UP", ano, sep="-")
  colnames(df)[3] <- paste(ano,"01", sep="-")
  colnames(df)[4] <- paste(ano,"02", sep="-")
  colnames(df)[5] <- paste(ano,"03", sep="-")
  colnames(df)[6] <- paste(ano,"04", sep="-")
  colnames(df)[7] <- paste(ano,"05", sep="-")
  colnames(df)[8] <- paste(ano,"06", sep="-")
  colnames(df)[9] <- paste(ano,"07", sep="-")
  colnames(df)[10] <- paste(ano,"08", sep="-")
  colnames(df)[11] <- paste(ano,"09", sep="-")
  colnames(df)[12] <- paste(ano,"10", sep="-")
  colnames(df)[13] <- paste(ano,"11", sep="-")
  colnames(df)[14] <- paste(ano,"12", sep="-")
  colnames(df)[15] <- paste("total", ano, sep="")
  df <- df[,-c(15)]
  
  df <- df[,c(15,1:14)]
  
  print(ano)


if (file == "35SerieHomIntervencaoPolicial2013.xls"){
  hdip.df <- df
  
} else {

  hdip.df <- merge(x = hdip.df, y = df[,c(1,4:14)], by = "UP", all = TRUE)
}
}

str(hdip.df)


#melt the dataframe
hdip.df <- melt(hdip.df, id.vars=1:3)
colnames(hdip.df) <- c("up_id","up_name","uop","mon_year","hdip")

#tranform data type to numeric
hdip.df$hdip <- as.numeric(hdip.df$hdip) 


hdip.yermon <- aggregate(hdip.df$hdip, by=list(Category=hdip.df$mon_year), FUN=sum, na.rm=TRUE)
colnames(hdip.yermon) <- c("yearmon","hdip")
hdip.yermon$yearmon <- as.yearmon(hdip.yermon$yearmon)

hdip.yermon <- hdip.yermon[c(1:47),]
hdip.yermon[47,]

##############################################


####################
####### VIZ ########
####################

ggplot(hdip.yermon, aes(x=yearmon,y=hdip))+geom_line(color="green")+
  #geom_smooth(color="red", se=FALSE)+
  ylab("Homicídio Decorrentes de Intervenção Policial")+
  xlab("Data")+
  
  annotate("rect", xmin=as.numeric(as.yearmon("Jan 2016")), xmax=as.numeric(as.yearmon("Jan 2017")),
           ymin=20, ymax=Inf, alpha=0.2, fill="red") +
  geom_text(aes(x=as.numeric(as.yearmon("Aug 2016"))-0.1, label="Ano Olímpico", y=30), colour="red", size=3) +
  theme_bw()

#  geom_vline(xintercept=as.numeric(as.yearmon("Aug 2014")), colour="red")+


#  geom_vline(xintercept=as.numeric(as.yearmon("Aug 2016")), colour="red")+
#  geom_text(aes(x=as.numeric(as.yearmon("Aug 2016"))-0.1, label="Olimpíadas", y=40), colour="red", angle=90, size=3) +

#    geom_vline(xintercept=as.numeric(as.yearmon("Okt 2016")), colour="blue")+
#  geom_text(aes(x=as.numeric(as.yearmon("Okt 2016"))+0.1, label="Troca de Secretário de Segurança", y=50), colour="blue", angle=90, size=3) +



ggsave("./plots_raw/hidp.png")

