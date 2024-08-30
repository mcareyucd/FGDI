#library
library("MFPCA")
library("ggfortify")
library("ggplot2")
library("ggmatplot")

#### Data #####################
data=read.csv("Parkinsons_Data.csv",header=TRUE)

# # Dividing the data into two parts for Left and Right 
#Left
leftdata<-data[data$Side=="L", ]
leftdataNewPelAngles<-leftdata[leftdata$VariableName=="PelvisAngles",] # dim (2433,100)
leftdataNewPelAnglesD1 <-leftdataNewPelAngles[leftdataNewPelAngles$Dim=="1",] # dim (811,100)
leftdataNewPelAnglesD2 <-leftdataNewPelAngles[leftdataNewPelAngles$Dim=="2",] # dim (811,100)
leftdataNewPelAnglesD3 <-leftdataNewPelAngles[leftdataNewPelAngles$Dim=="3",] # dim (811,100)
rownames(leftdataNewPelAnglesD1)=1:nrow(leftdataNewPelAnglesD1)
rownames(leftdataNewPelAnglesD2)=1:nrow(leftdataNewPelAnglesD2)
rownames(leftdataNewPelAnglesD3)=1:nrow(leftdataNewPelAnglesD3)
leftdataHipAngles<-leftdata[leftdata$VariableName=="HipAngles",]    # dim (2433,100)
leftdataHipAnglesD1 <-leftdataHipAngles[leftdataHipAngles$Dim=="1",] # dim (811,100)
leftdataHipAnglesD2 <-leftdataHipAngles[leftdataHipAngles$Dim=="2",] # dim (811,100)
leftdataHipAnglesD3 <-leftdataHipAngles[leftdataHipAngles$Dim=="3",] # dim (811,100)
rownames(leftdataHipAnglesD1)=1:nrow(leftdataHipAnglesD1)
rownames(leftdataHipAnglesD2)=1:nrow(leftdataHipAnglesD2)
rownames(leftdataHipAnglesD3)=1:nrow(leftdataHipAnglesD3)
leftdataKneeAngles<-leftdata[leftdata$VariableName=="KneeAngles",]   # dim (811,104)
rownames(leftdataKneeAngles)=1:nrow(leftdataKneeAngles)
leftdataAnkleAngles<-leftdata[leftdata$VariableName=="AnkleAngles",]  # dim (1622,104)
leftdataAnkleAnglesD1 <-leftdataAnkleAngles[leftdataAnkleAngles$Dim=="2",] # dim (811,100)
rownames(leftdataAnkleAnglesD1)=1:nrow(leftdataAnkleAnglesD1)
leftdataFootProgressAngles<-leftdata[leftdata$VariableName=="FootProgressAngles",] # dim (811,104)
rownames(leftdataFootProgressAngles)=1:nrow(leftdataFootProgressAngles)

#Right
rightdata<-data[data$Side=="R", ]
rightdataNewPelAngles<-rightdata[rightdata$VariableName=="PelvisAngles",] # dim (2433,100)
rightdataNewPelAnglesD1 <-rightdataNewPelAngles[rightdataNewPelAngles$Dim=="1",] # dim (811,100)
rightdataNewPelAnglesD2 <-rightdataNewPelAngles[rightdataNewPelAngles$Dim=="2",] # dim (811,100)
rightdataNewPelAnglesD3 <-rightdataNewPelAngles[rightdataNewPelAngles$Dim=="3",] # dim (811,100)
rownames(rightdataNewPelAnglesD1)=1:nrow(rightdataNewPelAnglesD1)
rownames(rightdataNewPelAnglesD2)=1:nrow(rightdataNewPelAnglesD2)
rownames(rightdataNewPelAnglesD3)=1:nrow(rightdataNewPelAnglesD3)
rightdataHipAngles   <-rightdata[rightdata$VariableName=="HipAngles",]    # dim (2433,100)
rightdataHipAnglesD1 <-rightdataHipAngles[rightdataHipAngles$Dim=="1",] # dim (811,100)
rightdataHipAnglesD2 <-rightdataHipAngles[rightdataHipAngles$Dim=="2",] # dim (811,100)
rightdataHipAnglesD3 <-rightdataHipAngles[rightdataHipAngles$Dim=="3",] # dim (811,100)
rownames(rightdataHipAnglesD1)=1:nrow(rightdataHipAnglesD1)
rownames(rightdataHipAnglesD2)=1:nrow(rightdataHipAnglesD2)
rownames(rightdataHipAnglesD3)=1:nrow(rightdataHipAnglesD3)
rightdataKneeAngles<-rightdata[rightdata$VariableName=="KneeAngles",]   # dim (811,104)
rownames(rightdataKneeAngles)=1:nrow(rightdataKneeAngles)
rightdataAnkleAngles<-rightdata[rightdata$VariableName=="AnkleAngles",]  # dim (1622,104)
rightdataAnkleAnglesD1 <-rightdataAnkleAngles[rightdataAnkleAngles$Dim=="2",] # dim (811,100)
rownames(rightdataAnkleAnglesD1)=1:nrow(rightdataAnkleAnglesD1)
rightdataFootProgressAngles<-rightdata[rightdata$VariableName=="FootProgressAngles",] # dim (811,104)
rownames(rightdataFootProgressAngles)=1:nrow(rightdataFootProgressAngles)

######################################## FGDI 

ind_P = which(leftdataNewPelAnglesD2[,3] %in% c(c("WBDS01",
                                                  "WBDS02",
                                                  "WBDS03",
                                                  "WBDS04",
                                                  "WBDS05",
                                                  "WBDS06",
                                                  "WBDS07",
                                                  "WBDS08",
                                                  "WBDS09",
                                                  "WBDS10",
                                                  "WBDS11",
                                                  "WBDS12",
                                                  "WBDS13",
                                                  "WBDS14",
                                                  "WBDS15",
                                                  "WBDS16",
                                                  "WBDS17",
                                                  "WBDS18",
                                                  "WBDS19",
                                                  "WBDS20",
                                                  "WBDS21",
                                                  "WBDS22",
                                                  "WBDS23",
                                                  "WBDS24",
                                                  "WBDS25",
                                                  "WBDS26",
                                                  "WBDS27",
                                                  "WBDS28",
                                                  "WBDS29",
                                                  "WBDS30",
                                                  "WBDS31",
                                                  "WBDS32",
                                                  "WBDS33",
                                                  "WBDS34",
                                                  "WBDS35",
                                                  "WBDS36",
                                                  "WBDS37",
                                                  "WBDS38",
                                                  "WBDS39",
                                                  "WBDS40",
                                                  "WBDS41",
                                                  "WBDS42",
                                                  "SUB01_off",
                                                  "SUB02_off",
                                                  "SUB03_off",
                                                  "SUB05_off",
                                                  "SUB06_off",
                                                  "SUB07_off",
                                                  "SUB08_off",
                                                  "SUB09_off",
                                                  "SUB10_off",
                                                  "SUB11_off",
                                                  "SUB12_off",
                                                  "SUB13_off",
                                                  "SUB14_off",
                                                  "SUB15_off",
                                                  "SUB16_off",
                                                  "SUB18_off",
                                                  "SUB19_off",
                                                  "SUB20_off",
                                                  "SUB21_off",
                                                  "SUB22_off",
                                                  "SUB24_off")))


####   FGDI

ind=seq(10,ncol(rightdataFootProgressAngles),by=1)
CPData <- list(leftdataNewPelAnglesD2[ind_P,ind],
               leftdataNewPelAnglesD1[ind_P,ind],
               leftdataNewPelAnglesD3[ind_P,ind],
               leftdataHipAnglesD2[ind_P,ind],
               leftdataHipAnglesD1[ind_P,ind],
               leftdataHipAnglesD3[ind_P,ind],
               leftdataKneeAngles[ind_P,ind],
               leftdataAnkleAnglesD1[ind_P,ind],
               leftdataFootProgressAngles[ind_P,ind],
               rightdataNewPelAnglesD2[ind_P,ind],
               rightdataNewPelAnglesD1[ind_P,ind],
               rightdataNewPelAnglesD3[ind_P,ind],
               rightdataHipAnglesD2[ind_P,ind],
               rightdataHipAnglesD1[ind_P,ind],
               rightdataHipAnglesD3[ind_P,ind],
               rightdataKneeAngles[ind_P,ind],
               rightdataAnkleAnglesD1[ind_P,ind],
               rightdataFootProgressAngles[ind_P,ind])

source("FGDIF.R")
ID = c(rep("Case",21),rep("Control",42))
FGDI = FGDIF(CPData,ID,0.99)

RMSE_FGDI = matrix(0,63,18)
for(i in 1:18){
  RMSE_FGDI[,i] = sqrt(1/101*rowSums((CPData[[i]]-FGDI$Fits[[i]])^2))
}


####   GPS

CPData2 <- list(leftdataNewPelAnglesD1[ind_P,ind],
                leftdataNewPelAnglesD2[ind_P,ind],
                leftdataNewPelAnglesD3[ind_P,ind],
                leftdataHipAnglesD1[ind_P,ind],
                leftdataHipAnglesD2[ind_P,ind],
                leftdataHipAnglesD3[ind_P,ind],
                leftdataKneeAngles[ind_P,ind],
                leftdataAnkleAnglesD1[ind_P,ind],
                leftdataFootProgressAngles[ind_P,ind],
                rightdataHipAnglesD1[ind_P,ind],
                rightdataHipAnglesD2[ind_P,ind],
                rightdataHipAnglesD3[ind_P,ind],
                rightdataKneeAngles[ind_P,ind],
                rightdataAnkleAnglesD1[ind_P,ind],
                rightdataFootProgressAngles[ind_P,ind])

source("GPS.R")
ID = c(rep("Case",21),rep("Control",42))
GPSv <- GPS(CPData2,ID)

####   Overall Abnormality

#Overal
ind=seq(10,ncol(rightdataFootProgressAngles),by=1)
CPData3 <- cbind(leftdataNewPelAnglesD1[ind_P,ind],
                leftdataNewPelAnglesD2[ind_P,ind],
                leftdataNewPelAnglesD3[ind_P,ind],
                leftdataHipAnglesD1[ind_P,ind],
                leftdataHipAnglesD2[ind_P,ind],
                leftdataHipAnglesD3[ind_P,ind],
                leftdataKneeAngles[ind_P,ind],
                leftdataAnkleAnglesD1[ind_P,ind],
                leftdataFootProgressAngles[ind_P,ind],
                rightdataHipAnglesD1[ind_P,ind],
                rightdataHipAnglesD2[ind_P,ind],
                rightdataHipAnglesD3[ind_P,ind],
                rightdataKneeAngles[ind_P,ind],
                rightdataAnkleAnglesD1[ind_P,ind],
                rightdataFootProgressAngles[ind_P,ind])

source("overall_abnormality_fun.R")
source("prcompRecon.R")
G=t(CPData3)
Gcase = G[,ID=="Case"]
GControl = G[,ID=="Control"]
OA = matrix(0,ncol(Gcase),1) 
for(i in 1:ncol(G)){
  OAv = overall_abnormality_fun(as.vector(G[,i]),t(GControl))
  OA[i] = OAv[[1]]
}

refpop_means <- colMeans(t(GControl))
refpop_sd <- apply(t(GControl), 2, stats::sd)
RMSE_OAv = matrix(0,63,15)
for(i in 1:ncol(G)){
  Subj_sc <- (as.vector(G[,i]) - refpop_means)/refpop_sd
  Subj_projs <- Subj_sc %*% OAv[[2]]$rotation[, 1:OAv[[3]]]
  fits <- OAv[[2]]$rotation[, 1:OAv[[3]]]%*%t(Subj_projs)
  fits <- scale(t(as.matrix(fits)), center=FALSE, scale=1/OAv[[2]]$scale)
  fits <- scale(fits , center=-OAv[[2]]$center, scale=FALSE)
  for(j in 1:15){
    RMSE_OAv[i,j]=sqrt(1/101*sum((as.vector(G[(1+(101*(j-1))):(101*(j)),i])-fits[(1+(101*(j-1))):(101*(j))])^2))
  }
}

####   Figure 5
cor.test(FGDI$zFGDI,GPSv[[1]], method="kendall") 
cor.test(FGDI$zFGDI,OA, method="kendall") 
cor.test(OA,GPSv[[1]], method="kendall") 

source("rescale_d.R")
data = data.frame(RMSE=c(rescale_d(FGDI$zFGDI),rescale_d(OA),rescale_d(GPSv[[1]])),
                  method=c(rep("FGDI",63),rep("OA",63),rep("GPS",63)),
                  Severity=as.factor(c(as.character(Scale[ind_I]),rep("Healthy",42))))

library("forcats")
p2 <- data %>% mutate(Severity = fct_relevel(Severity, 
                                             "Healthy","1","2",
                                             "3","4")) %>%
  ggplot(aes(y=RMSE, x=method, fill=Severity)) + 
  geom_boxplot()+xlab("Method") + ylab("Abnormaility")+ theme_minimal()+scale_fill_grey()


data = data.frame(RMSE=rescale_d(FGDI$zFGDI),
                  Severity=as.factor(c(as.character(Scale[ind_I]),rep("Healthy",42))))
data=data[1:21,]
kruskal.test(RMSE ~ Severity, data =data)

data = data.frame(RMSE=rescale_d(OA),
                  Severity=as.factor(c(as.character(Scale[ind_I]),rep("Healthy",42))))
data=data[1:21,]
kruskal.test(RMSE ~ Severity, data =data)

data = data.frame(RMSE=rescale_d(GPSv[[1]]),
                  Severity=as.factor(c(as.character(Scale[ind_I]),rep("Healthy",42))))
data=data[1:21,]

kruskal.test(RMSE ~ Severity, data =data)

####   Figure 6

data = data.frame(RMSE=c(rowMeans(RMSE_FGDI),rowMeans(RMSE_OAv)),
                  method=as.factor(c(rep("FGDI",63),rep("OA",63))),
                  Severity=as.factor(c(as.character(Scale[ind_I]),rep("Healthy",42))))

library("forcats")
p2 <- data %>% mutate(Severity = fct_relevel(Severity, 
                                             "Healthy","1","2",
                                             "3","4")) %>%
  ggplot(aes(y=RMSE, x=method, fill=Severity)) + 
  geom_boxplot()+ ylab("RMSE")+xlab("Method") +theme_minimal()+scale_fill_grey()

mean(data$RMSE[data$method=="FGDI"])
mean(data$RMSE[data$method=="OA"])