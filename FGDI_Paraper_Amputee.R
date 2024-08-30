#### Data #####################
data=read.csv("Amputee_Data.csv",header=TRUE)

# # Dividing the data into two parts for Left and Right 
#Left
leftdata<-data[data$Side=="L", ]
leftdataNewPelAngles<-leftdata[leftdata$VariableName=="PelvisAngles",] 
leftdataNewPelAnglesD1 <-leftdataNewPelAngles[leftdataNewPelAngles$Dim=="1",] 
leftdataNewPelAnglesD2 <-leftdataNewPelAngles[leftdataNewPelAngles$Dim=="2",] 
leftdataNewPelAnglesD3 <-leftdataNewPelAngles[leftdataNewPelAngles$Dim=="3",] 
rownames(leftdataNewPelAnglesD1)=1:nrow(leftdataNewPelAnglesD1)
rownames(leftdataNewPelAnglesD2)=1:nrow(leftdataNewPelAnglesD2)
rownames(leftdataNewPelAnglesD3)=1:nrow(leftdataNewPelAnglesD3)
leftdataHipAngles<-leftdata[leftdata$VariableName=="HipAngles",]    
leftdataHipAnglesD1 <-leftdataHipAngles[leftdataHipAngles$Dim=="1",] 
leftdataHipAnglesD2 <-leftdataHipAngles[leftdataHipAngles$Dim=="2",] 
leftdataHipAnglesD3 <-leftdataHipAngles[leftdataHipAngles$Dim=="3",] 
rownames(leftdataHipAnglesD1)=1:nrow(leftdataHipAnglesD1)
rownames(leftdataHipAnglesD2)=1:nrow(leftdataHipAnglesD2)
rownames(leftdataHipAnglesD3)=1:nrow(leftdataHipAnglesD3)
leftdataKneeAngles<-leftdata[leftdata$VariableName=="KneeAngles",]   
rownames(leftdataKneeAngles)=1:nrow(leftdataKneeAngles)
leftdataAnkleAngles<-leftdata[leftdata$VariableName=="AnkleAngles",]  
leftdataAnkleAnglesD1 <-leftdataAnkleAngles[leftdataAnkleAngles$Dim=="2",] 
rownames(leftdataAnkleAnglesD1)=1:nrow(leftdataAnkleAnglesD1)
leftdataFootProgressAngles<-leftdata[leftdata$VariableName=="FootProgressAngles",] 
rownames(leftdataFootProgressAngles)=1:nrow(leftdataFootProgressAngles)

#Right
rightdata<-data[data$Side=="R", ]
rightdataNewPelAngles<-rightdata[rightdata$VariableName=="PelvisAngles",] 
rightdataNewPelAnglesD1 <-rightdataNewPelAngles[rightdataNewPelAngles$Dim=="1",] 
rightdataNewPelAnglesD2 <-rightdataNewPelAngles[rightdataNewPelAngles$Dim=="2",] 
rightdataNewPelAnglesD3 <-rightdataNewPelAngles[rightdataNewPelAngles$Dim=="3",] 
rownames(rightdataNewPelAnglesD1)=1:nrow(rightdataNewPelAnglesD1)
rownames(rightdataNewPelAnglesD2)=1:nrow(rightdataNewPelAnglesD2)
rownames(rightdataNewPelAnglesD3)=1:nrow(rightdataNewPelAnglesD3)
rightdataHipAngles   <-rightdata[rightdata$VariableName=="HipAngles",]    
rightdataHipAnglesD1 <-rightdataHipAngles[rightdataHipAngles$Dim=="1",] 
rightdataHipAnglesD2 <-rightdataHipAngles[rightdataHipAngles$Dim=="2",] 
rightdataHipAnglesD3 <-rightdataHipAngles[rightdataHipAngles$Dim=="3",] 
rownames(rightdataHipAnglesD1)=1:nrow(rightdataHipAnglesD1)
rownames(rightdataHipAnglesD2)=1:nrow(rightdataHipAnglesD2)
rownames(rightdataHipAnglesD3)=1:nrow(rightdataHipAnglesD3)
rightdataKneeAngles<-rightdata[rightdata$VariableName=="KneeAngles",]   
rownames(rightdataKneeAngles)=1:nrow(rightdataKneeAngles)
rightdataAnkleAngles<-rightdata[rightdata$VariableName=="AnkleAngles",]  
rightdataAnkleAnglesD1 <-rightdataAnkleAngles[rightdataAnkleAngles$Dim=="2",] 
rownames(rightdataAnkleAnglesD1)=1:nrow(rightdataAnkleAnglesD1)
rightdataFootProgressAngles<-rightdata[rightdata$VariableName=="FootProgressAngles",] 
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
                                                  "TF01",
                                                  "TF02",
                                                  "TF05",
                                                  "TF06",
                                                  "TF07",
                                                  "TF08",
                                                  "TF09",
                                                  "TF10",
                                                  "TF11",
                                                  "TF12",
                                                  "TF13",
                                                  "TF14",
                                                  "TF15",
                                                  "TF16",
                                                  "TF17",
                                                  "TF18",
                                                  "TF19",
                                                  "TF20")))

ind=seq(10,ncol(rightdataFootProgressAngles),by=1)
CPData <- list(leftdataNewPelAnglesD2[ind_P,ind],
               leftdataNewPelAnglesD2[ind_P,ind],
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
ID = c(rep("Case",18),rep("Control",42))
FGDI = FGDIF(CPData,ID,0.99)

##### Figure 4 #################################
#Amutated Side
data_info=read.csv("Amp_Info.csv",header=TRUE)
Side=as.factor(data_info$Amputation.side)
ind_L=which(Side=="Left")
ind_R=which(Side=="Right")
Subjects = rightdataHipAnglesD3[ind_P[1:18],3]
KLevel=data_info$K.Level
AS = data_info$Amputation.side

data = data.frame(Sub=c(Subjects[ind_L],Subjects[ind_R]),
                  sFGDI=c(FGDI$SFGDIL[ind_L],FGDI$SFGDIR[ind_R]),
                  Severity=as.factor(c(KLevel[ind_L],KLevel[ind_R])),
                  AS = as.factor(c(AS[ind_L],AS[ind_R])))

wilcox.test(sFGDI ~ Severity, data = data,alternative = "greater")

library("ggpubr")
p1=ggline( data, x = "Severity", y = "sFGDI",
           add = c("mean_se", "dotplot"),
           ylab ="sFGDI on the Amputated Leg",xlab="K-Level")+ scale_color_grey(start = 0.8, end = 0.2) 

data = data.frame(Sub=c(Subjects[ind_R],Subjects[ind_L]),
                  sFGDI=c(FGDI$SFGDIL[ind_R],FGDI$SFGDIR[ind_L]),
                  Severity=as.factor(c(KLevel[ind_R],KLevel[ind_L])),
                  AS = as.factor(c(AS[ind_R],AS[ind_L])))

wilcox.test(sFGDI ~ Severity, data = data,alternative = "greater")

library("ggpubr")
p2=ggline( data, x = "Severity", y = "sFGDI",
           add = c("mean_se", "dotplot"),ylab ="sFGDI on the non Amputated Leg",xlab="K-Level")+ scale_color_grey(start = 0.8, end = 0.2) 

library("ggpubr")
library("gridExtra")
grid.arrange(p1, p2, ncol = 2, nrow = 1)

