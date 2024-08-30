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

##### Figure 1 #################################

ind_H=which(ID=="Control")
ind_S= which.max(as.numeric(abs(FGDI$zFGDI)))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[1]][ind_H,]))),
                         as.numeric(CPData[[1]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p1 <- ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[1]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Pelvis tilt") + 
  coord_cartesian(xlim = c(0, 100))+
  xlab("% of Gait cycle") + ylab("Angle")+theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))


df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[2]][ind_H,]))),
                         as.numeric(CPData[[2]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p2 <-  ggmatplot(x = seq(0,100,1),
                 y = t(as.matrix(CPData[[2]][ind_H,])),
                 plot_type = "line",color = c("lightgrey"),linetype="dotted",
                 size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Pelvis obliquity")+
  ylab("Angle")+theme_bw()+theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[3]][ind_H,]))),
                         as.numeric(CPData[[3]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p3 <- ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[3]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Pelvis rotation") + ylab("Angle")+
  xlab("% of Gait cycle") +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[4]][ind_H,]))),
                         as.numeric(CPData[[4]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p4 <-  ggmatplot(x = seq(0,100,1),
                 y = t(as.matrix(CPData[[4]][ind_H,])),
                 plot_type = "line",color = c("lightgrey"),linetype="dotted",
                 size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Hip flexion/extension") + ylab("Angle")+
  xlab("% of Gait cycle") +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[5]][ind_H,]))),
                         as.numeric(CPData[[5]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p5 <- ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[5]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Hip add/abduction") + ylab("Angle")+
  xlab("% of Gait cycle") +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[6]][ind_H,]))),
                         as.numeric(CPData[[6]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p6 <- ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[6]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Hip rotation") + ylab("Angle") +
  xlab("% of Gait cycle") + ylab("Angle")+theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[7]][ind_H,]))),
                         as.numeric(CPData[[7]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p7 <- ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[7]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Knee flexion/extension") + ylab("Angle")+
  xlab("% of Gait cycle") +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[8]][ind_H,]))),
                         as.numeric(CPData[[8]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p8 <-  ggmatplot(x = seq(0,100,1),
                 y = t(as.matrix(CPData[[8]][ind_H,])),
                 plot_type = "line",color = c("lightgrey"),linetype="dotted",
                 size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Ankle dorsi/plantarflexion") +  ylab("Angle") +
  xlab("% of Gait cycle") +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[9]][ind_H,]))),
                         as.numeric(CPData[[9]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p9 <- ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[9]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("LHS Foot int/external rotation") + ylab("Angle") +
  xlab("% of Gait cycle") +theme_bw()+theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[13]][ind_H,]))),
                         as.numeric(CPData[[13]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p10 <-  ggmatplot(x = seq(0,100,1),
                  y = t(as.matrix(CPData[[13]][ind_H,])),
                  plot_type = "line",color = c("lightgrey"),linetype="dotted",
                  size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("RHS Hip flexion/extension") + ylab("Angle")+
  xlab("% of Gait cycle") +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[14]][ind_H,]))),
                         as.numeric(CPData[[14]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p11 <- ggmatplot(x = seq(0,100,1),
                 y = t(as.matrix(CPData[[14]][ind_H,])),
                 plot_type = "line",color = c("lightgrey"),linetype="dotted",
                 size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("RHS Hip add/abduction") + ylab("Angle")+
  xlab("% of Gait cycle")  +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[15]][ind_H,]))),
                         as.numeric(CPData[[15]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p12 <-ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[15]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("RHS Hip rotation") + ylab("Angle") +
  xlab("% of Gait cycle") + ylab("Angle")+  theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[16]][ind_H,]))),
                         as.numeric(CPData[[16]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p13 <- ggmatplot(x = seq(0,100,1),
                 y = t(as.matrix(CPData[[16]][ind_H,])),
                 plot_type = "line",color = c("lightgrey"),linetype="dotted",
                 size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("RHS Knee flexion/extension") + ylab("Angle")+
  xlab("% of Gait cycle")  +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[17]][ind_H,]))),
                         as.numeric(CPData[[17]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p14 <- ggmatplot(x = seq(0,100,1),
                 y = t(as.matrix(CPData[[17]][ind_H,])),
                 plot_type = "line",color = c("lightgrey"),linetype="dotted",
                 size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("RHS Ankle dorsi/plantarflexion") +  ylab("Angle") +
  xlab("% of Gait cycle")  +theme_bw()+
  theme(legend.position = "none")+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))

df <- data.frame(xvals=c(seq(0,100,1),seq(0,100,1)),
                 yvals=c(as.numeric(colMeans(as.matrix(CPData[[18]][ind_H,]))),
                         as.numeric(CPData[[18]][ind_S,])),
                 grp=factor(c(rep("Healthy",101),rep("Parkinson's",101))))

p15 <-ggmatplot(x = seq(0,100,1),
                y = t(as.matrix(CPData[[18]][ind_H,])),
                plot_type = "line",color = c("lightgrey"),linetype="dotted",
                size = 1) +geom_line(data=df,aes(x=xvals,y=yvals,group=grp,color=factor(grp)))+
  ggtitle("RHS Foot int/external rotation") + ylab("Angle") +
  xlab("% of Gait cycle") +theme_bw()+
  scale_colour_manual(values=c(rep("lightgrey",42), 
                               "black", "green"))+theme(legend.position = "none")

library("ggpubr")
library("gridExtra")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
             p11, p12, p13, p14, p15, ncol = 3, nrow = 5, heights =rep(4,5))


##### Figure 2 #################################
#MAP

loc=c("Pel tilt","Hip flex","Hip flex","Knee flex","Knee flex",
      "Ank dors","Ank dors","Pel obl","Hip abd","Hip abd",
      "Pel rot","Hip rot","Hip rot","Foot rot","Foot rot")
Side = c("Both","Left","Right","Left","Right","Left",
         "Right","Both","Left","Right","Both","Left","Right",
         "Left","Right")
ind = c(1,4,13,7,16,8,14,2,5,11,3,6,15,9,18)

Scores = data.frame(y=round(matrix(FGDI$zFGDIU[ind_S,ind],1*15,1)),
                    loc=rep(loc,each=1),side=rep(Side,each=1)) 

library("forcats")
Scores %>%
  mutate(loc = fct_relevel(loc, 
                           "Pel tilt","Hip flex","Knee flex",
                           "Ank dors","Pel obl","Hip abd",
                           "Pel rot","Hip rot","Foot rot")) %>%
  ggplot(aes(x=loc, y=y, fill=side)) +
  geom_bar(stat="identity", position = 'dodge')+
  geom_text(aes(label=round(y)), vjust = -0.5, size = 3.5,
            position = position_dodge(width = 1))+
  scale_fill_brewer(palette="Paired")+scale_fill_grey(start = 0.5, end = 0.8) +
  theme_minimal()+xlab("Kinematic Variable") + ylab("sFGDI for a Subject with PD")

##### Figure 3 #################################
data_info=read.csv("PDGinfo.csv",header=TRUE)
Scale = as.numeric(data_info$OFF...Hoehn...Yahr[1:26])
Scale1 = as.numeric(data_info$OFF...UPDRS.II[1:26])
Scale2 = as.numeric(data_info$OFF...UPDRS.III[1:26])
Scale3 = as.factor(data_info$FoG.group[1:26])

ind_I = c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,24)

data = data.frame(Sub=rightdataHipAnglesD3[ind_P[1:21],3],
                  sFGDI=FGDI$zFGDI[1:21],
                  Severity=Scale[ind_I])

kruskal.test(sFGDI ~ Severity, data = data)

q1=qplot(Severity, sFGDI, data=data,main ="Combined Approach",xlab="Hoehn & Yahr")+
  theme(plot.title = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 9)) 

data = data.frame(Sub=rightdataHipAnglesD3[ind_P[1:21],3],
                  sFGDI=FGDI$zFGDI[1:21],
                  Severity=Scale3[ind_I])

res <- wilcox.test(sFGDI ~ Severity, data =data,
                   exact = FALSE,alternative ="greater")
res

q1a=qplot(Severity, sFGDI, data=data,main ="Combined Approach",xlab="Freezing of gait")+
  theme(plot.title = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 9)) 

data = data.frame(Sub=rightdataHipAnglesD3[ind_P[1:21],3],
                  sFGDI=FGDI$zFGDI[1:21],
                  Severity=Scale1[ind_I])

ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "grey", lty=2) +
    theme(plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 9)) +
    labs(title = paste(" Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)))+
    labs(x = "MDS-UPDRS Part II", y="sFGDI")
}

q2=ggplotRegression(lm(sFGDI ~ Severity, data = data))

data = data.frame(Sub=rightdataHipAnglesD3[ind_P[1:21],3],
                  sFGDI=FGDI$zFGDI[1:21],
                  Severity=Scale2[ind_I])

ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "grey", lty=2) +
    theme(plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 9)) +
    labs(title = paste(" Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)))+
    labs(x = "MDS-UPDRS Part III", y="sFGDI")
}

q3=ggplotRegression(lm(sFGDI ~ Severity, data = data))

grid.arrange(q1, q1a, q2, q3, ncol = 4, nrow = 1)
