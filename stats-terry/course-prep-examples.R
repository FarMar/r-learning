
#create bar plots and error bars for Message 1

data1_trep<-data.frame(list(treatment=rep(c("control","inoculated"),each=6,times=2), 
                       transporter=rep(c("N","P"),each=12),
                       rel_exp=c(rnorm(6,.1,.01),rnorm(6,.3,.02),rnorm(6,.4,.03), rnorm(6,.5,.03))))
                       
ggplot(data1_trep,aes(transporter,rel_exp,colour=treatment))+geom_point()

datab<-data.frame(list(treatment=rep(c("control","inoculated"),each=6,times=2), 
                            transporter=rep(c("N","P"),each=12),
                            rel_exp=c(rnorm(6,.1,.05),rnorm(6,.3,.15),rnorm(6,.4,.2), rnorm(6,.5,.2))))

ggplot(datab,aes(transporter,rel_exp,colour=treatment))+geom_point()

#Plot for Message 3
library(agridat)
turnip<-mcconway.turnip
turnip$variety<-turnip$gen
str(turnip)
ggplot(turnip,aes(factor(density),  yield, colour=variety))+geom_point()+
  geom_boxplot()+ theme(legend.position="bottom")

#Plot for Message 4: Example 2
milk<-brandt.switchback
str(milk)
library(lmerTest)
anova(lm(yield~trt,data=milk))
ggplot(milk,aes(trt,yield,colour=trt))+geom_boxplot()+
  scale_colour_manual(values=c("green","blue"))+
  labs(colour = "Diet")+annotate("text",label="p=0.702",x = 1.5, y = 1000, color = "black")

milk$Period<-as.numeric(factor(milk$period,labels=1:3))
lm1<-lm(yield~trt+period+group+ cow,data=milk)
anova(lm1)
summary(lm1)
ggplot(milk,aes(Period,yield,colour=cow))+geom_point()+geom_line()+
  facet_wrap(~group)+scale_x_continuous(breaks=seq(1, 3, 1))

milk



#Plot for Message 4: Example 1
library(MASS)
library(reshape2)
library(ggplot2)
barley<-immer
str(barley)
barley2<-melt(barley,id.vars=c("Loc","Var"), value.name = "Yield",variable.name="Year")
str(barley2)

anova(lm(Yield~Var, data=barley2))
ggplot(barley2, aes(Var, Yield)) + geom_boxplot()+
  annotate("text", label = "p=0.13, ANOVA df=4", x = 1.5, y = 180, color = "black")

anova(lm(Yield~Var+Year, data=barley2))
ggplot(barley2, aes(Var,Yield, colour=Year))+geom_boxplot()+
  scale_colour_manual(values = c("magenta","dark green"))+
  annotate("text", label = "p=0.106, ANOVA df=4", x = 1.5, y = 180, color = "black")
  
anova(lm(Yield~Var+Loc, data=barley2))
ggplot(barley2, aes(Var,Yield, colour=Loc))+geom_boxplot()+
  scale_colour_manual(values = c("magenta","dark green","red","yellow", "dark blue","green"))+
  annotate("text", label = "p=0.011, ANOVA df=4", x = 1.5, y = 180, color = "black")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(barley2, aes(Loc,Yield, colour=Var))+geom_boxplot()+
  scale_colour_manual(values=cbPalette)+
  annotate("text", label = "p=0.011, ANOVA df=4", x = 2, y = 180, color = "black")+
  labs(x="Location")

anova(lm(Yield~Var+Loc+Year, data=barley2))

#Chapter 1
#Wheat yield experiment
setwd("C:/Users/u4321232/Dropbox/CSIRO short course/Data")
#Make up data
wheat<-data.frame(list(ExpID=rep(1:2,each=12),
                       PlotID=rep(1:12,2),Variation=rep(c("High","Low"), each=12),
                       Variety=rep(c("Standard","New"),each=6,times=2),
                       Mean_Yield=rep(c(2.8,3.1), each=6, times=2)))
wheat$Variety<-relevel(wheat$Variety,ref="Standard")
set.seed(12345);wheat$Yield[1:12]<-wheat$Mean_Yield[1:12] + rnorm(12,0,1)
set.seed(1361);wheat$Yield[13:24]<-wheat$Mean_Yield[13:24] + rnorm(12,0,.2)
write.csv(wheat[,c("ExpID","PlotID","Variation","Variety","Yield")], "wheat yield.csv")
ggplot(wheat,aes(Variety,Yield,colour=Variety))+ geom_point()+facet_wrap(~Variation)+
  labs(y="Yield (tonnes/hectare)")+ theme(legend.position="top")+
  ylim(1,4)

wheat<-read.csv("wheat yield.csv")
wheat$Variety<-relevel(wheat$Variety,ref="Standard")
str(wheat)
wheat_H<-subset(wheat,Variation=="High")
wheat_L<-subset(wheat,Variation=="Low")
t.test(Yield~Variety, data=wheat_H, var.equal=TRUE)
t.test(Yield~Variety, data=wheat_L, var.equal=TRUE)

lm1<-lm(Yield~Variety, data=wheat_L)
anova(lm1)
summary(lm1)
library(emmeans)
emmeans(lm1,~Variety)
plot(lm1,which=1)

lm1.results<-summary(emmeans(lm1,~Variety))
ggplot(lm1.results,aes(Variety, emmean, fill=Variety))+geom_bar(stat="identity", width=.4)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE),width=0.1)+
  ylim(0,5)+
  labs(y="Mean Yield (t/hectare)")+
  annotate("text",x=1.5, y=4.5, label="p = 0.002")

wheat2<-wheat_L[,c("PlotID","Variety","Yield")]
set.seed(574638)
wheatPLUS<-data.frame(list(PlotID=13:18,Variety=rep("NewPlus",6),Yield=rnorm(6,3.3,.2)))
wheat2<-rbind(wheat2,wheatPLUS) 
ggplot(wheat2,aes(Variety,Yield,colour=Variety))+ geom_point()+
  labs(y="Yield (tonnes/hectare)")+ theme(legend.position="top")
write.csv(wheat2,"wheat yield PLUS.csv")
aov1<-aov(Yield~Variety, data=wheat2)
summary(aov(Yield~Variety, data=wheat2))
emmeans(aov1, pairwise~Variety)
lm2<-lm(Yield~Variety, data=wheat2)
anova(lm2)
summary(lm2)
lm2.results<-summary(emmeans(lm2,~Variety))
ggplot(lm2.results,aes(Variety, emmean, fill=Variety))+geom_bar(stat="identity", width=.4)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE),width=0.1)+
  ylim(0,5)+
  labs(y="Mean Yield (t/hectare)")+
  annotate("text",x=1.5, y=4.5, label="p = 0.002")
plot(lm2,which=1:2)
#Chapter 3 
#Sorghum drought tolerance, mock LWR
library(ggplot2)
library(emmeans)
setwd("C:/Users/u4321232/Dropbox/CSIRO short course/Data")
data2<-read.csv("Prac 3 mock LWR.csv")
str(data2)
View(data2)
ggplot(data2,aes(GeneB,LWR,colour=GeneA))+ geom_boxplot()+geom_point()

lm1<-lm(LWR~GeneA*GeneB, data=data2)
anova(lm1)
emmeans(lm1,pairwise~GeneA|GeneB)
plot(lm1, which=1:2)

#Which cultivars of cabbage give the best Headwt and Vit C content?
library(MASS)
cabbage<-cabbages
str(cabbage)
write.csv(cabbage,"Prac 3 cabbage data.csv")
cabbage<-read.csv("Prac 3 cabbage data.csv")
ggplot(cabbage,aes(Date,VitC,colour=Cult))+geom_boxplot()+
  theme_classic()
lm2<-lm(VitC~Cult+Date, data=cabbage)
anova(lm2)
emmeans(lm2,pairwise~Cult)
plot(lm2,which=1:2)

#Chapter 3, Example 3 Barley yield
library(MASS)
library(reshape2)
library(ggplot2)
barley<-immer
write.csv(barley,"Prac 3 barley yield.csv")
str(barley)
barley2<-melt(barley,id.vars=c("Loc","Var"), value.name = "Yield",variable.name="Year")
str(barley2)

anova(lm(Yield~Var, data=barley2))
ggplot(barley2, aes(Var, Yield)) + geom_boxplot()+
  annotate("text", label = "p=0.13, ANOVA df=4", x = 1.5, y = 180, color = "black")

anova(lm(Yield~Var+Year, data=barley2))
ggplot(barley2, aes(Var,Yield, colour=Year))+geom_boxplot()+
  scale_colour_manual(values = c("magenta","dark green"))
  annotate("text", label = "p=0.106, ANOVA df=4", x = 1.5, y = 180, color = "black")

anova(lm(Yield~Var+Loc, data=barley2))
ggplot(barley2, aes(Var,Yield, colour=Loc))+geom_boxplot()+
  scale_colour_manual(values = c("magenta","dark green","red","yellow", "dark blue","green"))+
  annotate("text", label = "p=0.011, ANOVA df=4", x = 1.5, y = 180, color = "black")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(barley2, aes(Loc,Yield, colour=Var))+geom_boxplot()+
  scale_colour_manual(values=cbPalette)+
  annotate("text", label = "p=0.011, ANOVA df=4", x = 2, y = 180, color = "black")+
  labs(x="Location")

lm5<-lm(Yield~Loc+Year+Var, data=barley2)
anova(lm5)
library(emmeans)
emmeans(lm5,pairwise~Var)
plot(lm5,which=1:2)
#Chapter 3, Example 4 forest plots from Statistics in Biology 
setwd("C:/Users/u4321232/Dropbox/CSIRO short course/Data")
forest<-read.csv("Prac 3 forest.csv")
str(forest)
ggplot(forest,aes(QuadDiam,Density, colour=StandType))+geom_point()
#model prototypes
model0<-data.frame(list(QuadDiam=forest$QuadDiam, Density=rnorm(41,3000,1500),
                        StandType=sample(forest$StandType,41,replace=FALSE)))
ggplot(model0,aes(QuadDiam, Density, colour=StandType)) + geom_point()+
  geom_hline(yintercept = 3085)+theme(legend.position="none")

model1<-data.frame(list(QuadDiam=forest$QuadDiam, Density=forest$Density,
                        StandType=sample(forest$StandType,41,replace=FALSE)))
summary(lm(Density~QuadDiam, data=model1))
ggplot(model1,aes(QuadDiam, Density, colour=StandType)) + geom_point()+
  geom_abline(intercept = 5153, slope=-162)+theme(legend.position="none")

summary(lm(Density~QuadDiam+StandType, data=forest))
ggplot(forest,aes(QuadDiam, Density, colour=StandType)) + geom_point()+
  geom_abline(intercept = 6752, slope=-207)+
  geom_abline(intercept = 6752-1153, slope=-207)+
  geom_abline(intercept = 6752-1345, slope=-207)+
  theme(legend.position="none")

ggplot(forest,aes(QuadDiam, Density, colour=StandType)) + geom_point()+
  stat_smooth(method="lm",level=0)+
  theme(legend.position="none")

lm6<-lm(Density~QuadDiam*StandType, data=forest)
anova(lm6)
emmeans(lm6,pairwise~StandType)


#Chapter 4, Example 1 Phtosynthesis

setwd("C:/Users/u4321232/Dropbox/CSIRO short course/Data")
photosynthesis<-read.csv("Prac 4 photosynthesis.csv")
str(photosynthesis)
photosynthesis$PlantID<-factor(photosynthesis$PlantID)
photosynthesis$Position<-factor(photosynthesis$Position)
photosynthesis$Temp<-factor(photosynthesis$Temp, labels=c("Low","High"))
model1<-lm(PhotoRate~Temp, data=photosynthesis)
anova(model1)
ggplot(photosynthesis,aes(Temp,PhotoRate,colour=Position))+geom_point()+
  labs(x="Temperature")

library(lmerTest)
library(emmeans)
lmer1<-lmer(PhotoRate~Temp+(1|Position), data=photosynthesis)
anova(lmer1)
summary(lmer1)
emmeans(lmer1,pairwise~Temp)
plot(lmer1)


#Final Example 1: Arabidopsis nitrogen experiment
library(ggplot2)
setwd("C:/Users/u4321232/Dropbox/CSIRO short course/Data")
nitro<-read.csv("Prac 5 Arabidopsis nitrogen.csv")
str(nitro)
nitro$Tank<-factor(nitro$Tank)
nitro$Nitrogen<-factor(nitro$Nitrogen)
nitro$Day<-factor(nitro$Day)
with(nitro, table(Tank, Day))
ggplot(nitro,aes(Day,log(PlantDM),colour=Nitrogen))+geom_boxplot()
lm1<-lm(log(PlantDM)~Nitrogen*Day, data=nitro)
anova(lm1)
lmer1<-lmer(log(PlantDM)~Nitrogen*Day+(1|Tank), data=nitro)
anova(lmer1)
summary(lmer1)
emmeans(lmer1,~Nitrogen*Day, type="response")
plot(lmer1)

#Example 2: Plant growth regulator
#make up data
#3 Latin Squares
library(dae)
Dose<-factor(rep(1:6, times=6))
plant5<-designRandomize(recipient = list(Column=6,Row=6),
                           nested.recipients = list(Row="Column"),
                           allocated = Dose, seed=24564 )
plant5$height<-10+as.numeric(plant5$Dose)+rnorm(36,0,2)+as.numeric(plant5$Row)-
  as.numeric(plant5$Column)
ggplot(plant5,aes(Dose,height))+geom_point()
lmer2<-lmer(height~Dose+(1|Row)+(1|Column), data=plant5)
anova(lmer2)
lm2<-lm(height~Dose, data=plant5)
anova(lm2)
write.csv(plant5,"Prac 5 plant growth.csv")

#Example 3: plant phenotype with nematodes

nem<-read.csv("Prac 5 nematode.csv")
str(nem)
nem$Experiment<-factor(nem$Experiment)
ggplot(nem,aes(Genotype, Plant.weight,colour=Treatment))+geom_boxplot()
ggplot(nem,aes(Genotype, lateral.root..root.weight,colour=Treatment))+geom_boxplot()
