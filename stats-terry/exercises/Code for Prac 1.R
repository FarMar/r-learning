#Code for Prac 1

#We will use following libraries (software packages written by someone else) for our purpose. It is usually a good practice to have the libraries set at the top of the script so that you and anyone else using the script can know upfron what software packages (libraries) will be used.
######NOTE: If you haven't installed libraries listed below then you will need to install them first as follows. You will need internet connection for the installation and you will have to remove the comment character "#" from the front of the line.
#install.packages("ggplot2")
#install.packages("emmeans")

library(ggplot2)
library(emmeans)



##We will set up a working directory as follows. The following directory path should be set to the directory path relevent to your computer.
setwd("C:/Users/u4321232/Dropbox/RSB short course/Data")

#Question #1: Seed orchard data

#Import data using read.csv function. There are numerous functions and packages that come preinstalled with R and read.csv is one of them. On the bottom right corner of your RStudiom there is help tab. you can search for help regarding the usage of packages, libraries, function. Please try read.csv as a search term in the help tab and have a look at the variety of options available when reading a csv file
data1 <- read.csv("Prac 1 seed orchard data.csv")

#Check data structure using 'str'ucture command. One can have data.frame, matrix, vector, list and many more data structures that various functionalities in R can understand and manipulate. For now, data.frame should suffice.
str(data1)

#Visualise data using ggplot
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+geom_point()
#you can create box plots
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+geom_boxplot()
#you can combine boxplot and point plot
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+geom_boxplot()+geom_point()
#you can change the background to black and white
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+geom_boxplot()+geom_point()+theme_bw()
#you can modify axis titles
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+geom_boxplot()+geom_point()+theme_bw()+xlab("Seed Lot")+ylab("Diameters at Breast Height (inches)")
#you can add title to the plot
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+
  geom_boxplot()+geom_point()+theme_bw()+xlab("Seed Lot")+
  ylab("Diameters at Breast Height (cm)")+labs(title="Tree Growth by Seed Lot Type")

#If you are happy with the plot and want to export the plot then you can do so by using the Export tab in the bottom right corner panel where plots are usually displayed.
#Programmatically, you can do so to export a PDF file
#open a pdf document with height 4 and width 6 inches.
pdf("seedlot.boxplot.pdf", height=4,width=6)
#draw the plot to the PDF file
ggplot(data1,aes(seedlot,dbh,colour=seedlot))+geom_boxplot()+geom_point()+
  theme_bw()+xlab("Seed Lot")+ylab("Diameter at Breast Height (inches)")+
  labs(title="Tree Growth by Seed Lot Type")
##close the PDF file. Dont forget to do this as it will keep writing to the same file.
dev.off()
#you can check out the newly created PDF file in the directory that was set intially at the top
##more information about the various ggplot functions and modification, go to http://docs.ggplot2.org/current/
##I would usually just google for a particular question such as "how to modify legend title ggplot2"
##There are more than many ways to achieve the same task with ggplot2, so feel free to select the answer 
##that suits you best. There is no right or wrong way of doing it.


#Fit model, inference, and estimate means (SE)
model1<-lm(dbh~seedlot, data=data1)
anova(model1)
summary(model1)
emmeans(model1,~seedlot)

##what is the data 'str'ucture for model1
str(model1)
##it shows as List of 13
##what are the items in list
names(model1)

#Assess model assumptions
plot(model1)
##NOTE: multiple plots are drawn when you hit enter. Once you are familiar with the type of plots and information content of plots, you can choose which plot to draw. For example we know that we would want the first plot only then we can use the following
plot(model1, which=1:2)
##again you can export the plot into PDF if you wish as follows
pdf("fitted.vs.residuals.pdf")
plot(model1, which=1)
dev.off()


##you can plot various aspects of the fitted model using ggplot2 for exploration by following help at http://docs.ggplot2.org/0.9.3.1/fortify.lm.html
##first we modify the data in the "model1" object for syntax that is acceptable by ggplot2
ggmodel1 <- fortify(model1,data1)
ggplot(ggmodel1, aes(.fitted,.resid))+geom_point()+geom_hline(yintercept=0)+geom_smooth(se=FALSE)

##get the emmeans output converted to a data.frame for easy understanding and manipulation
summarisedinfo <- summary(emmeans(model1,~seedlot))
ggplot(summarisedinfo,aes(seedlot,emmean,fill=seedlot))+geom_bar(stat="identity", width=.25)+
  ylim(0,40)+geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.10) + 
  theme(axis.text=element_text(size=10)) + xlab("Seed Lot") + ylab("Diameter at Breast Ht (cm)")

###once the plots are obtained, you can also write the tabular output to a file
write.table(summarisedinfo, file="summary.txt",sep="\t", row.names=F, quote = F)

##you can check help pages for any function by putting a question mark before the name of the function.
#?write.table


#Question #2 Pea data
#Import data and check data types
data2 <- read.csv("Prac 1 pea data.csv")
str(data2)

#Visualise data
ggplot(data2,aes(sugar,length,colour=sugar))+geom_point()+geom_boxplot()

#Fit model, inference, and estimate means (SE)
model2<-lm(length~sugar, data=data2)
anova(model2)
summary(model2)
emmeans(model2,pairwise~sugar)

#Set up graph summarising the model
sum_model <- summary(emmeans(model2,~sugar))
ggplot(sum_model,aes(sugar,emmean,fill=sugar))+geom_bar(stat="identity", width=.5)+
 geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE),width=0.10) + 
  theme(axis.text=element_text(size=10)) + xlab("sugar") + 
  ylab("length")



#Assess model assumptions
plot(model2,1)


