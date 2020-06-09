# This long script preprocess RT data and provides statistical analysis per subject. 
# Detach multiple libraries all at once.
Vectorize(detach)(name=paste0("package:", c("Hmisc","survival","splines")), unload=TRUE, character.only=TRUE)

library(ggplot2)
library(reshape2)
library(Rmisc)
library(purrr)
library(Hmisc)
library(dplyr)
library(tidyverse)
library(magrittr)  # Has to be the last library to load. This might give you headache...for WISC object..reload it..


rm(list=ls())

#1))
#levCatGA <- read.csv("C:/Users/slotfi/Documents/R/Test/Test/SEM1/CDA_T_PrePost.csv",header = FALSE)
df <- read.csv("C:/Users/slotfi/Documents/R/Loop/SH_Entire ERPS 59Data/SH_GrndMtxBins_All58D&RTerps.csv",header = TRUE)
df <- read.csv("C:/Users/slotfi/Documents/R/Loop/LG_Entire ERPS 51Data/LG_All50D_OnlyCosts.csv",header = TRUE)
df <- read.csv("C:/Users/slotfi/Documents/R/Loop/SHK_Entire ERPS 83Data/2Run_All83D.csv",header = TRUE)
df <- read.csv("C:/Users/slotfi/Documents/R/Loop/SHK_Entire ERPS 83Data/2Run_GrndMtxBins/2Run_GrndMtxBins_All83D&RTbehwos.csv",header = TRUE)
df <- read.csv("C:/Users/slotfi/Documents/R/Loop/SH_Entire ERPS 59Data/SH_GrndMtxBins_All58D&RTerps.csv",header = TRUE)


df = df %>% filter (!Subject %in% c(208,225)) # 206,448,466,472
df1 = df %>% filter (Subject %in% c(106,108:110,115,119,121:127,131,132,133,140:142,144,145,150,153,155,157,162,164))
df1 = df %>% filter (Subject %in% -c(112,116,117,124,132,152,120,165))
DataN = aggregate(df[,1], list(df$Subject), mean) %>% tally()

# & Subject != 116 & Subject != 117  & Subject != 124& Subject != 132 & Subject != 152 & Subject != 120  & Subject != 165)
#levCatGA = myData %>% slice(2:20301)
#2))
# names(levCatGA) <- c("Time","AveCR_BL","AveERN_BL","AveCR_PT","AveERN_PT","Subject")
#names(levCatGA) <- c("Time","CDA_T_Pre","CDA_T_POst","Subject")
myPlotTitle = "SHOCK No208&225"

Var1 = "SHOCK_AbsPure_AveFs"
Var2 = "SHOCK_Abs60_AveFs"

levCatGA = df %>% select (c(Time, Subject, Var1,Var2))

#3))
levCatGA <- levCatGA[(levCatGA$Time >= -200)& (levCatGA$Time <= 800),]


### 4)))
levCatGA$Subject <- as.factor(levCatGA$Subject)
levCatGA <- reshape2::melt(levCatGA,id.vars = c("Subject","Time"))
names(levCatGA) <- c("Subject","Time","condition","amplitude") #don't mess around with this. 


levCat.plot <- ggplot(levCatGA,aes(Time,amplitude))+
  scale_color_brewer(palette = "Set1")

##5))
#Now let's run t-tests on each timepoint (again, using purrr) and also summarize the 
#data using the summarySEwithin function from Rmisc.

#If you are running more than two conditions, you should either comments out 'runningT' or run equivalent ANOVA.
runningT <- levCatGA %>%
  split(.$Time) %>%
  #map(~aov(amplitude~condition, data = .))
  map(~t.test(amplitude~condition, paired = TRUE, data = .)) #aov(weight ~ group, data = my_data)

#testAOV <- aov(amplitude~condition, data = levCatGA)
#test <- summary(testAOV)
#> test[[1]][["Pr(>F)"]] ..Can't this working. When map it, there is no p-value to be pulled out.

runningSE <- levCatGA %>%
  split(.$Time) %>%
  map(~summarySEwithin(data = ., measurevar = "amplitude",
                       withinvars = "condition", idvar = "Subject"))
#Don't run this if more than 2 conditions is provided. 
#6)) I REMOVED this PART. NOT IMporant: to plot ERP with Standard between subject conficence intervals

### to Add Pvalues 
pvals <- data.frame(Time = unique(levCatGA$Time), 
                    p.value = map_dbl(runningT,"p.value"))
ggplot(pvals, aes(x = Time, y = p.value)) +
  geom_point()

###Now, this is the best plot, combining the mean waveforms, with SEM, statistical tests.

pvals$crit <- 0 + (pvals$p.value <= .05)
pvals$crit[pvals$crit == 0] <- NA




####### 7)) to plot ERPs with corrected within subject CIs
# library(magrittr)  # the Function "extract" of [magrittr] might give you headache...for WISC object..reload it..

WSCI <- map_df(runningSE, extract) %>%
  mutate(
    Time = rep(unique(levCatGA$Time), each = 2) 
    #Note, you'll have to change 2 to match the number of conditions
  )


#### Can dots as a variablity of data..
# timePoint <- 650 #time in ms to plot indiv points.
# closestTime <- levCatGA[which.min(abs(levCatGA$Time-timePoint)),]$Time #find the closest time in the actual data

######## 8) Plots with simple t-test comparisons...

levCat.plot+
  geom_ribbon(data = WSCI, aes(ymin = amplitude-ci, ymax = amplitude+ci,
                               fill = condition, colour = condition), 
              linetype="dashed", alpha = 0.05)+
  guides(fill = "none")+
  stat_summary(fun.y = mean, geom = "line", size = 1.5, aes(colour = condition))+ #size is the thickness of line.
  
  # scale_colour_manual(values = c("orange", "darkred", "blue", "darkgreen"))+ use this for manually giving colors for 4 categ.
  coord_cartesian(xlim=c(-200, 800), ylim=c(-2, 7)) + # This manually limits the coordinates. Setup according to DATA.
  scale_x_continuous(breaks = seq(from = -200, to = 800, by = 100)) + # setup the break points..
  
  labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")), colour = "") +
  # below is to setup the font size of the plot.      FRom here:http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software
  # theme(axis.text = element_text(size = 15)) +
  # theme(axis.title = element_text(size = 25)) +# change axis titles
  theme(text = element_text(size = 18)) +# this will change all text size (except geom_text)
  labs (title = paste0(myPlotTitle,"(n=",DataN,")")) +
  # Now let's setout the position of the legend. 1st number rightside (0-1), 2nd# upsize (0-1) 
  theme(legend.position = c(0.13, 0.8)) + 
  theme(legend.background = element_rect(fill="lightgray",  #change the legend color: e.g. "lightblue"
                                         size=0.1, linetype="solid"))+
  #If have only two waves (conditions, t-test): uncomment out to superimpose pvalues. 
  geom_line(data = pvals, aes(x = Time, y = crit-3),na.rm = TRUE,size = 2)+ 
  geom_vline(xintercept = 0, linetype = "dashed" )+  #this is to superimpose a vertical line.
  geom_hline(yintercept = 0, linetype = "dashed")  #to superimpose a horizental line.
# geom_point(data = subset(levCatGA,Time == closestTime),alpha = 0.6) # Could drop this if don't want dots at time pionts.
# scale_colour_manual(values=c("turquoise4", "violetred3")) # "blueviolet", "salmon" "turquoise4", "violetred3" 
# darkorchid darkred darkgreen chartreuse3   coral3 

############################

#pvals$critBon <- 0+ (p.adjust(pvals$p.value,"bonferroni") <= .05)
# pvals$critBon[pvals$critBon == 0] <- NA

levCat.plot +
  geom_ribbon(data = WSCI, aes(ymin = amplitude-ci, ymax = amplitude+ci,
                               fill = condition, colour = condition), 
              linetype="dashed", alpha = 0.05)+
  guides(fill = "none") +
  stat_summary(fun.y = mean, geom = "line", size = 1.5, aes(colour = condition))+ #size is the thickness of line.
  coord_cartesian(xlim=c(-200, 800), ylim=c(-3, 7)) + # This manually limits the coordinates. Setup according to DATA.
  scale_x_continuous(breaks = seq(from = -200, to = 800, by = 100)) + # setup the break points..
  labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")), colour = "") +
  theme(text = element_text(size = 18)) +# this will change all text size (except geom_text)
  labs (title = myPlotTitle) +
  theme(legend.position = c(0.13, 0.8)) + 
  theme(legend.background = element_rect(fill="lightgray",  #change the legend color: e.g. "lightblue"
                                         size=0.1, linetype="solid"))+
  geom_line(data = pvals, aes(x = Time, y = crit-3.5),na.rm = TRUE,size = 2)+ 
  annotate("text", x =-100, y = -1.4, label = "Uncorrected", 
           colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed" )+  #this is to superimpose a vertical line.
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(data = pvalsRT, aes(x = Time, y = crit-3.4),na.rm = TRUE,size = 2,colour = "red") +
  
  annotate("text", x =-50, y = -2.1, label = paste0("RTcorr",Var1), 
           colour = "red")+
  geom_line(data = pvals, aes(x = Time, y = critHolm-3.7),
            na.rm = TRUE, size = 2, colour = "blue") +
  annotate("text", x =-100, y = -1.6, label = "Bonferroni-Holm", 
           colour = "blue")+
  geom_line(data = pvals, aes(x = Time, y = critFDR-3.9), 
            na.rm = TRUE, size = 2, colour = "darkred") +
  annotate("text", x =-100, y = -1.9, label = "FDR",
           colour = "darkred") +
  scale_colour_manual(values=c("darkred", "darkgreen")) # comment out if does like these colors.
# scale_colour_manual(values=c("turquoise4", "violetred3")) # blueviolet salmon turquoise4 chartreuse3 darkred darkgreen chartreuse3   coral3 



######We can add all the waveforms along with the meanplots.This gives you some idea to spot the outliers.

levCat.plot+
  geom_line(aes(group = interaction(Subject,condition),colour = condition,alpha = 0.2))+
  guides(alpha= "none")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.4,aes(fill = condition))+
  guides(fill= "none")+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = condition))+
  labs(x = "Time (ms)",y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
  theme(text = element_text(size = 15)) +# this will change all text size (except geom_text)
  
  # Now let's setout the position of the legend. 1st number rightside (0-1), 2nd# upsize (0-1) 
  theme(legend.position = c(0.2, 0.8)) + 
  theme(legend.background = element_rect(fill="lightgray",  #change the legend color: e.g. "lightblue"
                                         size=1, linetype="solid"))+
  #
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")



################Now, do the same testing for Bonferroni, and FDR, for t-test corrections.


pvals$critBon <- 0+ (p.adjust(pvals$p.value,"bonferroni") <= .05)
pvals$critBon[pvals$critBon == 0] <- NA

pvals$critHolm <- 0 + (p.adjust(pvals$p.value, "holm") <= .05)
pvals$critHolm[pvals$critHolm == 0] <- NA

pvals$critFDR <- 0+(p.adjust(pvals$p.value, "fdr") <= .05)
pvals$critFDR[pvals$critFDR == 0] <- NA

levCat.plot +
  geom_ribbon(data = WSCI, aes(ymin = amplitude-ci, ymax = amplitude+ci,
                               fill = condition, colour = condition), 
              linetype="dashed", alpha = 0.05)+
  guides(fill = "none") +
  labs(x = "Time (ms)",
       y = expression(paste("Amplitude (",mu,"V)")), colour = "") +
  stat_summary(data = levCatGA, fun.y=mean, geom = "line", 
               aes(colour = condition)) +
  # stat_summary(data = levCatGA, fun.data = mean_cl_boot,
  #             geom = "ribbon", alpha = 0.3, aes(fill = condition)) +
  geom_line(data = pvals, aes(x = Time, y = crit-3), na.rm = TRUE,
            size = 2)+
  annotate("text", x =10, y = -2, label = "Uncorrected") +
  geom_line(data = pvals, aes(x = Time, y = critBon - 3.3),
            na.rm = TRUE, size = 2, colour = "darkgreen") +
  annotate("text", x =10, y = -2.3, label = "Bonferroni",
           colour = "darkgreen") +
  geom_line(data = pvals, aes(x = Time, y = critHolm-3.6),
            na.rm = TRUE, size = 2, colour = "blue") +
  annotate("text", x =10, y = -2.6, label = "Bonferroni-Holm", 
           colour = "blue")+
  geom_line(data = pvals, aes(x = Time, y = critFDR-3.9), 
            na.rm = TRUE, size = 2, colour = "darkred") +
  annotate("text", x =10, y = -2.9, label = "FDR",
           colour = "darkred")+
  geom_vline(xintercept = 0, linetype = "dashed" )+
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  theme(legend.position = c(0.13, 0.8)) + 
  theme(legend.background = element_rect(fill="lightgray",  #change the legend color: e.g. "lightblue"
                                         size=0.1, linetype="solid"))



### DIff Plot ###

levCatDiff <- levCatGA  # make sure levCatGA has all the original variables.
levCatDiff$Difference <- levCatGA[,4]-levCatGA[,3]
levCatDiff <- melt(levCatDiff[,c(1,2,5)], id.vars = c("Subject","Time"))
names(levCatDiff) <- c("Subject","Time","condition","amplitude")

levCat.plot+
  guides(fill = "none")+
  labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.3,aes(fill = condition))+
  stat_summary(fun.y = mean,geom = "line",size = 1,aes(colour = condition))+
  
  stat_summary(data = levCatDiff,fun.y=mean,geom = "line",aes(colour = condition))+
  stat_summary(data = levCatDiff,fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.3,aes(fill = condition))+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")


levCatDiff.plot <- ggplot(levCatDiff,aes(Time,amplitude))+
  scale_color_brewer(palette = "Set1")+
  theme_classic()

timePoint <- 650 #time in ms to plot indiv points.
closestTime <- levCatDiff[which.min(abs(levCatDiff$Time-timePoint)),]$Time #find the closest time in the actual data

levCatDiff.plot+
  labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
  stat_summary(fun.y=mean,geom = "line",aes(group = Subject),alpha = 0.4)+
  stat_summary(fun.y=mean,geom = "line",aes(colour = condition),size = 1)+
  stat_summary(fun.data = mean_cl_boot,geom = "ribbon",alpha = 0.4)+
  geom_point(data = subset(levCatDiff,Time == closestTime),alpha = 0.6)+
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed")





#This is the original data that variable will be mutated into. 
df1 <- read.csv("C:/Users/slotfi/Documents/R/Loop/SH_Entire ERPS 59Data/SH_GrndMtxBins_All58D&RTbehwos&PostTrialsRT_Final.csv",header = TRUE)
# This is the data set that variables will be picked up from. 
df2 <- read.csv("C:/Users/slotfi/Documents/R/Loop/1st Flanker data MeanNoOut19.4.27_1.csv",header = TRUE)

library(tidyverse)
library(readxl)
library("readxl")

# Run this funtions without changing anything. 

VLookup <- function(this, data, key, value) {
  m <- match(this, data[[key]])
  data[[value]][m]
}

# update the variables following $ sign and the 2nd var inside the parentheses. Inclusion_ERN Gender
df1$RTerp_Pure <- VLookup(df1$Subject, df2, "Subject", "RTerp_Pure")
df1$RTerp_Abs20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Abs20")
df1$RTerp_Con20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Con20")
df1$RTerp_Incon20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Incon20")
df1$RTerp_Abs60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Abs60")
df1$RTerp_Con60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Con60")
df1$RTerp_Incon60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Incon60")
df1$RTerp_Abs60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Abs60")
df1$RTerp_Conflict20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Conflict20")
df1$RTerp_Conflict60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Conflict60")
df1$RTerp_Filter20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Filter20")
df1$RTerp_Filter60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Filter60")

df1$SF_RTbehwo_Pure <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Pure")
df1$SF_RTbehwo_Abs20 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Abs20")
df1$SF_RTbehwo_Con20 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Con20")
df1$SF_RTbehwo_Incon20 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Incon20")
df1$SF_RTbehwo_Abs60 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Abs60")
df1$SF_RTbehwo_Con60 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Con60")
df1$SF_RTbehwo_Incon60 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Incon60")
df1$SF_RTbehwo_Abs60 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Abs60")
df1$SF_RTbehwo_Conflict20 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Conflict20")
df1$SF_RTbehwo_Conflict60 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Conflict60")
df1$SF_RTbehwo_Filter20 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Filter20")
df1$SF_RTbehwo_Filter60 <- VLookup(df1$Subject, df2, "Subject", "SF_RTbehwo_Filter60")


df1$SHK_RTbehwo_Pure <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Pure")
df1$SHK_RTbehwo_Abs20 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Abs20")
df1$SHK_RTbehwo_Con20 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Con20")
df1$SHK_RTbehwo_Incon20 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Incon20")
df1$SHK_RTbehwo_Abs60 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Abs60")
df1$SHK_RTbehwo_Con60 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Con60")
df1$SHK_RTbehwo_Incon60 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Incon60")
df1$SHK_RTbehwo_Abs60 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Abs60")
df1$SHK_RTbehwo_Conflict20 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Conflict20")
df1$SHK_RTbehwo_Conflict60 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Conflict60")
df1$SHK_RTbehwo_Filter20 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Filter20")
df1$SHK_RTbehwo_Filter60 <- VLookup(df1$Subject, df2, "Subject", "SHK_RTbehwo_Filter60")

df1 = df1 %>% 
  select(-ends_with("_time"),-ends_with("_Subject"))

df1$PostAbsDiff_Con20 <- VLookup(df1$Subject, df2, "Subject", "PostAbsDiff_Con20")
df1$PostAbsDiff_Incon20 <- VLookup(df1$Subject, df2, "Subject", "PostAbsDiff_Incon20")
df1$PostAbsDiff_Con60 <- VLookup(df1$Subject, df2, "Subject", "PostAbsDiff_Con60")
df1$PostAbsDiff_Incon60 <- VLookup(df1$Subject, df2, "Subject", "PostAbsDiff_Incon60")
df1$PostAbsDiff_Conflict20 <- VLookup(df1$Subject, df2, "Subject", "PostAbsDiff_Conflict20")
df1$PostAbsDiff_Conflict60 <- VLookup(df1$Subject, df2, "Subject", "PostAbsDiff_Conflict60")

df1$PostConDiff_Abs20 <- VLookup(df1$Subject, df2, "Subject", "PostConDiff_Abs20")
df1$PostConDiff_Incon20 <- VLookup(df1$Subject, df2, "Subject", "PostConDiff_Incon20")
df1$PostConDiff_Abs60 <- VLookup(df1$Subject, df2, "Subject", "PostConDiff_Abs60")
df1$PostConDiff_Incon60 <- VLookup(df1$Subject, df2, "Subject", "PostConDiff_Incon60")

df1$PostInconDiff_Abs20 <- VLookup(df1$Subject, df2, "Subject", "PostInconDiff_Abs20")
df1$PostInconDiff_Con20 <- VLookup(df1$Subject, df2, "Subject", "PostInconDiff_Con20")
df1$PostInconDiff_Abs60 <- VLookup(df1$Subject, df2, "Subject", "PostInconDiff_Abs60")
df1$PostInconDiff_Con60 <- VLookup(df1$Subject, df2, "Subject", "PostInconDiff_Con60")

df1$ACC_FlanShort_Pure_Abs <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_Pure_Abs")
df1$ACC_FlanShort_20_Abs <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_20_Abs")
df1$ACC_FlanShort_20_Cong <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_20_Cong")
df1$ACC_FlanShort_20_Incog <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_20_Incog")
df1$ACC_FlanShort_60_Abs <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_60_Abs")
df1$ACC_FlanShort_60_Cong <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_60_Cong")
df1$ACC_FlanShort_60_Incog <- VLookup(df1$Subject, df2, "Subject", "ACC_FlanShort_60_Incog")
df1$RTerp_Abs60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Abs60")
df1$RTerp_Conflict20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Conflict20")
df1$RTerp_Conflict60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Conflict60")
df1$RTerp_Filter20 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Filter20")
df1$RTerp_Filter60 <- VLookup(df1$Subject, df2, "Subject", "RTerp_Filter60")

df1$IE_Mea_FlanShort_Pure_Abs <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_Pure_Abs")
df1$IE_Mea_FlanShort_20_Abs <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_20_Abs")
df1$IE_Mea_FlanShort_20_Cong <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_20_Cong")
df1$IE_Mea_FlanShort_20_Incog <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_20_Incog")
df1$IE_Mea_FlanShort_60_Abs <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_60_Abs")
df1$IE_Mea_FlanShort_60_Cong <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_60_Cong")
df1$IE_Mea_FlanShort_60_Incog <- VLookup(df1$Subject, df2, "Subject", "IE_Mea_FlanShort_60_Incog")

write.table(df1, file="C:/Users/slotfi/Documents/R/Loop/SHK_Entire ERPS 83Data/2Run_GrndMtxBins/2Run_GrndMtxBins_All83D&RTbehwos.csv", row.names = F, sep=",") #P3bFCs_3548 N2Fs2731  

names(df1)
