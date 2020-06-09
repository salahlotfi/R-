
# This script combines multiple inputs and spits out nice EEG plots. 
# Let obtain RT where it is sign. and add them as a geom_line to refelct that
# on the currect diff waves plot.
# Remember, the df should include all ERP vars and associated conditions RTs.
# RT corr will be only for one of the variables of final plot.

library(ggplot2)
library(reshape2)
library(Rmisc)
library(purrr)
library(magrittr)  # the Function "extract" of [magrittr] might give you headache...for WISC object..reload it..
library(Hmisc)
library(dplyr)
library(tidyverse)

rm(list=ls())
df <- read.csv("C:/Users/slotfi/Documents/R/Loop/SH_Entire ERPS 59Data/SH_GrndMtxBins_All58D&RTbehwos&PostTrialsRT_Final.csv",header = TRUE) #SH_GrndMtxBins_All58D&RTerps
df = df %>% filter (!Subject %in% c(117)) # 208,225 206,448,466,472
DataN = aggregate(df[,1], list(df$Subject), mean) %>% tally()


####### obtain RT pvals part.
Var5 = "RTbehwo_Conflict60"  # RTerp_InFilter60
Var6 = "Confli60_AveCs"  # Confli60_AveCs

df1 = df %>% select (c(Time, Subject, Var5, Var6))
runninglm <- df1 %>%
  split(.$Time) %>%
  map(~lm ( RTbehwo_Conflict60 ~ Confli60_AveCs, data = .))

pvalsRT <- data.frame(Time = unique(df1$Time), 
                      r.squared = runninglm %>% map (summary) %>% map_dbl("r.squared"), # map_dbl extracts only from a vector of one elements.
                      p.value = runninglm %>% map (summary) %>% map_dfr(4) %>% slice(8) %>% t(),
                      t.value = runninglm %>% map (summary) %>% map_dfr(4) %>% slice(2) %>% t()) # map_dfr extracts from a matrix as a d.f.
pvalsRT = pvalsRT %>% mutate(Correlation = sqrt(r.squared))
#### setup the pvalue critical value. 
pvalsRT$crit <- 0 + (pvalsRT$p.value <= .05) # play around with this p value for a desired value.
pvalsRT$crit[pvalsRT$crit == 0] <- NA

######################
###### lets plot the variables of interest here.

myPlotTitle = "SHORT no117"

Var1 = "Con60_AvePs"
Var2 = "Incon60_AvePs"
Var3 = "Con60_AvePs" # if need be for plotting more than two condition. Change WISC to 4 too.
Var4 = "Incon60_AvePs" # if need be for plotting more than two condition. Change WISC to 4 too.

dfMelted = df %>% select (c(Time, Subject, Var1,Var2))
## use this if more than two vars.
# dfMelted = df %>% select (c(Time, Subject, Var1,Var2,Var3,Var4)) 


#### Just run this to see if there is any outlier data.
library(ggpubr)
ggscatter(df1, x = Var1, y = Var2, 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Reaction Time", ylab = "Electrode Amplitude")

#3))
dfMelted <- dfMelted[(dfMelted$Time >= -200)& (dfMelted$Time <= 800),]


### 4)))
dfMelted$Subject <- as.factor(dfMelted$Subject)
dfMelted <- reshape2::melt(dfMelted,id.vars = c("Subject","Time"))
names(dfMelted) <- c("Subject","Time","condition","amplitude") #don't mess around with this. 

dfMelted.plot <- ggplot(dfMelted,aes(Time,amplitude))+
  scale_color_brewer(palette = "Set1")

##5))
#Now let's run t-tests on each timepoint (again, using purrr) and also summarize the 
#data using the summarySEwithin function from Rmisc.

#If you are running more than two conditions, you should either comments out 'runningT' or run equivalent ANOVA.
runningT <- dfMelted %>%
  split(.$Time) %>%
  #map(~aov(amplitude~condition, data = .))
  map(~t.test(amplitude~condition, paired = TRUE, data = .)) #aov(weight ~ group, data = my_data)

#testAOV <- aov(amplitude~condition, data = dfMelted)
#test <- summary(testAOV)
#> test[[1]][["Pr(>F)"]] ..Can't this working. When map it, there is no p-value to be pulled out.

runningSE <- dfMelted %>%
  split(.$Time) %>%
  map(~summarySEwithin(data = ., measurevar = "amplitude",
                       withinvars = "condition", idvar = "Subject"))
#Don't run this if more than 2 conditions is provided. 
#6)) I REMOVED this PART. NOT IMporant: to plot ERP with Standard between subject conficence intervals

### to Add Pvalues 
pvals <- data.frame(Time = unique(dfMelted$Time), 
                    p.value = map_dbl(runningT,"p.value"))
ggplot(pvals, aes(x = Time, y = p.value)) +
  geom_point()

###Now, this is the best plot, combining the mean waveforms, with SEM, statistical tests.

pvals$crit <- 0 + (pvals$p.value <= .05)
pvals$crit[pvals$crit == 0] <- NA



####### 7)) to plot ERPs with corrected within subject CIs

WSCI <- map_df(runningSE, extract) %>%
  mutate(
    Time = rep(unique(dfMelted$Time), each = 2) 
    #Note, you'll have to change 2 to match the number of conditions
  )


#### Can dots as a variablity of data..
# timePoint <- 650 #time in ms to plot indiv points.
# closestTime <- dfMelted[which.min(abs(dfMelted$Time-timePoint)),]$Time #find the closest time in the actual data

######## 8) Plots with simple t-test comparisons...

dfMelted.plot+
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
# geom_point(data = subset(dfMelted,Time == closestTime),alpha = 0.6) # Could drop this if don't want dots at time pionts.

############################
# This inludes all test comparison test. 
#pvals$critBon <- 0+ (p.adjust(pvals$p.value,"bonferroni") <= .05)
# pvals$critBon[pvals$critBon == 0] <- NA

pvals$critHolm <- 0 + (p.adjust(pvals$p.value, "holm") <= .05)
pvals$critHolm[pvals$critHolm == 0] <- NA

pvals$critFDR <- 0+(p.adjust(pvals$p.value, "fdr") <= .05)
pvals$critFDR[pvals$critFDR == 0] <- NA

dfMelted.plot +
  geom_ribbon(data = WSCI, aes(ymin = amplitude-ci, ymax = amplitude+ci,
                               fill = condition, colour = condition), 
              linetype="dashed", alpha = 0.05)+
  guides(fill = "none") +
  stat_summary(fun.y = mean, geom = "line", size = 1.5, aes(colour = condition))+ #size is the thickness of line.
  coord_cartesian(xlim=c(-200, 800), ylim=c(-3, 5)) + # This manually limits the coordinates. Setup according to DATA.
  scale_x_continuous(breaks = seq(from = -200, to = 800, by = 100)) + # setup the break points..
  labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")), colour = "") +
  theme(text = element_text(size = 18)) +# this will change all text size (except geom_text)
  labs (title = paste0(myPlotTitle,"(n=",DataN,")"))  +
  theme(legend.position = c(0.13, 0.8)) + 
  theme(legend.background = element_rect(fill="lightgray",  #change the legend color: e.g. "lightblue"
                                         size=0.1, linetype="solid"))+
  geom_line(data = pvals, aes(x = Time, y = crit-3.5),na.rm = TRUE,size = 2)+ 
  annotate("text", x =-100, y = -1.4, label = "Uncorrected", 
           colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed" )+  #this is to superimpose a vertical line.
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(data = pvalsRT, aes(x = Time, y = crit-3.4),na.rm = TRUE,size = 2,colour = "red") +
  
  annotate("text", x =-50, y = -2.1, label = paste0("RTcorr",Var2), 
           colour = "red")+
  geom_line(data = pvals, aes(x = Time, y = critHolm-3.7),
            na.rm = TRUE, size = 2, colour = "blue") +
  annotate("text", x =-100, y = -1.6, label = "Bonferroni-Holm", 
           colour = "blue")+
  geom_line(data = pvals, aes(x = Time, y = critFDR-3.9), 
            na.rm = TRUE, size = 2, colour = "darkred") +
  annotate("text", x =-100, y = -1.9, label = "FDR",
           colour = "darkred")  +
  scale_colour_manual(values=c("blueviolet", "salmon")) # "blueviolet", "salmon" "turquoise4", "violetred3" 
# darkorchid "red", "darkorchid" darkred darkgreen  dodgerblue2 


######We can add all the waveforms along with the meanplots.This gives you some idea to spot the outliers.

dfMelted.plot+
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
  geom_hline(yintercept = 0,linetype = "dashed")+
  scale_colour_manual(values=c("blueviolet", "salmon"))



