

# Very helpful codes to run analysis for a subset of data
# Also, we using a factor predictor, just use "factor" to indicate it is  factor, instead of the whole code to apply it to df. 
aov.sub <- aovp(Value ~ Sample, subset(my_data,my_data$SafeShock == "Safe")  )

# to print out all table
df %>% print(n = Inf)
write.table(my_data_rmv_GAD, file="C:/Users/slotfi/Documents/R/HLM/1stRun_AllGoodData_GADBad.csv", row.names = F, sep=",", )  


modelRnd1 = lmer(RT_wo_Outliers ~ factor(Conditions)+  (1|Subject),  # TrialID works as a Time
                 data=subset(my_data,my_data$SafeShock == "Safe"),
                 REML=TRUE)
#This is great too.
modelRnd1 = lmer(AveFs3040 ~ RT + STAIT_Total + (1|Subject),  # TrialID works as a Time
                 data=subset(my_data,my_data$flag != "9" & my_data$ACC == "1"),
                 REML=TRUE)

# Run constract in R for everything..Anova Manova, HLM

m.emm <- emmeans(modelRnd1, "Conditions")#, pbkrtest.limit = 19460)
m.emm
# Note: D.f. calculations have been disabled because the number of observations exceeds 3000.
# To enable adjustments, add the argument 'pbkrtest.limit = 19460' (or larger)

m.emm.df <-
  m.emm %>%
  broom::tidy()
m.emm.df %>%
  ggplot(aes(Conditions, estimate, ymin=asymp.LCL, ymax=asymp.LCL)) + #conf.low ymax=conf.high
  geom_pointrange() +
  ylab("RT NoOutliers")
plot(emmeans(modelRnd1, "Conditions"))

contrast(m.emm, 'tukey') %>%
  broom::tidy()

library(pander)
contrast(m.emm, 'trt.vs.ctrl') %>%
  broom::tidy() %>%
  head %>%pander
