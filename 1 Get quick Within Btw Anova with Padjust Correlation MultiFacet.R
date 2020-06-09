

## https://indrajeetpatil.github.io/ggstatsplot/
## quick, strong data analysis with P adjusted stats and plots..nice...

library(rcompanion)

my_dataShK = my_data3 %>% filter (RT_wo_Outliers != 0)# & SafeShock == "Shock")
my_dataSH = my_data2 %>% filter (RT_wo_Outliers != 0)
my_data3 = my_data3 %>% mutate (ReacProc1 = str_replace(ReacProc,"PURE","SafePURE"))

Sum = rcompanion::groupwiseMean(RT_wo_Outliers ~ factor(Conditions) + TrialsID + TrialsID^2,  # + factor(SafeShock) 
                                data   = subset (my_data4, my_data4$SafeShock == "Shock" & my_data4$RT_wo_Outliers != 0),  # my_data4$SafeShock == "Shock" &
                                conf   = 0.95,
                                digits = 3,
                                traditional = F, # with no missing, use F, T. 
                                percentile  = T)
Sum
library(ggplot2)

pd = position_dodge(.2)

ggplot(Sum, aes(x =    factor(Conditions),
                y =    Mean,
                color = factor(SafeShock))) +
  geom_errorbar(aes(ymin=Percentile.lower, # Percentile.lower Trad.lower
                    ymax=Percentile.upper),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("2nsRun ErrRate & 95% CL") +
  facet_grid(~TrialsProg)


# for reproducibility and data
set.seed(123)
library(WRS2)

# plot  Absolutely make sure that Variables of interest are define as Factors...
## install.packages(pkgs = "ggstatsplot")
## ggwithinstats
ggstatsplot::ggbetweenstats(
  data = my_data1,
  x = TrialType.1,
  y = RT_wo_Outliers,
  pairwise.comparisons = TRUE, # show pairwise comparison test results
  pairwise.annotation = "p.value", # how do you want to annotate the pairwise comparisons
  p.adjust.method = "fdr", # method :   'arg' should be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  title = "Short RT_wo_Outliers",
  #caption = "Data source: `WRS2` R package",
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE,
  messages = FALSE
)

my_dataRMV5 = my_dataRMV %>% slice (0:500) 

## Lets grab random sample of 100 from each condition.
my_dataRMV = my_data1 %>% group_by(Conditions) %>% do(sample_n(.,100))
my_dataRMV = as.data.frame(my_dataRMV)


my_dataRMV$TrialType.1 = as.factor(my_dataRMV$TrialType.1)
# ggwithinstats   
ggstatsplot::ggbetweenstats(
  data = my_dataRMV,
  x = TrialType.1,
  y = RT,
  pairwise.comparisons = TRUE, # show pairwise comparison test results
  title = "Short_ACC",
  #caption = "Data source: `WRS2` R package",
  ggtheme = ggthemes::theme_fivethirtyeight(),
  ggstatsplot.layer = FALSE,
  messages = FALSE) + facet_grid(~TrialProg)

#  + # further modification outside of ggstatsplot
# ggplot2::coord_cartesian(ylim = c(180, 1370)) +
#ggplot2::scale_y_continuous(breaks = seq(180, 1370, by = 50)))

my_data3 =  my_data3 %>%
  group_by (Subject, ReacProc) %>%
  mutate(ReacProcAveErr = (1 - mean(ACC))*100)

my_data1$TrialsProg = as.factor(my_data1$TrialsProg)
my_data3$TrialTypeAll = as.factor(my_data3$TrialTypeAll)
my_data3$SafeShock = as.factor(my_data3$SafeShock)

# for reproducibility
set.seed(123)

# Can run within and between stats with multipe Facest...
ggstatsplot::grouped_ggbetweenstats(
  data = dplyr::filter(
    .data = my_data4,
    SafeShock %in% c("Safe", "Shock") # define each facet here...
  ),
  x = TrialTypeAll,
  y = RT_wo_Outliers,
  grouping.var = SafeShock, # grouping variable
  pairwise.comparisons = TRUE, # display significant pairwise comparisons
  pairwise.annotation = "p.value", # how do you want to annotate the pairwise comparisons
  p.adjust.method = "fdr", # method :   'arg' should be one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  # adding new components to `ggstatsplot` default
  ggplot.component = list(ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())),
  k = 3,
  title.prefix = "GOOD (n=82) Safe or Shock",
  # caption = substitute(paste(italic("Source"), ":IMDb (Internet Movie Database)")),
  palette = "default_jama",
  package = "ggsci",
  messages = FALSE,
  #plotgrid.args = list(nrow = 2),
  title.text = "RT as a function of TriaTypes and Time Elapsed"
)


## Scatterplot and correlation stats##
ggstatsplot::ggscatterstats(
  data   = subset (my_data4, my_data4$SafeShock == "Shock" & my_data4$RT_wo_Outliers != 0),  # my_data4$SafeShock == "Shock" &
  x = Conditions,
  y = RT_wo_Outliers,
  xlab = "REM sleep (in hours)",
  ylab = "Amount of time spent awake (in hours)",
  title = "Understanding mammalian sleep",
  messages = FALSE
)

my_data4$TrialTypeAll = as.factor(my_data4$TrialTypeAll)
## Scatterplot and correlation stats ##
## with multipe facets##
ggstatsplot::grouped_ggscatterstats(
  data = dplyr::filter(
    .data = my_dataERN,
    Condition %in% c("ERN_Pure", "ERN_Abs20", "ERN_Abs60", "ERN_Incon20", "ERN_Incon60")
  ),
  x = ERNCz30100,
  y = RT_PostError,
  # label.var = title,
  #label.expression = length > 200,
  k = 3, # no. of decimal places in the results
  xfill = "#E69F00",
  yfill = "#8b3058",
  xlab = "ERN at C",
  grouping.var = Condition, # grouping variable
  title.prefix = "Condition",
  ggtheme = ggplot2::theme_grey(),
  ggplot.component = list(ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis())),
  messages = FALSE,
  #plotgrid.args = list(nrow = 2),
  title.text = "Relationship between PostError Slowing and ERN"
)
#   ggplot.component = list(ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())),
#   ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(2, 9, 1), limits = (c(2, 9)))

