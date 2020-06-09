# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust


# Adjusting for p value given multiple comparison issue. 
# simply write donw the obtained p values in the parantheses for the corresponding measures (comparisons) and get the corrected p values. 

# Given a set of p-values, returns p-values adjusted using one of several methods.



# false discovery rate and BY are really good methods, apparently better than bonferroni.

options(scipen=999)  # to remove scientific notation.

p.adjust(c( .000001, .004, .037), "fdr")

# pick one of these methods 
p.adjust.methods

"holm"       "hochberg"   "hommel"     "bonferroni" "BH"         "BY"         "fdr"        "none"      

p.adjust(c( .302, .00001, .415, .061, .006), "BH")
p.adjust(c( .07, .03), "fdr")



Lucky_Couple_Win_The_Bed <- sample (c("MahshadHadi", "MaryamSala", "ShadiMilad", "SimaAli"),
                                    size=2, replace=F)
Lucky_Couple_Win_The_Bed

