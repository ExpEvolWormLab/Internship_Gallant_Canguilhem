library(dplyr)
library(lme4)
library(emmeans)
library(sjPlot)
library(webshot)
#import the cleaned data
long_worms = read.csv2("data_sheets/cleaned_data.csv")

#specify our variables as factor and not strings or number
cols_change <- c("Cycle","Block","Population","Treatment","Plates_nbr","Replicate","Env","Pop_type","Sex_struct","Day", "Trt_3", "Trt_6", "Rep2")
long_worms[cols_change] <- lapply(long_worms[cols_change], as.factor)
#now we have just the datas for the treatments with males to compare Dio and Andr pop


#1st model on daily fecundity
mod1 <- lmer(Fec_yesterday ~ Env*Day*Trt_3*Pop_type + Block + (1|Population/Rep2), data=long_worms)

# Perform drop1 analysis
drop1_results <- drop1(mod1, test = "Chisq")

#create the table with the details of each effet and save it in HTML with our other graphs
tab_1_html <- tab_model(mod1, show.df = TRUE)
writeLines(as.character(tab_1_html), "Results & graphs/Rapport/mod1_details.html")

#2nd model on total fecundity
long_worms <- long_worms %>% filter(Block!=1 & Day==3)

mod2 <- lmer(Tot_per_worm ~ Env*Trt_3*Pop_type + (1|Population), data=long_worms)

# Perform drop1 analysis
drop2_results <- drop1(mod2, test = "Chisq")

#create the table with the details of each effet and save it in HTML with our other graphs
tab_2_html <- tab_model(mod2, show.df = TRUE)
writeLines(as.character(tab_2_html), "Results & graphs/Rapport/mod2_details.html")
