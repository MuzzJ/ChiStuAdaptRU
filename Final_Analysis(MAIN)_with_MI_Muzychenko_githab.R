# Yulia N. Muzychenko
# yulia.n.muzychenko@gmail.com
# summer 2024
#
#Script 1/2

# Install libraries####
install.packages("rstatix")
install.packages("readr")
install.packages("mice")
install.packages("dplyr")
install.packages("careless")
install.packages("xlsx")
install.packages("purrr")
install.packages("tidyr")
install.packages("base")
install.packages("lavaan")
install.packages("semTools")
install.packages("miceafter")
install.packages("semPlot")
install.packages("sem")
install.packages("miceadds")

#### 1.\| Careless response detection####
# Load libraries
library(readr)
library(mice)
library(dplyr)
library(careless)
library(readxl)
library(xlsx)
library(purrr)
library(tidyr)
library(base)
# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

#Loading the data
df = read_excel(path = '4unies_quantitative.xlsx')
summary(df)


#### Calculate Mahalanobis distance for multivariate outlier detection####

df_vshort<-df[,c(2:7,44:85)]

mahalanobis_results1 <- mahad(df_vshort[,c(1:6)], plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)
mahalanobis_results2 <- mahad(df_vshort[,c(7:19)], plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)
mahalanobis_results3 <- mahad(df_vshort[,c(20:29)], plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)
mahalanobis_results4 <- mahad(df_vshort[,c(30:34)], plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)
mahalanobis_results5 <- mahad(df_vshort[,c(35:38)], plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)
mahalanobis_results6 <- mahad(df_vshort[,c(39:48)], plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)

df<-cbind(df,mahalanobis_results1)
df<-cbind(df,mahalanobis_results2)
df<-cbind(df,mahalanobis_results3)
df<-cbind(df,mahalanobis_results4)
df<-cbind(df,mahalanobis_results5)
df<-cbind(df,mahalanobis_results6)

mahalanobis_results <- mahad(df_vshort, plot = TRUE, flag = TRUE, confidence = 0.999, na.rm = TRUE)
summary(mahalanobis_results$flagged)

df<-cbind(df,mahalanobis_results)

#### Longstring ####
longstring_results<-longstring(df_vshort, avg=TRUE)
boxplot(longstring_results$avgstr)
hist(longstring_results$longstr)
df<-cbind(df,longstring_results)
#### Intra-individual Response Variability####
# Within-row standard deviations

# For AS
row_sd_As<-apply(df[,2:4],1,sd,na.rm=TRUE)
print(row_sd_As)
df<-cbind(df,row_sd_As)
summary(df)
# For CCA
row_sd_CCA<-apply(df[,5:7],1,sd,na.rm=TRUE)
print(row_sd_CCA)
df<-cbind(df,row_sd_CCA)
summary(df)

# For SOC
row_sd_Soc<-apply(df[,44:56],1,sd,na.rm=TRUE)
print(row_sd_Soc)
df<-cbind(df,row_sd_Soc)
summary(df)
table(df$row_sd_Soc)

# For ES
row_sd_Es<-apply(df[,57:66],1,sd,na.rm=TRUE)
print(row_sd_Es)
df<-cbind(df,row_sd_Es)
summary(df)
table(df$row_sd_Es)

# For CCM
row_sd_CCM<-apply(df[,67:71],1,sd,na.rm=TRUE)
print(row_sd_CCM)
df<-cbind(df,row_sd_CCM)
summary(df)
table(df$row_sd_CCM)

# for altruism
row_sd_Alt<-apply(df[,72:75],1,sd,na.rm=TRUE)
print(row_sd_Alt)
df<-cbind(df,row_sd_Alt)
summary(df)
table(df$row_sd_Alt)

# For pss
row_sd_pss<-apply(df[,76:85],1,sd,na.rm=TRUE)
print(row_sd_pss)
df<-cbind(df,row_sd_pss)
summary(df)
table(df$row_sd_pss)

#### Even-odd consistency####
df_vshort<-df[,c(1:6,34:65,80:89)]
#### Inverse decoding #################
# CCA - Cross-cultural Adjustment [3 items]
# CCM - Cross-cultural Motivation [5 items]
# ES - Emotional Stability [10 items]
# ES - items 1,2,3,4,6,7,8,9 are reverse coded
# SOC - Sense of Coherence [13 items]
# SOC- items 1,2,3,7,10 are reverse coded
# AS - Assignment Satisfaction [3 items]
# AS - item 2 is reverse coded

# For As, only the second item
table(df_vshort$as2)
df_vshort$as2<-recode(df_vshort$as2,'1'=5,'2'=4,'4'=2,'5'=1)
table(df_vshort$as2,useNA = "always")

# Es items all except for 5 and 10.
df_vshort$es1<-recode(df_vshort$es1,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es2<-recode(df_vshort$es2,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es3<-recode(df_vshort$es3,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es4<-recode(df_vshort$es4,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es6<-recode(df_vshort$es6,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es7<-recode(df_vshort$es7,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es8<-recode(df_vshort$es8,'1'=5,'2'=4,'4'=2,'5'=1)
df_vshort$es9<-recode(df_vshort$es9,'1'=5,'2'=4,'4'=2,'5'=1)

# For sense of coherence
df_vshort$soc1<-recode(df_vshort$soc1,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df_vshort$soc2<-recode(df_vshort$soc2,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df_vshort$soc3<-recode(df_vshort$soc3,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df_vshort$soc7<-recode(df_vshort$soc7,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df_vshort$soc10<-recode(df_vshort$soc10,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)

#For stress scale 4,5,7,8
df_vshort$pss4<-recode(df_vshort$pss4,'0'=4,'1'=3,'3'=1,'4'=0)
df_vshort$pss5<-recode(df_vshort$pss5,'0'=4,'1'=3,'3'=1,'4'=0)
df_vshort$pss7<-recode(df_vshort$pss7,'0'=4,'1'=3,'3'=1,'4'=0)
df_vshort$pss8<-recode(df_vshort$pss8,'0'=4,'1'=3,'3'=1,'4'=0)


#### Even-odd computation####
even_odd_results <- evenodd(df_vshort,c(3,3,13,10,5,4,10),diag = FALSE)
df<-cbind(df,even_odd_results)
summary(even_odd_results)
boxplot(even_odd_results)


#### Safe metrics for further inspection####
write.xlsx(df, file = "df_scores.xlsx",
           sheetName="1", append=TRUE)


#### Psychometric Synonyms and Antonyms are calculated in SPSS####


#### 2. \| Imputation of the data ####

#Loading the libraries
library(mice)
library(readxl)
library(dplyr)
library(xlsx)

# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

#Loading the data after the data cleaning
df = read_excel(path = '4unies_quantitative_no_sds_clean_noid.xlsx')


#### Inverse decoding #################
# ES - Emotional Stability [10 items]
# ES - items 1,2,3,4,6,7,8,9 are reverse coded
# SOC - Sense of Coherence [13 items]
# SOC- items 1,2,3,7,10 are reverse coded
# AS - Assignment Satisfaction [3 items]
# AS - item 2 is reverse coded

# For As, only the second item
df$as2<-recode(df$as2,'1'=5,'2'=4,'4'=2,'5'=1)

# Es items all except for 5 and 10.
df$es1<-recode(df$es1,'1'=5,'2'=4,'4'=2,'5'=1)
df$es2<-recode(df$es2,'1'=5,'2'=4,'4'=2,'5'=1)
df$es3<-recode(df$es3,'1'=5,'2'=4,'4'=2,'5'=1)
df$es4<-recode(df$es4,'1'=5,'2'=4,'4'=2,'5'=1)
df$es6<-recode(df$es6,'1'=5,'2'=4,'4'=2,'5'=1)
df$es7<-recode(df$es7,'1'=5,'2'=4,'4'=2,'5'=1)
df$es8<-recode(df$es8,'1'=5,'2'=4,'4'=2,'5'=1)
df$es9<-recode(df$es9,'1'=5,'2'=4,'4'=2,'5'=1)

# For sense of coherence
df$soc1<-recode(df$soc1,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc2<-recode(df$soc2,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc3<-recode(df$soc3,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc7<-recode(df$soc7,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)
df$soc10<-recode(df$soc10,'1'=7,'2'=6,'3'=5,'5'=3,'6'=2,'7'=1)

#For stress scale 4,5,7,8
df$pss4<-recode(df$pss4,'0'=4,'1'=3,'3'=1,'4'=0)
df$pss5<-recode(df$pss5,'0'=4,'1'=3,'3'=1,'4'=0)
df$pss7<-recode(df$pss7,'0'=4,'1'=3,'3'=1,'4'=0)
df$pss8<-recode(df$pss8,'0'=4,'1'=3,'3'=1,'4'=0)

#### Imputation ####
# Explore missing data 
md.pattern(df)


#Set seed
set.seed(1234)

imp<-mice(df,maxit = 0,print=F) # a dry run.


predM<-imp$predictorMatrix # in the predictor matrix: columns are predictors, rows are predicted vars.
predM[,c("exchProgDue", "sf23", "sf24", "sf25", "sf26", 
         "sf27","sf28","sf29","sf30","sf31")]<-0 #  these vars aren't used as predictors for any vars.

predM
####

# Use blots to exclude different values per column
# Create blots object
blots <- make.blots(df)
# Exclude 3 through 4 from School donor pool
 blots$School$exclude <- c("3","4")
# Exclude 2, 3, and 4 from School donor pool (school=1)
# blots$School$exclude <- c("2","3","4")
# Exclude 1, 3, and 4 from School donor pool (school=2)
# blots$School$exclude <- c("1","3","4")


imp <- mice(df, m =20, method = "pmm", pred =predM,
            blots = blots,  print = TRUE)

blots$School$exclude %in% unlist(c(imp$imp$School)) # MUST be all FALSE

#recode School into 3 dummy-variables

library(dplyr)

full.impdata <- complete(imp, 'long', include = TRUE) %>%
  mutate(School1_2 = if_else(School== 2, 1, 0))
imp <- as.mids(full.impdata)
full.impdata <- complete(imp, 'long', include = TRUE) %>%
  mutate(School1_3 = if_else(School== 3, 1, 0))
imp <- as.mids(full.impdata)
full.impdata <- complete(imp, 'long', include = TRUE) %>%
  mutate(School1_4 = if_else(School== 4, 1, 0))
imp <- as.mids(full.impdata)


#### Safe imputed datasets to dataframes####
imp_n1<-complete(imp,1)
summary(imp_n1)
table(imp_n1$School,useNA = "always")
table(df$School,useNA = "always")
imp_n2<-complete(imp,2)
imp_n3<-complete(imp,3)
imp_n4<-complete(imp,4)
imp_n5<-complete(imp,5)
imp_n6<-complete(imp,6)
imp_n7<-complete(imp,7)
imp_n8<-complete(imp,8)
imp_n9<-complete(imp,9)
imp_n10<-complete(imp,10)
imp_n11<-complete(imp,11)
imp_n12<-complete(imp,12)
imp_n13<-complete(imp,13)
imp_n14<-complete(imp,14)
imp_n15<-complete(imp,15)
imp_n16<-complete(imp,16)
imp_n17<-complete(imp,17)
imp_n18<-complete(imp,18)
imp_n19<-complete(imp,19)
imp_n20<-complete(imp,20)

# Save data in the environment as an Rdata file. 
save.image(file = "imp_mice_short.RData")
# save.image(file = "imp_mice_short_School1.RData")
# save.image(file = "imp_mice_short_School2.RData")
#### Skewness and kurtosis####
#Checked in SPSS

#### Levene's test to see if we can join data sets.####
#The rule of thumb, at least one item per a scale should be invariant(not different).

library(miceafter)
imp_list <- mids2milist(imp)

ra <- with(data=imp_list,
           expr = levene_test(as1 ~ factor(School)))
pool_levenetest(ra, method = "D1")

ra <- with(data=imp_list,
           expr = levene_test(cca1 ~ factor(School)))
pool_levenetest(ra, method = "D1")

ra <- with(data=imp_list,
           expr = levene_test(es1 ~ factor(School)))
pool_levenetest(ra, method = "D1")

ra <- with(data=imp_list,
           expr = levene_test(soc2 ~ factor(School)))
pool_levenetest(ra, method = "D1")

ra <- with(data=imp_list,
           expr = levene_test(ccm1 ~ factor(School)))
pool_levenetest(ra, method = "D1")


#### 3. \|CFA ####
library(lavaan)
library(semTools)

# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

load("imp_mice_short.RData")
# load("imp_mice_short_School1.RData")
# load("imp_mice_short_School2.RData")
install.packages("remotes")

library(remotes)
remotes::install_github("yrosseel/lavaan")
remotes::install_github("simsem/semTools/semTools",force = TRUE)
remotes::install_github("TDJorgensen/lavaan.mi",force = TRUE)
library(lavaan)
library(semTools)
library(lavaan.mi)
Sys.setenv(LANG = "en")
#### CFA. Emotional stability ####
es_model<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10'
fit_es <- cfa.mi(es_model, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)
summary(fit_es, fit.measures = TRUE, test="D2")
modindices.mi(fit_es, sort = TRUE, maximum.number = 10)
standardizedSolution(fit_es)
compRelSEM(fit_es)

es_model_1<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
            es1~~es2
            es4~~es5
            es8~~es9
            es1~~es9
            es5~~es10
            es2~~es9
            es7~~es8
            es9~~es10
            es2~~es8
            es2~~es10'
fit_es_1 <- cfa.mi(es_model_1, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)
summary(fit_es_1, fit.measures = TRUE, test="D2")

CR_es_1<-compRelSEM(fit_es_1)


#### CFA. Sense of coherence ####
soc_model<-'SOC =~ soc1 + soc2 + soc3 + soc4 + soc5 + soc6 + soc7 + soc8 + soc9 + soc10 + soc11 + soc12 + soc13'
fit_soc <- cfa.mi(soc_model, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)
summary(fit_soc, fit.measures = TRUE, test="D2")
modindices.mi(fit_soc, sort = TRUE, maximum.number = 10)
standardizedSolution(fit_soc)

soc_model_1<-'SOC =~ soc4 + soc8 + soc9
            + soc11 + soc12 + soc13
            soc8~~soc9'
fit_soc_1 <- cfa.mi(soc_model_1, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)


summary(fit_soc_1, fit.measures = TRUE, test="D2")
modindices.mi(fit_soc_1, sort = TRUE, maximum.number = 10)
standardizedSolution(fit_soc_1)

CR_soc_1<-compRelSEM(fit_soc_1)


#### CFA. Sense of coherence. Second order SOC ####

soc_model_2<-'comp =~ soc2+soc6+soc8+soc9+soc11
              man =~ soc3+soc5+soc10+soc13
              mean =~ soc1+soc4+soc7+soc12
              SOC =~ comp+man+mean'

fit_soc_2 <- cfa.mi(soc_model_2, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)
summary(fit_soc_2, fit.measures = TRUE, test="D2")
modindices.mi(fit_soc_2, sort = TRUE, maximum.number = 10)
standardizedSolution(fit_soc_2)
compRelSEM(fit_soc_2, higher = "SOC")
AVE(fit_soc_2)
anova(fit_soc, fit_soc_1, fit_soc_2,test = "D3")


#### CFA. Cross-cultural motivation ####
ccm_model<-'CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5'
fit_ccm <- cfa.mi(ccm_model, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)
summary(fit_ccm, fit.measures = TRUE, test="D2")
modindices.mi(fit_ccm, sort = TRUE, maximum.number = 10)


ccm_model_1<-'CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
            ccm4~~ccm5
            ccm2~~ccm5
            ccm1~~ccm2'
fit_ccm_1 <- cfa.mi(ccm_model_1, data = imp, estimator = "ULSM", ordered=TRUE, std.lv = TRUE, FUN = fitMeasures)
summary(fit_ccm_1, fit.measures = TRUE, test="D2")

CR_ccm_1<-compRelSEM(fit_ccm_1)

#### CFA. Cross-cultural Adjustment + Assignment Satisfaction ####
cca_as_model<-'CCA =~ cca1 + cca2 + cca3
            AS =~ as1 + as2 + as3'
fit_cca_as <- cfa.mi(cca_as_model, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
summary(fit_cca_as, fit.measures = TRUE, test="D2")
modindices.mi(fit_cca, sort = TRUE, maximum.number = 10)

compRelSEM(fit_cca_as)

#### CFA. Total measurement model. 1####
total_model<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc1 + soc2 + soc3 + soc4 + soc5 + soc6 + soc7 + soc8 + soc9 + soc10
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3'
fit_total <- cfa.mi(total_model, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
summary(fit_total,fit.measures = TRUE, test="D2")

a<-standardizedSolution.mi(fit_total)
View(a)
AVE(fit_total)
compRelSEM(fit_total)

modindices.mi(fit_total, sort = TRUE, maximum.number = 10)
##Model has good fit, but overall global fit is not that good, we shall correct SOC to increase AVE

#### CFA. Total measurement model. 2####

# Since SOC has problems, we shall look into modification indices that are related to this variable.
# We have soc1 and soc10 which are related to latent variables, let's try to run a model without them.

total_model_2<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~  soc2 + soc3 + soc4 + soc5 + soc6 + soc7 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3'
fit_total_2 <- cfa.mi(total_model_2, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
summary(fit_total_2,fit.measures = TRUE, test="D2")

a<-standardizedSolution.mi(fit_total_2)
View(a)
AVE(fit_total_2)
compRelSEM(fit_total_2)
modindices.mi(fit_total_2, sort = TRUE, maximum.number = 10)

#### CFA. Total measurement model. 3####

#Following the same logic, soc7 has relation to 3 latent variables, therefore we shall try the model without it.

total_model_3<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~   soc2 + soc3 + soc4 + soc5 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3'
fit_total_3 <- cfa.mi(total_model_3, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
summary(fit_total_3,fit.measures = TRUE, test="D2")

a<-standardizedSolution.mi(fit_total_3)
View(a)
AVE(fit_total_3)
compRelSEM(fit_total_3)

modindices.mi(fit_total_3, sort = TRUE, maximum.number = 10)

#### CFA. Total measurement model. 4####

# The indices indicate that soc4 is related to 3 latent factors, let's try to remove it

total_model_4<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc2 + soc5 + soc6 + soc8 + soc9 
                      + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3'
#                A=~a1+a2+a3+a4'
fit_total_4 <- cfa.mi(total_model_4, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
summary(fit_total_4,fit.measures = TRUE, test="D2", ci = TRUE)

a<-standardizedSolution.mi(fit_total_4)
View(a)
AVE(fit_total_4)
compRelSEM(fit_total_4)
modindices.mi(fit_total_4, sort = TRUE, maximum.number = 10)


#### Final CFA. Total measurement model. 5####

# soc 3 and soc2 keep popping up, let's delete them
# model 5 has overall good fit, we accept this model as the final one

total_model_5<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc5 + soc6 + soc8 + soc9 
                      + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3'
fit_total_5 <- cfa.mi(total_model_5, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
summary(fit_total_5,fit.measures = TRUE, test="D2", ci = TRUE)

a<-standardizedSolution.mi(fit_total_5)
View(a)
AVE(fit_total_5)
compRelSEM(fit_total_5)
modindices.mi(fit_total_5, sort = TRUE, maximum.number = 10)


# McDonald's omega and Chronbah's alpha for ordinal scales 
reliability(fit_total_5)


#####Build an object summarizing fit indices across multiple models.####
out<-compareFit(fit_total_5,  fit_total, nested = TRUE)
summary(out)

#### Common Method bias: Common Method Factor and Harman Single factor test ####
#Harman Single Factor Test
single_model<-'Single=~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10 +
                soc4 + soc5 + soc6 + soc8 + soc9 + soc11 + soc12 + soc13 +
                ccm1 + ccm2 + ccm3 + ccm4 + ccm5 + cca1 + cca2 + cca3 +
                as1 + as2 + as3'
fit_single <- cfa.mi(single_model, data = imp, estimator = "ULSM", ordered=TRUE)
AVE(fit_single)


#### Let's check correlations with a marker variable #####
total_model_5<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc5 + soc6 + soc8 + soc9 
                      + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4'
fit_total_5 <- cfa.mi(total_model_5, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)
a<-standardizedSolution.mi(fit_total_5)
View(a)

####CLF####

model_CMB<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc5 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4
method=~es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10 +
soc4 + soc5 + soc6 + soc8 + soc9 + soc11 + soc12 + soc13 +
ccm1 + ccm2 + ccm3 + ccm4 + ccm5 + cca1 + cca2 + cca3 + as1 + as2 + as3+
a1+a2+a3+a4
method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
'
fit_CMB <- cfa.mi(model_CMB, data = imp, estimator = "ULSM", ordered=TRUE, FUN = fitMeasures)

#The zero-constrained test ####

model_CMB_0<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc5 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4
method=~0*es1 + 0*es2 + 0*es3 + 0*es4 + 0*es5 + 0*es6 + 0*es7 + 0*es8 + 0*es9 + 0*es10 +
0*soc4 + 0*soc5 + 0*soc6 + 0*soc8 + 0*soc9 + 0*soc11 + 0*soc12 + 0*soc13 +
0*ccm1 + 0*ccm2 + 0*ccm3 + 0*ccm4 + 0*ccm5 + 0*cca1 + 0*cca2 + 0*cca3 + 0*as1 + 0*as2 + 0*as3 +
0*a1+0*a2+0*a3+0*a4

method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
method ~~ 1*method'
fit_CMB_0 <- cfa.mi(model_CMB_0, data = imp, estimator = "ULSM", std.lv = TRUE, FUN = fitMeasures)

#The zero-constrained chi-square difference test resulted in a insignificant result####
out<-compareFit(fit_CMB_0,  fit_CMB, nested = FALSE)
summary(out)
# ##### Model Fit Indices 
# (i.e., reject null, i.e., response bias is not zero), we should run an equal-constrained test. 
# This test determines whether the response bias is evenly distributed across factors.

#The equal-constrained test ####
#No need to run it, it's just in case someone needs it

model_CMB_1<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc5 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4

method=~1*es1 + 1*es2 + 1*es3 + 1*es4 + 1*es5 + 1*es6 + 1*es7 + 1*es8 + 1*es9 + 1*es10 +
1*soc4 + 1*soc5 + 1*soc6 + 1*soc8 + 1*soc9 + 1*soc11 + 1*soc12 + 1*soc13 +
1*ccm1 + 1*ccm2 + 1*ccm3 + 1*ccm4 + 1*ccm5 + 1*cca1 + 1*cca2 + 1*cca3 + 1*as1 + 1*as2 + 1*as3+
1*a1+1*a2+1*a3+1*a4
method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
'
fit_CMB_1 <- cfa.mi(model_CMB_1, data = imp, estimator = "ULSMV", ordered=TRUE, FUN = fitMeasures)
summary(fit_CMB_1, fit.measures = TRUE, test="D2", ci = TRUE)

#A test of equal specific bias demonstrated unevenly distributed bias
out<-compareFit(fit_CMB_1,  fit_CMB_0, nested = FALSE)
summary(out)

#### 4. \|SEM####
library(lavaan)
library(semTools)
library(lavaan.mi)

# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

load("imp_mice_short.RData")
# load("imp_mice_short_School1.RData")
# load("imp_mice_short_School2.RData")


#### Extract factor lodgings from imputed data sets.####
model_CMB<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc5 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                '
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n1, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n1<-cbind(imp_n1,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n2, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n2<-cbind(imp_n2,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n3, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n3<-cbind(imp_n3,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n4, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n4<-cbind(imp_n4,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n5, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n5<-cbind(imp_n5,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n6, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n6<-cbind(imp_n6,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n7, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n7<-cbind(imp_n7,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n8, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n8<-cbind(imp_n8,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n9, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n9<-cbind(imp_n9,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n10, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n10<-cbind(imp_n10,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n11, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n11<-cbind(imp_n11,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n12, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n12<-cbind(imp_n12,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n13, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n13<-cbind(imp_n13,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n14, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n14<-cbind(imp_n14,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n15, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n15<-cbind(imp_n15,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n16, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n16<-cbind(imp_n16,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n17, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n17<-cbind(imp_n17,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n18, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n18<-cbind(imp_n18,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n19, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n19<-cbind(imp_n19,fscores)
fit_imp1 <- lavaan::cfa(model_CMB, data = imp_n20, estimator = "ULSM", ordered=TRUE)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
imp_n20<-cbind(imp_n20,fscores)

library("tidyverse") 

data = list(imp_n1, imp_n2, imp_n3,imp_n4,imp_n5,imp_n6,imp_n7,imp_n8,imp_n9,imp_n10,imp_n11,imp_n12,
            imp_n13,imp_n14,imp_n15,imp_n16,imp_n17,imp_n18,imp_n19,imp_n20) 
class(data)


save.image(file = "imp_mice_short_factor.RData")
# save.image(file = "imp_mice_short_School1_factor.RData")
# save.image(file = "imp_mice_short_School2_factor.RData")


#### Correlation analysis to choose covariates #####
library(miceadds)
library(rstatix)
corr<-micombine.cor(data, variables=c(85:89, 92, 95:103), conf.level=0.95,
              method="pearson", nested=FALSE)
table<-attr(corr, "r_matrix")

corr<-corr[,-c(4:8)]
corr$p<- p_format(corr$p, digits = 3)
corr$r<- p_format(corr$r, digits = 3)
tabel$School <- p_format(table$School, digits = 3)

corr<-corr %>%  filter(variable1=="SOC"|variable1=="AS"|variable1=="ES"|variable1=="CCM"|variable1=="CCA")
corr<-corr %>%  filter(variable2!="SOC"&variable2!="AS"&variable2!="ES"&variable2!="CCM"&variable2!="CCA")
corr<-corr %>%  filter(p<0.05)
View(corr)

write.xlsx(table, file = "correlation_controls.xlsx", sheetName="1", append=TRUE)

df3<-imp_n1[,c(85:89, 92, 95:103)]

res <- cor(df3)
round(res, 2)
library(Hmisc)
res2 <- rcorr(as.matrix(df3), type="spearman")
tab1<-res2$P
tab2<-res2$r

write.xlsx(tab2, file = "correlation_controls.xlsx", sheetName="2", append=TRUE)
write.xlsx(tab1, file = "correlation_controls.xlsx", sheetName="3", append=TRUE)


##correct correlations for ordinal and nominal variables in the table, example:
miceadds::mi.anova(data, formula="age ~ School" )
micombine.chisquare(data, df, display=TRUE, version=1)

install.packages("confintr")
library(confintr)
cramersv(imp_n1[c("School", "exchProg")])

#School~exchProg
mean(0.1702694, 0.1702694,0.169072,0.1578388, 0.1619104,0.1679102,
     0.1667833,0.1667833,0.1667833,0.1750353,0.1588271,0.156052,
     0.1763201,0.173781,0.169072,0.169072,0.1679102,0.1679102,
     0.177636,0.1578388)


#School~gender
mean(0.231176,0.2380306, 0.2401535,0.2401535,0.246611,
     0.246611,0.2394095,0.2363823,0.2344415, 0.2319055,
     0.236099,0.239068,0.2349463,0.239068,0.2401535,
     0.2398944,0.2344415,0.2412875,0.2424703,0.2424703)
chisq.test(imp_n13$School, imp_n13$exchProg, correct=FALSE)


#### RUN SEM####

library(lavaan)
library(semTools)
library(lavaan.mi)

# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

load("imp_mice_short_factor.RData")
# load("imp_mice_short_School1_factor.RData")
# load("imp_mice_short_School2_factor.RData")

####Mediation####
####(all theoretically based control variables)

model_mediation <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
CCA ~ b*ES + c*SOC 
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c63*age
AS ~ d* ES + e*SOC + f*CCA  
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c64*age + c74*School1_2+ 
c84*School1_3+c94*School1_4


#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)
total_serial:= d +(a * c * f)
b==c

'

sem_mediation <- sem.mi(model_mediation, data = data, estimator = "MLR")
summary(sem_mediation, fit.measures = TRUE, rsquare = TRUE)
standardizedSolution.mi(sem_mediation)


####Unadjusted mediation model
model_mediation_ua <- '
# regression
SOC~a*ES
CCA ~ b*ES + c*SOC
AS ~ d* ES + e*SOC + f*CCA  


#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)
total_serial:= d +(a * c * f)

'

sem_mediation_ua <- sem.mi(model_mediation_ua, data = data, estimator = "MLR")
summary(sem_mediation_ua, fit.measures = TRUE, rsquare = TRUE, test="D2")
standardizedSolution.mi(sem_mediation_ua)

#Check if the model without controls is better
anova(sem_mediation_ua,sem_mediation)
out<-compareFit(sem_mediation_ua,sem_mediation, nested = FALSE)
summary(out)


####Alternative models####
model_alternative <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
AS ~ b*ES + c*SOC 
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c64*age + c74*School1_2+ 
c84*School1_3+c94*School1_4
CCA ~ d* ES + e*SOC + f*AS 
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c63*age

#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)
total_serial:= d +(a * c * f)
'

sem_alt <- sem.mi(model_alternative, data = data, estimator = "MLR")
summary(sem_alt, fit.measures = TRUE, standardized = TRUE)


#Check if alternative model is better
out<-compareFit(sem_alt,  sem_mediation, nested = FALSE)
summary(out)


##### model fit for moderation####
model_moderation_fit <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
CCA ~ b*ES + m1*CCM + c*SOC 
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c63*age
AS ~ d* ES + m2*CCM + e*SOC + f*CCA 
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c64*age + c74*School1_2+ 
c84*School1_3+c94*School1_4

##account for relationships(additional to improve the model fit)
CCM~ES
SOC~~CCM
'


sem_short_fit <- sem.mi(model_moderation_fit, data = data, estimator = "MLR")
summary(sem_short_fit, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE)


#Check for equal betas
sem_short_fit_CCA <- sem.mi(model_short, data = data, estimator = "MLR")
sem_short_fit_AS <- sem.mi(model_short, data = data, estimator = "MLR")
anova(sem_short_fit,sem_short_fit_CCA)
anova(sem_short_fit,sem_short_fit_AS)

#### Moderation#####

model_moderation <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
CCA ~ b*ES + m1*CCM + m11*ES:CCM + c*SOC + m12*SOC:CCM
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c63*age
AS ~ d* ES + m2*CCM + m21*ES:CCM + e*SOC + m22*SOC:CCM + f*CCA 
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c64*age + c74*School1_2+ 
c84*School1_3+c94*School1_4

#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)

##account for relationships
CCM~ES
SOC~~CCM
'


sem_moderation <- sem.mi(model_moderation, data = data, estimator = "MLR")
summary(sem_moderation, fit.measures = TRUE, rsquare=TRUE, test="D2")
standardizedSolution.mi(sem_moderation)


# here we input variation of the significant variable (in our case it's ES)
# Get the variance of ES from model fit for mediation step
probe<-probe2WayMC(sem_moderation, c("ES","CCM","ES:CCM"),
                   "CCA","CCM",
                   c(-sqrt(0.578),0,sqrt(0.578)))
probe

plotProbe(probe,c(-3,3))
dev.copy(jpeg,filename="plot2.jpg")
dev.off ()


###Unadjusted moderation model####

model_short2 <- '
# regression
SOC~a*ES
CCA ~ b*ES + m1*CCM + c*SOC 
AS ~ d* ES + m2*CCM + e*SOC + f*CCA 

#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)

##account for relationships
CCM~ES
SOC~~CCM
'


sem_short2_fit <- sem.mi(model_short2, data = data, estimator = "MLR")
summary(sem_short2_fit, fit.measures = TRUE, rsquare=TRUE, test="D2")



model_short2 <- '
# regression
SOC~a*ES
CCA ~ b*ES + m1*CCM + m11*ES:CCM + c*SOC + m12*SOC:CCM
AS ~ d* ES + m2*CCM + m21*ES:CCM + e*SOC + m22*SOC:CCM + f*CCA 

#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)

##account for relationships
CCM~ES
SOC~~CCM
'


sem_short2 <- sem.mi(model_short2, data = data, estimator = "MLR")
summary(sem_short2, fit.measures = TRUE, rsquare=TRUE, test="D2")
standardizedSolution.mi(sem_short2)

#Check if models without controls are better
anova(sem_short_fit,sem_short2_fit)
anova(sem_short,sem_short2)
