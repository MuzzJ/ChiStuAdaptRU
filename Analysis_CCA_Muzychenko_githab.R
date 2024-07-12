#Complete Case Analysis
#Loading the libraries
library(mice)
library(readxl)
library(dplyr)
library(xlsx)
# Set the working directory to the folder containing the data file.
input.data.path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/")
setwd(input.data.path)

#Loading the data
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

#### Levene's test to see if we can join datasets.####

install.packages("lawstat")
library(lawstat)
levene.test(df$as1, df$School)
levene.test(df$cca1, df$School)
levene.test(df$ccm1, df$School)
levene.test(df$es1, df$School)
levene.test(df$soc1, df$School)

####CFA####
library(lavaan)

#Final model5.
#One item was deleted to increase the model fit.
total_model_5<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A =~ a1+a2+a3+a4'
fit_total_5 <- cfa(total_model_5, data = df, std.lv = TRUE)
summary(fit_total_5,fit.measures = TRUE)
standardizedSolution(fit_total_5)
AVE(fit_total_5)
compRelSEM(fit_total_5)
modindices(fit_total_5, sort = TRUE, maximum.number = 10)
reliability(fit_total_5)
all<-standardizedSolution(fit_total_5, output = "data.frame")
###CMB###
model_CMB<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4
method=~es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10 +
soc4 + soc6 + soc8 + soc9 + soc11 + soc12 + soc13 +
ccm1 + ccm2 + ccm3 + ccm4 + ccm5 + cca1 + cca2 + cca3 + as1 + as2 + as3+
a1+a2+a3+a4
method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
'
fit_CMB <- cfa(model_CMB, data = df, std.lv = TRUE)
summary(fit_CMB,fit.measures = TRUE)
all<-standardizedSolution(fit_CMB, output = "data.frame")

#The zero-constrained test ####

model_CMB_0<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4
method=~0*es1 + 0*es2 + 0*es3 + 0*es4 + 0*es5 + 0*es6 + 0*es7 + 0*es8 + 0*es9 + 0*es10 +
0*soc4 + 0*soc6 + 0*soc8 + 0*soc9 + 0*soc11 + 0*soc12 + 0*soc13 +
0*ccm1 + 0*ccm2 + 0*ccm3 + 0*ccm4 + 0*ccm5 + 0*cca1 + 0*cca2 + 0*cca3 + 0*as1 + 0*as2 + 0*as3
+0*a1+0*a2+0*a3+0*a4

method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
method ~~ 1*method'
fit_CMB_0 <- cfa(model_CMB_0, data = df, std.lv = TRUE)
summary(fit_CMB_0,fit.measures = TRUE)
standardizedSolution(fit_CMB_0)


# The zero-constrained chi-square difference test resulted in a significant result###########################
out<-compareFit(fit_CMB_0,  fit_CMB, nested = FALSE)
summary(out)

# (i.e., reject null, i.e., response bias is not zero), we should run an equal-constrained test. 
# This test determines whether the response bias is evenly distributed across factors.

#The equal-constrained test ####
model_CMB_1<-'ES =~ es1 + es2 + es3 + es4 + es5 + es6 + es7 + es8 + es9 + es10
                SOC =~ soc4 + soc6 + soc8 + soc9
                        + soc11 + soc12 + soc13
                CCM =~ ccm1 + ccm2 + ccm3 + ccm4 + ccm5
                CCA =~ cca1 + cca2 + cca3
                AS =~ as1 + as2 + as3
                A=~a1+a2+a3+a4

method=~1*es1 + 1*es2 + 1*es3 + 1*es4 + 1*es5 + 1*es6 + 1*es7 + 1*es8 + 1*es9 + 1*es10 +
1*soc4 + 1*soc6 + 1*soc8 + 1*soc9 + 1*soc11 + 1*soc12 + 1*soc13 +
1*ccm1 + 1*ccm2 + 1*ccm3 + 1*ccm4 + 1*ccm5 + 1*cca1 + 1*cca2 + 1*cca3 + 1*as1 + 1*as2 + 1*as3
+1*a1+1*a2+1*a3+1*a4
method~~0*ES
method~~0*SOC
method~~0*CCM
method~~0*CCA
method~~0*AS
method~~0*A
'
fit_CMB_1 <- cfa(model_CMB_1, data = df, std.lv = TRUE)
summary(fit_CMB_1,fit.measures = TRUE)
standardizedSolution(fit_CMB_1)


#A test of equal specific bias demonstrated unevenly distributed bias.###########################
out<-compareFit(fit_CMB_1,  fit_CMB, nested = FALSE)
summary(out)

compRelSEM(fit_CMB)
compRelSEM(fit_total_5)
reliability(fit_total_5)
reliability(fit_CMB)

#clean the dataset from the rows with na is factor items of interest
df2<-df
df2<-df2[!is.na(df2$a1),]
df2<-df2[!is.na(df2$a2),]
df2<-df2[!is.na(df2$a3),]
df2<-df2[!is.na(df2$a4),]
df2<-df2[!is.na(df2$as1),]
df2<-df2[!is.na(df2$as2),]
df2<-df2[!is.na(df2$as3),]
df2<-df2[!is.na(df2$cca1),]
df2<-df2[!is.na(df2$cca2),]
df2<-df2[!is.na(df2$cca3),]
df2<-df2[!is.na(df2$ccm1),]
df2<-df2[!is.na(df2$ccm2),]
df2<-df2[!is.na(df2$ccm3),]
df2<-df2[!is.na(df2$ccm4),]
df2<-df2[!is.na(df2$ccm5),]
df2<-df2[!is.na(df2$es1),]
df2<-df2[!is.na(df2$es2),]
df2<-df2[!is.na(df2$es3),]
df2<-df2[!is.na(df2$es4),]
df2<-df2[!is.na(df2$es5),]
df2<-df2[!is.na(df2$es6),]
df2<-df2[!is.na(df2$es7),]
df2<-df2[!is.na(df2$es8),]
df2<-df2[!is.na(df2$es9),]
df2<-df2[!is.na(df2$es10),]
df2<-df2[!is.na(df2$soc4),]
df2<-df2[!is.na(df2$soc6),]
df2<-df2[!is.na(df2$soc8),]
df2<-df2[!is.na(df2$soc9),]
df2<-df2[!is.na(df2$soc11),]
df2<-df2[!is.na(df2$soc12),]
df2<-df2[!is.na(df2$soc13),]

summary(df$School)

fit_imp1 <- lavaan::cfa(model_CMB, data = df2)
fscores <- lavPredict(fit_imp1 , type="lv", method="regression")
df2<-cbind(df2,fscores)

df3<-df2[,c(85:89, 92, 95:100)]

res <- cor(df3)
round(res, 2)

#### RUN SEM####
####Mediation####
model_mediation <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
CCA ~ b*ES + c*SOC 
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c63*age
AS ~ d* ES + e*SOC + f*CCA  
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c64*age + c74*School

#idirect
indirect_ES := b*f
indirect_SOC := c*f
ind_eff_serial := a * c * f

#total
total_ES := d +(b*f)
total_SOC := e +(c*f)
total_serial:= d +(a * c * f)

'


sem_mediation <- sem(model_mediation, data = df2)
summary(sem_mediation, fit.measures = TRUE, standardized = TRUE)
standardizedSolution(sem_mediation)

findRMSEApower(rmsea0 = .08, rmseaA = .06, df = 8, n = 541)

####Unadjusted mediation####
model_mediation2 <- '
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


sem_mediation2 <- sem(model_mediation2, data = df2)
summary(sem_mediation2, fit.measures = TRUE, standardized = TRUE)

####Alternative models####
model_alternative <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
AS ~ b*ES + c*SOC 
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c64*age + c74*School
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


sem_alt <- sem(model_alternative, data = df2)
summary(sem_alt, fit.measures = TRUE, standardized = TRUE)


out<-compareFit(sem_alt,  sem_mediation, nested = FALSE)
summary(out)

##### model fit for moderation####
model_mf <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
CCA ~ b*ES + m1*CCM + c*SOC 
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c63*age
AS ~ d* ES + m2*CCM + e*SOC + f*CCA 
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c64*age + c74*School

##account for relationships
CCM~ES
SOC~~CCM
'


sem_mf <- sem(model_mf, data = df2)
summary(sem_mf, fit.measures = TRUE, standardized = TRUE)
modindices.mi(sem_mf, sort = TRUE, maximum.number = 10)
findRMSEApower(rmsea0 = .08, rmseaA = .097, df = 10, n = 541)


####moderation#####
model_short <- '
# regression
ES~ c11*gender
SOC~a*ES+c12*gender 
CCA ~ b*ES + m1*CCM + m11*ES:CCM + c*SOC + m12*SOC:CCM
+ c13*gender + c23*language + c33*ses + c43*exchProg + c53*cctraindue + c63*age
AS ~ d* ES + m2*CCM + m21*ES:CCM + e*SOC + m22*SOC:CCM + f*CCA 
+ c14*gender + c24*language + c34*ses + c44*exchProg + c54*cctraindue + c64*age + c74*School


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


sem_short <- sem(model_short, data = df2)
summary(sem_short, fit.measures = TRUE, standardized = TRUE)
library(semTools)

probe<-probe2WayMC(sem_short, c("ES","CCM","ES:CCM"),
                   "CCA","CCM",
                   c(-sqrt(0.583),0,sqrt(0.583)))
probe

plotProbe(probe,c(-3,3))
dev.copy(jpeg,filename="plot_CCA.jpg")
dev.off ()



####Unadjusted moderation#####
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


sem_short2 <- sem(model_short2, data = df2)
summary(sem_short2, fit.measures = TRUE, standardized = TRUE)


#####Calculate descriptive statistics

#ES
mean(colMeans(df2[ , c(56:65)]))
sd(colMeans(df2[ , c(56:65)]))
#SOC
mean(colMeans(df2[ , c(46,48,50:51,53:55)]))
sd(colMeans(df2[ , c(46,48,50:51,53:55)]))
#CCA
mean(colMeans(df2[ , c(4:6)]))
sd(colMeans(df2[ , c(4:6)]))
#AS
mean(colMeans(df2[ , c(1:3)]))
sd(colMeans(df2[ , c(1:3)]))
#CCM
mean(colMeans(df2[ , c(66:70)]))
sd(colMeans(df2[ , c(66:70)]))


#OneSample t-test
df2$ES<-rowMeans(df2[ , c(56:65)])
t.test(df2$ES, mu = 2.9)
df2$SOC<-rowMeans(df2[ , c(46,48,50:51,53:55)])
t.test(df2$SOC, mu = 4.2)
df2$CCA<-rowMeans(df2[ , c(4:6)])
t.test(df2$CCA, mu = 4.8)
df2$AS<-rowMeans(df2[ , c(1:3)])
t.test(df2$AS, mu = 3.6)
df2$CCM<-rowMeans(df2[ , c(66:70)])
t.test(df2$CCM, mu = 4.8)


mean(df2$ES[df2$gender==1])
sd(df2$ES[df2$gender==1])
mean(df2$ES[df2$gender==2])
sd(df2$ES[df2$gender==2])

mean(df2$SOC[df2$gender==1])
sd(df2$SOC[df2$gender==1])
mean(df2$SOC[df2$gender==2])
sd(df2$SOC[df2$gender==2])

mean(df2$AS[df2$gender==1])
sd(df2$AS[df2$gender==1])
mean(df2$AS[df2$gender==2])
sd(df2$AS[df2$gender==2])


mean(df2$CCA[df2$gender==1])
sd(df2$CCA[df2$gender==1])
mean(df2$CCA[df2$gender==2])
sd(df2$CCA[df2$gender==2])

mean(df2$CCM[df2$gender==1])
sd(df2$CCM[df2$gender==1])
mean(df2$CCM[df2$gender==2])
sd(df2$CCM[df2$gender==2])

