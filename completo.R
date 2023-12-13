#Librerie e funzioni
library(multilevel)
library(openxlsx)
library(PerformanceAnalytics)
library(readr)
library(dplyr)
library(lavaan)
source("funzioni.R")

#carico dataset dipendenti
CARDUCCI_DIPENDENTI <- read.csv("dip.csv", header = T, sep = ",")
qualita <- CARDUCCI_DIPENDENTI[, 6:8]
soddisfazione <- CARDUCCI_DIPENDENTI[, 4:5]
intenzione <- CARDUCCI_DIPENDENTI[, 1:3]

#carico dataset indipendenti
CARDUCCI_INDIPENDENTI <- read.csv("indip.csv", header = T, sep = ",")
tranquillita<- CARDUCCI_INDIPENDENTI[,1:2]
personale<- CARDUCCI_INDIPENDENTI[,3:5]
servizi<- CARDUCCI_INDIPENDENTI[,6:9]
alloggio<- CARDUCCI_INDIPENDENTI[,10:11]
spazi<- CARDUCCI_INDIPENDENTI[,12:13]
#creo dataframe completo
tutto <- data.frame(qualita, soddisfazione, intenzione, tranquillita, personale, servizi, alloggio, spazi)
tutto.cov <- cov(tutto, use = "pairwise.complete.obs")
write.table(tutto.cov, "tutto.cov", sep = " ", row.names = FALSE, col.names = FALSE,dec =".")

############ Common Variance ############
completo.cov <- cov(tutto,use="pairwise.complete.obs")
completo.efa <- factanal(covmat = completo.cov,factors = 1)
print(completo.efa)
# Factor1
# SS loadings      6.650
# Proportion Var   0.317
Non è presente common variance, infatti, la varianza spiegata dal modello risulta essere pari al 31,7%. 
########## Convergent validity ##########
tutto.model <- '
qualita =~ Q1 +Q2 + Q3
soddisfazione =~ S3 + S4
intenzione =~ I1 + I2 + I3
tranquillita =~ T1 + T2
personale =~ P1 +P2 +P3
servizi =~ SE1 + SE2 + SE3 + SE4
alloggio =~ A1 + A2
spazi =~ SC1 + SC2
'
fit <- cfa(tutto.model,data=tutto)
condisc.fit <- condisc(fit)
# $Squared_Factor_Correlation
# qualit sddsfz intnzn trnqll persnl serviz allogg spazi
# qualita        1.000                                                
# soddisfazione  0.921  1.000                                         
# intenzione     0.598  0.569  1.000                                  
# tranquillita   0.063  0.041  0.145  1.000                           
# personale      0.176  0.107  0.212  0.160  1.000                    
# servizi        0.537  0.476  0.363  0.280  0.197  1.000             
# alloggio       0.309  0.172  0.277  0.101  0.071  0.410  1.000      
# spazi          0.095  0.049  0.245  0.186  0.234  0.068  0.044 1.000
# 
# $Average_Variance_Extracted
# qualita soddisfazione    intenzione  tranquillita     personale       servizi 
# 0.620         0.730         0.829         0.537         0.410         0.341 
# alloggio         spazi 
# 0.512         0.371  
# gli item, nel complesso, sono una buona misura del fattore a cui sono associati, è presente convergent validity, abbiamo valori più modesti per quanto
#riguarda spazi,servizi e personale. Invece, abbiamo valori più alti per quanto riguarda intenzione e soddisfazione
#unisco soddisfazione e qualita, poichè in una residenza come il Carducci abbiamo una situazione altamente esperenziale e la qualità è associata alla soddisfazione
tutto.model <- '
soddisfazione =~ S3 + S4 + Q1 + Q2 + Q3
intenzione =~ I1 + I2 + I3
tranquillita =~ T1 + T2
personale =~ P1 +P2 +P3
servizi =~ SE1 + SE2 + SE3 + SE4
alloggio =~ A1 + A2
spazi =~ SC1 + SC2
'
fit <- cfa(tutto.model,data=tutto)
condisc.fit <- condisc(fit)
# $Squared_Factor_Correlation
# sddsfz intnzn trnqll persnl serviz allogg spazi
# soddisfazione  1.000                                         
# intenzione     0.599  1.000                                  
# tranquillita   0.050  0.144  1.000                           
# personale      0.139  0.213  0.161  1.000                    
# servizi        0.515  0.362  0.277  0.197  1.000             
# alloggio       0.235  0.276  0.101  0.070  0.411  1.000      
# spazi          0.073  0.250  0.191  0.243  0.063  0.043 1.000
# 
# $Average_Variance_Extracted
# soddisfazione    intenzione  tranquillita     personale       servizi      alloggio 
# 0.648         0.829         0.538         0.410         0.341         0.513 
# spazi 
# 0.365 
# dopo aver unito soddisfazione e qualità abbiamo che gli item, nel complesso, sono una buona misura del fattore a cui sono associati, è presente convergent validity,abbiamo valori più modesti per quanto riguarda spazi,servizi e personale. Invece, abbiamo valori più alti per quanto riguarda intenzione e soddisfazione
#############Discriminant validity##########
Rij <- as.matrix(condisc.fit$Squared_Factor_Correlation)
Rij[ Rij == 1 ] <- 0

ave <- c(condisc.fit$Average_Variance_Extracted)
mRij <- apply(Rij, FUN = max, MARGIN = 2)

ave > mRij
# soddisfazione    intenzione  tranquillita     personale       servizi       alloggio 
#          TRUE          TRUE          TRUE          TRUE         FALSE          TRUE 
#         spazi 
#          TRUE 
#vediamo che i fattori, ad eccezione di "servizi", non si sovrappongono tra loro, 
#questo comunque è coerente anche da quanto visto nell'analisi esplorativa delle indipendenti, è molto probabile che il fattore 'servizi' si sovrappone con fattore alloggio 
##########Composite reliability############
library(dplyr)
comp_reliability(fit)
# # A tibble: 7 × 2
# lhs           composite_reliability_ec
# <chr>                            <dbl>
# 1 alloggio                         0.412
# 2 intenzione                       0.828
# 3 personale                        0.406
# 4 servizi                          0.403
# 5 soddisfazione                    0.742
# 6 spazi                            0.273
# 7 tranquillita                     0.443

#ad esclusione del fattore spazi, i valori sono tutti superiori a 0.4, quindi 
#i fattori risultano essere modestamente affidabili rispetto agli item che misurano

############ Grafici ###############
qualita.m<-apply(qualita, FUN=mean,MARGIN=1)
soddisfazione.m<-apply(soddisfazione, FUN=mean,MARGIN=1)
intenzione.m<-apply(intenzione, FUN=mean,MARGIN=1)
tranquillita.m <- apply(tranquillita, FUN = mean, MARGIN = 1)
personale.m <- apply(personale, FUN = mean, MARGIN = 1)
servizi.m <- apply(servizi, FUN = mean, MARGIN = 1)
alloggio.m <- apply(alloggio, FUN = mean, MARGIN = 1)
spazi.m <- apply(spazi, FUN = mean, MARGIN = 1)
completo.m <- data.frame(qualita.m,soddisfazione.m,intenzione.m,tranquillita.m,
                         personale.m,servizi.m,alloggio.m,spazi.m)
# grafico correlazioni
library("PerformanceAnalytics")
chart.Correlation(completo.m, histogram = TRUE, pch = "+")

#grafici importanza performance
Q.lm<-lm(qualita.m ~ tranquillita.m + personale.m + servizi.m +
           alloggio.m + spazi.m)
summary(Q.lm)
# Call:
#   lm(formula = qualita.m ~ tranquillita.m + personale.m + servizi.m + 
#        alloggio.m + spazi.m)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3220 -0.7643  0.1327  0.7681  2.0474 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.55540    0.49301   1.127   0.2620    
# tranquillita.m -0.06038    0.07150  -0.844   0.3999    
# personale.m     0.14488    0.08816   1.643   0.1027    
# servizi.m       0.51759    0.09166   5.647  9.6e-08 ***
#   alloggio.m      0.17255    0.06799   2.538   0.0123 *  
#   spazi.m         0.06145    0.07836   0.784   0.4343    
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.038 on 132 degrees of freedom
# Multiple R-squared:  0.3681,	Adjusted R-squared:  0.3442 
# F-statistic: 15.38 on 5 and 132 DF,  p-value: 6.674e-12
S.lm<-lm(soddisfazione.m ~ tranquillita.m + personale.m + servizi.m +
           alloggio.m + spazi.m)
summary(S.lm)
# Call:
#   lm(formula = soddisfazione.m ~ tranquillita.m + personale.m + 
#        servizi.m + alloggio.m + spazi.m)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8042 -0.8625 -0.1211  1.0289  2.7025 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.72474    0.58130   1.247    0.215    
# tranquillita.m -0.11083    0.08431  -1.315    0.191    
# personale.m     0.10911    0.10394   1.050    0.296    
# servizi.m       0.62267    0.10808   5.761  5.6e-08 ***
#   alloggio.m      0.09245    0.08017   1.153    0.251    
# spazi.m         0.07127    0.09239   0.771    0.442    
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.224 on 132 degrees of freedom
# Multiple R-squared:  0.2996,	Adjusted R-squared:  0.273 
# F-statistic: 11.29 on 5 and 132 DF,  p-value: 4.462e-09

I.lm<-lm(intenzione.m ~ tranquillita.m + personale.m + servizi.m +
           alloggio.m + spazi.m)
summary(I.lm)
# Call:
#   lm(formula = intenzione.m ~ tranquillita.m + personale.m + servizi.m + 
#        alloggio.m + spazi.m)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.9934 -0.6593 -0.0035  0.8470  2.3814 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.08669    0.51365  -0.169 0.866227    
# tranquillita.m  0.04960    0.07450   0.666 0.506659    
# personale.m     0.20099    0.09185   2.188 0.030402 *  
#   servizi.m       0.33385    0.09550   3.496 0.000644 ***
#   alloggio.m      0.22601    0.07084   3.190 0.001775 ** 
#   spazi.m         0.25290    0.08164   3.098 0.002382 ** 
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.082 on 132 degrees of freedom
# Multiple R-squared:  0.393,	Adjusted R-squared:   0.37 
# F-statistic: 17.09 on 5 and 132 DF,  p-value: 5.205e-13

#qualita
importance<-c(Q.lm$coefficients[2:6])
performance<-c(mean(tranquillita.m), mean(personale.m), mean(servizi.m), mean(alloggio.m), mean(spazi.m))

plot(importance, performance, type="n", ylab="performance", xlab="importance",xlim = c(-0.5,1))
text(importance, performance, c("tranquillita", "personale", "servizi","alloggio", "spazi"))
abline(h=mean(performance), lty="dotted")
abline(v=mean(importance), lty="dotted")
title(main="Grafico per qualita'")
#soddisfazione
importance<-c(S.lm$coefficients[2:6])
performance<-c(mean(tranquillita.m), mean(personale.m), mean(servizi.m), mean(alloggio.m), mean(spazi.m))

plot(importance, performance, type="n", ylab="performance", xlab="importance",xlim = c(-0.5,1))
text(importance, performance, c("tranquillita", "personale", "servizi","alloggio", "spazi"))
abline(h=mean(performance), lty="dotted")
abline(v=mean(importance), lty="dotted")
title(main="Grafico per soddisfazione")
#intenzione
importance<-c(I.lm$coefficients[2:6])
performance<-c(mean(tranquillita.m), mean(personale.m), mean(servizi.m), mean(alloggio.m), mean(spazi.m))

plot(importance, performance, type="n", ylab="performance", xlab="importance",xlim = c(-0.5,1))
text(importance, performance, c("tranquillita", "personale", "servizi","alloggio", "spazi"))
abline(h=mean(performance), lty="dotted")
abline(v=mean(importance), lty="dotted")
title(main="Grafico per intenzione")

#UNENDO QUALITA E SODDISFAZIONE
soddisfazione <- cbind(soddisfazione,qualita)
soddisfazione.m<-apply(soddisfazione, FUN=mean,MARGIN=1)
completo.m <- data.frame(soddisfazione.m,intenzione.m,tranquillita.m,
                         personale.m,servizi.m,alloggio.m,spazi.m)

# grafico correlazioni
chart.Correlation(completo.m, histogram = TRUE, pch = "+")

#grafico importance-performance
S.lm<-lm(soddisfazione.m ~ tranquillita.m + personale.m + servizi.m +
           alloggio.m + spazi.m)
summary(S.lm)
# Call:
#   lm(formula = soddisfazione.m ~ tranquillita.m + personale.m + 
#        servizi.m + alloggio.m + spazi.m)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.4759 -0.7504 -0.0010  0.8112  1.9587 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.62314    0.48971   1.272   0.2055    
# tranquillita.m -0.08056    0.07103  -1.134   0.2587    
# personale.m     0.13057    0.08757   1.491   0.1383    
# servizi.m       0.55962    0.09105   6.146 8.76e-09 ***
#   alloggio.m      0.14051    0.06754   2.080   0.0394 *  
#   spazi.m         0.06538    0.07783   0.840   0.4024    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.031 on 132 degrees of freedom
# Multiple R-squared:  0.3713,	Adjusted R-squared:  0.3475 
# F-statistic: 15.59 on 5 and 132 DF,  p-value: 4.863e-12

#soddisfazione
importance<-c(S.lm$coefficients[2:6])
performance<-c(mean(tranquillita.m), mean(personale.m), mean(servizi.m), mean(alloggio.m), mean(spazi.m))

plot(importance, performance, type="n", ylab="performance", xlab="importance",xlim = c(-0.5,1))
text(importance, performance, c("tranquillita", "personale", "servizi","alloggio", "spazi"))
abline(h=mean(performance), lty="dotted")
abline(v=mean(importance), lty="dotted")
title(main="Grafico per soddisfazione (qualita' inglobata)")
