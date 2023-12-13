#LIBRERIE E FUNZIONE
library(multilevel)
library(openxlsx)
library(PerformanceAnalytics)
library(readr)
library(dplyr)
library(lavaan)

if.item.deleted <- function(data)
{
  nitem <- NCOL(data)
  for(i in seq(nitem)){
    cogAff_red <- data[,-i]
    cdr <- cronbach(cogAff_red)$Alpha
    print(paste("Alpha ",cdr,"se cancello", i, "\n", sep = " "))
  }
}

#CARICO DATASET
CARDUCCI_DIPENDENTI <- read_csv("CARDUCCI DIPENDENTI.csv")
#Rows: 138 Columns: 10                                                                                                                
#── Column specification ──────────────────────────────────────────────────────────────────────────
#Delimiter: ","
#dbl (10): I1, I2, I3, S1, S2, S3, S4, Q1, Q2, Q3

cardu<-CARDUCCI_DIPENDENTI
dim(cardu)
#138 osservazioni, 10 variabili
#[1] 138  10
names(cardu)
#[1] "I1" "I2" "I3" "S1" "S2" "S3" "S4" "Q1" "Q2" "Q3"
dipendenti <-cardu
summary(cardu)
cardu$Q1 <- 8 - cardu$Q1#invertiamo la prima domanda relativa alla qualità,invertendo la scala, in modo che risultasse coerente con quella degli altri item
cardu.cov <- cov(cardu) #matrice covarianza 
cardu.efa <- factanal(covmat = cardu.cov, factors = 3)#analisi fattoriale a 3 fattori e cutoff 0,4
print(cardu.efa, cutoff = .4)

#Call:
#  factanal(factors = 3, covmat = cardu.cov)

#Uniquenesses:
#  I1    I2    I3    S1    S2    S3    S4    Q1    Q2    Q3 
#0.130 0.213 0.142 0.317 0.428 0.018 0.402 0.500 0.371 0.267 


#Loadings:
#  Factor1 Factor2 Factor3
#I1  0.856                 
#I2  0.828                 
#I3  0.840                 
#S1  0.664   0.486         
#S2  0.647                 
#S3          0.822   0.405 
#S4          0.689         
#Q1          0.650         
#Q2          0.703         
#Q3  0.449   0.720         

#Factor1 Factor2 Factor3
#SS loadings      3.656   3.336   0.219
#Proportion Var   0.366   0.334   0.022
#Cumulative Var   0.366   0.699   0.721

#The degrees of freedom for the model is 18 and the fit was 0.1028

#Otteniamo una varianza spiegata dal modello fattoriale del 72.1% circa con unicità delle variabili tutte contenute. 
#Decidiamo, nonostante la varianza spiegata sia elevata, di eliminare S1 e di rifare l'analisi fattoriale a tre fattori.


cardu<- cardu[, -4] #elimiamo il primo item di soddisfazione perchè appare su due fattori contemporaneamente
cardu.efa <- factanal(covmat = cardu.cov, factors = 3)
print(cardu.efa, cutoff = .4)
#Call:
#  factanal(factors = 3, covmat = cardu.cov)

#Uniquenesses:
#  I1    I2    I3    S2    S3    S4    Q1    Q2    Q3 
#0.136 0.209 0.142 0.425 0.005 0.417 0.468 0.337 0.308 

#Loadings:
#  Factor1 Factor2 Factor3
#I1 0.843                  
#I2 0.824                  
#I3 0.834                  
#S2 0.643                  
#S3         0.767   0.525  
#S4         0.663          
#Q1         0.678          
#Q2         0.737          
#Q3 0.439   0.706          

#Factor1 Factor2 Factor3
#SS loadings      3.120   3.060   0.373
#Proportion Var   0.347   0.340   0.041
#Cumulative Var   0.347   0.687   0.728

#The degrees of freedom for the model is 12 and the fit was 0.0711



#abbassando il cut off diventa chiaro che la soddisfazione contiene in se una valutazione della qualità. 

cardu<- cardu[, -4] #elimiamo il secondo item di soddisfazione
cardu.cov <- cov(cardu)
cardu.efa <- factanal(covmat = cardu.cov, factors = 3)
print(cardu.efa, cutoff = .45)

#Call:
#factanal(factors = 3, covmat = cardu.cov)

#Uniquenesses:
#  I1    I2    I3    S3    S4    Q1    Q2    Q3 
#0.121 0.222 0.134 0.064 0.410 0.463 0.332 0.320 

#Loadings:
#  Factor1 Factor2 Factor3
#I1          0.846         
#I2          0.810         
#I3          0.832         
#S3  0.804                 
#S4  0.679                 
#Q1  0.688                 
#Q2  0.745                 
#Q3  0.705                 

#Factor1 Factor2 Factor3
#SS loadings      3.056   2.658   0.219
#Proportion Var   0.382   0.332   0.027
#Cumulative Var   0.382   0.714   0.742

#The degrees of freedom for the model is 7 and the fit was 0.033 

#Si ottiene una varianza totale spiegata dai tre fattori del 79% circa, unicit? degli item tutte contenute, 
#e una matrice dei factor loadings diagonale a blocchi.


#Il giudizio di performance sopvrascrive coompletamente le aspettative  (servizio esperienziale)
#il servizio erogato dalla residenza carducci è caratterizzato da un'elevata esperenzialità e quindi 
#il giudizio di soddisfazione assimila il giudizio di qualità, 
#la soddisfazione è un modo per capire la percezione delle aspettative, ci sono casi in cui la soddisfazione è determinata interamente dalle aspettative. 
#il giudizio di performance sovrascrive completamente le aspettative.
write.table(cardu, "dip.csv", sep = ",",
            col.names = TRUE, row.names = TRUE, dec = ".") #SALVO DIPENDENTI PER MODELLO COMPLETO 


write.table(cardu.cov, "carducci.cov", sep = " ",
            col.names = FALSE, row.names = FALSE, dec = ".")


#### ANALISI CHRONBACH ####
intenzione <- cardu[, c("I1", "I2", "I3")]
soddisfazione <- cardu[, c("S3", "S4")]
qualita <- cardu[, c("Q1", "Q2", "Q3")]

cronbach(intenzione)$Alpha
#[1] 0.9350315 
round(cor(intenzione), 2)
#I1   I2   I3
#I1 1.00 0.82 0.85
#I2 0.82 1.00 0.82
#I3 0.85 0.82 1.00

cronbach(soddisfazione)$Alpha
#[1] 0.8380537
round(cor(soddisfazione), 2)
#S3   S4
#S3 1.00 0.72
#S4 0.72 1.00

cronbach(qualita)$Alpha
#[1] 0.8250247
round(cor(qualita), 2)
#Q1   Q2   Q3
#Q1 1.00 0.62 0.57
#Q2 0.62 1.00 0.66
#Q3 0.57 0.66 1.00

if.item.deleted(qualita)
#[1] "Alpha  0.797949746143647 se cancello 1 \n"
#[1] "Alpha  0.724153485940957 se cancello 2 \n"
#[1] "Alpha  0.758351446599278 se cancello 3 \n"


#Si ottengono degli alpha di Cronbach elevati, quello dell'intenzione elevato, gli item che
#non conviene eliminare nessun item perchè si ridurrebbe l'alfa di cronbach


####
#install.packages("funzioni.R")
#install.packages("dplyr")
#install.packages("lavaan")
library("lavaan")
library("dplyr")
source("funzioni.R")

#SVOLTA SU LISREL
#### ANALISI CFA ####
variabili.model <- '
qualita  =~ Q1 + Q2 + Q3
soddisf =~ S3 + S4
intenz =~ I1 + I2 + I3
'
fit <- cfa(variabili.model, data = cardu)
summary(fit, fit.measures = TRUE, modindices = TRUE)
#lavaan 0.6.16 ended normally after 36 iterations
#
# Estimator                                         ML
# Optimization method                           NLMINB
# Number of model parameters                        19
# 
# Number of observations                           138
# 
# Model Test User Model:
#   
#   Test statistic                                18.030
# Degrees of freedom                                17
# P-value (Chi-square)                           0.387
# 
# Model Test Baseline Model:
#   
#   Test statistic                               884.952
# Degrees of freedom                                28
# P-value                                        0.000
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.999
# Tucker-Lewis Index (TLI)                       0.998
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -1564.723
# Loglikelihood unrestricted model (H1)      -1555.708
# 
# Akaike (AIC)                                3167.445
# Bayesian (BIC)                              3223.063
# Sample-size adjusted Bayesian (SABIC)       3162.953
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.021
# 90 Percent confidence interval - lower         0.000
# 90 Percent confidence interval - upper         0.081
# P-value H_0: RMSEA <= 0.050                    0.718
# P-value H_0: RMSEA >= 0.080                    0.055
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.022
# 
# Parameter Estimates:
#   
#   Standard errors                             Standard
# Information                                 Expected
# Information saturated (h1) model          Structured
# 
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)
# qualita =~                                          
#   Q1                1.000                           
# Q2                0.942    0.103    9.148    0.000
# Q3                0.981    0.104    9.421    0.000
# soddisf =~                                          
#   S3                1.000                           
# S4                0.797    0.068   11.769    0.000
# intenz =~                                           
#   I1                1.000                           
# I2                0.998    0.061   16.262    0.000
# I3                0.980    0.054   18.206    0.000
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)
# qualita ~~                                          
#   soddisf           1.670    0.261    6.409    0.000
# intenz            1.218    0.209    5.828    0.000
# soddisf ~~                                          
#   intenz            1.459    0.227    6.425    0.000
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .Q1                1.293    0.175    7.380    0.000
# .Q2                0.677    0.102    6.661    0.000
# .Q3                0.614    0.098    6.285    0.000
# .S3                0.408    0.114    3.579    0.000
# .S4                0.829    0.119    6.951    0.000
# .I1                0.308    0.060    5.145    0.000
# .I2                0.492    0.077    6.396    0.000
# .I3                0.282    0.056    5.000    0.000
# qualita           1.412    0.297    4.753    0.000
# soddisf           2.151    0.321    6.701    0.000
# intenz            1.748    0.249    7.015    0.000
# 
# Modification Indices:
#   
#   lhs op rhs    mi    epc sepc.lv sepc.all sepc.nox
# 1  qualita =~  S3 0.043 -1.543  -1.833   -1.146   -1.146
# 2  qualita =~  S4 0.043  1.229   1.460    0.985    0.985
# 3  qualita =~  I1 0.021  0.015   0.018    0.012    0.012
# 4  qualita =~  I2 0.663 -0.091  -0.108   -0.072   -0.072
# 5  qualita =~  I3 0.358  0.060   0.071    0.051    0.051
# 6  soddisf =~  Q1 0.389  0.404   0.593    0.360    0.360
# 7  soddisf =~  Q2 0.250 -0.291  -0.426   -0.307   -0.307
# 8  soddisf =~  Q3 0.009 -0.057  -0.083   -0.059   -0.059
# 9  soddisf =~  I1 0.246 -0.039  -0.057   -0.040   -0.040
# 10 soddisf =~  I2 0.550 -0.063  -0.092   -0.062   -0.062
# 11 soddisf =~  I3 1.368  0.089   0.131    0.093    0.093
# 12  intenz =~  Q1 1.777 -0.206  -0.273   -0.166   -0.166
# 13  intenz =~  Q2 0.319 -0.071  -0.094   -0.068   -0.068
# 14  intenz =~  Q3 2.800  0.214   0.283    0.202    0.202
# 15  intenz =~  S3 0.043  0.034   0.045    0.028    0.028
# 16  intenz =~  S4 0.043 -0.027  -0.036   -0.024   -0.024
# 17      Q1 ~~  Q2 1.889  0.140   0.140    0.150    0.150
# 18      Q1 ~~  Q3 1.068 -0.106  -0.106   -0.119   -0.119
# 19      Q1 ~~  S3 0.653  0.080   0.080    0.111    0.111
# 20      Q1 ~~  S4 0.033 -0.019  -0.019   -0.019   -0.019
# 21      Q1 ~~  I1 0.008  0.006   0.006    0.010    0.010
# 22      Q1 ~~  I2 0.014  0.010   0.010    0.012    0.012
# 23      Q1 ~~  I3 1.103 -0.071  -0.071   -0.117   -0.117
# 24      Q2 ~~  Q3 0.080 -0.026  -0.026   -0.040   -0.040
# 25      Q2 ~~  S3 0.356  0.049   0.049    0.093    0.093
# 26      Q2 ~~  S4 1.457 -0.101  -0.101   -0.134   -0.134
# 27      Q2 ~~  I1 1.914  0.073   0.073    0.159    0.159
# 28      Q2 ~~  I2 1.033 -0.062  -0.062   -0.107   -0.107
# 29      Q2 ~~  I3 0.683 -0.042  -0.042   -0.096   -0.096
# 30      Q3 ~~  S3 1.959 -0.116  -0.116   -0.232   -0.232
# 31      Q3 ~~  S4 2.156  0.122   0.122    0.171    0.171
# 32      Q3 ~~  I1 2.357  0.079   0.079    0.181    0.181
# 33      Q3 ~~  I2 0.032  0.011   0.011    0.019    0.019
# 34      Q3 ~~  I3 0.518 -0.036  -0.036   -0.086   -0.086
# 35      S3 ~~  I1 6.068 -0.125  -0.125   -0.353   -0.353
# 36      S3 ~~  I2 0.001 -0.002  -0.002   -0.005   -0.005
# 37      S3 ~~  I3 6.566  0.126   0.126    0.372    0.372
# 38      S4 ~~  I1 0.041  0.012   0.012    0.023    0.023
# 39      S4 ~~  I2 0.004 -0.004  -0.004   -0.007   -0.007
# 40      S4 ~~  I3 0.075 -0.015  -0.015   -0.031   -0.031
# 41      I1 ~~  I2 0.475  0.056   0.056    0.144    0.144
# 42      I1 ~~  I3 0.649 -0.070  -0.070   -0.238   -0.238
# 43      I2 ~~  I3 0.002  0.004   0.004    0.010    0.010

#### COMMON VARIANCE: 1 FATTORE ####

cardu.efa <- factanal(covmat = cardu.cov, factors = 1) #common variance:analisi fattoriale con un unico fattore- si osserva un bias con common variance alta
print(cardu.efa, cutoff = .4) 
#varianza alta quindi il singolo fattore qualità spiega il 62% della varianza, ancora una volta emerge il problema dell'identificazioen della soddifazione con la qualità

#               Factor1
#SS loadings      4.933
#Proportion Var   0.617


#### CONVERGENT VALIDITY ####

condisc.fit <- condisc(fit)

#$Squared_Factor_Correlation
# $Squared_Factor_Correlation
# qualit soddsf intenz
# qualita  1.000              
# soddisf  0.918  1.000       
# intenz   0.601  0.566  1.000
# 
# $Average_Variance_Extracted
# qualita soddisf  intenz 
# 0.620   0.732   0.829 


#gli item nel complesso sono una buona misura del fattore a cui sono associati



#### DISCRIMINANT VALIDITY ####

Rij <- as.matrix(condisc.fit$Squared_Factor_Correlation)
Rij[ Rij == 1 ] <- 0

ave <- c(condisc.fit$Average_Variance_Extracted)
mRij <- apply(Rij, FUN = max, MARGIN = 2)

ave > mRij

#qualita soddisf  intenz 
#FALSE   FALSE    TRUE
#la condizione di sovrapposizione fra qialità e soddisfazione è confermata dalla discriminat validity per cui procediamo ad un'analisi a due fattori in cui soddisfazioen e qualità vengono associate

#### COMPOSITE RELIABILITY ####
comp_reliability(fit)

#lhs     composite_reliability
#<chr>                   <dbl>
# 1 intenz                  0.935
# 2 qualita                 0.824
# 3 soddisf                 0.849
#i fattori risultano essere affidabili rispetto agli item che li misurano (presenza di composite reliability)

####LINEAR AND MEDIATION MODEL ####

source("process.R")
intenzione.m <- apply(cardu[, 1:3], FUN = mean, MARGIN = 1)
soddisfazione.m <- apply(cardu[, 4:5], FUN = mean, MARGIN = 1)
qualita.m <- apply(cardu[, 6:8], FUN = mean, MARGIN = 1)

cardu.m <- data.frame(qualita.m, soddisfazione.m,
                         intenzione.m)
library("PerformanceAnalytics")
chart.Correlation(cardu.m, histogram = TRUE, pch = "+")

process(data = cardu.m, y = "intenzione.m", x="qualita.m",
        m=c("soddisfazione.m"),
        model = 4)

#Direct effect of X on Y:
#  effect        se         t         p      LLCI      ULCI
#0.3973    0.1089    3.6471    0.0004    0.1819    0.6128


#Indirect effect(s) of X on Y:
#  Effect    BootSE  BootLLCI  BootULCI
#soddisfazione.m    0.3120    0.1017    0.1302    0.5287

#nei modelli l'intervallo di confidenza non contiene lo zero per cui vi è una mediazione parziale. 

#### 2 VARIABILI DIPENDENTI ####
#### LA SODDISFAZIONE INGLOBA LA QUALITA' PERCHE' IL SERVIZIO E' ALTAMENTE ESPERIENZIALE ####

intenzione <- cardu[, c("I1", "I2", "I3")]
soddisfazione <- cardu[, c("S3", "S4", "Q1", "Q2", "Q3")]

cronbach(intenzione)$Alpha
# [1] 0.9350315
round(cor(intenzione), 2)
#     I1   I2   I3
#I1 1.00 0.82 0.85
#I2 0.82 1.00 0.82
#I3 0.85 0.82 1.00
#non conviene eliminare nessun item, perchè ridurrebbe l'alfa di cronbach


cronbach(soddisfazione)$Alpha
# [1] 0.8988353

round(cor(soddisfazione), 2)
#     S3   S4   Q1   Q2   Q3
#S3 1.00 0.72 0.65 0.71 0.72
#S4 0.72 1.00 0.54 0.58 0.66
#Q1 0.65 0.54 1.00 0.62 0.57
#Q2 0.71 0.58 0.62 1.00 0.66
#Q3 0.72 0.66 0.57 0.66 1.00

if.item.deleted(soddisfazione)
#[1] "Alpha  0.857265350391532 se cancello 1 \n"
#[1] "Alpha  0.881402571415443 se cancello 2 \n"
#[1] "Alpha  0.892467706760611 se cancello 3 \n" <---
#[1] "Alpha  0.876732034117133 se cancello 4 \n"
#[1] "Alpha  0.874257430826184 se cancello 5 \n"
soddisfazione <- soddisfazione[ , -3]
if.item.deleted(soddisfazione)
#[1] "Alpha  0.837806101509417 se cancello 1 \n"
#[1] "Alpha  0.87196260728464 se cancello 2 \n"
#[1] "Alpha  0.873701666818709 se cancello 3 \n"
#[1] "Alpha  0.859867935912047 se cancello 4 \n"
#non conviene eliminare nessun item, perchè ridurrebbe l'alfa di cronbach



########################################################
library("lavaan")
library("dplyr")
source("funzioni.R")
################### CFA
variabili.model <- '
soddisf =~ S3 + S4 + Q2 + Q3
intenz =~ I1 + I2 + I3
'
fit <- cfa(variabili.model, data = cardu)
summary(fit, fit.measures = TRUE, modindices = TRUE)

variabili.model <- '
soddisf =~ S3 + S4 + Q2 + Q3
intenz =~ I1 + I2 + I3
S3 ~~  S4
'
fit <- cfa(variabili.model, data = cardu)
summary(fit, fit.measures = TRUE, modindices = TRUE)

################## common variance ########################
cardu<- data.frame(soddisfazione, intenzione)
cardu.cov <- cov(cardu)
carducci.efa <- factanal(covmat = cardu.cov, factors = 1)
print(cardu.efa, cutoff = .4)
#               Factor1
#SS loadings      4.933
#Proportion Var   0.617

#### CONVERGENT VALIDITY ####
condisc.fit <- condisc(fit)
#$Squared_Factor_Correlation
#soddsf intenz
#soddisf  1.000       
#intenz   0.626  1.000

#$Average_Variance_Extracted
#soddisf  intenz 
#0.667   0.829 


#### DISCRIMINANT VALIDITY ####
Rij <- as.matrix(condisc.fit$Squared_Factor_Correlation)
Rij[ Rij == 1 ] <- 0

ave <- c(condisc.fit$Average_Variance_Extracted)
mRij <- apply(Rij, FUN = max, MARGIN = 2)

ave > mRij
#soddisf  intenz 
#   TRUE    TRUE 
#com ipotizzato inizialmente la discriminant validity è presente e i due fattori non si sovrappongono, è risultata corretta l'ipotesi di associazione fra soddisfazione e qualità

##################Composite reliability####################
comp_reliability(fit)
# A tibble: 2 × 2
#lhs     composite_reliability
#<chr>                   <dbl>
#  1 intenz                  0.935
#  2 soddisf                 0.890


################# Linear and Mediation Models
intenzione.m <- apply(cardu[, 1:3], FUN = mean, MARGIN = 1)
soddisfazione.m <- apply(cardu[, 4:7], FUN = mean, MARGIN = 1)

carducci.m <- data.frame(soddisfazione.m,intenzione.m)
library("PerformanceAnalytics")
chart.Correlation(carducci.m, histogram = TRUE, pch = "+")



#ANALISI CONFERMATIVA COMMENTI 
#2 FATTORI 
#RMSEA=0,022<0,5 accettabile 
#chi quadro/df= 1,068<2 va bene
#AGFI=0,93 va bene perchè è >0,9
#NFI=0,99,   CFI=1,00 vanno bene poichè <0,9, NNFI=1,00 va bene poichè >0,95
#RMR(residuo quadratuico medio)=0,051, RMR standardizzato=0,023 va bene poichè <0,08
#
#3 FATTORI 
#RMSEA=0,019<0,5 accettabile
#chi quadro/df=1,052 va bene poichè è minore di 2 
#AGFI=0,93 va bene poichè >0,9
#NFI=0,99 CFI=1,00 va bene perchè <0,9 e NNFI=1,00 va bene perchè >0,95
#RMR(residuo quadratuico medio)=0,049, RMR standardizzato=0,022 va bene poichè <0,08
#