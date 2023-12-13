#LIBRERIE E FUNZIONE
library(multilevel)
library(PerformanceAnalytics)
library(readxl)
library(openxlsx)
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
library(readr)

#CREAZIONE DATASET INDIPENDENTI
CARDUCCI_INDIPENDENTI <- read.csv("cardu.csv", header = T, sep = ";")
cardu<-CARDUCCI_INDIPENDENTI
cardu<-cardu[, -1]
View(cardu)
SE<- cardu[, (1:4)]
A<- cardu[, (5:8)]
SC<- cardu[, (9:12)]
ST<- cardu[, (13:18)]
C<- cardu[, (19)]
P<- cardu[, (20:21)]
cardu <- data.frame(SE, A, SC, ST, C, P)

#EFFETTUO L'ANALISI FATTORIALE
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 6)
print(cardu.efa, cutoff = .4)

#ELIMINIAMO GLI ITEMS CON FACTOR LOADINGS MINORI DEL CUTOFF
cardu<- cardu[, -3]  #TOLGO SE3 
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 6)
print(cardu.efa, cutoff = .4)

cardu<- cardu[, -4] #TOLGO A1
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 6)
print(cardu.efa, cutoff = .4)

cardu<- cardu[, -14] #TOLGO ST4
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 6)
print(cardu.efa, cutoff = .4)

cardu<- cardu[, -15] #TOLGO ST6
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 6)
print(cardu.efa, cutoff = .4)

cardu<- cardu[, -15] #TOLGO C
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 5)
print(cardu.efa, cutoff = .4)

cardu<- cardu[, -1] #TOLGO SE1
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 5)
print(cardu.efa, cutoff = .4)

cardu<- cardu[, -12] #TOLGO ST3
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 5)
print(cardu.efa, cutoff = .4)

#TOGLIA,O GLI ITEM CHE SI SOVRAPPONGONO SU PIù FATTORI
cardu<- cardu[, -5] #TOLGO A4
cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 5)
print(cardu.efa, cutoff = .4)

#RIORDINO LE COLONNE
cardu<- cardu[, c("ST1","ST2","SE4","P1","P2","SE2","SC2","SC3","ST5","A2","A3","SC1","SC4")]

#RINOMINO LE COLONNE
colnames(cardu)[1]="T1"
colnames(cardu)[2]="T2"
colnames(cardu)[3]="P1"
colnames(cardu)[4]="P2"
colnames(cardu)[5]="P3"
colnames(cardu)[6]="SE1"
colnames(cardu)[7]="SE2"
colnames(cardu)[8]="SE3"
colnames(cardu)[9]="SE4"
colnames(cardu)[10]="A1"
colnames(cardu)[11]="A2"
colnames(cardu)[12]="SC1"
colnames(cardu)[13]="SC2"

cardu.cov <- cov(cardu, use = "pairwise.complete.obs")
cardu.efa <- factanal(covmat = cardu.cov, factors = 5)
print(cardu.efa, cutoff = .4)
# Call:
#   factanal(factors = 5, covmat = cardu.cov)
# 
# Uniquenesses:
#   T1    T2    P1    P2    P3   SE1   SE2   SE3   SE4    A1    A2   SC1 
# 0.483 0.426 0.692 0.005 0.668 0.608 0.579 0.582 0.388 0.635 0.005 0.450 
# SC2 
# 0.724 
# 
# Loadings:
#   Factor1 Factor2 Factor3 Factor4 Factor5
# T1                   0.664                 
# T2                   0.736                 
# P1   0.460                                 
# P2   0.981                                 
# P3   0.441                                 
# SE1          0.591                         
# SE2          0.448                         
# SE3          0.499                         
# SE4          0.660                         
# A1                           0.457         
# A2                           0.976         
# SC1                                  0.711 
# SC2                                  0.478 
# 
# Factor1 Factor2 Factor3 Factor4 Factor5
# SS loadings      1.509   1.473   1.441   1.323   1.009
# Proportion Var   0.116   0.113   0.111   0.102   0.078
# Cumulative Var   0.116   0.229   0.340   0.442   0.520
# 
# The degrees of freedom for the model is 23 and the fit was 0.0987 

#VARIANZA SPIEGATA DEL 52%

#RAGGRUPPO GLI ITEM NEI NUOVI COSTRUTTI
tranquillita<- cardu[,1:2]
personale<- cardu[,3:5]
servizi<- cardu[,6:9]
alloggio<- cardu[,10:11]
spazi<- cardu[,12:13]

indip <- data.frame(tranquillita, personale, servizi, alloggio, spazi)
write.table(indip, "indip.csv", sep = ",", 
            row.names = TRUE, col.names = TRUE,dec = ".")

indip.cov <- cov(indip, use = "pairwise.complete.obs")
write.table(indip.cov, "indip.cov", sep = " ", 
            row.names = FALSE, col.names = FALSE,dec = ".")


#ANALISI CRONBACH E CORRELAZIONE INTER-ITEM
cronbach(tranquillita)$Alpha
# [1] 0.6849949
round(cor(tranquillita),2)
# T1   T2
# T1 1.00 0.53
# T2 0.53 1.00

cronbach(personale)$Alpha
# [1] 0.6585128
round(cor(personale),2)
#     P1   P2   P3
# P1 1.00 0.48 0.28
# P2 0.48 1.00 0.43
# P3 0.28 0.43 1.00

cronbach(servizi)$Alpha
# [1] 0.6670987
round(cor(servizi),2)
#      SE1  SE2  SE3  SE4
# SE1 1.00 0.28 0.32 0.35
# SE2 0.28 1.00 0.32 0.33
# SE3 0.32 0.32 1.00 0.42
# SE4 0.35 0.33 0.42 1.00
if.item.deleted(servizi)
# [1] "Alpha  0.621193635725507 se cancello 1 \n"
# [1] "Alpha  0.627045026195061 se cancello 2 \n"
# [1] "Alpha  0.582221188215139 se cancello 3 \n"
# [1] "Alpha  0.571396165577695 se cancello 4 \n"

cronbach(alloggio)$Alpha
# [1] 0.6767127
round(cor(alloggio),2)
# A1   A2
# A1 1.00 0.51
# A2 0.51 1.00

cronbach(spazi)$Alpha
# [1] 0.5303054
round(cor(spazi),2)
# SC1  SC2
# SC1 1.00 0.36
# SC2 0.36 1.00

# I CRONBACH NON AUMENTANO ELIMINANDO GLI ITEM QUINDI DECIDIAMO DI MANTENERLI

#### CFA ####
library(lavaan)
indipendenti.model <- '
tranquillita =~ T1 + T2
personale =~ P1 +P2 +P3
servizi =~ SE1 + SE2 + SE3 + SE4
alloggi =~ A1 + A2
spazi =~ SC1 + SC2
'
fit <- cfa(indipendenti.model, data = cardu)
summary(fit, fit.measures = TRUE, modindices = TRUE)

# Estimator                                         ML
# Optimization method                           NLMINB
# Number of model parameters                        36
# 
# Number of observations                           138
# 
# Model Test User Model:
#   
#   Test statistic                                88.670
# Degrees of freedom                                55
# P-value (Chi-square)                           0.003
# 
# Model Test Baseline Model:
#   
#   Test statistic                               423.132
# Degrees of freedom                                78
# P-value                                        0.000
# 
# User Model versus Baseline Model:
#   
#   Comparative Fit Index (CFI)                    0.902
# Tucker-Lewis Index (TLI)                       0.862
# 
# Loglikelihood and Information Criteria:
#   
#   Loglikelihood user model (H0)              -3153.331
# Loglikelihood unrestricted model (H1)      -3108.996
# 
# Akaike (AIC)                                6378.662
# Bayesian (BIC)                              6484.044
# Sample-size adjusted Bayesian (SABIC)       6370.152
# 
# Root Mean Square Error of Approximation:
#   
#   RMSEA                                          0.067
# 90 Percent confidence interval - lower         0.039
# 90 Percent confidence interval - upper         0.091
# P-value H_0: RMSEA <= 0.050                    0.141
# P-value H_0: RMSEA >= 0.080                    0.200
# 
# Standardized Root Mean Square Residual:
#   
#   SRMR                                           0.070
# 
# Parameter Estimates:
#   
#   Standard errors                             Standard
# Information                                 Expected
# Information saturated (h1) model          Structured
# 
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)
# tranquillita =~                                     
#   T1                1.000                           
# T2                0.766    0.161    4.758    0.000
# personale =~                                        
#   P1                1.000                           
# P2                1.042    0.206    5.047    0.000
# P3                0.814    0.174    4.683    0.000
# servizi =~                                          
#   SE1               1.000                           
# SE2               1.152    0.299    3.848    0.000
# SE3               1.478    0.345    4.279    0.000
# SE4               1.241    0.298    4.167    0.000
# alloggi =~                                          
#   A1                1.000                           
# A2                0.820    0.181    4.525    0.000
# spazi =~                                            
#   SC1               1.000                           
# SC2               1.029    0.335    3.069    0.002
# 
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)
# tranquillita ~~                                     
#   personale         0.503    0.178    2.819    0.005
# servizi           0.537    0.171    3.136    0.002
# alloggi           0.519    0.220    2.358    0.018
# spazi             0.489    0.183    2.675    0.007
# personale ~~                                        
#   servizi           0.322    0.119    2.710    0.007
# alloggi           0.303    0.162    1.870    0.061
# spazi             0.412    0.145    2.835    0.005
# servizi ~~                                          
#   alloggi           0.629    0.183    3.441    0.001
# spazi             0.145    0.098    1.483    0.138
# alloggi ~~                                          
#   spazi             0.236    0.162    1.453    0.146
# 
# Variances:
#   Estimate  Std.Err  z-value  P(>|z|)
# .T1                1.278    0.363    3.518    0.000
# .T2                1.044    0.230    4.530    0.000
# .P1                1.417    0.238    5.952    0.000
# .P2                0.908    0.204    4.455    0.000
# .P3                1.371    0.203    6.751    0.000
# .SE1               2.243    0.294    7.621    0.000
# .SE2               1.916    0.265    7.232    0.000
# .SE3               1.351    0.235    5.741    0.000
# .SE4               1.280    0.200    6.408    0.000
# .A1                1.039    0.365    2.849    0.004
# .A2                1.573    0.298    5.273    0.000
# .SC1               1.164    0.278    4.184    0.000
# .SC2               1.567    0.317    4.947    0.000
# tranquillita      1.714    0.463    3.702    0.000
# personale         0.931    0.280    3.325    0.001
# servizi           0.557    0.233    2.389    0.017
# alloggi           1.701    0.459    3.708    0.000
# spazi             0.750    0.302    2.482    0.013
# 
# Modification Indices:
#   
#   lhs op rhs     mi    epc sepc.lv sepc.all sepc.nox
# 1   tranquillita =~  P1  2.377  0.209   0.273    0.178    0.178
# 2   tranquillita =~  P2 11.535 -0.451  -0.590   -0.426   -0.426
# 3   tranquillita =~  P3  5.160  0.275   0.360    0.256    0.256
# 4   tranquillita =~ SE1  6.118 -0.408  -0.534   -0.319   -0.319
# 5   tranquillita =~ SE2 11.425  0.539   0.705    0.433    0.433
# 6   tranquillita =~ SE3  0.209 -0.074  -0.097   -0.061   -0.061
# 7   tranquillita =~ SE4  0.378 -0.089  -0.116   -0.080   -0.080
# 8   tranquillita =~  A1  0.596 -0.138  -0.181   -0.109   -0.109
# 9   tranquillita =~  A2  0.596  0.113   0.148    0.090    0.090
# 10  tranquillita =~ SC1  0.445  0.137   0.180    0.130    0.130
# 11  tranquillita =~ SC2  0.445 -0.141  -0.185   -0.120   -0.120
# 12     personale =~  T1  0.297  0.150   0.145    0.084    0.084
# 13     personale =~  T2  0.297 -0.115  -0.111   -0.077   -0.077
# 14     personale =~ SE1  0.151 -0.078  -0.075   -0.045   -0.045
# 15     personale =~ SE2  1.385  0.227   0.219    0.135    0.135
# 16     personale =~ SE3  1.217  0.215   0.207    0.129    0.129
# 17     personale =~ SE4  3.549 -0.328  -0.316   -0.216   -0.216
# 18     personale =~  A1  2.137 -0.326  -0.314   -0.190   -0.190
# 19     personale =~  A2  2.137  0.267   0.258    0.156    0.156
# 20     personale =~ SC1  0.066  0.095   0.092    0.066    0.066
# 21     personale =~ SC2  0.066 -0.098  -0.094   -0.061   -0.061
# 22       servizi =~  T1  1.259 -0.568  -0.424   -0.245   -0.245
# 23       servizi =~  T2  1.259  0.435   0.325    0.227    0.227
# 24       servizi =~  P1  0.931  0.231   0.172    0.113    0.113
# 25       servizi =~  P2  1.141 -0.250  -0.187   -0.135   -0.135
# 26       servizi =~  P3  0.036  0.041   0.030    0.022    0.022
# 27       servizi =~  A1  3.125 -2.653  -1.981   -1.197   -1.197
# 28       servizi =~  A2  3.124  2.176   1.624    0.986    0.986
# 29       servizi =~ SC1  2.453  0.411   0.307    0.222    0.222
# 30       servizi =~ SC2  2.453 -0.423  -0.316   -0.206   -0.206
# 31       alloggi =~  T1  0.270  0.088   0.115    0.067    0.067
# 32       alloggi =~  T2  0.270 -0.068  -0.088   -0.062   -0.062
# 33       alloggi =~  P1  0.685  0.097   0.127    0.083    0.083
# 34       alloggi =~  P2  1.197 -0.124  -0.162   -0.117   -0.117
# 35       alloggi =~  P3  0.160  0.043   0.056    0.040    0.040
# 36       alloggi =~ SE1  0.000  0.000   0.000    0.000    0.000
# 37       alloggi =~ SE2  1.761 -0.249  -0.325   -0.200   -0.200
# 38       alloggi =~ SE3  1.412  0.232   0.302    0.188    0.188
# 39       alloggi =~ SE4  0.014 -0.021  -0.027   -0.018   -0.018
# 40       alloggi =~ SC1  1.099  0.143   0.187    0.135    0.135
# 41       alloggi =~ SC2  1.099 -0.147  -0.192   -0.125   -0.125
# 42         spazi =~  T1  2.548  0.516   0.446    0.258    0.258
# 43         spazi =~  T2  2.548 -0.395  -0.342   -0.239   -0.239
# 44         spazi =~  P1  0.226  0.121   0.105    0.068    0.068
# 45         spazi =~  P2  2.881 -0.423  -0.366   -0.264   -0.264
# 46         spazi =~  P3  2.150  0.335   0.290    0.205    0.205
# 47         spazi =~ SE1  0.090  0.065   0.057    0.034    0.034
# 48         spazi =~ SE2 11.012  0.697   0.603    0.370    0.370
# 49         spazi =~ SE3  0.274  0.111   0.096    0.060    0.060
# 50         spazi =~ SE4 12.490 -0.667  -0.577   -0.395   -0.395
# 51         spazi =~  A1  0.000  0.001   0.001    0.000    0.000
# 52         spazi =~  A2  0.000 -0.001   0.000    0.000    0.000
# 53            T1 ~~  P1  2.980  0.283   0.283    0.210    0.210
# 54            T1 ~~  P2  2.162 -0.217  -0.217   -0.201   -0.201
# 55            T1 ~~  P3  0.129  0.055   0.055    0.042    0.042
# 56            T1 ~~ SE1  2.765 -0.317  -0.317   -0.187   -0.187
# 57            T1 ~~ SE2  0.771  0.160   0.160    0.102    0.102
# 58            T1 ~~ SE3  0.743 -0.150  -0.150   -0.114   -0.114
# 59            T1 ~~ SE4  0.084 -0.046  -0.046   -0.036   -0.036
# 60            T1 ~~  A1  0.014  0.021   0.021    0.018    0.018
# 61            T1 ~~  A2  0.935  0.167   0.167    0.118    0.118
# 62            T1 ~~ SC1  0.018  0.023   0.023    0.019    0.019
# 63            T1 ~~ SC2  1.030  0.192   0.192    0.136    0.136
# 64            T2 ~~  P1  0.308 -0.076  -0.076   -0.062   -0.062
# 65            T2 ~~  P2  1.982 -0.171  -0.171   -0.175   -0.175
# 66            T2 ~~  P3  4.553  0.274   0.274    0.229    0.229
# 67            T2 ~~ SE1  1.953 -0.222  -0.222   -0.145   -0.145
# 68            T2 ~~ SE2  2.192  0.224   0.224    0.159    0.159
# 69            T2 ~~ SE3  0.079  0.040   0.040    0.034    0.034
# 70            T2 ~~ SE4  1.431  0.158   0.158    0.137    0.137
# 71            T2 ~~  A1  0.458 -0.098  -0.098   -0.094   -0.094
# 72            T2 ~~  A2  0.143 -0.054  -0.054   -0.042   -0.042
# 73            T2 ~~ SC1  0.175 -0.059  -0.059   -0.053   -0.053
# 74            T2 ~~ SC2  0.604 -0.119  -0.119   -0.093   -0.093
# 75            P1 ~~  P2  1.615  0.347   0.347    0.306    0.306
# 76            P1 ~~  P3  5.232 -0.456  -0.456   -0.327   -0.327
# 77            P1 ~~ SE1  0.056  0.041   0.041    0.023    0.023
# 78            P1 ~~ SE2  0.162  0.066   0.066    0.040    0.040
# 79            P1 ~~ SE3  0.182 -0.065  -0.065   -0.047   -0.047
# 80            P1 ~~ SE4  0.003 -0.007  -0.007   -0.005   -0.005
# 81            P1 ~~  A1  0.111 -0.052  -0.052   -0.043   -0.043
# 82            P1 ~~  A2  0.951  0.154   0.154    0.103    0.103
# 83            P1 ~~ SC1  0.003  0.008   0.008    0.006    0.006
# 84            P1 ~~ SC2  0.031 -0.029  -0.029   -0.020   -0.020
# 85            P2 ~~  P3  1.252  0.236   0.236    0.211    0.211
# 86            P2 ~~ SE1  0.436  0.101   0.101    0.071    0.071
# 87            P2 ~~ SE2  1.412 -0.173  -0.173   -0.131   -0.131
# 88            P2 ~~ SE3  2.005  0.192   0.192    0.173    0.173
# 89            P2 ~~ SE4  0.016  0.016   0.016    0.015    0.015
# 90            P2 ~~  A1  0.279 -0.075  -0.075   -0.077   -0.077
# 91            P2 ~~  A2  0.090 -0.042  -0.042   -0.035   -0.035
# 92            P2 ~~ SC1  0.778 -0.123  -0.123   -0.119   -0.119
# 93            P2 ~~ SC2  0.377  0.094   0.094    0.079    0.079
# 94            P3 ~~ SE1  3.929 -0.328  -0.328   -0.187   -0.187
# 95            P3 ~~ SE2  0.178  0.066   0.066    0.041    0.041
# 96            P3 ~~ SE3  0.016  0.018   0.018    0.013    0.013
# 97            P3 ~~ SE4  0.529 -0.097  -0.097   -0.073   -0.073
# 98            P3 ~~  A1  0.522 -0.106  -0.106   -0.089   -0.089
# 99            P3 ~~  A2  2.314  0.228   0.228    0.155    0.155
# 100           P3 ~~ SC1  0.261  0.071   0.071    0.056    0.056
# 101           P3 ~~ SC2  0.000  0.001   0.001    0.001    0.001
# 102          SE1 ~~ SE2  0.763  0.177   0.177    0.085    0.085
# 103          SE1 ~~ SE3  0.134  0.073   0.073    0.042    0.042
# 104          SE1 ~~ SE4  1.752  0.238   0.238    0.140    0.140
# 105          SE1 ~~  A1  0.089 -0.056  -0.056   -0.036   -0.036
# 106          SE1 ~~  A2  0.036 -0.036  -0.036   -0.019   -0.019
# 107          SE1 ~~ SC1  3.641  0.319   0.319    0.197    0.197
# 108          SE1 ~~ SC2  0.001  0.005   0.005    0.003    0.003
# 109          SE2 ~~ SE3  1.641 -0.255  -0.255   -0.158   -0.158
# 110          SE2 ~~ SE4  0.009 -0.017  -0.017   -0.011   -0.011
# 111          SE2 ~~  A1  0.546  0.133   0.133    0.094    0.094
# 112          SE2 ~~  A2  6.722 -0.469  -0.469   -0.270   -0.270
# 113          SE2 ~~ SC1  1.695  0.206   0.206    0.138    0.138
# 114          SE2 ~~ SC2  3.247  0.318   0.318    0.183    0.183
# 115          SE3 ~~ SE4  0.302 -0.108  -0.108   -0.082   -0.082
# 116          SE3 ~~  A1  0.376 -0.110  -0.110   -0.093   -0.093
# 117          SE3 ~~  A2  4.408  0.370   0.370    0.254    0.254
# 118          SE3 ~~ SC1  3.610  0.282   0.282    0.225    0.225
# 119          SE3 ~~ SC2  3.850 -0.321  -0.321   -0.221   -0.221
# 120          SE4 ~~  A1  0.759  0.140   0.140    0.122    0.122
# 121          SE4 ~~  A2  0.579 -0.122  -0.122   -0.086   -0.086
# 122          SE4 ~~ SC1  8.561 -0.401  -0.401   -0.329   -0.329
# 123          SE4 ~~ SC2  0.769 -0.133  -0.133   -0.094   -0.094
# 124           A1 ~~ SC1  0.046 -0.035  -0.035   -0.031   -0.031
# 125           A1 ~~ SC2  1.781  0.234   0.234    0.183    0.183
# [ reached 'max' / getOption("max.print") -- omitted 2 rows ]

#### COMMON VARIANCE: 1 FATTORE ####

indip.efa <- factanal(covmat = indip.cov, factors = 1) #common variance:analisi fattoriale con un unico fattore- si osserva un bias con common variance alta
print(indip.efa, cutoff = .4) 
# Factor1
# SS loadings      2.789
# Proportion Var   0.215

# Non è presente common variance, infatti, la varianza spiegata dal modello strutturale con un solo fattore risulta essere pari al 21,5%. 

#### CONVERGENT VALIDITY ####
library("lavaan")
source("funzioni.R")
condisc.fit <- condisc(fit)
# $Squared_Factor_Correlation
# trnqll persnl serviz allogg spazi
# tranquillita  1.000                           
# personale     0.159  1.000                    
# servizi       0.301  0.200  1.000             
# alloggi       0.092  0.058  0.416  1.000      
# spazi         0.186  0.244  0.050  0.044 1.000
# 
# $Average_Variance_Extracted
# tranquillita    personale      servizi      alloggi        spazi 
# 0.532        0.411        0.338        0.521        0.364 

# Non è presente convergent validity: gli item nel complesso spiegano una modesta parte della varianza

#### DISCRIMINANT VALIDITY ####
Rij <- as.matrix(condisc.fit$Squared_Factor_Correlation)
Rij[ Rij == 1 ] <- 0

ave <- c(condisc.fit$Average_Variance_Extracted)
mRij <- apply(Rij, FUN = max, MARGIN = 2)

ave > mRij
# tranquillita    personale      servizi      alloggi        spazi 
# TRUE         TRUE        FALSE         TRUE         TRUE 

#### COMPOSITE RELIABILITY ####
comp_reliability(fit)
# lhs          composite_reliability_ec
# <chr>                           <dbl>
# 1 alloggi                         0.418
# 2 personale                       0.406
# 3 servizi                         0.394
# 4 spazi                           0.274
# 5 tranquillita                    0.434

# Il fattore spazi risulta essere in parte sovrapposta ad alloggi

####LINEAR AND MEDIATION MODEL ####
source("process.R")
head(indip)
tranquillita.m <- apply(indip[, 1:2], FUN = mean, MARGIN = 1)
personale.m <- apply(indip[, 3:5], FUN = mean, MARGIN = 1)
servizi.m <- apply(indip[, 6:9], FUN = mean, MARGIN = 1)
alloggio.m <- apply(indip[, 10:11], FUN = mean, MARGIN = 1)
spazi.m <- apply(indip[, 12:13], FUN = mean, MARGIN = 1)
indip.m <- data.frame(tranquillita.m,personale.m,servizi.m,
                      alloggio.m,spazi.m)
library("PerformanceAnalytics")
chart.Correlation(indip.m, histogram = TRUE, pch = "+")
