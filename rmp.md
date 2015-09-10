Project Regression Models : The Motor Trend Analyst
========================================================

**Questions**
1. "Is an automatic or manual transmission better for MPG”
2. "Quantify the MPG difference between automatic and manual transmissions"

**Executive summary**

Manual transmission appear to give better mpg efficiency in cars tested, yet quantifying the improvement varies significantly with other characteristics of the cars examined. So its better looking around for other characteristics that also play a significant role in mpg efficiency.

**Analysis Method**
1. Explore datasets, to understand variables correlations and approach
2. Fit mutliple models, compare their diagnostics and pick best
3. Compare best model fit.
4. Calculate difference of estimated MPG for these two cases.


**Analysis**

By checking the details of the mtcars dataset, the variable that defines the transmission is "am", a dummyvariable, with values : am   Transmission (0 = automatic, 1 = manual), that for convenience, I will rename to "auto" & "manual" respectively.


PHASE 1 - **EXPLORE**

```r
data(mtcars)
mtc<-data.frame(mtcars)
```
First, I will explore correlations in this set to discover which variables are relatively significant variables to include in the model and which aren't

```r
pairs(mtc, panel = panel.smooth, main = "MT Cars", col = 13 + (mtc$am >0))
```

![plot of chunk o](figure/o-1.png) 
I observe that indeed a few variables are correlated in with each other and with mpg, presenting a monotonous distribution.
Also by coloring the two transmission types, I notice that they are distinct.

Then, I will quantify these correlations of variables to exclude those that are highly correlated with each other

```r
 M<-abs(cor(mtc))
    which(M> 0.9, arr.ind=T)
```

```
##      row col
## mpg    1   1
## cyl    2   2
## disp   3   2
## cyl    2   3
## disp   3   3
## hp     4   4
## drat   5   5
## wt     6   6
## qsec   7   7
## vs     8   8
## am     9   9
## gear  10  10
## carb  11  11
```
    
Variables disp and cyl have correlation >0.9 so we could use only one of them as a predictor. If I want to select a few more that appear to highly correlated, I lower my threshold to 0.8 :


```r
which(M> 0.8, arr.ind=T)
```

```
##      row col
## mpg    1   1
## cyl    2   1
## disp   3   1
## wt     6   1
## mpg    1   2
## cyl    2   2
## disp   3   2
## hp     4   2
## vs     8   2
## mpg    1   3
## cyl    2   3
## disp   3   3
## wt     6   3
## cyl    2   4
## hp     4   4
## drat   5   5
## mpg    1   6
## disp   3   6
## wt     6   6
## qsec   7   7
## cyl    2   8
## vs     8   8
## am     9   9
## gear  10  10
## carb  11  11
```
I notice that the following are the most correlated with the outcome (mpg) : wt,cyl, disp,hp - So if I keep those with best mpg correlation I select : **wt** (and exclude disp) & **cyl** (andxclude hp, vs, disp) &
So finally I conclude to the following variables as my principle components :
wt, cyl, am, gear, drat, qsec, card

PHASE 2 : **FIT DIFFERENT MODELS FOR AM=1**

Let's check what fitting with the above insights yields to. First I convert my dummy variable in a character set, to make my results more easily interpretable.

```r
mtc$am<-as.character(mtc$am)
mtc$am[mtc$am=="0"]<-"auto"
mtc$am[mtc$am=="1"]<-"manual"
```
Then I fit various models, one generic with all variables and a few incorporating the selections of principal variables I have done before.

```r
  fit<-lm(formula = mpg ~ .,  data = mtc); f0<-summary(fit)
  fit1<-lm(formula = mpg ~ wt + cyl + am + gear + drat + qsec + carb,  data = mtc) ;f1<-summary(fit1)
  fit2<-lm(formula = mpg ~ wt  + am + gear + drat + qsec + carb,  data = mtc); f2<-summary(fit2)
  fit3<-lm(formula = mpg ~ wt  + am  + drat + qsec + carb,  data = mtc); f3<-summary(fit3)
  fit4<-lm(formula = mpg ~ wt  + am  + drat ,  data = mtc); f4<-summary(fit4)
```
For the above models I check some diagnostics 

```r
anova(fit1,fit1,fit2, fit3, fit4)
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ wt + cyl + am + gear + drat + qsec + carb
## Model 2: mpg ~ wt + cyl + am + gear + drat + qsec + carb
## Model 3: mpg ~ wt + am + gear + drat + qsec + carb
## Model 4: mpg ~ wt + am + drat + qsec + carb
## Model 5: mpg ~ wt + am + drat
##   Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
## 1     24 154.92                                
## 2     24 154.92  0     0.000                   
## 3     25 155.13 -1    -0.207 0.0321 0.859399   
## 4     26 156.75 -1    -1.621 0.2511 0.620898   
## 5     28 266.99 -2  -110.236 8.5385 0.001582 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
paste("Residuals Variation : f0:", f0$sigma, "f1:" ,f1$sigma,"f2:" , f2$sigma, "f3:" , f3$sigma,"f4:" , f4$sigma) 
```

```
## [1] "Residuals Variation : f0: 2.65019702786551 f1: 2.54070523315097 f2: 2.49103469536119 f3: 2.45538614573072 f4: 3.08792331557445"
```

```r
summary(fit)$coefficients ; summary(fit3)$coefficients; summary(fit4)$coefficients; 
```

```
##                Estimate  Std. Error    t value   Pr(>|t|)
## (Intercept) 12.30337416 18.71788443  0.6573058 0.51812440
## cyl         -0.11144048  1.04502336 -0.1066392 0.91608738
## disp         0.01333524  0.01785750  0.7467585 0.46348865
## hp          -0.02148212  0.02176858 -0.9868407 0.33495531
## drat         0.78711097  1.63537307  0.4813036 0.63527790
## wt          -3.71530393  1.89441430 -1.9611887 0.06325215
## qsec         0.82104075  0.73084480  1.1234133 0.27394127
## vs           0.31776281  2.10450861  0.1509915 0.88142347
## ammanual     2.52022689  2.05665055  1.2254035 0.23398971
## gear         0.65541302  1.49325996  0.4389142 0.66520643
## carb        -0.19941925  0.82875250 -0.2406258 0.81217871
```

```
##               Estimate Std. Error    t value    Pr(>|t|)
## (Intercept)  9.9242985  8.2592008  1.2016052 0.240347811
## wt          -3.1108432  0.9050238 -3.4373056 0.001988359
## ammanual     2.9639102  1.6233702  1.8257759 0.079391329
## drat         1.2070622  1.3974554  0.8637572 0.395619972
## qsec         0.9145380  0.3603192  2.5381332 0.017476995
## carb        -0.6023241  0.4432180 -1.3589794 0.185825976
```

```
##               Estimate Std. Error    t value     Pr(>|t|)
## (Intercept) 29.8969570  7.4601914  4.0075322 4.118044e-04
## wt          -4.9418861  0.8714365 -5.6709651 4.451953e-06
## ammanual    -0.8310724  1.7094449 -0.4861651 6.306335e-01
## drat         1.7879779  1.6401099  1.0901574 2.849371e-01
```
I observe that indeed the model fit3 with the fewest predictors (regressors) is the one with strongest coefficients, and smaller standard error, as well as an minimal residual variation.
This can be visualized by plotting residuals for model all (in red) versus model with fewest significant variables (blue) and the one with fewest variables (signficant or not, green)


```r
e0 <- resid(fit)
e3 <- resid(fit3)
e4 <- resid(fit4)
par(mfrow = c(1, 3))
plot(mtc$mpg, e0,  
     xlab = "miles per gallon", 
     ylab = "Residuals (Fit all)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : length(mtc$mpg)) 
lines(c(mtc$mpg[i], mtc$mpg[i]),c(e0[i], 0), col = "red" , lwd = 2)

plot(mtc$mpg, e3,  
     xlab = "miles per gallon", 
     ylab = "Residuals (Fit Less-Is-More)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : length(mtc$mpg)) 
lines(c(mtc$mpg[i], mtc$mpg[i]),c(e3[i], 0), col = "blue" , lwd = 2)

plot(mtc$mpg, e4,  
     xlab = "miles per gallon", 
     ylab = "Residuals (Fit Skinny)", 
     bg = "lightblue", 
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : length(mtc$mpg)) 
lines(c(mtc$mpg[i], mtc$mpg[i]),c(e4[i], 0), col = "green" , lwd = 2)
```

![plot of chunk b](figure/b-1.png) 


PHASE 3 : **CALCULATE CASES**
From the coefficients it occurs that a manual transmission increases the efficiency of miles per gallon by 2.96 with a standard error of 1.6233. So, it appears that the existence of the manual transmission, indeed improves mpg.

PHASE 4 : **Quantify Impact on MPG**

How much better is the manual transmission for mpg? 
With confidence of 97% this is quantified by getting predicted values from our model (fit3)

```r
sumCoef<-summary(fit3)$coefficients 
sumCoef[1,1]+c(-1,1)*qt(.975,df=fit5$df)*sumCoef[1,2]
```

```
## Error in qt(0.975, df = fit5$df): object 'fit5' not found
```
The results move in the zone of -6.99 to 26.84 which means that there is significant challenge in predicting the change in mpg per transmission type with other variables included. 
If transmission was the only variable, then :


```r
fitam<-lm(formula = mpg ~ am,  data = mtc) ;fam<-summary(fitam)
sumCoef<-fam$coefficients 
sumCoef[1,1]+c(-1,1)*qt(.975,df=fitam$df)*sumCoef[1,2]
```

```
## [1] 14.85062 19.44411
```
With 97,5% confidence a manual transmission gives 14.85 to 19.44 miles per gallon more than an automatic transmission.
