## ----setup, include=FALSE, message=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(fig.pos = 'H', echo = FALSE, warning = FALSE)
library(knitr)
library(ggplot2)
library(tidyverse)
library(semPlot)
library(lavaan)
library(gmodels)

load("Anxeitydata.RData")
dat = as.data.frame(X)


## --------------------------------------------------------------------------------------------------------
table = dat
colnames(table) = c("jittery", "heart rate", "think get through", "think interfere", "think failing", "Worry before", "Uneasy")
table[1:8,] %>% 
  kable(digits = 2, caption = "Head 8 out of 335 observations")


## --------------------------------------------------------------------------------------------------------
options(knitr.kable.NA = '')
data.frame(latent = c("physical symptoms", NA, "intrusve thoughts", NA, NA, "worrying", NA),
           measurement = c("jittery", "heart_rate", "think_get_through", "thoughts_interfere", "Think_failing", "Worry_before", "Uneasy")) %>% 
   kable(caption = "Latent variables and Measurements")


## --------------------------------------------------------------------------------------------------------
options(knitr.kable.NA = '')
data.frame(latent = c("physical symptoms", NA, "intrusve thoughts", NA, NA, "worrying", NA),
           measurement = c("jittery", "heart_rate", "think_get_through", "thoughts_interfere", "Think_failing", "Worry_before", "Uneasy")) %>% 
   kable(caption = "Latent variables and Measurements")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     anxeity =~ Uneasy + Think_get_through + Thoughts_interfere + Jittery + Worry_before + Think_failing + Heartbeat_fast
   # regressions

   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval1 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_1 = inspect(fit, 'r2')


## --------------------------------------------------------------------------------------------------------

allmodels1 = data.frame(Eval1)
allmodels1 = t(allmodels1)
rownames(allmodels1) = c("One-factor models")
kable(allmodels1, caption = "Evaluation of One-factor model", digits = 2)


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions

   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)jians
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval2 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_2 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 1", out.width="70%"----------------------------------------------
include_graphics("1.png")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry + thoughts
   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval3 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_3 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 2", out.width="70%"----------------------------------------------
include_graphics("2.png")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry
   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval4 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_4 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 3", out.width="70%"----------------------------------------------
include_graphics("3.png")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry
     thoughts ~ worry
   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval5 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_5 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 4", out.width="70%"----------------------------------------------
include_graphics("4.png")


## ----include=FALSE---------------------------------------------------------------------------------------
Eval1
Eval2
Eval3
Eval4
Eval5


## --------------------------------------------------------------------------------------------------------
allmodels = read.csv("allmodels.csv")
names(allmodels) = c("Model", "AIC", "BIC", "CFI", "RMSEA", "SRMR")
kable(allmodels, caption = "Evaluation of models", digits = c(0,0,0,2,2,3))


## ----eval=FALSE------------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry
   # residual covariances
'

fit = cfa(model, data=dat)

#s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
pdf(file = "sem.pdf")
semPaths(fit, what = "paths", whatLabels="par", intercepts=FALSE, 
         nCharNodes=0, residuals =TRUE,
         nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         layout="tree2",curvePivot=TRUE)
dev.off()

#Eval4 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
#r2_4 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Fitted model", out.width="90%"-----------------------------------------
include_graphics("sem.png")


## --------------------------------------------------------------------------------------------------------

lf= inspect(fit,what="std")$lambda

lf2 = as.data.frame(lf)

kable(lf2, caption = "Factor Loadings", digits = 2)


## --------------------------------------------------------------------------------------------------------
summary(fit)


## ----eval=FALSE, echo=TRUE, tidy=TRUE--------------------------------------------------------------------
## ----setup, include=FALSE, message=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(fig.pos = 'H', echo = FALSE, warning = FALSE)
library(knitr)
library(ggplot2)
library(tidyverse)
library(semPlot)
library(lavaan)
library(gmodels)

load("Anxeitydata.RData")
dat = as.data.frame(X)


## --------------------------------------------------------------------------------------------------------
table = dat
colnames(table) = c("jittery", "heart rate", "think get through", "think interfere", "think failing", "Worry before", "Uneasy")
table[1:8,] %>% 
  kable(digits = 2, caption = "Head 8 out of 335 observations")


## --------------------------------------------------------------------------------------------------------
options(knitr.kable.NA = '')
data.frame(latent = c("physical symptoms", NA, "intrusve thoughts", NA, NA, "worrying", NA),
           measurement = c("jittery", "heart_rate", "think_get_through", "thoughts_interfere", "Think_failing", "Worry_before", "Uneasy")) %>% 
   kable(caption = "Latent variables and Measurements")


## --------------------------------------------------------------------------------------------------------
options(knitr.kable.NA = '')
data.frame(latent = c("physical symptoms", NA, "intrusve thoughts", NA, NA, "worrying", NA),
           measurement = c("jittery", "heart_rate", "think_get_through", "thoughts_interfere", "Think_failing", "Worry_before", "Uneasy")) %>% 
   kable(caption = "Latent variables and Measurements")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     anxeity =~ Uneasy + Think_get_through + Thoughts_interfere + Jittery + Worry_before + Think_failing + Heartbeat_fast
   # regressions

   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval1 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_1 = inspect(fit, 'r2')


## --------------------------------------------------------------------------------------------------------

allmodels1 = data.frame(Eval1)
allmodels1 = t(allmodels1)
rownames(allmodels1) = c("One-factor models")
kable(allmodels1, caption = "Evaluation of One-factor model", digits = 2)


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions

   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)jians
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval2 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_2 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 1", out.width="70%"----------------------------------------------
include_graphics("1.png")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry + thoughts
   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval3 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_3 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 2", out.width="70%"----------------------------------------------
include_graphics("2.png")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry
   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval4 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_4 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 3", out.width="70%"----------------------------------------------
include_graphics("3.png")


## ----include=FALSE---------------------------------------------------------------------------------------
model <- '
   # latent variables
     phy_symp =~ Jittery + Heartbeat_fast
     thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
     worry =~ Uneasy + Worry_before
   # regressions
     phy_symp ~ worry
     thoughts ~ worry
   # residual covariances
'

fit = cfa(model, data=dat)

s = summary(fit)
#s$PE
#semPaths(fit)
#semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
         #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
#semPaths(fit,whatLabels="par", intercepts=FALSE, 
         #nCharNodes=0, residuals =TRUE,
         #nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
         #layout="tree2",curvePivot=TRUE)

Eval5 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
r2_5 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Model 4", out.width="70%"----------------------------------------------
include_graphics("4.png")


## ----include=FALSE---------------------------------------------------------------------------------------
Eval1
Eval2
Eval3
Eval4
Eval5


## --------------------------------------------------------------------------------------------------------
allmodels = read.csv("allmodels.csv")
names(allmodels) = c("Model", "AIC", "BIC", "CFI", "RMSEA", "SRMR")
kable(allmodels, caption = "Evaluation of models", digits = c(0,0,0,2,2,3))


## ----eval=FALSE------------------------------------------------------------------------------------------
## model <- '
##    # latent variables
##      phy_symp =~ Jittery + Heartbeat_fast
##      thoughts =~ Think_get_through + Thoughts_interfere + Think_failing
##      worry =~ Uneasy + Worry_before
##    # regressions
##      phy_symp ~ worry
##    # residual covariances
## '
## 
## fit = cfa(model, data=dat)
## 
## #s = summary(fit)
## #s$PE
## #semPaths(fit)
## #semPaths(fit, what='std', nCharNodes=6, sizeMan=10,
##          #edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
## pdf(file = "sem.pdf")
## semPaths(fit, what = "paths", whatLabels="par", intercepts=FALSE,
##          nCharNodes=0, residuals =TRUE,
##          nCharEdges=0, curveAdjacent = TRUE,title=TRUE,
##          layout="tree2",curvePivot=TRUE)
## dev.off()
## 
## #Eval4 = fitMeasures(fit, c("aic","bic" ,"cfi","rmsea","srmr"))
## #r2_4 = inspect(fit, 'r2')


## ----fig.align="center", fig.cap="Fitted model", out.width="90%"-----------------------------------------
include_graphics("sem.png")


## --------------------------------------------------------------------------------------------------------

lf= inspect(fit,what="std")$lambda

lf2 = as.data.frame(lf)

kable(lf2, caption = "Factor Loadings", digits = 2)


## --------------------------------------------------------------------------------------------------------
summary(fit)


## ----eval=FALSE, echo=TRUE, tidy=TRUE--------------------------------------------------------------------
## #purl("HW2.Rmd", output = "code2.R")



