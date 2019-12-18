## ----setup, include=FALSE, message=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(fig.pos = 'H', echo = FALSE, warning = FALSE)
library(knitr)
library(ggplot2)
library(tidyverse)
library(dummies)
library(DT)
library(lmtest)
library(sjPlot)
library(ROCR)
library(POCRE)
library(caret)
library(e1071)


## ----fig.cap="Linear Regression Model", out.width="50%", fig.align="center"---------------------------------------
table = data.frame(x = 1:10,
                   y = c(rep(0,5), rep(1,5)))
table %>% 
   ggplot(aes(x = x, y = y)) +
   geom_point(color = "red") +
   theme_bw() +
   geom_abline(aes(intercept=-0.3, slope=0.15), color = "blue")


## ----fig.cap="Logit Model", out.width="50%", fig.align="center"---------------------------------------------------
table %>% 
   ggplot(aes(x = x, y = y)) +
   geom_point(color = "red") +
   theme_bw() +
   geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) 


## -----------------------------------------------------------------------------------------------------------------
table = data.frame(Obs_Pred = c("Pred 0","Pred 1"), 
                   Obs_0 = c("True Negative(TN)","False Positive(FP)"),
                   Obs_1 = c("False Negative(FN)","True Negative(TN)"))

# Then for PDF:
kable(table, caption = "Confusion Matrix", digits = 2)


## -----------------------------------------------------------------------------------------------------------------
dat_vote <- read.csv("vote_parties.csv", na.strings = "No answer")
dat <- read.csv("vote_parties.csv", na.strings = c("No answer", "Don't know"))
dat <- na.omit(dat)
names(dat)[names(dat) == "a5"] <- "PaperTime"
names(dat)[names(dat) == "b1"] <- "PoInt"
names(dat)[names(dat) == "b11"] <- "VoteLast"
names(dat)[names(dat) == "b12"] <- "VoteParty"
names(dat)[names(dat) == "c1"] <- "HapScale"
names(dat)[names(dat) == "c5"] <- "Crime5year"
names(dat)[names(dat) == "c15"] <- "HealthLvl"
names(dat)[names(dat) == "c28"] <- "SweBorn"
names(dat)[names(dat) == "c33"] <- "FSweBorn"
names(dat)[names(dat) == "c35"] <- "MSweBorn"
names(dat)[names(dat) == "d4"] <- "Spouse3year"
names(dat)[names(dat) == "d6"] <- "Married"
names(dat)[names(dat) == "d9"] <- "TotalChildren"
names(dat)[names(dat) == "e4"] <- "OptLvl"
names(dat)[names(dat) == "e5"] <- "PosLvl"
names(dat)[names(dat) == "e7"] <- "SatLvl"
names(dat)[names(dat) == "f1"] <- "FamSize"
names(dat)[names(dat) == "f2"] <- "Gender"
names(dat)[names(dat) == "f3"] <- "Bdate"
names(dat)[names(dat) == "f5"] <- "Area"
names(dat)[names(dat) == "f27"] <- "FormUnemp"
names(dat)[names(dat) == "f30"] <- "TradeUn"
names(dat)[names(dat) == "f32"] <- "IncLvl"


## -----------------------------------------------------------------------------------------------------------------

dat_t = dat

dat_t$PaperTime = as.character(dat_t$PaperTime)
dat_t$PaperTime[dat_t$PaperTime == "No time at all"] = 0
dat_t$PaperTime[dat_t$PaperTime == "Less than 0,5 hour"] = 1
dat_t$PaperTime[dat_t$PaperTime == "0,5 hour to 1 hour"] = 2
dat_t$PaperTime[dat_t$PaperTime == "More than 1 hour, up to "] = 3
dat_t$PaperTime[dat_t$PaperTime == "More than 1,5 hours, up "] = 4
dat_t$PaperTime[dat_t$PaperTime == "More than 2 hours, up to"] = 5
dat_t$PaperTime[dat_t$PaperTime == "More than 2,5 hours, up to"] = 6
dat_t$PaperTime[dat_t$PaperTime == "More than 2,5 hours, up "] = 6
dat_t$PaperTime[dat_t$PaperTime == "More than 3 hours"] = 7
dat_t$PaperTime[dat_t$PaperTime == "Don't know"] = 88

dat_t$PoInt = as.character(dat_t$PoInt)
dat_t$PoInt[dat_t$PoInt == "Very interested"] = 1
dat_t$PoInt[dat_t$PoInt == "Quite interested"] = 2
dat_t$PoInt[dat_t$PoInt == "Hardly interested"] = 3
dat_t$PoInt[dat_t$PoInt == "Not at all interested"] = 4
dat_t$PoInt[dat_t$PoInt == "Don't know"] = 88

dat_t$VoteLast = as.character(dat_t$VoteLast)
dat_t$VoteLast[dat_t$VoteLast == "Yes"] = 1
dat_t$VoteLast[dat_t$VoteLast == "No"] = 2
dat_t$VoteLast[dat_t$VoteLast == "Not eligible to vote"] = 3
dat_t$VoteLast[dat_t$VoteLast == "Don't know"] = 88

dat_t$HapScale = as.character(dat_t$HapScale)
dat_t$HapScale[dat_t$HapScale == "Extremely unhappy"] = 0
dat_t$HapScale[dat_t$HapScale == "Extremely happy"] = 10
dat_t$HapScale[dat_t$HapScale == "Refusal"] = 99
dat_t$HapScale[dat_t$HapScale == "Don't know"] = 88

dat_t$HealthLvl = as.character(dat_t$HealthLvl)
dat_t$HealthLvl[dat_t$HealthLvl == "Very good"] = 1
dat_t$HealthLvl[dat_t$HealthLvl == "Good"] = 2
dat_t$HealthLvl[dat_t$HealthLvl == "Fair"] = 3
dat_t$HealthLvl[dat_t$HealthLvl == "Bad"] = 4
dat_t$HealthLvl[dat_t$HealthLvl == "Very bad"] = 5
dat_t$HealthLvl[dat_t$HealthLvl == "Don't know"] = 88

dat_t$SweBorn = as.character(dat_t$SweBorn)
dat_t$SweBorn[dat_t$SweBorn == "Yes"] = 1
dat_t$SweBorn[dat_t$SweBorn == "No"] = 2
dat_t$SweBorn[dat_t$SweBorn == "Don't know"] = 88

dat_t$FSweBorn = as.character(dat_t$FSweBorn)
dat_t$FSweBorn[dat_t$FSweBorn == "Yes"] = 1
dat_t$FSweBorn[dat_t$FSweBorn == "No"] = 2
dat_t$FSweBorn[dat_t$FSweBorn == "Don't know"] = 88

dat_t$MSweBorn = as.character(dat_t$MSweBorn)
dat_t$MSweBorn[dat_t$MSweBorn == "Yes"] = 1
dat_t$MSweBorn[dat_t$MSweBorn == "No"] = 2
dat_t$MSweBorn[dat_t$MSweBorn == "Don't know"] = 88

dat_t$Crime5year = as.character(dat_t$Crime5year)
dat_t$Crime5year[dat_t$Crime5year == "Yes"] = 1
dat_t$Crime5year[dat_t$Crime5year == "No"] = 2
dat_t$Crime5year[dat_t$Crime5year == "Don't know"] = 88

dat_t$Spouse3year = as.character(dat_t$Spouse3year)
dat_t$Spouse3year[dat_t$Spouse3year == "Yes"] = 1
dat_t$Spouse3year[dat_t$Spouse3year == "No"] = 2
dat_t$Spouse3year[dat_t$Spouse3year == "Don't know"] = 88

dat_t$Married = as.character(dat_t$Married)
dat_t$Married[dat_t$Married == "Yes"] = 1
dat_t$Married[dat_t$Married == "No"] = 2

dat_t$TotalChildren = as.character(dat_t$TotalChildren)
dat_t$TotalChildren[dat_t$TotalChildren == "Not applicable"] = 0
dat_t$TotalChildren[dat_t$TotalChildren == "1.000000"] = 1
dat_t$TotalChildren[dat_t$TotalChildren == "2.000000"] = 2
dat_t$TotalChildren[dat_t$TotalChildren == "3.000000"] = 3
dat_t$TotalChildren[dat_t$TotalChildren == "4.000000"] = 4
dat_t$TotalChildren[dat_t$TotalChildren == "5.000000"] = 5
dat_t$TotalChildren[dat_t$TotalChildren == "6.000000"] = 6
dat_t$TotalChildren[dat_t$TotalChildren == "7.000000"] = 7
dat_t$TotalChildren[dat_t$TotalChildren == "8.000000"] = 8
dat_t$TotalChildren[dat_t$TotalChildren == "9.000000"] = 9
dat_t$TotalChildren[dat_t$TotalChildren == "10.000000"] = 10
dat_t$TotalChildren[dat_t$TotalChildren == "11.000000"] = 11
dat_t$TotalChildren[dat_t$TotalChildren == "12.00000"] = 12

dat_t$OptLvl = as.character(dat_t$OptLvl)
dat_t$OptLvl[dat_t$OptLvl == "Agree strongly"] = 1
dat_t$OptLvl[dat_t$OptLvl == "Agree"] = 2
dat_t$OptLvl[dat_t$OptLvl == "Neither agree nor disagr"] = 3
dat_t$OptLvl[dat_t$OptLvl == "Disagree"] = 4
dat_t$OptLvl[dat_t$OptLvl == "Disagree strongly"] = 5
dat_t$OptLvl[dat_t$OptLvl == "Don't know"] = 88

dat_t$PosLvl = as.character(dat_t$PosLvl)
dat_t$PosLvl[dat_t$PosLvl == "Agree strongly"] = 1
dat_t$PosLvl[dat_t$PosLvl == "Agree"] = 2
dat_t$PosLvl[dat_t$PosLvl == "Neither agree nor disagr"] = 3
dat_t$PosLvl[dat_t$PosLvl == "Disagree"] = 4
dat_t$PosLvl[dat_t$PosLvl == "Disagree strongly"] = 5
dat_t$PosLvl[dat_t$PosLvl == "Don't know"] = 88

dat_t$SatLvl = as.character(dat_t$SatLvl)
dat_t$SatLvl[dat_t$SatLvl == "Agree strongly"] = 1
dat_t$SatLvl[dat_t$SatLvl == "Agree"] = 2
dat_t$SatLvl[dat_t$SatLvl == "Neither agree nor disagr"] = 3
dat_t$SatLvl[dat_t$SatLvl == "Disagree"] = 4
dat_t$SatLvl[dat_t$SatLvl == "Disagree strongly"] = 5
dat_t$SatLvl[dat_t$SatLvl == "Don't know"] = 88

dat_t$Gender = as.character(dat_t$Gender)
dat_t$Gender[dat_t$Gender == "Male"] = 1
dat_t$Gender[dat_t$Gender == "Female"] = 2

dat_t$FormUnemp = as.character(dat_t$FormUnemp)
dat_t$FormUnemp[dat_t$FormUnemp == "Yes"] = 1
dat_t$FormUnemp[dat_t$FormUnemp == "No"] = 2
dat_t$FormUnemp[dat_t$FormUnemp == "Don't know"] = 88

dat_t$TradeUn = as.character(dat_t$TradeUn)
dat_t$TradeUn[dat_t$TradeUn == "Yes, currently"] = 1
dat_t$TradeUn[dat_t$TradeUn == "Yes, previously"] = 2
dat_t$TradeUn[dat_t$TradeUn == "No"] = 3
dat_t$TradeUn[dat_t$TradeUn == "Don't know"] = 88

dat_t$IncLvl = as.character(dat_t$IncLvl)
dat_t$IncLvl[dat_t$IncLvl == "J"] = 1
dat_t$IncLvl[dat_t$IncLvl == "R"] = 2
dat_t$IncLvl[dat_t$IncLvl == "C"] = 3
dat_t$IncLvl[dat_t$IncLvl == "M"] = 4
dat_t$IncLvl[dat_t$IncLvl == "F"] = 5
dat_t$IncLvl[dat_t$IncLvl == "S"] = 6
dat_t$IncLvl[dat_t$IncLvl == "K"] = 7
dat_t$IncLvl[dat_t$IncLvl == "P"] = 8
dat_t$IncLvl[dat_t$IncLvl == "D"] = 9
dat_t$IncLvl[dat_t$IncLvl == "H"] = 10
dat_t$IncLvl[dat_t$IncLvl == "U"] = 11
dat_t$IncLvl[dat_t$IncLvl == "N"] = 12
dat_t$IncLvl[dat_t$IncLvl == "Refusal"] = 77
dat_t$IncLvl[dat_t$IncLvl == "Don't know"] = 88

dat_t$Area = as.character(dat_t$Area)
dat_t$Area[dat_t$Area == "A big city"] = 1
dat_t$Area[dat_t$Area == "Suburbs or outskirts of "] = 2
dat_t$Area[dat_t$Area == "Town or small city"] = 3
dat_t$Area[dat_t$Area == "Country village"] = 4
dat_t$Area[dat_t$Area == "Farm or home in countrys"] = 5
dat_t$Area[dat_t$Area == "Don't know"] = 88


## ----out.width="80%"----------------------------------------------------------------------------------------------
dat %>% 
  filter(VoteParty != "NOT APPLICABLE") %>% 
  filter(VoteParty != "REFUSAL") %>% 
  filter(VoteParty != "DONT KNOW") %>% 
  filter(VoteParty != "NO ANSWER") %>% 
  count(VoteParty) %>%
  mutate(VoteParty = fct_reorder(VoteParty, n, .desc = FALSE)) %>%
  ggplot() +
  geom_bar(aes(x = VoteParty, y = (n)/sum(n), fill = VoteParty), stat = 'identity') +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage") +
  guides(fill = "none") +
  coord_flip() +
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------

table = data.frame(VarName = c("TradeUn", "PoInt", "FamSize", "Area"),
                   Measure = c("Trade Union membership", "Political interest", "Size of family", "Urbanisation"),
                   ExpEffect = c("Neg.", "Neg.", "Pos.", "Pos."))

kable(table, caption = "Chosen variables", digits = 2)



## ----echo=FALSE---------------------------------------------------------------------------------------------------

dat_t$PoInt <- as.numeric(dat_t$PoInt)
dat_t$TradeUn <- as.numeric(dat_t$TradeUn)
dat_t$FamSize <- as.numeric(dat_t$FamSize)
dat_t$Area <- as.numeric(dat_t$Area)

m <- glm(as.factor(socialdemocrats) ~ PoInt+TradeUn+FamSize+Area, data = dat_t, 
         family = binomial(link='logit'))

# Getting log likelihood value (for test of likert restriction)


## -----------------------------------------------------------------------------------------------------------------
table2 = data.frame(Coefficients = c("Intercept", "PoInt", "TradeUn", "FamSize", "Area"),
                   Estimate = c(-0.50969, 0.17109, -0.53338 , -0.10878, 0.06648),
                   Significant = c("-", "Yes", "Yes", "Yes", "No"))

kable(table2, caption = "Logit Model", digits = 2)


## -----------------------------------------------------------------------------------------------------------------
preds <- predict.glm(m, dat_t, type="response")


## ----eval=FALSE---------------------------------------------------------------------------------------------------
## # Predict individ 18
## id18 = dat_t[18, ]
## # response returns predictions as probability (not log odds)
## pred1 <- predict.glm(m, id18, type="response")
## 
## # change a value of id15 to
## id18$PoInt = 2
## pred2 = predict.glm(m, id18, type="response")
## 
## # Difference in probability
## pred1-pred2


## ----eval=FALSE---------------------------------------------------------------------------------------------------
## # Predict individ 18
## id18 = dat_t[18, ]
## # response returns predictions as probability (not log odds)
## pred3 <- predict.glm(m, id18, type="response")
## 
## # change a value of id15 to
## id18$TradeUn = 2
## pred4 = predict.glm(m, id18, type="response")
## 
## # Difference in probability
## pred3-pred4


## ----eval=FALSE---------------------------------------------------------------------------------------------------
## # Predict individ 18
## id18 = dat_t[18, ]
## # response returns predictions as probability (not log odds)
## pred5 <- predict.glm(m, id18, type="response")
## 
## 
## # change a value of id18 to
## id18$FamSize = 5
## pred6 = predict.glm(m, id18, type="response")
## 
## # Difference in probability
## pred5-pred6


## -----------------------------------------------------------------------------------------------------------------
table3 = data.frame(Nr = c(1, 2, 3, 4, 5),PoInt = c(0.03394278, 0, -0.03646351, -0.0751499 , NA),
                   TradeUn = c(0, 0.09676355, 0.1668725, NA, NA),
                   FamSize = c(-0.04676795, -0.0229091 , 0, 0.02188785, 0.04270068 ))

kable(table3, caption = "Changes in individual #18", digits = 2)


## -----------------------------------------------------------------------------------------------------------------


m1 <- glm(as.factor(socialdemocrats) ~ as.factor(PoInt)+TradeUn+FamSize+Area, data = dat_t, 
         family = binomial(link='logit'))


lr = lrtest(m, m1)


## ----fig.cap="Chisquare Distribution", out.width="60%", fig.align="center"----------------------------------------
dist_chisq(p = 0.05, deg.f = 2)


## -----------------------------------------------------------------------------------------------------------------


m1 <- glm(as.factor(socialdemocrats) ~ PoInt+as.factor(TradeUn)+FamSize+Area, data = dat_t, 
         family = binomial(link='logit'))



lr = lrtest(m, m1)


## ----fig.cap="Chisquare Distribution", out.width="60%", fig.align="center"----------------------------------------
dist_chisq(p = 0.05, deg.f = 1)


## -----------------------------------------------------------------------------------------------------------------


m1 <- glm(as.factor(socialdemocrats) ~ PoInt+TradeUn+as.factor(FamSize)+Area, data = dat_t, 
         family = binomial(link='logit'))



lr = lrtest(m, m1)


## ----fig.cap="Chisquare Distribution", out.width="60%", fig.align="center"----------------------------------------
dist_chisq(p = 0.05, deg.f = 7)


## -----------------------------------------------------------------------------------------------------------------
m1 <- glm(as.factor(socialdemocrats) ~ PoInt+TradeUn+FamSize+as.factor(Area), data = dat_t, family = binomial(link='logit'))
m1_null <- glm(as.factor(socialdemocrats) ~ 1, data = dat_t, family = binomial(link='logit'))
R2_logit = 1 - logLik(m1)/logLik(m1_null)
lr = lrtest(m, m1)


## ----fig.cap="Chisquare Distribution", out.width="60%", fig.align="center"----------------------------------------
dist_chisq(p = 0.05, deg.f = 1)


## -----------------------------------------------------------------------------------------------------------------
preds <- predict.glm(m1, dat_t, type="response")
for(i in 1:length(preds))
{
  if (preds[i]<0.5)
  {
    preds[i]=0
  }
  else {preds[i]=1}
}

preds <- as.data.frame(preds)
pred_obs <- cbind(dat_t$socialdemocrats, preds)
pred_obs <- as.data.frame(pred_obs)
cm =confusionMatrix(as.factor(pred_obs$preds), as.factor(pred_obs$`dat_t$socialdemocrats`), positive = NULL, dnn = c("Prediction", "Observed"))


## -----------------------------------------------------------------------------------------------------------------
rownames(cm$table) = c("Pred 0", "Pred 1")
colnames(cm$table) = c("Obs. 0", "Obs. 1")
kable(cm$table, caption = "Predicted and observed values", digits = 2)


## ----fig.cap="Plot for Cut-off Point", out.width="60%", fig.align="center"----------------------------------------
preds <- predict.glm(m1, dat_t, type="response")
pred = prediction(preds, dat_t$socialdemocrats)
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, ylab="Specificity", xlab="Cutoff")
par(new=TRUE)
plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2))
mtext("Specificity",side=4, padj=-2, col='red')


## -----------------------------------------------------------------------------------------------------------------
preds <- predict.glm(m1, dat_t, type="response")
for(i in 1:length(preds))
{
  if (preds[i]<0.28)
  {
    preds[i]=0
  }
  else {preds[i]=1}
}

preds <- as.data.frame(preds)
pred_obs <- cbind(dat_t$socialdemocrats, preds)
pred_obs <- as.data.frame(pred_obs)
cm =confusionMatrix(as.factor(pred_obs$preds), as.factor(pred_obs$`dat_t$socialdemocrats`), positive = NULL, dnn = c("Prediction", "Observed"))


## -----------------------------------------------------------------------------------------------------------------
rownames(cm$table) = c("Pred 0", "Pred 1")
colnames(cm$table) = c("Obs. 0", " 1")
kable(cm$table, caption = "Predicted and observed values", digits = 2)


## -----------------------------------------------------------------------------------------------------------------
m <- glm(as.factor(socialdemocrats) ~ PoInt+TradeUn+FamSize+as.factor(Area), data = dat_t, 
         family = binomial(link='probit'))
m_null = glm(as.factor(socialdemocrats) ~ 1, data = dat_t, family = binomial(link='probit'))
R2_probit = 1 - logLik(m)/logLik(m_null)


## -----------------------------------------------------------------------------------------------------------------
table2 = data.frame(Coefficients = c("Intercept", "PoInt", "TradeUn", "FamSize", "Area"),
                   Estimate = c(-0.33873, 0.10292, -0.30996 , -0.06322, 0.04038),
                   Significant = c(NA, "Yes", "Yes", "Yes", "No"))

kable(table2, caption = "Probit Model", digits = 2)


## ----fig.cap="Plot for Cutoff Point", out.width="60%", fig.align="center"-----------------------------------------
preds <- predict.glm(m, dat_t, type="response")
pred = prediction(preds, dat_t$socialdemocrats)
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, ylab="Specificity", xlab="Cutoff")
par(new=TRUE)
plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2))
mtext("Specificity",side=4, padj=-2, col='red')


## -----------------------------------------------------------------------------------------------------------------
preds <- predict.glm(m, dat_t, type="response")
for(i in 1:length(preds))
{
  if (preds[i]<0.3)
  {
    preds[i]=0
  }
  else {preds[i]=1}
}

preds <- as.data.frame(preds)
pred_obs <- cbind(dat_t$socialdemocrats, preds)
pred_obs <- as.data.frame(pred_obs)


cm1 =confusionMatrix(as.factor(pred_obs$preds), as.factor(pred_obs$`dat_t$socialdemocrats`), positive = NULL, dnn = c("Prediction", "Observed"))


## -----------------------------------------------------------------------------------------------------------------
rownames(cm$table) = c("Pred 0", "Pred 1")
colnames(cm$table) = c("Obs. 0", "Obs. 1")
rownames(cm1$table) = c("Pred 0", "Pred 1")
colnames(cm1$table) = c("Obs. 0", "Obs. 1")
kable(cm$table, caption = "Predicted and observed values for Logit model", digits = 2)
kable(cm1$table, caption = "Predicted and observed values for Probit model", digits = 2)


## -----------------------------------------------------------------------------------------------------------------
table = pred_obs
names(table) = c("observed", "predicted")
kable(table[1:20,], caption = "Predicted outcome in the Probit model (Head 20)")


## -----------------------------------------------------------------------------------------------------------------
library(InformationValue)
optCutOff <- optimalCutoff(dat_t$socialdemocrats, preds)[1]


## -----------------------------------------------------------------------------------------------------------------
#purl("HW1.Rmd", output = "code1.R")

