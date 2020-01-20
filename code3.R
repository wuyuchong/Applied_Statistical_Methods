## ----setup, include=FALSE, message=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(fig.pos = 'H', echo = FALSE, warning = FALSE, comment = "")
library(knitr)
library(ggplot2)
library(tidyverse)
library(caret)
library(fastDummies)
library(car)
options(knitr.kable.NA = '')
dat = read.csv("b4 - hwa - wage - data.csv")


## --------------------------------------------------------------------------------------------------------
names(dat) = c("birth_year", "gender", "citizenship", "language", "lived_with", "father_schooling", "father_occupation", "mother_schooling", "mother_occupation", "live", "county", "education", "marital", "experience", "category", "wage")


## --------------------------------------------------------------------------------------------------------
dat[is.na(dat)] = 0
means = c()
variances = c()
mins = c()
maxs = c()
for(i in 1:length(colnames(dat))){
   means[i] = mean(dat[[colnames(dat)[i]]])
   variances[i] = var(dat[[colnames(dat)[i]]])
   mins[i] = min(dat[[colnames(dat)[i]]])
   maxs[i] = max(dat[[colnames(dat)[i]]])
}

stats = data.frame(colnames(dat), means, variances, mins, maxs)
kable(stats, caption = "Preliminary Analysis for the variables", digits = 2)


## --------------------------------------------------------------------------------------------------------
dat = dat %>% 
   mutate(log_wage = log(wage)) %>% 
   mutate(experience_sq = experience^2)
m1 = lm(log(wage) ~ education + experience + experience_sq, data = dat)
sum = summary(m1)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the regression model", digits = 2)


## ----out.width="70%", fig.align="center", fig.cap="Residuals Plot for the regression model"--------------
data.frame(index = 1:length(m1$residuals), residuals = m1$residuals) %>% 
   ggplot(aes(x = index, y = residuals)) +
   geom_point(color = "red", alpha = 0.5) +
   theme_bw()


## --------------------------------------------------------------------------------------------------------
bptest = lmtest::bptest(m1)
table = data.frame(BP = bptest$statistic, df = bptest$parameter, p_value = bptest$p.value)
rownames(table) = "bptest"
kable(table, caption = "Brausch-Pagan test", digits = 2)


## ----out.width="50%", fig.align="center", fig.cap="QQ-plot for the model before Box-Cox transformation"----
qqnorm(m1$residuals)
qqline(m1$residuals, col = "red")


## --------------------------------------------------------------------------------------------------------
BClog_wage <- BoxCoxTrans(dat$log_wage)
dat <- cbind(dat, log_wage_BC = predict(BClog_wage, dat$log_wage))
m2 <- lm(dat$log_wage_BC ~ dat$education + dat$experience + dat$experience_sq)


## --------------------------------------------------------------------------------------------------------
bptest = lmtest::bptest(m2)
table = data.frame(BP = bptest$statistic, df = bptest$parameter, p_value = bptest$p.value)
rownames(table) = "bptest"
kable(table, caption = "Brausch-Pagan test for the model after Box-Cox transformation", digits = 2)


## ----out.width="50%", fig.align="center", fig.cap="QQ-plot for the model after Box-Cox transformation"----
qqnorm(m2$residuals)
qqline(m2$residuals, col = "red")


## --------------------------------------------------------------------------------------------------------
sum = summary(m2)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model after Box-Cox transformation", digits = 2)


## ----out.width="70%", fig.align="center", fig.cap="Difference in Wages between men and women"------------
dat_gender = dat
for(i in 1:nrow(dat_gender))
{
   if(dat_gender$gender[i] == 1)
   {
      dat_gender$gender[i] = "Male"
   }
   if(dat_gender$gender[i] == 2)
   {
      dat_gender$gender[i] = "Female"
   }
   if(dat_gender$wage[i] > 200)
   {
      dat_gender$wage[i] = 0
   }
}
dat_gender %>% 
   ggplot(aes(x = gender, fill = gender, y = wage)) +
   geom_boxplot()


## --------------------------------------------------------------------------------------------------------
m = aggregate(wage ~ gender, dat, mean)
m$gender = c("Male", "Female")
kable(m, caption = "Comparison in mean of wages between male and female workers", digits = 2)
difference = m$wage[1]-m$wage[2]


## --------------------------------------------------------------------------------------------------------
# Shapiro-Wilk normality test for Men's wage
shapiro = with(dat_gender, shapiro.test(wage[gender == "Male"]))
table = data.frame(method = shapiro$method, statistic = shapiro$statistic, p_value = shapiro$p.value)
rownames(table) = "Male"
kable(table, caption = "Shapiro-Wilk normality test for Men's wage", digits = 2)

# Shapiro-Wilk normality test for Women's wage
shapiro = with(dat_gender, shapiro.test(wage[gender == "Female"]))
table = data.frame(method = shapiro$method, statistic = shapiro$statistic, p_value = shapiro$p.value)
rownames(table) = "Female"
kable(table, caption = "Shapiro-Wilk normality test for woman's wage", digits = 2)


## --------------------------------------------------------------------------------------------------------
wilcox = wilcox.test(wage ~ gender, data = dat_gender, exact = FALSE)
table = data.frame(method = wilcox$method, statistic = shapiro$statistic, p_value = shapiro$p.value)
rownames(table) = NULL
kable(table, caption = "Non parametric two-samples Wilcoxon rank test for woman's wage", digits = 3)


## --------------------------------------------------------------------------------------------------------
dat <- fastDummies::dummy_cols(dat, select_columns = "gender")
m3 <- lm(wage ~ gender_1, dat)
sum = summary(m3)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model including dummy variable", digits = 2)


## --------------------------------------------------------------------------------------------------------
table = data.frame(Method = c("Difference of Means","Regression"),
                   Result = c("9.97", "9.97"),
                   Test = c("Non parametric two-samples Wilcoxon rank test", "T-test"),
                   Conclusion = c("significant", "significant"))


# Then for PDF:
kable(table, caption = "Wage difference between male and female", digits = 2)


## --------------------------------------------------------------------------------------------------------
m4 <- lm(wage ~ gender_1 + education, dat)
sum = summary(m4)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model containing education variable", digits = 2)


## ----out.width="40%", fig.align="center", fig.cap="F distribution"---------------------------------------
x = seq(0, 5, length = 100)
distribution = df(x = x, df1 = 1, df2 = 1715)
plot(x, distribution,type="l")


## --------------------------------------------------------------------------------------------------------
m5 <- lm(wage ~ gender_1 + education + experience, dat)
sum = summary(m5)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model containing variable education and experience", digits = 2)


## ----out.width="40%", fig.align="center", fig.cap="F distribution"---------------------------------------
x = seq(0, 5, length = 100)
distribution = df(x = x, df1 = 1, df2 = 1715)
plot(x, distribution,type="l")


## --------------------------------------------------------------------------------------------------------
dat_m = subset(dat, gender == "1")
dat_f = subset(dat, gender == "2")

model_m = lm(log_wage ~ education + experience, dat_m)
model_f = lm(log_wage ~ education + experience, dat_f)

difference = ((model_m$coefficients[1]-model_f$coefficients[1])+(model_m$coefficients[2]-model_f$coefficients[2])*mean(dat_f$education)+(model_m$coefficients[3]-model_f$coefficients[3])*mean(dat_f$experience))/((model_m$coefficients[1]-model_f$coefficients[1])+(model_m$coefficients[2]-model_f$coefficients[2])*mean(dat_f$education)+(model_m$coefficients[3]-model_f$coefficients[3])*mean(dat_f$experience)+model_m$coefficients[2]*(mean(dat_m$education)-mean(dat_f$education))+model_m$coefficients[3]*(mean(dat_m$experience)-mean(dat_f$experience)))


## --------------------------------------------------------------------------------------------------------
fit1 <- lm(education ~ mother_schooling + father_schooling + live + father_occupation + mother_occupation + lived_with, dat)
sum = summary(fit1)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model: education ~ IV", digits = 3)


## --------------------------------------------------------------------------------------------------------
fit2 <- lm(m2$residuals ~ mother_schooling + father_schooling + live + mother_occupation + lived_with, dat)
sum = summary(fit2)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model: residuals ~ IV", digits = 3)


## --------------------------------------------------------------------------------------------------------
sls <- lm(education ~ mother_schooling + father_schooling + lived_with, dat)
sum = summary(sls)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the model: education ~ IV", digits = 3)
IV = fitted.values(sls)
dat = cbind(dat, IV)


## --------------------------------------------------------------------------------------------------------
tsls2 = lm(wage ~ gender_1 + IV, dat)
sum = summary(tsls2)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the Two-Stage least squares model containing education variable", digits = 3)


## ----out.width="40%", fig.align="center", fig.cap="F distribution"---------------------------------------
x = seq(0, 5, length = 100)
plot(x, df(x = x, df1 = 1, df2 = 1715),type="l")


## --------------------------------------------------------------------------------------------------------
tsls3 = lm(wage ~ gender_1 + IV + experience, dat)
sum = summary(tsls3)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the Two-Stage least squares model containing education variable", digits = 3)


## ----out.width="40%", fig.align="center", fig.cap="F distribution"---------------------------------------
x = seq(0, 5, length = 100)
plot(x, df(x = x, df1 = 1, df2 = 1715),type="l")


## --------------------------------------------------------------------------------------------------------
sls_m = lm(education ~ mother_schooling + father_schooling + lived_with, dat_m)
sls_f = lm(education ~ mother_schooling + father_schooling + lived_with, dat_f)

IV_m = fitted.values(sls_m)
IV_f = fitted.values(sls_f)

dat_m = cbind(dat_m, IV_m)
dat_f = cbind(dat_f, IV_f)

model_m = lm(log_wage_BC ~ IV_m + experience + experience_sq, dat_m)
model_f = lm(log_wage_BC ~ IV_f + experience + experience_sq, dat_f)

difference = ((model_m$coefficients[1]-model_f$coefficients[1])+(model_m$coefficients[2]-model_f$coefficients[2])*mean(IV_f)+(model_m$coefficients[3]-model_f$coefficients[3])*mean(dat_f$experience))/((model_m$coefficients[1]-model_f$coefficients[1])+(model_m$coefficients[2]-model_f$coefficients[2])*mean(IV_f)+(model_m$coefficients[3]-model_f$coefficients[3])*mean(dat_f$experience)+model_m$coefficients[2]*(mean(IV_m)-mean(IV_f))+model_m$coefficients[3]*(mean(dat_m$experience)-mean(dat_f$experience)))


## --------------------------------------------------------------------------------------------------------
table_a = data.frame(Model = c("OLS","2SLS", "Difference"),
                  Gender = c("8.95", "10.01", "1.06"),
                  Education = c("1.84", "1.01", "-0.83"))

table_b = data.frame(Model = c("OLS","2SLS", "Difference"),
                  Gender = c("5.61", "7.96", "2.35"),
                  Education = c("2.39", "1.94", "-0.45"),
                  Experience = c("0.53", "0.36", "-0.16"))

table_c = data.frame(Model = c("OLS","2SLS", "Difference"),
                  Log_wage = c("0.44", "0.47", "0.03"),
                  Wage = c("1.19", "1.27", "0.08"))


## --------------------------------------------------------------------------------------------------------
kable(table_a, caption = "Table for comparison, task 4a and 6a")


## --------------------------------------------------------------------------------------------------------
kable(table_b, caption = "Table for comparison, task 4b and 6b")


## --------------------------------------------------------------------------------------------------------
kable(table_c, caption = "Table for comparison, task 4c and 6c")


## --------------------------------------------------------------------------------------------------------
mx = lm(log_wage ~ IV:gender + experience:gender, data = dat_gender)
sum = summary(mx)
sum$coefficients %>% 
   kable(caption = "Regression Coefficients for the Two-Stage least squares model grouped by gender", digits = 3)


## --------------------------------------------------------------------------------------------------------
test = linearHypothesis(mx, "IV:genderFemale=IV:genderMale")
as.data.frame(test) %>% 
   kable(caption = "Linear hypothesis test for education", digits = 2)


## --------------------------------------------------------------------------------------------------------
test = linearHypothesis(mx, "genderFemale:experience=genderMale:experience")
as.data.frame(test) %>% 
   kable(caption = "Linear hypothesis test for experience", digits = 2)


## --------------------------------------------------------------------------------------------------------
summary(m1)


## --------------------------------------------------------------------------------------------------------
summary(m2)


## --------------------------------------------------------------------------------------------------------
summary(m3)


## --------------------------------------------------------------------------------------------------------
summary(m4)


## --------------------------------------------------------------------------------------------------------
summary(m5)


## --------------------------------------------------------------------------------------------------------
linearHypothesis(mx, "IV:genderFemale=IV:genderMale")


## --------------------------------------------------------------------------------------------------------
linearHypothesis(mx, "genderFemale:experience=genderMale:experience")

