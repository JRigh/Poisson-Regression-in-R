#---------------------------------------
# Poisson/Quasipoisson regression models
#---------------------------------------

# 1. Histogram of Poisson distributed observations

library(tidyverse)
set.seed(1986)
s1 <- data.frame('data' = rpois(n = 1000, lambda = 0.5))
s2 <- data.frame('data' = rpois(n = 1000, lambda = 2))
s3 <- data.frame('data' = rpois(n = 1000, lambda = 10))

p1 <- s1 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
               fill = 'firebrick3') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 0.5') +
  theme_minimal()
  scale_color_gradient(low="firebrick1", high="firebrick4")
p2 <- s2 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'firebrick3') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 2') +
  theme_minimal()
p3 <- s3 %>% ggplot() +
  geom_bar(aes(x = data, y = stat(count / sum(count))), width = 0.75,
           fill = 'firebrick3') +
  labs(x = 'y', y = 'proportion', title = lambda~ '= 10') +
  theme_minimal()

library(gridExtra)

grid.arrange(p1, p2, p3, nrow = 1)

# 2. Affairs dataset

data(Affairs, package = 'AER')
set.seed(1994)
data <- Affairs[sample(nrow(Affairs), size = 20, replace = FALSE),-c(8)]
head(data)
#      affairs gender age yearsmarried children religiousness education rating
# 295        0   male  32           10      yes             4        20      4
# 204        1   male  42           15      yes             4        16      5
# 1584       0 female  37           10      yes             4        16      5
# 1682       7 female  32           15      yes             5        18      4
# 1669       2 female  27            4       no             1        17      1
# 645        0 female  27           10      yes             4        16      3

dim(data)
# [1] 20  8
class(data)
# [1] "data.frame"

# 3. fit a Poisson regression model

# Poisson model
poisson.model <- glm(affairs ~ .,
                     family = 'poisson', data = data)
summary(poisson.model)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3392  -0.7669  -0.4425   0.1047   1.8788  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
# (Intercept)    0.04201    2.58877   0.016  0.98705   
# gendermale    -0.32727    0.60877  -0.538  0.59085   
# age           -0.04331    0.05139  -0.843  0.39929   
# yearsmarried   0.22417    0.11645   1.925  0.05423 . 
# childrenyes    0.94834    0.67143   1.412  0.15782   
# religiousness  0.68438    0.45728   1.497  0.13449   
# education     -0.01677    0.11092  -0.151  0.87984   
# rating        -1.17513    0.43671  -2.691  0.00713 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 102.924  on 19  degrees of freedom
# Residual deviance:  19.145  on 12  degrees of freedom
# AIC: 60.837
# 
# Number of Fisher Scoring iterations: 6

# p-value of Residual deviance goodness-of-fit test
1 - pchisq(deviance(poisson.model), df = poisson.model$df.residual)
# [1] 0.08507918

# Pearson's goodness-of-fit
Pearson <- sum((data$affairs - poisson.model$fitted.values)^2 
               / poisson.model$fitted.values)
1 - pchisq(Pearson, df = poisson.model$df.residual)
# [1] 0.1053663

# Checking mean = variance assumption
lambdahat <-fitted(poisson.model)

par(mfrow=c(1,2), pty="s")
plot(lambdahat,(data$affairs-lambdahat)^2,
     xlab=expression(hat(lambda)), ylab=expression((y-hat(lambda))^2 ))
plot(lambdahat, resid(poisson.model,type="pearson"), 
     xlab=expression(hat(lambda)), ylab="Pearson Residuals") 

# 4. Estimated dispersion parameter
Pearson / poisson.model$df.residual
# [1] 1.529472

# 5. Fit a Quasipoisson regression model
quasipoisson.model <- glm(affairs ~ .,
                          family = 'quasipoisson', data = data)
summary(quasipoisson.model)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    0.04201    3.20159   0.013   0.9897  
# gendermale    -0.32727    0.75287  -0.435   0.6715  
# age           -0.04331    0.06355  -0.682   0.5085  
# yearsmarried   0.22417    0.14402   1.557   0.1455  
# childrenyes    0.94834    0.83037   1.142   0.2757  
# religiousness  0.68438    0.56552   1.210   0.2495  
# education     -0.01677    0.13718  -0.122   0.9047  
# rating        -1.17513    0.54008  -2.176   0.0503 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for quasipoisson family taken to be 1.529477)
# 
# Null deviance: 102.924  on 19  degrees of freedom
# Residual deviance:  19.145  on 12  degrees of freedom
# AIC: NA
# 
# Number of Fisher Scoring iterations: 6


# p-value of Residual deviance goodness-of-fit test
1 - pchisq(deviance(quasipoisson.model), df = quasipoisson.model$df.residual)
# [1] 0.08507918

# Pearson's goodness-of-fit
Pearson <- sum((data$affairs - quasipoisson.model$fitted.values)^2 
               / quasipoisson.model$fitted.values)
1 - pchisq(Pearson, df = quasipoisson.model$df.residual)
# [1] 0.1053663

# 6. Variable selection using BIC
library(MASS)

stepAIC(poisson.model, direction = 'both', k = log(dim(data)[1]))
# ...
# Step:  AIC=61.42
# affairs ~ yearsmarried + children + religiousness + rating
# 
#                 Df Deviance    AIC
# <none>               20.753 61.423
# + age            1   19.461 63.128
# - children       1   25.501 63.176
# + gender         1   19.879 63.546
# + education      1   20.750 64.417
# - yearsmarried   1   32.187 69.862
# - religiousness  1   32.965 70.640
# - rating         1   57.142 94.817

# 7. Fit a second Quasipoisson model regression model (2)
quasipoisson.model.2 <- glm(affairs ~ children + yearsmarried + religiousness + rating,
                          family = 'quasipoisson', data = data)
summary(quasipoisson.model.2)


# 8. Perform leave-one-out crossvalidation (LOOCV)
pred.cv.mod_1 <- pred.cv.mod_2 <- numeric(dim(data)[1])
for(i in 1:dim(data)[1]) {
  mod_1 = glm(affairs ~ .,
              family = 'quasipoisson', data = data, subset = -i)
  mod_2 = glm(affairs ~ children + yearsmarried + religiousness + rating,
              family = 'quasipoisson', data = data, subset = -i)
  pred.cv.mod_1[i] = predict.glm(mod_1, data[i,], type = 'response' )
  pred.cv.mod_2[i] = predict.glm(mod_2, data[i,], type = 'response')
}

error.mod_1 = (1/dim(data)[1]) * sum((data$affairs - pred.cv.mod_1)^2) 
error.mod_2 = (1/dim(data)[1]) * sum((data$affairs - pred.cv.mod_2)^2) 

# Root Mean Squared Error (RMSE)
sqrt(c(error.mod_1, error.mod_2))
# [1] 59196.342   337.297

# 9. Diagnostic plots
par(mfrow = c(2,3))
plot(quasipoisson.model.2, which = 1:6)

# 10. Remove outlier
round(cooks.distance(quasipoisson.model.2)) # observation 1218 is atypical
data2 <- data[ - which.max(round(cooks.distance(quasipoisson.model.2))), ]

# 11. Final model
quasipoisson.model.3 = glm(affairs ~ children + yearsmarried + religiousness + rating,
              family = 'quasipoisson', data = data2, maxit = 100)
summary(quasipoisson.model.3)
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.9411  -0.4168  -0.2106   0.1702   1.4563  
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -0.14080    0.70522  -0.200 0.844619    
#   childrenyes   -2.74742    1.10230  -2.492 0.025841 *  
#   yearsmarried   0.30447    0.07101   4.288 0.000751 ***
#   religiousness  1.64490    0.39165   4.200 0.000891 ***
#   rating        -2.03423    0.39565  -5.141 0.000150 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for quasipoisson family taken to be 0.7485071)
# 
# Null deviance: 78.204  on 18  degrees of freedom
# Residual deviance: 11.233  on 14  degrees of freedom
# AIC: NA
# 
# Number of Fisher Scoring iterations: 6

# p-value of Residual deviance goodness-of-fit test
1 - pchisq(deviance(quasipoisson.model.3), df = quasipoisson.model.3$df.residual)
# [1] 0.667648

# Pearson's goodness-of-fit
Pearson <- sum((data2$affairs - quasipoisson.model.3$fitted.values)^2 
               / quasipoisson.model.3$fitted.values)
1 - pchisq(Pearson, df = quasipoisson.model.3$df.residual)
# [1] 0.7263845

# 12. Proportion of deviance explained by the model
round(1 - (quasipoisson.model.3$deviance / quasipoisson.model.3$null.deviance),4) 
# [1] 0.8564

# 13. Interpretation of the model
round(exp(quasipoisson.model.3$coefficients[2]),3)
round(exp(quasipoisson.model.3$coefficients[3]),3)
round(exp(quasipoisson.model.3$coefficients[4]),3)
round(exp(quasipoisson.model.3$coefficients[5]),3)

# The problems of overdispersion, covariate selection and influence of outliers have been addressed. Our final Quasipoisson model is a good fit for the data. About $86 \%$ of the deviance is explained by the model.
# The level of religiousness and the number of years of marriage seem to be positively related to the average number of affairs, whereas having children and a happy self rated marriage seem to be negatively related to the average number of affairs. Caution however since the dataset only contains 19 observations.
# If an individual has one child or more, the change in mean given all other covariates held constant is  $e^{-2.75} \approx 0.064$, hence a decrease of $93,6 \%$ of the average number of affairs in the past year.
# For one more year of marriage, the change in mean given all other covariates held constant is  $e^{0.304} \approx 1.36$, hence an increase of $36 \%$ of the average number of affairs in the past year.
# When the self rating of the marriage changes from unhappy to happy, the change in mean given all other covariates held constant is  $e^{-2.034} \approx 0.13$, hence a decrease of $87 \%$ of the average number of affairs in the past year.

#----
# end
#----


