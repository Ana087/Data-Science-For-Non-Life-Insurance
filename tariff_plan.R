library(tidyverse)
library(gridExtra)
library(mgcv)

# fitting a Poisson GLM using a logarithmic link function
freq_glm_1 <- glm(nbrtotc ~ sexp, offset = log(duree), 
                  family = poisson(link = "log"), 
                  data = data)

summary(freq_glm_1)


# creating new data frames
male_driver <- data.frame(duree = 1, sexp = "Male")
female_driver <- data.frame(duree = 1, sexp = "Female")

# predicting
predict(freq_glm_1, newdata = male_driver, 
        type = "response")

predict(freq_glm_1, newdata = male_driver, 
        type = "link")

# fitting a glm
freq_glm <- glm(nbrtotc ~ ageph + sexp + usec + sportc,offset = log(duree), 
                family = poisson(link = "log"), 
                data = data)

summary(freq_glm)

freq_glm$coef

plot(freq_glm$fitted.values, data$nbrtotc, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(freq_glm$fitted ~ data$nbrtotc), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

g1 <- glm(nbrtotc ~ 1 + ageph + sexp + usec + sportc, poisson, offset = log(duree),data=data)
anova(g1, test="Chisq")
