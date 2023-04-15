model <- gam(nbrtotc ~ s(ageph, bs = "cr"), 
             method = "REML", 
             family = gaussian, data = data)

model2 <- gam(nbrtotc ~ sexp + s(ageph) + fuelc + usec, 
              offset = log(duree), family=poisson(link="log"),
              data = data)

summary(model2)

plot(model2, select=1)
plot(model2, select=2)

# define risk profiles
drivers <- data.frame(duree = c(1,1,1),
                      sexp = c("Male","Female", "Female"),
                      fuelc = c("Gasoil","Gasoil","Petrol"),
                      ageph = c(18,45,65),
                      usec = c("Private","Private","Professional"))


predict(model2, newdata=drivers, type="response")


# fitting frequency

model3 <- gam(nbrtotc ~ sexp + agecar + fuelc + split + usec + fleetc + sportc + coverp + powerc + s(ageph) + ti(codposs), 
              offset = log(duree), family=poisson(link="log"),
              data = data)

summary(model3)

# best running model according to the article ?
model4 <- gam(nbrtotc ~ fuelc + coverp + powerc + s(ageph) + ti(codposs), 
              offset = log(duree), family=poisson(link="log"),
              data = data)

summary(model4)


# fitting severity

model5 <- gam(avg ~ 1 + s(ageph) + coverp, family=Gamma(link="log") ,data=data)

summary(model5)





