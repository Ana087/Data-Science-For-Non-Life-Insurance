# setting up
library(tidyverse)
install.packages("readxl")
library(readxl)

getwd()
setwd("C:/Users/aname/OneDrive/Documents/KU Leuven/Magisterij/SEMESTER 2/DSFNL/Assignment")

# uploading the dataset
Assignment = read.csv("Assignment.csv",sep=",",dec=".",head=TRUE)
original_data = as_tibble(Assignment)

# renaming the variables to lowercase
data <- original_data %>%
  # rename all columns 
  rename_all(function(.name) {
    .name %>% 
      # replace all names with the lowercase versions
      tolower 
  })

# adding a severity column, calculated by total_claim_amount/number_of_claims for each policyholder
avg = c(data$chargtot/data$nbrtotc)
data <- bind_cols(data,avg)
data <- rename(data, avg = ...17 )

# a frequency column named nbrtotan is already included in the data


# CALCULATING EMPIRICAL CLAIM FREQUENCY

mean(data$nbrtotc) # the mean of the number of claims reported

# empirical claim frequency
emp_freq = sum(data$nbrtotc)/sum(data$duree)
# empirical claim severity
emp_sev = sum(data$chargtot)/sum(data$nbrtotc)
# the pure premium
pure_premium = emp_freq * emp_sev

# CALCULATING EMPIRICAL VARIANCE
emp_var = sum((data$nbrtotc - emp_freq *data$duree)^2)/sum(data$duree)


# GRAPHICALLY SHOWING THE NUMBER OF CLAIMS
KULbg <- "#116E8A"
graph1 <- ggplot(data, aes(nbrtotc)) + theme_bw() + 
  geom_bar(col = KULbg, fill = KULbg) + 
  labs(x = "Number of claims", y = "Absolute frequency") +
  ggtitle("Number of claims")

graph1

graph2 <- ggplot(data, aes(nbrtotc)) + theme_bw() + 
  geom_bar(aes(weight = duree), col = KULbg, 
           fill = KULbg) + 
  labs(x = "Number of claims", y = "Absolute frequency (in exposure)") +
  ggtitle("Number of claims")

graph2

g <- ggplot(data, aes(nbrtotc)) + theme_bw()
graph3 <- g + geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), 
             col = KULbg, fill = KULbg) + 
  labs(x = "Number of claims",y = "Relative frequency") +
  ggtitle("Relative number of claims")

graph3


# ANALYSIS BY SEX
freq_by_gender <- data %>% 
  group_by(sexp) %>% 
  summarize(emp_freq = sum(nbrtotc) / sum(duree))

graph4 <- ggplot(freq_by_gender, aes(x = sexp,y=emp_freq)) + geom_bar(col = KULbg, fill = KULbg, alpha = .5,stat="identity") +
          ggtitle("Claim frequency based on gender") +
          labs(x = "Sex",y = "Empirical claim frequency")

graph4

analysis_by_gender <- data %>%
  group_by(sexp) %>%
  summarize(tot_claims = sum(nbrtotc),
            tot_expo = sum(duree), tot_obs = n())


# ANALYSIS BY AGE
analysis_by_age <- data %>%
  group_by(ageph) %>%
  summarize(tot_claims = sum(nbrtotc),
            tot_expo = sum(duree), tot_obs = n())

graph5 <- ggplot(data, aes(ageph)) + theme_bw() + 
  geom_histogram(binwidth = 2, col = KULbg, 
                 fill = KULbg, 
                 alpha = .5) +
  labs(x = "Age", y = "Absolute frequency") +
  ggtitle("Claims based on age")

graph5

# calculating the frequency by age
freq_by_age <- data %>% 
  group_by(ageph) %>% 
  summarize(emp_freq = sum(nbrtotc) / sum(duree))

graph6 <- ggplot(freq_by_age, aes(x = ageph, y = emp_freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", color = KULbg, 
           fill = KULbg, alpha = .5) +
  labs(x = "Age", y = "Empirical frequency") +
  ggtitle("Empirical claim frequency per 
                                age")

graph6


# FUNCTIONS FOR GRAPHS
col <- KULbg
fill <- KULbg
ylab <- "Relative frequency"

# wrapper functions
ggplot.bar <- function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))), col = col, fill = fill, alpha = 0.5) + labs(x = xlab, y = ylab)
}

ggplot.hist <- function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), binwidth = binwidth, col = col, fill = fill, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}

## -----------------------------------------------------------------------------------
# Constructing graphs of relative frequency
# age
plot.eda.ageph <- ggplot.hist(data, data$ageph, "Age", 2)
# exposure
plot.eda.exp <- ggplot.hist(data, data$duree, "Exposure", 0.05)
# number of claims
plot.eda.nclaims <- ggplot.bar(data, variable = data$nbrtotc, "Number of claims")
# severity
data.sev <- data %>% filter(nbrtotan > 0 & chargtot <= 81000) # see SAJ paper for motivation
plot.eda.amount <- ggplot(data = data.sev, aes(chargtot)) + geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) + xlim(0, 1e4) + ylab(ylab) + xlab("Severity") + theme_bw()
# car age
plot.eda.agec <- ggplot.bar(data, data$agecar, "Car age")
# sex of the policyholder
plot.eda.sex <- ggplot.bar(data, data$sexp, "Sex")
# type of fuel
plot.eda.fuel <- ggplot.bar(data, data$fuelc, "Fuel")
# premium split
plot.eda.premium <- ggplot.bar(data, data$split, "Premium split")
# use of car
plot.eda.use <- ggplot.bar(data, data$usec, "Use")
# belonging to a fleet
plot.eda.fleet <- ggplot.bar(data, data$fleetc, "Fleet")
# sport car
plot.eda.sport <- ggplot.bar(data, data$sportc, "Sport car")
# coverage
plot.eda.coverage <- ggplot.bar(data, data$coverp, "Coverage")
# power of the car
plot.eda.power <- ggplot.bar(data, data$powerc, "Power")



# Putting these together
library(gridExtra)
grid.arrange(plot.eda.ageph,plot.eda.exp,plot.eda.nclaims,plot.eda.amount,plot.eda.agec,plot.eda.sex,
             plot.eda.fuel,plot.eda.premium,plot.eda.use,plot.eda.fleet,plot.eda.sport,plot.eda.coverage,plot.eda.power,ncol = 3)



# uploading the postal code information data
inspost = read_excel("inspost.xls")
post_data = as_tibble(inspost)

#ANALYSIS BY POSTAL CODE
analysis_by_post <- data %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(duree))


coord <- post_data[,3:5]
codes <- data["codposs"]

lat = rep(0,dim(data)[1])
long = rep(0,dim(data)[1])
data <- bind_cols(data,lat,long)
data <- rename(data, lat = ...18 )
data <- rename(data, long = ...19 )

for (i in 1:(dim(data)[1])) {
  for (j in 1:(dim(coord)[1])) {
    if (codes[i,1] == coord[j,3]) {
    data[i,18:19] = coord[j,1:2]
    }
  }
}


 