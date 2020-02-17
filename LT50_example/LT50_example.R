install.packages("MASS")
library(MASS)

data=read.delim("ThermalToleranceData.txt")

summary(data)
head(data)

#GLM for HH
keepHH=c(1) #### change this line to a different Treatment number each time
HH_data=subset(data, Treat_Num %in% keepHH)
attach(HH_data)
y1=cbind(N_alive,N_dead)
model1=glm(y1~Temperature, family=binomial)
summary(model1)

#LT values for HH (LT10, LT25, and LT50)
LT_HH = dose.p(model1, p=c(0.9,0.75,0.5))
LT_HH
#              Dose         SE
# p = 0.90: 28.32748 0.12709530
# p = 0.75: 28.78999 0.09639024
# p = 0.50: 29.25250 0.08421410


# GLM for HL
keepHL=c(2) #### change this line to a different Treatment number each time
HL_data=subset(data, Treat_Num %in% keepHL)
attach(HL_data)
y2=cbind(N_alive,N_dead)
model2=glm(y2~Temperature, family=binomial)
summary(model2)

# LT values for HL (LT10, LT25, and LT50)
LT_HL = dose.p(model2,p=c(0.9,0.75,0.5))
LT_HL
#               Dose        SE
# p = 0.90: 28.63421 0.1213651
# p = 0.75: 29.03973 0.0912634
# p = 0.50: 29.44524 0.0765837


# GLM for LH 
keepLH=c(3) #### change this line to a different Treatment number each time
LH_data=subset(data, Treat_Num %in% keepLH)
attach(LH_data)
y3=cbind(N_alive,N_dead)
model3=glm(y3~Temperature, family=binomial)
summary(model3)

# LT values of LH (LT10, LT25, and LT50)
LT_LH = dose.p(model3,p=c(0.9,0.75,0.5))
LT_LH
#               Dose         SE
# p = 0.90: 28.05444 0.13945147
# p = 0.75: 28.55889 0.10215999
# p = 0.50: 29.06333 0.08676059


# GLM for LL 
keepLL=c(4) #### change this line to a different Treatment number each time
LL_data=subset(data, Treat_Num %in% keepLL)
attach(LL_data)
y4=cbind(N_alive,N_dead)
model4=glm(y4~Temperature, family=binomial)
summary(model4)

# LT values for LL (LT10, LT25, and LT50)
LT_LL = dose.p(model4,p=c(0.9,0.75,0.5))
LT_LL
#               Dose        SE
# p = 0.90: 27.68414 0.1730699
# p = 0.75: 28.38283 0.1266301
# p = 0.50: 29.08152 0.1060744


#------------------------
#LT50 values
HHdose = 29.25250
HHstd = 0.08421410
HLdose = 29.44524
HLstd = 0.0765837
LHdose = 29.06333
LHstd = 0.08676059
LLdose = 29.08152
LLstd = 0.1060744

# quick plot of the data
d = data.frame(
  x  = c(1:4)
  , y  = c(HHdose,HLdose,LHdose,LLdose)
  , sd = c(HHstd,HLstd,LHstd,LLstd))

install.packages("Hmisc", dependencies=T)
library("Hmisc")
plot(d$x, d$y)

with (
  data = d
  , expr = errbar(x, y, y+sd, y-sd, add=F, pch=1, cap=.015, col="blue", xlab = "Treatment")
)

#---------------------
# quick line plot of the data
data=read.delim("ThermalToleranceData.txt")

# Plot survivorship curves by temperature
library(ggplot2)
library(viridis)

#colorblind friendly colors
graph_colors <- c("#D55E00", "#993399", "#009E73","#0072B2")
survivaltrace <- ggplot(data=data, aes(x=Temperature, y=Survival, group=Treatment, colour=Treatment))+geom_line()+geom_point()+
  labs(x="Temperature (C)", y="Proportion survived") +
  scale_color_manual(values=graph_colors) +
  theme_classic()
survivaltrace


#--------------
# Plot a logistic curve over our data
attach(HH_data)
plot(Temperature,(N_alive/ (N_alive + N_dead)), xlab = "Temperature (C)",ylab = "Survivorship (%)", col = "#D55E00", pch = 20)
# plot a logistic curve over the raw data values
# define a function that will calculate y-values along a logistic curve
summary(model1) # intercept = 69.484, slope = -2.375 
logisticline_HH = function(z) {eta_HH = 69.484 + -2.375 * z; 1 / (1 + exp(-eta_HH))}
# create a vector of temperature values that will be used as input to our function 'logisticline_HH' to create the y-values to plot our cuve
# define object 'x' that is filled with a sequence of temperature limits (16.1-32.2) using a step size of 0.001
x = seq(16.1,32.2,0.001)
# add line to existing plot
lines(x,logisticline_HH(x), new = TRUE, col = "#D55E00")

attach(HL_data)
points(Temperature,(N_alive/ (N_alive + N_dead)), col = "#993399", pch = 20)
# plot a logistic curve over the raw data values
# define a function that will calculate y-values along a logistic curve
summary(model2) # intercept = 79.7723, slope = -2.7092 
logisticline_HL = function(z) {eta_HL = 79.7723 + -2.7092 * z; 1 / (1 + exp(-eta_HL))}
lines(x,logisticline_HL(x), new = TRUE, col = "#993399")

attach(LH_data)
points(Temperature,(N_alive/ (N_alive + N_dead)), col = "#009E73", pch = 20)
# plot a logistic curve over the raw data values
# define a function that will calculate y-values along a logistic curve
summary(model3) # intercept = 63.2961, slope = -2.1779 
logisticline_LH = function(z) {eta_LH = 63.2961 + -2.1779 * z; 1 / (1 + exp(-eta_LH))}
lines(x,logisticline_LH(x), new = TRUE, col = "#009E73")

attach(LL_data)
points(Temperature,(N_alive/ (N_alive + N_dead)), col = "#0072B2", pch = 20)
# plot a logistic curve over the raw data values
# define a function that will calculate y-values along a logistic curve
summary(model4) # intercept = 45.7275, slope = -1.5724
logisticline_LL = function(z) {eta_LL = 45.7275 + -1.5724 * z; 1 / (1 + exp(-eta_LL))}
lines(x,logisticline_LL(x), new = TRUE, col = "#0072B2")

