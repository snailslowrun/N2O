library(tidyverse)


#denitrification

# NO3
FdNO3_SWAT<- "SWAT outputs"

##daycent
FdNO3_daycent <- function(NO3){
  fdno3= 11000 + (40000* atan(pi*0.002*(NO3-180)))/pi
  return(fdno3)
}

NO3<- runif(200, 0, 350)
Daycent<-FdNO3_daycent(NO3)

a<-ggplot()+
  geom_line(aes(x=NO3, y=Daycent))+
  labs(x="NO3 (ug N g-1)", y="Fd(NO3) (g N ha-1 d-1)", title = "NO3 (Daycent)")+
  theme(#axis.text.x = element_text(size = 11),
        #axis.text.y = element_text(size = 11),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))))

##DNDC
FdNO3_DNDC <- "No3 concentration in soil water (kg N/ha)"







#orgnic C
FdC_SWAT<- "a parameter in SWAT"


##daycent
FdC_daycent <- function(carbon){
  fdc= 24000/(1 + 200/(exp(0.35 * carbon))) - 100
  return(fdc)
}

carbon<-runif(200,0, 40)
Daycentc<-FdC_daycent(carbon)

b<-ggplot()+
  geom_line(aes(x=carbon, y=Daycentc))+
  labs(x="Soil Respiration (kg C ha-1 d-1)", y="Fd(CO2) (g N ha-1 d-1)", title = "Soil Respiration (Daycent)")+
  theme(#axis.text.x = element_text(size = 11),
        #axis.text.y = element_text(size = 11),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))))

##DNDC
FdC_DNDC <- "dissolved organic carbon content (kg C/ha)"












# temperature
soilT<- runif(100, 0, 65)

##SWAT
FdT_SWAT <- function(SoilT){
  fdt = 0.9 * SoilT / (SoilT + exp(9.93- 0.312 * SoilT)) + 0.1
  return(fdt)
}

SWAT<-FdT_SWAT(soilT)


##daycent
FdT_daycent <- "Daycent does not consider the impact of temperature on denitrification"


##DNDC
FdT_DNDC <- function(soilT){
  if (soilT<= 60) {
    fdt = 2 ^ ((soilT - 22.5)/10)
  }
  else if (soilT > 60) {
    fdt = 0
  }
  return(fdt)
}

DNDC<-vector()
for(i in 1:length(soilT)){
  DNDC[i]=FdT_DNDC(soilT[i])
}


summa<-as.tibble(cbind(soilT, SWAT, DNDC))
summa_gather<-gather(summa, key = "model", value = "Fd(t)", -soilT)

c<-ggplot()+
  geom_line(aes(x=summa_gather$soilT, y=summa_gather$`Fd(t)`, linetype=as.factor(summa_gather$model)))+
  scale_y_continuous(breaks = 1:15)+
  labs(x="Soil Temperature (°C)", y="Fd(t)", linetype="Model", title = "Soil Temperature")+
  theme(#axis.text.x = element_text(size = 11),
        #axis.text.y = element_text(size = 11),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        legend.background = element_blank(),
        legend.position = c(0.2,0.8),
        legend.key = element_blank())








#soil water
WFPS_SWAT <- SW/FC

##daycent
FdWFPS_daycent <- function(a,b,c,d,WFPS){
  fdwfps = a / (b ^ (c / b ^ (d * WFPS)))
  return(fdwfps)
}

sandya<-1.56
sandyb<-12.0
sandyc<-16.0
sandyd<-2.01

mediuma<-4.82
mediumb<-14.0
mediumc<-16.0
mediumd<-1.39

finea<-60
fineb<-18.0
finec<-22.0
fined<-1.06

WFPS<-runif(200, 0.3,0.9)

daycentsandy<-FdWFPS_daycent(sandya,sandyb,sandyc,sandyd, WFPS)
daycentmedium<-FdWFPS_daycent(mediuma, mediumb, mediumc, mediumd, WFPS)
daycentfine<-FdWFPS_daycent(finea, fineb, finec, fined, WFPS)

zusamm<-as.tibble(cbind(WFPS, daycentsandy, daycentmedium, daycentfine))
colnames(zusamm)<-c("WFPS", "sandy","medium","fine")
zusamm_gather<-gather(zusamm, key = "soil texture", value = "value", -WFPS)


d<-ggplot()+
  geom_line(aes(x=zusamm_gather$WFPS, y=zusamm_gather$value, col=as.factor(zusamm_gather$`soil texture`),
                linetype=as.factor(zusamm_gather$`soil texture`)),size=0.8)+
  labs(x="WFPS", y="Fd(WFPS)", col="Soil texture", title = "WFPS (Daycent)",linetype="Soil texture")+
  theme(#axis.text.x = element_text(size = 11),
        #axis.text.y = element_text(size = 11),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        #legend.background = element_blank(),
        legend.position = c(0.2,0.75),
        legend.key = element_blank())


##DNDC
FdWFPS_DNDC <- "in DNDC soil moisture is considered as Eh"








#soil pH
#SWAT
FdpH_SWAT <- "SWAT cannot capture the changes of soil pH"


#Daycent
FdpH_daycent <- "Daycent does not consider the effect if soil pH on denitrification"


#DNDC
FdpH_no3 <- 1 - 1/(1 + exp((soilpH - 4.25)/0.5))


FdpH_nox <- 1 - 1/ (1 + exp((soilpH - 5.25)/1))


FdpH_n2o <- 1 - 1/(1 + exp((soilpH - 6.25)/1.5))






# Denitrificaiton together
library(gridExtra)
grid.arrange(a, b, c, d, ncol=2, name=c("a","b","c","d"))

install.packages("cowplot")
library(cowplot)
cowplot::plot_grid(a, b, c, d, ncol = 2, labels = "AUTO")




















#nitrification

#NH4
FnNH4_SWAT <-"SWAT uses the amount of ammonium in soil layer, nitrification regulator and volatilization regulator
to calculate the total amount of nitrification and ammonia volatilization. And then partitioned between the two processes"

##daycent
FnNH4_daycent<-function(NH4){
  fnnh4=1 - exp(-0.0105*NH4)
  return(fnnh4)
}

NH4<-runif(200, 0, 50)
daycentnh4<-FnNH4_daycent(NH4)

na<-ggplot()+
  geom_line(aes(x=NH4, y=daycentnh4))+
  labs(x="NH4 (ug g-1)", y="Fn(NH4)", title = "NH4 (Daycent)")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))))

##DNDC
FnNH4_DNDC<-"NH4+ is concentration of ammonium in soil (kg N/ha)"







#organic C
orgC_SWAT<- "this is a parameter in SWAT"

orgC_daycent<-"furtherly check"


orgC_DNDC<-"dissolved organic carbon content (kg C/ha)"








#temperature
soilT<-runif(200, 5, 60)

FnT_SWAT <- function(SoilT){
  fnt = 0.041 * (SoilT -5)
  return(fnt)
}

SWATt<-FnT_SWAT(soilT)


FnT_daycent <- function(soilT){
  fnt = -0.06 + 0.13 * exp(0.07 * soilT)
  return(fnt)
}

daycentt<-FnT_daycent(soilT)


FnT_DNDC <- function(soilT){
  fnt=3.503 ^ ((60-soilT)/25.78) * exp(3.503 * (soilT - 34.22)/25.78)
  return(fnt)
}

dndct<-FnT_DNDC(soilT)


together<-as.tibble(cbind(soilT, SWATt, daycentt, dndct))
colnames(together)<- c("soilT", "SWAT", "Daycent", "DNDC")
together_gather<-gather(together, key = "model", value = "value", -soilT)

nb<-ggplot()+
  geom_line(aes(x=together_gather$soilT, y=together_gather$value, linetype=as.factor(together_gather$model),
                col=as.factor(together_gather$model)), size=0.8)+
  labs(x="Soil Temperature (°C)", y="Fn(t)", linetype="Model", title = "Soil Temperature", col="Model")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        legend.position = c(0.2,0.75),
        legend.key = element_blank())





#soil pH
FnpH_SWAT <- "SWAT cannot account for changes in pH"


SpH<- runif(100, 0, 9)
#daycent
FnpH_daycent <- function(SpH){
  fnph = 0.56 + atan(pi * 0.45 * (-5 + SpH)) / pi
  return(fnph)
}

phdaycent<-FnpH_daycent(SpH)

##DNDC
FnpH_DNDC <- SpH    #soil pH


phdndc<-SpH

phsumma<-as.tibble(cbind(SpH, phdaycent, phdndc))
colnames(phsumma)<-c("pH", "Daycent", "DNDC")
phsumma_gather<-gather(phsumma, key = "Model", value = "value", -pH)


nc<-ggplot()+
  geom_line(aes(x=phsumma_gather$pH, y=phsumma_gather$value, linetype=as.factor(phsumma_gather$Model)))+
  labs(x="soil pH", y="Fn(pH)", linetype="Model", title = "Soil pH")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        legend.position = c(0.2, 0.8),
        legend.key = element_blank())







#soil WFPS
FnWFPS_SWAT <- function(SWLY, WPLY, FCLY){
  if (SWLY < 0.25 * FCLY - 0.75 * WPLY){
    FO<- (SWLY - WPLY) / (0.25 * (FCLY - WPLY))
  }
  else if(SWLY > 0.25 * FCLY - 0.75 * WPLY) {
    FO<- 1
  }
  return(FO)
}



##daycent
FnWFPS_daycent <- function(a, b,c,d, WFPS){
  fnwfps= ((WFPS-b)/(a-b)) ^ (d * (b-a)/(a-c)) * ((WFPS-c)/(a-c)) ^ d
  return(fnwfps)
}

sandyna<-0.55
sandynb<-1.70
sandync<--0.007
sandynd<-3.22


mediumna<-0.6
mediumnb<-1.27
mediumnc<-0.0012
mediumnd<-2.84

nWFPS<-runif(200, 0, 1)

sandydaycent<-FnWFPS_daycent(sandyna, sandynb, sandync, sandynd, nWFPS)
meidumdaycent<-FnWFPS_daycent(mediumna, mediumnb, mediumnc, mediumnd, nWFPS)




##DNDC
FnWFPS_DNDC <- function(WFPS){
  if (WFPS > 0.05){
    fnwfps= 0.8 + 0.21 * (1 - WFPS)
  }
  else if (WFPS<= 0.05){
    fnwfps=0
  }
  return(fnwfps)
}


ndndc<-vector()
for(i in 1: length(nWFPS)){
  ndndc[i]<-FnWFPS_DNDC(nWFPS[i])
}



ndzusamm<-as.tibble(cbind(nWFPS, sandydaycent, meidumdaycent, ndndc))
colnames(ndzusamm)<-c("WFPS", "Daycent(sandy)","Daycent(medium)", "DNDC")
ndzusamm_gather<-gather(ndzusamm, key = "soil texture", value = "value", -WFPS)




nd<-ggplot()+
  geom_line(aes(x=ndzusamm_gather$WFPS, y=ndzusamm_gather$value, linetype=as.factor(ndzusamm_gather$`soil texture`),
                col=as.factor(ndzusamm_gather$`soil texture`)),size=0.8)+
  labs(x="WFPS", y="Fn(WFPS)", title = "WFPS", linetype="Model", col="Model")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        legend.position = c(0.60, 0.31),
        legend.text = element_text(size =7 ),
        legend.title = element_text(size = 9),
        legend.key = element_blank(),
        legend.background = element_blank())



# nitrificaiton together
library(cowplot)
cowplot::plot_grid(na, nb, nc, nd, ncol = 2, labels = "AUTO")














#partition N2O from N2

#NO3
RNO3_daycent<- function(NO3){
  frno3<- (1-(0.5 + (1*atan(pi * 0.01 * (NO3 - 190)))/ pi)) * 25
  return(frno3)
}


NO3<-runif(200, 0, 350)
Rdaycent<-RNO3_daycent(NO3)

ra<-ggplot()+
  geom_line(aes(x=NO3, y= Rdaycent))+
  labs(x="NO3 (ug N g-1)", y= "Fr(NO3)", title = "NO3 (Daycent)")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))))
  








# soil respiration
RC_daycent<-function(CO2){
  frc<- 13 + (30.78 * atan(pi * 0.07 * (CO2 - 13)))/pi
  return(frc)
}


co2<- runif(200, 0, 35)
Cdaycent<-RC_daycent(co2)


rb<-ggplot()+
  geom_line(aes(x= co2, y=Cdaycent))+
  labs(x=" Soil Respiration (kg C ha-1 d-1)", y=" Fr(CO2)", title = "Soil Respiration (Daycent)")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))))








#WFPS
Rwfps_daycent<- function(WFPS){
  frwfps<- 1.4/ 13^ (17 / 13^ (2.2 * WFPS))
  return(frwfps)
}



wfps<-runif(200, 0, 1)
sdaycent<-Rwfps_daycent(wfps)


rc<-ggplot()+
  geom_line(aes(x=wfps, y=sdaycent))+
  labs(x="WFPS", y="Fr(WFPS)", title = "WFPS (Daycent)")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))))







#pH
RNO3_DNDC<-function(SpH){
  fphno3<-1 - 1/ (1 + exp( (SpH - 4.25)/ 0.5))
  return(fphno3)
}



RNO_DNDC<- function(SpH) {
  fphno <- 1 - 1 / (1 + exp((SpH - 5.25) / 1))
  return(fphno)
}




Rn2O_DNDC<- function(SpH) {
  fphn2o<- 1 - 1/ (1 + exp( (SpH - 6.25) / 1.5))
  return(fphn2o)
}



SpH<-runif(200, 0, 9)
phno3<-RNO3_DNDC(SpH)
phno<-RNO_DNDC(SpH)
phn2o<-Rn2O_DNDC(SpH)


ph<-as.tibble(cbind(SpH, phno3, phno,phn2o))
colnames(ph)<-c("SoipH", "NO3", "NO", "N2O")
ph_gather<-gather(ph, key = "nitrogenous oxides", value = "value", -SoipH)



rd<-ggplot()+
  geom_line(aes(x=ph_gather$SoipH, y=ph_gather$value, linetype=as.factor(ph_gather$`nitrogenous oxides`),
                col=as.factor(ph_gather$`nitrogenous oxides`)), size=0.8)+
  labs(x= "soil pH", y="Fr(pH)", title = "Soil pH (DNDC)", linetype="Nitrogenous oixdes", col="Nitrogenous oixdes")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        axis.line.y = element_line(arrow = arrow(length = unit(0.1, "inches"))),
        legend.position = c(0.25, 0.75),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key = element_blank(),
        legend.background = element_blank())





cowplot::plot_grid(ra, rb, rc, rd, ncol = 2, labels = "AUTO")




