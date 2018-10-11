###Does the order in which Ambiguity is presented affect the Ambiguity effect on Punishment?.
#Presentation orders for Ambiguity rounds: Ambiguity_Order

# 0- 0011 - 'R1R2R3R4', 'R2R1R3R4', 'R1R2R4R3', 'R2R1R4R3'. *BASELINE (original position of Ambiguity rounds)
# 1- 1100 - 'R3R4R1R2', 'R3R4R2R1', 'R4R3R1R2', 'R4R3R2R1'.
# 2- 0101 - 'R1R3R2R4', 'R2R3R1R4', 'R1R4R2R3', 'R2R4R1R3'.
# 3- 1010 - 'R3R1R4R2', 'R3R2R4R1', 'R4R1R3R2', 'R4R2R3R1'.
# 4- 1001 - 'R3R1R2R4', 'R3R2R1R4', 'R4R1R2R3', 'R4R2R1R3'.
# 5- 0110 - 'R1R3R4R2', 'R2R3R4R1', 'R1R4R3R2', 'R2R4R3R1'.

#Creation of Categorical Variable: Ambiguity_Order.
dm$Ambiguity_Order <- ifelse(dm$Round_Order %in% c('R1R2R3R4', 'R2R1R3R4', 'R1R2R4R3', 'R2R1R4R3'),0,
                             ifelse(dm$Round_Order %in% c('R3R4R1R2', 'R3R4R2R1', 'R4R3R1R2', 'R4R3R2R1'),1,
                                    ifelse(dm$Round_Order %in% c('R1R3R2R4', 'R2R3R1R4', 'R1R4R2R3', 'R2R4R1R3'),2,
                                           ifelse(dm$Round_Order %in% c('R3R1R4R2', 'R3R2R4R1', 'R4R1R3R2', 'R4R2R3R1'),3,
                                                  ifelse(dm$Round_Order %in% c('R3R1R2R4', 'R3R2R1R4', 'R4R1R2R3', 'R4R2R1R3'),4,
                                                         ifelse(dm$Round_Order %in% c('R1R3R4R2', 'R2R3R4R1', 'R1R4R3R2', 'R2R4R3R1'),5,-999))))))
dm$Ambiguity_Order <- as.character(dm$Ambiguity_Order)

#Creation of dummy-variables: 0-Baseline, 1-Compared Order.
dm$AmbDummy_1 <- ifelse(dm$Ambiguity_Order==0,0,
                        ifelse(dm$Ambiguity_Order==1,1,NA))
dm$AmbDummy_2 <- ifelse(dm$Ambiguity_Order==0,0,
                        ifelse(dm$Ambiguity_Order==2,1,NA))
dm$AmbDummy_3 <- ifelse(dm$Ambiguity_Order==0,0,
                        ifelse(dm$Ambiguity_Order==3,1,NA))
dm$AmbDummy_4 <- ifelse(dm$Ambiguity_Order==0,0,
                        ifelse(dm$Ambiguity_Order==4,1,NA))
dm$AmbDummy_5 <- ifelse(dm$Ambiguity_Order==0,0,
                        ifelse(dm$Ambiguity_Order==5,1,NA))

#Effect of Position of specific Rounds.
#Position R1 - 00
dm$Position_R1 <- ifelse(dm$T1=="R1"&dm$Uncertainty==0&dm$Ambiguity==0,0,
                      ifelse(dm$T2=="R1"&dm$Uncertainty==0&dm$Ambiguity==0,1,
                             ifelse(dm$T3=="R1"&dm$Uncertainty==0&dm$Ambiguity==0,2,
                                    ifelse(dm$T4=="R1"&dm$Uncertainty==0&dm$Ambiguity==0,3,NA))))
#Position R2 - 10
dm$Position_R2 <- ifelse(dm$T1=="R2"&dm$Uncertainty==1&dm$Ambiguity==0,0,
                         ifelse(dm$T2=="R2"&dm$Uncertainty==1&dm$Ambiguity==0,1,
                                ifelse(dm$T3=="R2"&dm$Uncertainty==1&dm$Ambiguity==0,2,
                                       ifelse(dm$T4=="R2"&dm$Uncertainty==1&dm$Ambiguity==0,3,NA))))
#Position R3 - 01
dm$Position_R3 <- ifelse(dm$T1=="R3"&dm$Uncertainty==0&dm$Ambiguity==1,0,
                         ifelse(dm$T2=="R3"&dm$Uncertainty==0&dm$Ambiguity==1,1,
                                ifelse(dm$T3=="R3"&dm$Uncertainty==0&dm$Ambiguity==1,2,
                                       ifelse(dm$T4=="R3"&dm$Uncertainty==0&dm$Ambiguity==1,3,NA))))

#Position R4 - 11
dm$Position_R4 <- ifelse(dm$T1=="R4"&dm$Uncertainty==1&dm$Ambiguity==1,0,
                         ifelse(dm$T2=="R4"&dm$Uncertainty==1&dm$Ambiguity==1,1,
                                ifelse(dm$T3=="R4"&dm$Uncertainty==1&dm$Ambiguity==1,2,
                                       ifelse(dm$T4=="R4"&dm$Uncertainty==1&dm$Ambiguity==1,3,NA))))

dm$Position <- rowSums(dm[c("Position_R1","Position_R2","Position_R3","Position_R4")],na.rm = TRUE)
dm[c("Position_R1","Position_R2","Position_R3","Position_R4")]<-NULL


######################## ANALYSES OF EFFECTS OF ORDER ###############################

#Analyses of effect of Ambiguity_Order on Punishment.
#Ambiguity as Level 1 Factor, and Order as Level 2 Factor.
Order_mod <- lmer(Punishment.x~Ambiguity+Ambiguity_Order+Ambiguity*Ambiguity_Order+(1+Ambiguity|ID),data=dm) 
summary(Order_mod)
#Average Punishment based on Ambiguity Order.
library(dplyr)
dm_Order0 <- filter(dm,Ambiguity_Order==0)
dm_Order1 <- filter(dm,Ambiguity_Order==1)
dm_Order2 <- filter(dm,Ambiguity_Order==2)
dm_Order3 <- filter(dm,Ambiguity_Order==3)
dm_Order4 <- filter(dm,Ambiguity_Order==4)
dm_Order5 <- filter(dm,Ambiguity_Order==5)
mean(dm_Order0$Punishment.x)
mean(dm_Order1$Punishment.x)
mean(dm_Order2$Punishment.x)
mean(dm_Order3$Punishment.x)
mean(dm_Order4$Punishment.x)
mean(dm_Order5$Punishment.x)

#Simple effect Analyses in Orders 1 and 2.
Order1_mod <- lmer(Punishment.x~Ambiguity+Uncertainty+(1+Ambiguity|ID),data=dm_Order1) 
summary(Order1_mod)
Order2_mod <- lmer(Punishment.x~Ambiguity+Uncertainty+(1+Ambiguity|ID),data=dm_Order2) 
summary(Order2_mod)
summary(EP1)

#Analysis of Position of Round on Punishment.
#Position and Ambiguity as Level 1 Factors.
Position_mod <- lmer(Punishment.x~Ambiguity+Uncertainty+Position+(1|ID),data=dm) 
summary(Position_mod)

Position_TEST <- lmer(Punishment.x~Ambiguity*ObserverJS.z+Ambiguity_Order+(1+Ambiguity|ID),data=dm)
summary(Position_TEST)

Position_mod2 <- lmer(Punishment.x~Ambiguity*ObserverJS.z+Position+(1|ID),data=dm) 
summary(Position_mod2)

RUN SAME ANALYSES FOR COMPENSATION.
