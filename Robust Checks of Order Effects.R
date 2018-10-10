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

#Analyses of effect of Ambiguity_Order on Punishment.
Order_mod <- lmer(Punishment.x~Ambiguity+Ambiguity_Order+Ambiguity*Ambiguity_Order+(1+Ambiguity|ID),data=dm) 
summary(Order_mod)

library(dplyr)
dm_Order0 <- filter(dm,Ambiguity_Order==0)
mean(dm_Order0$Punishment.x)
dm_Order1 <- filter(dm,Ambiguity_Order==1)
mean(dm_Order1$Punishment.x)
dm_Order2 <- filter(dm,Ambiguity_Order==2)
mean(dm_Order2$Punishment.x)
dm_Order3 <- filter(dm,Ambiguity_Order==3)
mean(dm_Order3$Punishment.x)
dm_Order4 <- filter(dm,Ambiguity_Order==4)
mean(dm_Order4$Punishment.x)
dm_Order5 <- filter(dm,Ambiguity_Order==5)
mean(dm_Order5$Punishment.x)


#Simple effect Analyses.
Order1_mod <- lmer(Punishment.x~Ambiguity+Uncertainty+(1+Ambiguity|ID),data=dm_Order1) 
summary(Order1_mod)
Order2_mod <- lmer(Punishment.x~Ambiguity+Uncertainty+(1+Ambiguity|ID),data=dm_Order2) 
summary(Order2_mod)
summary(EP1)


Prediction of non of the dummies significant.
If significant, run the analysis with that order.
