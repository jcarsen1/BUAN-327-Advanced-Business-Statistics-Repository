library(ggplot2)

experience = c(1,1,2,4)
peereval = c(4.7, 4, 3.8, 3.3)
bonus = c(1,2,2,6)
mydata = data.frame(experience, peereval, bonus)

#############################################
###########SIMPLE LINEAR REGRESSION##########
#############################################


ggplot(mydata, aes(x=experience, y = bonus)) +
  geom_point() + 
  geom_smooth(method = "lm", se=FALSE)


reg_bonus_exp = lm( bonus ~ experience , mydata) 


summary(reg_bonus_exp)
anova(reg_bonus_exp)


#how was the R2 obtained?
anovatable = anova(reg_bonus_exp)

SSR = anovatable$`Sum Sq`[1]
SSE = anovatable$`Sum Sq`[2]
#how were the pvalues obtained?
2*pt(4.648, df = 2, lower.tail = FALSE)

pf(21.6,1,2, lower.tail = FALSE)

###EXAMPLES of ASSUMPTIONS violations####
#########################################

foc6 = read.csv("foc6.csv")

ggplot(foc6, aes(x=Period, y = SALES)) +
  geom_point()


telemark = read.csv("telemark5.txt")

ggplot(telemark, aes(x=MONTHS, y = CALLS)) +
  geom_point()





#############################################
###########MULTIPLE LINEAR REGRESSION########
#############################################
reg_bonus_exp_peereval = lm( bonus ~ experience + peereval,
                             mydata) 
summary(reg_bonus_exp_peereval)
anova(reg_bonus_exp_peereval)

ggplot(mydata, aes(x=experience, y = bonus)) +
  geom_point()

ggplot(mydata, aes(x=peereval, y = bonus)) +
  geom_point()

#how were the pvalues obtained?
2*pt(1.515,1, lower.tail = FALSE)

pf(6.177,2,1,lower.tail = FALSE)

#############################################
###########ANOTHER MULTIPLE LINEAR REGRESSION########
#############################################

#make sure hospitals.csv is in your home folder
hospitals = read.csv("hospitals.csv")
colnames(hospitals)
test = hospitals[hospitals$State ==2 ,c("Total.Expense", "Beds", "Admissions") ]


ggplot(hospitals, aes(x=Beds, y = Total.Expense)) +
  geom_point()



nrow(test)
reg1=lm(Total.Expense~Beds+Admissions, test)
summary(reg1)
anova(reg1)






#### code below compares models###

reg0=lm(Total.Expense~Beds, test)
reg1.5=lm(Total.Expense~Beds + Admissions, test)
anova(reg1.5, reg0)
