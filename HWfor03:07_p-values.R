#Question 1 

Weight_Loss_Program_Group <- c(3, 4, 2, 5, 6, 3, 4, 5, 7, 6, 5, 4, 5, 6, 7)
No_Program_Group <- c(2, 1, 0, 1, 2, 1, 0, 2, 1, 3, 2, 1, 1, 2, 2)

t.test(Weight_Loss_Program_Group,
       No_Program_Group)

data:  Weight_Loss_Program_Group and No_Program_Group
t = 7.7904, df = 22.04, p-value = 9.033e-08
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  2.494984 4.305016
sample estimates:
  mean of x mean of y 
4.8       1.4 

#The weight loss program is effective as the p value is close to zero 


#Question 2 

Educational_Tool_Classroom <- c(78, 85, 90, 87, 92, 76, 88, 91, 84, 89)
Traditional_Classroom <- c(75, 79, 80, 77, 82, 76, 81, 83, 78, 80)

t.test(Educational_Tool_Classroom,
       Traditional_Classroom)

data:  Educational_Tool_Classroom and Traditional_Classroom
t = 3.6541, df = 12.997, p-value = 0.002915
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  2.820545 10.979455
sample estimates:
  mean of x mean of y 
86.0      79.1 

#The educational tools are effective as the p value is far from a significant value