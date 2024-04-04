#this file is to be submitted on github for grading and for public view!

#Prompt: 
#You will analyze the data from the Hospital data set (on Moodle, inside
#data folder) and identify trends and patterns in the current operations.
#You need to use R.

#Scenario: 
#You have been hired as a consultant by a large hospital chain. As part of its
#expansion plan, the organization wants to open a new hospital in a small- to
#medium-sized town (with a population of 10,000–50,000). Previously, senior
#management hired a market research firm to do an initial feasibility analysis.
#The firm presented the management team with two options:
#       Option A: Open a small facility of 90–100 beds
#       Option B: Open a medium-sized facility of 300–400 beds

#The hospital’s vice president of operations and finance has tasked you with
#doing an in-depth analysis to determine which of the two options will be the
#most profitable, given the expense range of $55–$75 million ($ amounts in
#Hospital dataset are expressed in 000’s of dollars). The hospital has provided
#you with a data set that contains data about admissions, personnel, and
#hospital beds, and attributes of the data such as outpatient visits, births, total
#expense, and census information.

library(dplyr)
library(ggplot2)

hospitals <-read_csv("hospitals.csv")

#1. Exploratory Analysis
#Questions below are from the assignment sheet, and will be recorded here for you. 
#Below each question will be the code necessary to answer them, the result of the code from the console, 
#and a brief explanation of what you're looking at

#a) How big is the dataset?
dim(hospitals)
2000   18
#the dim() function shows us that the "hospitals" date frame has 2000 rows and 18 columns. 

#b) What are the names of the columns?
colnames(hospitals)
[1] "Hospital Number"       "State"                 "Region Number"         "Control Number"       
[5] "Service Number"        "Beds"                  "Admissions"            "Census"               
[9] "Outpatient Visits"     "Births"                "Total Expense"         "Payroll Expense"      
[13] "Personnel"             "Region"                "Control"               "Service"              
[17] "Gen Med & Surg or Not" "Births or Not" 
#colnames() pulled all 18 colmun names from "hospitals" as seen above. 

#c) What data types are each column? 
str(hospitals)
spc_tbl_ [2,000 × 18] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
$ Hospital Number      : chr [1:2000] "0001" "0002" "0003" "0004" ...
$ State                : num [1:2000] 1 1 1 1 1 1 1 1 1 1 ...
$ Region Number        : num [1:2000] 6 6 6 6 6 6 6 6 6 6 ...
$ Control Number       : num [1:2000] 4 6 1 1 5 4 6 1 1 5 ...
$ Service Number       : num [1:2000] 1 1 1 1 2 1 1 1 1 1 ...
$ Beds                 : num [1:2000] 247 69 101 236 100 ...
$ Admissions           : num [1:2000] 11801 3086 4076 7342 2538 ...
$ Census               : num [1:2000] 144 29 43 113 90 227 230 931 19 12 ...
$ Outpatient Visits    : num [1:2000] 120979 89568 122433 196025 11897 ...
$ Births               : num [1:2000] 1111 455 475 534 0 ...
$ Total Expense        : num [1:2000] 147587 66150 63002 104601 29113 ...
$ Payroll Expense      : num [1:2000] 53436 25627 26426 42704 15460 ...
$ Personnel            : num [1:2000] 947 467 749 968 328 ...
$ Region               : chr [1:2000] "East South Central" "East South Central" "East South Central" "East South Central" ...
$ Control              : chr [1:2000] "NonGov NonProfit" "Other Nonprofit" "Gov NonFed" "Gov NonFed" ...
$ Service              : chr [1:2000] "Gen Med & Surg" "Gen Med & Surg" "Gen Med & Surg" "Gen Med & Surg" ...
$ Gen Med & Surg or Not: num [1:2000] 1 1 1 1 0 1 1 1 1 1 ...
$ Births or Not        : num [1:2000] 1 1 1 1 0 1 1 1 1 0 ...
#While we do not need all the data here, the str() function can tell us the type of data of each column by indicating 
#whether it is numeric (num) or a character (chr) string. 
#Hospital Number's, Region's, Control's, and Service's data is all in character form. 
#All other columns consist of numerical data. 

#d) Are there missing values?
any(is.na(hospitals))
FALSE
#The result of FALSE from the in.na fucntion shows us there are no missing values in the data frame. 

#e) Which hospital has the lowest number of beds? 
hospitals%>%slice_min(Beds)
1064 and 1751 are tied at 3 beds

#f) Which hospital has the lowest expense?
#Note that the column name "Total Expense" had to be changed to 
hospitals <- hospitals %>%
  rename(Total_Expense = `Total Expense`)
hospitals%>%slice_min(Total_Expense)
826

#g) How many hospitals deliver babies?
sum(hospitals$`Births or Not` > 0)
1112
#By using the sum function and asking for the total number of hospitals hospitals with more than 0 births, 
#we know the total number of hospitals that deliver babies is 1112. 

#h)Using ggplot, scatterplot number of beds vs total expense
ggplot(hospitals, aes(x = Beds, y = Total_Expense)) +
  geom_point()+ 
  labs(x = "Number of Beds", y = "Total Expense")
#run the ggplot to view it in the plot tab  

#i) Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospitals, aes(x = Admissions, y = Total_Expense)) +
geom_point()+ 
  labs(x = "Admissions", y = "Total Expense")
#run the ggplot to view it in the plot tab  

#j) Using dplyr and ggplot, scatterplot beds vs Total Expense but only for hospitals that deliver babies
birth_hospitals <- hospitals %>% 
  filter(`Births or Not`>0)

ggplot(birth_hospitals, aes(x = Beds, y = Total_Expense)) +
  geom_point()+ 
  labs(x = "Beds", y = "Total Expense")
#run the ggplot to view it in the plot tab 

#k)One more question that you believe would be useful : 
   #My own question: What are the the mean total expenses' for hospitals Option A hospitals and Option B hospitals? 

option_A_hospitals <- hospitals %>%
  filter(hospitals$Beds >= 90 & hospitals$Beds <= 100)
mean(option_A_hospitals$Total_Expense)
$60,060.64

option_B_hospitals <- hospitals %>%
  filter(hospitals$Beds >= 300 & hospitals$Beds <= 400)
mean(option_B_hospitals$Total_Expense)
$339,435.2
#To find the mean total expense of each option of hospitals, I first created two tables, filtering the requirments of the two options from 
#hospitals. Then, I found the means of the Total_Expense column of each of these two new tables, giving us the monetary amounts shown above

#2. Descriptive Analysis: Analyze the current hospital data to identify trends and patterns in hospital admissions and costs. Create one pie
#chart, one column/bar chart, one line chart (Are your charts useful for answering the initial question Option A or Option B?)

# i: Pie Chart: One slice should be labeled Admissions.Choose another attribute for the second slice (census)
total_admissions<-sum(hospitals$Admissions)
#total_admissions= 15,315,964
total_census<-sum(hospitals$Census)
#total_census= 256,033
pie_data <- data.frame(category = c("Total Admissions", "Total Census"),
                       value = c(total_admissions, total_census))
ggplot(pie_data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Total Admissions vs Total Census", x = NULL, y = NULL) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_void()
# this pie chart gives a visual representation of the proportion of total population in the table compared to the overwhelmingly greater number of admissions. 
#it is hard to compare admissions with other columns as admissions measures people, and not many other columns measure number of people either. 
#The pie chart isn't that help, especially as we need to analyze number of beds and their relation to total expenses. 

#ii: Column/Bar Chart: Ensure that one column is titled Admissions. Choose a different attribute for the other column(expense).
total_expense<-sum(hospitals$Total_Expense)
#total_expense=$386,759,396
total_admissions
#total_admissions= 15,315,964
bar_data <- data.frame(category = c("Total Expense", "total_admissions"),
                       value = c(total_expense, total_admissions))
ggplot(bar_data, aes(x = category, y = value, fill = category)) +
  geom_col() +
  labs(title = "Total Admissions vs Total Expense",
       x = NULL, y = "Value") +
  scale_fill_manual(values = c("yellow", "red")) +
  theme_minimal()
#this column/bar chart shows in a pure numerical sense, the total expenses of all the hospitals combined and the total admissions. At this point, you may be able to see,
#ggplot is not the tool for determining whether Option A hospitals or Option B hospitals are better. The units are mostly different across the data frame making visualizations like this
#not ideal. We will need to use regression models to really determine which option is better... maybe that will be a question ;)

#iii: Line Chart: one of the lines should represent Expense; choose another attribute for the second line(beds).

ggplot(hospitals, aes(x = Beds, y = Total_Expense)) +
  geom_line() +
  labs(title = "Expenses vs Beds",
       x = "Beds",
       y = "Expenses") +
  theme_minimal()
#this line chart shows (while hard to decipher) every hospital. It shows a mostly positive trend between total expenses in relation to number of beds in each hospital. As said before
#ggplot is not the tool for determining whether Option A hospitals or Option B hospitals are better, we will need to use regression models. 

#3) Perform one simple regression (one predictor) to provide recommendations.

#i: Dependent Variable: Total Expense 
#   Independent Variable: Beds

#model: 
lm_model_1 <- lm(Total_Expense ~ Beds, hospitals)
summary(lm_model_1)
Residuals:
  Min       1Q   Median       3Q      Max 
-3411270   -54257    -9389    25420  2990235 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -16060.93    5937.12  -2.705  0.00688 ** 
  Beds          1084.56      19.63  55.243  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 204300 on 1998 degrees of freedom
Multiple R-squared:  0.6043,	Adjusted R-squared:  0.6041 
F-statistic:  3052 on 1 and 1998 DF,  p-value: < 2.2e-16

#ii: The value of R^2 is 0.6043

#iii: The R^2 measures the variance in the dependent variable, predicted by the independent variable. 
#.    Here, the R^2 of 0.6043 denotes if the independent variable (beds) changes by 1 value, the dependent variable (expenses), changes by 0.6043 on average across the population of 2000 hospitals.  

#iv: 0.00688 (dv) and 2e-16(iv) are the p-values for each variable, with <2.2e-16 being the combined p-value. 
#    While there are three p-values reported, only one is of importance, the p-value of the independent variable 2e-16. 
#    The p-value indicates statistical significance between the number of beds and the expenses of the individual hospitals as it is close to zero and far from 0.05. 

#v: An educated guess, but I believe the right attribute size (#of beds) that seems appropriate to lead me into the expense range of 50-75 million is Option A, 90-100 beds. 

#4) Perform one multivariate regression to provide recommendations

#i: Dependent Variable: Total Expense
#   Independent Variables: Beds and Admissions

#model: 
lm_model_2 <- lm(Total_Expense ~ Beds + Admissions, hospitals)
summary(lm_model_2)
Residuals:
  Min      1Q  Median      3Q     Max 
-956819  -46542     901   18079 2953802 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.433e+04  4.816e+03  -2.975  0.00296 ** 
  Beds         2.244e+02  3.107e+01   7.222 7.25e-13 ***
  Admissions   2.147e+01  6.656e-01  32.247  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 165700 on 1997 degrees of freedom
Multiple R-squared:  0.7398,	Adjusted R-squared:  0.7396 
F-statistic:  2839 on 2 and 1997 DF,  p-value: < 2.2e-16
#ii: The value of R^2 is 0.7398

#iii/v: The R^2 measures the variance in the dependent variable, predicted by the independent variables. 
#     Here, the R^2 of 0.7398 denotes if the independent variables (beds and admissions) changes by 1 value, the dependent variable (expenses), changes by 0.7398 on average across the population of 2000 hospitals.

#iv/v: 0.00296(dv), 7.25e-13(iv1) and 2e-16(iv2) are the p-values for each variable, with <2.2e-16 being the combined p-value. 
#    While there are four p-values reported, only one is of importance, the p-values of the independent variables 7.25e-13 and 2e-16. 
#    The p-values indicates statistical significance between the number of beds and the expenses of the individual hospitals and the statistical significance between the number of admissions and the expenses 
#    of each hospital as they are close to zero and far from 0.05. 

#5) Conclusion: Option A is the better option 
# -My results show a decently strong relation between the number of beds and expenses, with the relation becoming even stronger when accounting for admissions
# -This experiment isn't perfect from my side and the data given (I'm sorry professor, you're still my favorite at mc). I wish I could have made the data smaller to include only small and medium towns, 
#  that would have excluded some hospitals with a number of beds within the ranges of options A and B. You can't make a conclusion when you're missing data like that. The experiment is also flawed as we are asked
#  to determine what is the most profitable, but are never given anything about revenue. Yes we are given a specific cost measure for payroll expenses, but that isn't enough, we need revenue. 
# -As for how I arrived at my conclusion, I figured lower beds and admissions have proven to have a strong relation to lower expenses, theoretically reducing total expenses in the hospitals, not being able to account 
#  for revenue. 
 

