library(dplyr)
library(ggplot2)

sp500=read.csv("sp500-companies-fixed.csv")


#1. Frequency Table 
freq = sp500 %>% group_by(Sector)%>%summarise(Frequency=n())
ggplot(freq, aes(x=Sector, y=Frequency, fill = Sector)) +
  geom_col()

#2. Column Chart
freq %>% ggplot(aes(x=Sector, y=Sector, fill=Sector)) + 
  geom_col()

#3. Histogram for market caps
mc = sp500 %>% group_by(Market.Cap.) %>% summarise(Frequency = n())
ggplot(mc, aes(x="Market.Cap.", y="Frequency"))+
  geom_histogram()

#4. Pie Chart
freq %>% ggplot(aes(x="", y=Frequency, fill=Sector)) + 
  geom_bar(stat = "identity", width=1) +
  coord_polar("y", start=0)

#5. What is the distribution of Market caps?

mcfreq <- sp500 %>% 
  group_by(Market.Cap.) %>% 
  summarise(Frequency = n())

ggplot(mcfreq, aes(x = Market.Cap.)) +
  geom_histogram(binwidth = 1e11, fill = "skyblue", color = "black") +
  labs(x = "Market Cap", y = "Frequency") +
  theme_minimal()

#6. If you pick one stock what are the chances it is a Technology Stock?

technology_count <- sp500 %>%
  filter(Sector == 'Technology') %>%
  summarise(total=n())  #n gives the row count of tech
                  #summarise(n()) creates a new table with the total number of rows, with the number being under the column named "n()"
                  #now with total=n, the column is now called total

total_count <- sum(freq$Frequency) #I am using the freq table, and using the $ to only look at the 'Frequency' column where the data is 

tech_proportion <- technology_count / total_count

tech_proportion

#7. The best portfolio you could have picked last quarter was Pfizer, Moderna, Zoom, Google and Amgen. If you bought 5 random stocks, what was the chance you would have picked these 5?
PerfPort = 1/choose(528,5)
PerfPort
2.980303e-12

#8. If you randomly choose a winning stock, what are the chances you owned it?


sp500 %>% group_by(UpLastQtr, Owned) %>% 
  summarise(Frequency=n()) %>% filter()

#UpLastQtr Owned Frequency
#<dbl> <dbl>     <int>
#1         0     0       428  loser and unowned 
#2         0     1        27  loser and owned
#3         1     0        64  up last quarter and unowned
#4         1     1        11  up last quarter and owned 

winning_count <- sp500 %>% 
  filter(UpLastQtr == 1) %>%
  summarise(total = n())
75


owned_count <- sp500 %>% 
  filter(Owned == 1) %>%
  summarise(total = n())
38

11/38

#9. Compare winning stocks vs stocks you owned. 
# two facets of winning stocks vs owned stocks; bars by sector

ggplot(win_vs_owned_table, aes(x = factor(Owned), y = Frequency, fill = factor(UpLastQtr))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of UpLastQtr by Owned Status",
       x = "Owned", y = "Frequency") +
  scale_fill_manual(values = c("blue", "green"),
                    labels = c("UpLastQtr = FALSE", "UpLastQtr = TRUE")) +
  theme_minimal()
