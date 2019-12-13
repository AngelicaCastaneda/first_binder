library(tidyverse)
library(emmeans) #for running pairwise comparisons
library(afex)  #for running ANOVA


data_3 <- read_csv("ANOVA_data3.csv")
data_3
#We will need to use Factorial ANOVA (we have 2 factors with 2 levels)

data_3$Colour <- as.factor(data_3$Colour)#setting Colour as a factor
data_3$Size <- as.factor(data_3$Size)#setting Size as a factor
data_3

#Generating Descriptives
data_3 %>%
  group_by(Size, Colour) %>%
  summarise(mean = mean(RT), sd = sd(RT))

#Visualising data
data_3 %>%
  ggplot(aes(x = Size:Colour, y = RT, colour = Size:Colour)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .5) +
  guides(colour = FALSE) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") + #used for 95% confidence interval
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Condition")

mode3 <- aov_4(RT ~ Size * Colour + (1 + Size * Colour | Subject), data = data_3)
summary(mode3)
#Shows that there is significant difference
#We need to run a pariwise analysis to find where the difference(s) lie

emmeans(mode3, pairwise ~ Size * Colour, adjust = "Bonferroni")
#Every condition differ from each other condition 





  
  
  