###############################################################################
#   A study of Phytophthora infestans in the potato plant in the              #
#       localities of Comas and Oxapampa in Peru, 2005.                       #
#                                                                             #
#        Using the Metan R package                                            #
#                                                                             #
#                                                                             #        
###############################################################################


# Loading R packages ------------------------------------------------------

library(agricolae)
library(tidyverse)
library(metan)
library(outliers)
library(ggpubr)


# Loading data ------------------------------------------------------------
data(CIC)

comas <- CIC$comas
oxa <- CIC$oxapampa

View(comas)
View(oxa)

#Choosing necessary columns

com <- comas %>%
  select("Locality", "Row", "Genotype", "Rep", "Nplant", "AUDPC", "relative")

ox <- oxa %>%
  select("Locality", "Row", "Genotype", "Rep", "Nplant", "AUDPC", "relative")

potatoe_data <- rbind(com, ox)

View(potatoe_data)

# Exploratory Data Analysis -----------------------------------------------

ggplot(potatoe_data, aes(x= Locality, y = AUDPC)) + 
  geom_boxplot(aes(fill= Locality)) + ggtitle("Boxplot for AUDPC by locality") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("audpc by locality.png")

ggplot(potatoe_data, aes(x=relative)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram for Relative AUDPC") +
  theme(plot.title = element_text(hjust = 0.5)) #Data is not normal

ggsave("relative AUDPC.png")

#Normality test
ggqqplot(potatoe_data$relative)

shapiro.test(potatoe_data$relative)

#Density plot
ggdensity(potatoe_data, x = "relative",
          add = "mean", rug = TRUE,
          color = "Locality", fill = "Locality",
          palette = c("#00AFBB", "#E7B800"))+
  geom_text(data = aggregate(relative~ Locality, data = potatoe_data, FUN = mean),
            aes(x = relative, y = Inf, color = Locality, label = round(relative,2)), 
            vjust = 1) + geom_vline(xintercept =  median(potatoe_data$relative))

ggsave("Density plot by location.png")

#Violin Plot
my_comparisons <- list( c("Comas", "Oxapampa") )
ggviolin(potatoe_data, x = "Locality", y = "AUDPC", fill = "Locality",
         palette = c("#00AFBB", "#E7B800"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)                                      # Add global the p-value 

ggsave("Violin plot and Wilcoxon test.png")

#summarise data

potatoe_data %>%
  group_by(Genotype) %>%
  summarise(mean_AUDPC = mean(AUDPC), std_AUDPC = sd(AUDPC))

#Outlier detection test
grubbs.test(potatoe_data$relative) #No significant outliers



# Single site analysis ----------------------------------------------------

#Model 1
model1 <- aov(relative ~ Genotype + Row + Rep, data = potatoe_data)
model1 #Residual SE = 0.1205
summary(model1)

plot(model1)

#Including covariate
model2 <- aov(relative ~ Nplant + Genotype + Row + Rep, data = potatoe_data)
model2 #Residual SE = 0.1174
summary(model2)

plot(model2)

#Genotype * Environment
model3 <- aov(relative ~ Nplant + Locality + Rep + (1|Genotype) + 
                (1|Genotype:Row), data = potatoe_data )

