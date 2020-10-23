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

#summarise data

potatoe_data %>%
  group_by(Genotype) %>%
  summarise(mean_AUDPC = mean(AUDPC), std_AUDPC = sd(AUDPC))

#Outlier detection test
grubbs.test(potatoe_data$relative) #No significant outliers


#Single site analysis


#Multi site analysis
model <- with(potatoe_data, AMMI(Locality, Genotype, Rep, relative, )) #Not possible 

plot(model,0,1,angle=20,ecol="brown")
