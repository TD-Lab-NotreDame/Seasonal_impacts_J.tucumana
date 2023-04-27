#### SET UP #### 
## LOAD REQUIRED PACKAGES
library(ggplot2)
library(patchwork)
library(equatiomatic)
library(dplyr)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(devtools)
library(factoextra)
library(ggside)
library(gridExtra)
library(gtsummary)
library(lme4)
library(car)
library(AICcmodavg)
library(ggpubr)

## SET WORKING DIRECTORY
setwd("~/Desktop/Torres-Dowdall/Natural History Paper/csv files")

## LOAD DATA 
# Climate data for El Colmenar from 2006-2014
historical_climate <- read.csv("Colmenar_historical_climate.csv")
# Climate data for El Colmenar for year of study 
Year_of_study <- read.csv("Year_of_study_climate.csv")
# Photoperiod data for the year of study / location of study 
photoperiod <- read.csv("Daylength.csv")
# Size data for sexual dimorphism analysis 
sizes <- read.csv("Size_data.csv")
sizes <- sizes[1:423,1:4]
# Average numbers of immature/mature oocytes and embryos by month 
counts <- read.csv("counts.csv")
counts$Month <- factor(counts$Month, levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
# Size and fecundity data for all females
size_fecundity <- read.csv("Size_fecundity.csv")
size_fecundity$Month <- factor(size_fecundity$Month, levels = c("July","August","September","October","November","December","January","February","March","April","May","June"))
# Size and fecundity data for only breeding females
breeders <- read.csv("breeders.csv")
breeders$Month <- factor(breeders$Month, levels = c("October","November","December","January","February","March","April","May","June"))
# summary of environmental and fecundity data
rep <- read.csv("summary_env_fec.csv")
rep$Month <- factor(rep$Month, levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
# Egg and embryo data for each female
eggs_embryos <- read.csv("Eggs_Embryos.csv")
# Environmental and fecundity data for all females with 0 or 1 indicating breeding status
env_fec <- read.csv("env_fec.csv")
env_fec$Month <- factor(env_fec$Month, levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
# historical climate with month names
historical_climate_2 <- read.csv("historical_climate_2.csv")
historical_climate_2 <- historical_climate_2[1:97,]
historical_climate_2$month <- factor(historical_climate_2$month, levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
####

#### CLIMATE DATA ####
# Historical data (climate for El Colmenar)
panel_1 <- ggplot(historical_climate, aes(x=month, y=rainfall)) + geom_point() +
  ylab("Rainfall (mm)") + geom_smooth(se=TRUE, method="loess", color="#0287ad", span=0.7) + 
  xlab("Month") + theme_classic() 

panel_2 <- ggplot(historical_climate, aes(x=month, y=air_temp)) + geom_point() + 
  ylab("Air Temperature (Celsius)") + geom_smooth(se=TRUE, method="loess",color="#0287ad") +
  xlab("Month") + theme_classic()

panel_3 <- ggplot(historical_climate, aes(x=month, y=humidity)) + geom_point() + ylab("Humidity (%)") + 
  geom_smooth(se=TRUE, method="loess", color="#0287ad") + xlab("Month") + theme_classic()

# Photoperiod (year of study)
panel_4 <- ggplot(photoperiod, aes(x=Month, y=minutes_daylight)) + geom_point() + stat_smooth(method="loess", se=FALSE, color="#0287ad", span=0.5) + ylab("Daylight (minutes)") + 
  xlab("Month") + theme_classic()

# FIGURE 1C
figure_1C <- ((panel_1|panel_2)/(panel_3|panel_4))
figure_1C

# What does each month look like?
ggplot(historical_climate_2, aes(x = air_temp, y = rainfall, color=month)) + geom_point() +
  xlab("Air temperature (degrees Celsius)") + ylab("Rainfall (mm)") + theme_classic() 
ggplot(historical_climate_2, aes(x = air_temp, y = rainfall, color=month)) + geom_point() +
  xlab("Air temperature (degrees Celsius)") + ylab("Rainfall (mm)") + theme_classic() + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

#### SEXUAL DIMORPHISM DATA ####
# Is data normally distributed?
males <- sizes[sizes$Sex=="Male",]
# males$Month <- factor(males$Month, levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
# ggplot(males, aes(x = factor(Month), y = Weight..total.)) + stat_summary(fun = "mean", geom = "bar") + xlab("Month") + ylab("Average male weight (g)") + theme_classic()
# ggplot(males, aes(x = factor(Month), y = Length..total.)) + stat_summary(fun = "mean", geom = "bar") + xlab("Month") + ylab("Average male length (mm)") + theme_classic()
# ggplot(males, aes(x=Length..total., y=Weight..total.)) + geom_point() + stat_smooth(method="loess", se=TRUE, color="olivedrab", span=0.5) + ylab("Weight (g)") + xlab("Length (mm)") + theme_classic()
# ggplot(males, aes(x=Weight..total.)) + geom_histogram() + theme_classic() + ylab("Number of males") + xlab("Weight (g)")
# ggplot(males, aes(x=Length..total.)) + geom_histogram() + theme_classic() + ylab("Number of males") + xlab("Length (mm)")
females <- sizes[sizes$Sex=="Female",]
# females$Month <- factor(females$Month, levels=c("July","August","September","October","November","December","January","February","March","April","May","June"))
# ggplot(females, aes(x = factor(Month), y = Weight..total.)) + stat_summary(fun = "mean", geom = "bar") + xlab("Month") + ylab("Average female weight (g)") + theme_classic()
# ggplot(females, aes(x=Length..total., y=Weight..total.)) + geom_point() + stat_smooth(method="loess", se=TRUE, color="olivedrab", span=0.5) + ylab("Weight (g)") + xlab("Length (mm)") + theme_classic()
# ggplot(females, aes(x=Weight..total.)) + geom_histogram() + theme_classic() + ylab("Number of females") + xlab("Weight (g)")
# ggplot(females, aes(x=Length..total.)) + geom_histogram() + theme_classic() + ylab("Number of females") + xlab("Length (mm)")
# ggplot(females, aes(x = factor(Month), y = Length..total.)) + stat_summary(fun = "mean", geom = "bar") + xlab("Month") + ylab("Average female length (mm)") + theme_classic()

# Figure 2
panel_5 <- ggplot(sizes, aes(y = Weight..total., x = Length..total., color=Sex)) + geom_point() + scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab("Log (Length(mm))") + ylab("Log (Weight (g))") + theme_classic() + geom_smooth(method=lm, se=FALSE, aes(color=factor(Sex))) + scale_color_manual(values=c("#83c172","#0287ad"))
panel_6 <- ggplot(sizes, aes(x=Weight..total., fill=Sex)) + geom_density(alpha=0.5) + scale_x_continuous(trans='log10') +
  scale_fill_manual(values=c("#83c172","#0287ad")) + theme_classic() + xlab("Total weight (g)") + ylab("Density") 
panel_6 <- panel_6 + coord_flip()
panel_7 <- ggplot(sizes, aes(x=Length..total., fill=Sex)) + scale_x_continuous(trans='log10') +
  geom_density(alpha=0.5) + scale_fill_manual(values=c("#83c172","#0287ad")) + theme_classic() + xlab("Total length (mm)") + ylab("Density")
panel_8 <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )
grid.arrange(panel_7, panel_8, panel_5, panel_6, ncol=2, nrow=2, widths=c(4,2), heights=c(2,4))

# Fits for linear regression of sexual dimorphism 
model1 <- lm(log(Weight..total.)~log(Length..total.), data=sizes)
summary(model1)
anova(model1)
AIC(model1)

model2 <- lm(log(Weight..total.)~log(Length..total.)*Sex, data=sizes)
summary(model2)
anova(model2)
AIC(model2)

model3 <-  lm(log(Weight..total.)~log(Length..total.)*Month, data=sizes)
summary(model3)
anova(model3)
AIC(model3)

model4 <-  lm(log(Weight..total.)~log(Length..total.)*Sex*Month, data=sizes)
summary(model4)
anova(model4)
AIC(model4)

model5 <-  lm(log(Weight..total.)~log(Length..total.)*Month, data=females)
summary(model5)
anova(model5)
AIC(model5)

model6 <-  lm(log(Weight..total.)~log(Length..total.)*Month, data=males)
summary(model6)
anova(model6)
AIC(model6)

# Mixed linear model 
mixed_model <- lmer(log(Weight..total.)~log(Length..total.)*Sex + (log(Length..total.)|Month), data=sizes, REML = FALSE)
summary(mixed_model)
Anova(mixed_model)
anova(mixed_model)

# Mean and SD for size data
mean(females$Length..total.)
sd(females$Length..total.)
mean(males$Length..total.)
sd(males$Length..total.)

mean(females$Weight..total.)
sd(females$Weight..total.)
mean(males$Weight..total.)
sd(males$Weight..total.)

#### REPRODUCTION DATA ####
## Embryo and oocyte data
# Scatterplots
embryo_counts <- eggs_embryos[eggs_embryos$Type=="Embryos",]
ggplot(embryo_counts, aes(x=Month, y=Count)) + geom_point(color="#0287ad", size=0.8) +
  ylab("Number") +
  xlab("Month") + theme_classic() + stat_summary(fun.y=mean, colour="#0287ad", geom="point", size=5) + stat_summary(fun.y=mean, colour="#0287ad", geom="line")+ ylim(c(0,220))

immature_counts <- eggs_embryos[eggs_embryos$Type=="Immature",]
ggplot(immature_counts, aes(x=Month, y=Count)) + geom_point(color="#83c172", size=0.8) +
  ylab("Number") +
  xlab("Month") + theme_classic() + stat_summary(fun.y=mean, colour="#83c172", geom="line") + ylim(c(0,220))+ stat_summary(fun.y=mean, colour="#83c172", geom="point", size=5) 

mature_counts <- eggs_embryos[eggs_embryos$Type=="Mature",]
ggplot(mature_counts, aes(x=Month, y=Count)) + geom_point(color="#333b81", size=0.8) +
  ylab("Number") +
  xlab("Month") + theme_classic() + stat_summary(fun.y=mean, colour="#333b81", geom="line") + ylim(c(0,220))+ stat_summary(fun.y=mean, colour="#333b81", geom="point", size=5) 

## Size fecundity data
# figure 5
ggplot(breeders, aes(x = Standard_length, y = Embryos, color=Month)) + geom_point() +
  xlab("Standard Length (mm)") + ylab("Number of Embryos") + theme_classic() + 
  scale_color_manual(values = c("October" = "#333b81", "November"="#009ba9", "December"="#00ab88", "January"="#aab513", "February"="#ffa600")) +
  geom_smooth(method=lm, se=FALSE, aes(color=factor(Month)))
october <- breeders[breeders$Month=="October",]
ggplot(october, aes(x = Standard_length, y = Embryos)) + geom_point() +
  xlab("Standard Length (mm)") + ylab("Number of Embryos") + theme_classic() +
  geom_smooth(method=lm, se=FALSE)
# Equations of linear regressions 
length_lm <- lm(Embryos~Standard_length * Month, data=breeders)
summary(length_lm)
anova(length_lm)
AIC(length_lm)

extract_eq(length_lm, use_coefs = TRUE)

# Max and mean for size-fecundity 
max(size_fecundity$total_weight)
max(size_fecundity$Embryos)
max(size_fecundity$Standard_length)
mean(breeders$total_weight)
mean(breeders$Embryos)
mean(breeders$Standard_length)

#### PCA ####
rep2 <- rep[,c(3,4,5,15)]
colnames(rep2)[2] ="Air temperature"
colnames(rep2)[4] ="Photoperiod"
res.pca <- prcomp(rep2,  scale = TRUE)
var <- get_pca_var(res.pca)
var
head(var$coord)
ind <- get_pca_ind(res.pca)
ind
ind$coord

corr_matrix <- cor(rep2)
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[,1:2]
data.pca

eig.val <- get_eigenvalue(data.pca)
eig.val
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("olivedrab", "salmon2", "skyblue3"),
             repel = TRUE)

# Logistic regression with dim1 and dim2
Dim1 <- ggplot(env_fec, aes(x=Dim1, y=Embryos)) + geom_point(position=position_jitter(width=0.1,height=.02)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, color="#0287ad") + theme_classic() + xlab("PC1") + ylab("Breeding Status")
Dim1_fec <- glm(Embryos ~ Dim1, data=env_fec, family=binomial(link="logit"))
summary(Dim1_fec)
tbl_regression(Dim1_fec, exponentiate = TRUE)

Dim2 <- ggplot(env_fec, aes(x=Dim2, y=Embryos)) + geom_point(position=position_jitter(width=0.1,height=.02)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, color="#0287ad") + theme_classic() + xlab("PC2") + ylab("Breeding Status")
Dim2_fec <- glm(Embryos ~ Dim2, data=env_fec, family=binomial(link="logit"))
summary(Dim2_fec)
tbl_regression(Dim2_fec, exponentiate = TRUE)

Dim_fec_graphs <- ((Dim1/Dim2)) +
  plot_annotation(tag_levels = 'A') 
Dim_fec_graphs

Dim1_Dim2_fec <- glm(Embryos ~ Dim2 + Dim1, data=env_fec, family=binomial(link="logit"))
summary(Dim1_Dim2_fec)
tbl_regression(Dim1_Dim2_fec, exponentiate = TRUE)

#### ENV FEC ####
# Correlation tests with environmental variables
cor(env_fec$Temp, env_fec$Photoperiod, method=c("pearson"))
cor(env_fec$Temp, env_fec$Rainfall, method=c("pearson"))
cor(env_fec$Rainfall, env_fec$Humidity, method=c("pearson"))

# Figure S1
ggscatter(env_fec, x = "Temp", y = "Photoperiod", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Temperature (degrees Celsius)", ylab = "Photoperiod (minutes daylength)")

# Creating models for model selection 
Null.model <- glm(Embryos ~ 1, data=env_fec, family=binomial(link="logit"))
summary(Null.model)

Temperature.Humidity.Rainfall.model <- glm(Embryos ~ Temp_yos + Humidity_yos + Rainfall_yos, data=env_fec, family=binomial(link="logit"))
summary(Temperature.Humidity.Rainfall.model)
tbl_regression(Temperature.Humidity.Rainfall.model, exponentiate = TRUE)

Temperature.model <- glm(Embryos ~ Temp_yos, data=env_fec, family=binomial(link="logit"))
summary(Temperature.model)
tbl_regression(Temperature.model, exponentiate = TRUE)

Rainfall.model <- glm(Embryos ~ Rainfall_yos, data=env_fec, family=binomial(link="logit"))
summary(Rainfall.model)
tbl_regression(Rainfall.model, exponentiate = TRUE)

Humidity.model <- glm(Embryos ~ Humidity_yos, data=env_fec, family=binomial(link="logit"))
summary(Humidity.model)
tbl_regression(Humidity.model, exponentiate = TRUE)

Temperature.Rainfall.model <- glm(Embryos ~ Temp_yos + Rainfall_yos, data=env_fec, family=binomial(link="logit"))
summary(Temperature.Rainfall.model)
tbl_regression(Temperature.Rainfall.model, exponentiate = TRUE)

Temperature.Humidity.model <- glm(Embryos ~ Temp_yos + Humidity_yos, data=env_fec, family=binomial(link="logit"))
summary(Temperature.Humidity.model)

Rainfall.Humidity.model <- glm(Embryos ~ Rainfall_yos + Humidity_yos, data=env_fec, family=binomial(link="logit"))
summary(Rainfall.Humidity.model)

# Model selection 
Models <- list(Null.model, Temperature.Humidity.Rainfall.model, Temperature.model, Rainfall.model, Humidity.model, Temperature.Rainfall.model, Temperature.Humidity.model, Rainfall.Humidity.model)
Model.names <- c('Null.model','Temp.Humidity.Rainfall','Temp','Rainfall','Humidity','Temp.Rainfall','Temp.Humidity','Rainfall.humidity')
aictab(cand.set = Models, modnames = Model.names)


temp <- ggplot(env_fec, aes(x=Temp_yos, y=Embryos)) + geom_point(position=position_jitter(width=0.2,height=.02)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, color="#0287ad") + theme_classic() + xlab("Air Temperature (degrees Celsius)") + ylab("Breeding Status")
rainfall <- ggplot(env_fec, aes(x=Rainfall_yos, y=Embryos)) + geom_point(position=position_jitter(width=0.9,height=.02)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, color="#0287ad") + theme_classic() + xlab("Rainfall (mm)") + ylab("Breeding Status")
humidity <- ggplot(env_fec, aes(x=Humidity_yos, y=Embryos)) + geom_point(position=position_jitter(width=0.9,height=.02)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE, color="#0287ad") + theme_classic() + xlab("Humidity (%)") + ylab("Breeding Status")
graphs <- ((temp/rainfall/humidity)) +
  plot_annotation(tag_levels = 'A') 
graphs

# climate data (rainfall) bar graphs (visualized with embryo count)
temp2 <- ggplot(rep, aes(x = Month, y = Temp_yos)) + 
  geom_bar(position='dodge', stat='identity', fill="#0287ad", width=0.5) +
  geom_line(aes(x=Month, y=number.embryos*0.8, group=1)) + xlab("Month") + ylab("Air Temperature (degrees Celsius)") + theme_classic() +
  scale_y_continuous(sec.axis=sec_axis(~./.8, name="Embryo Count"))
rainfall2 <- ggplot(rep, aes(x = Month, y = Rainfall_yos)) + 
  geom_bar(position='dodge', stat='identity', fill="#0287ad", width=0.5) +
  geom_line(aes(x=Month, y=number.embryos*5, group=1)) + xlab("Month") + ylab("Rainfall (mm)") + theme_classic() +
  scale_y_continuous(sec.axis=sec_axis(~./5, name="Embryo Count"))
humidity2 <- ggplot(rep, aes(x = Month, y = Humidity_yos)) + 
  geom_bar(position='dodge', stat='identity', fill="#0287ad", width=0.5) +
  geom_line(aes(x=Month, y=number.embryos*2, group=1)) + xlab("Month") + ylab("Humidity (%)") + theme_classic() +
  scale_y_continuous(sec.axis=sec_axis(~./2, name="Embryo Count"))

graphs2 <- ((temp2/rainfall2/humidity2)) +
  plot_annotation(tag_levels = 'A') 
graphs2

