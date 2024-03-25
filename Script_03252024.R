# Title: "Social and metabolic mediation of growth performance in a temperate estuarine fish"
# Author: E. Hoots
# Last updated: 25-March-2024

rm(list=ls())

library(readxl)
library(ggplot2)
library(ggpubr)
library(ggbreak)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(lme4)

#Don't forget to set your working directory!!!

#read in relevant datasheets

tb_months <- read_excel("3 - Growth and MO2 Rates.xlsx", sheet = "1.MR_Growth")

tb_sgr <- read_excel("3 - Growth and MO2 Rates.xlsx", sheet = "2.SGR_Long") %>%
  mutate(
    Stage = as.factor(Stage)
  )

tb_data <- read_excel("3 - Growth and MO2 Rates.xlsx", sheet = "3.MR_Growth_Long") %>%
  mutate(
    Stage = as.factor(Stage)
  )

#mark fish 29 and 23 as outliers in the dataset (see Supplement: Outlier Analysis)
for (i in 1:nrow(tb_data)) {
  if (tb_data$FishID[i] == 29 | tb_data$FishID[i] == 23)
    tb_data$Outlier[i] <- "Yes"
}

#model SGR over initial mass and calculate predicted values and residuals
mod_SGR <-lmer(SGR ~ log(Mass) + (1|FishID), data = tb_sgr)

summary(mod_SGR)

tb_sgr$predict <- predict(mod_SGR, type = "response", allow.new.levels = TRUE)
tb_sgr$resid <- resid(mod_SGR)

plot(mod_SGR)
hist(resid(mod_SGR))

tb_sgr$ci_lower <- tb_sgr$predict - 1.96 * sqrt(diag(vcov(mod_SGR)))
tb_sgr$ci_upper <- tb_sgr$predict + 1.96 * sqrt(diag(vcov(mod_SGR)))

#Plot SGR vs. mass with model line and confidence interval
Fig1.1 <- ggplot(tb_sgr, aes(x = Mass, y = SGR)) +
  geom_point(aes(color = Category, shape = Stage), size = 3) +  # Plot original data points
  geom_smooth(aes(y = predict), method = "lm", formula = y ~ log(x), color = "darkgray", 
              linetype = "dashed", se = FALSE) +  # Plot model line
  #geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray", alpha = 0.4) +
  labs(x = "Mass (g)", y = "SGR (% mass increase/day)") +
  theme_classic() +
  scale_color_manual(values = c("limegreen", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 0, colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))


Fig1.1

#Plot residuals of SGR - mod_SGR predictions across mass values 
Fig1.2 <- gg_SMRvsSGR_resid <- ggplot(data = tb_sgr) +
  geom_point(aes(y = resid, x = Mass, colour = Category, shape = Stage), size = 3) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", size = 1) +
  labs(x = "Mass (g)", y = "SGR model residuals (% mass increase/day)") +
  scale_color_manual(values = c("limegreen", "orange", "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 0, colour = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))


Fig1.2

tb_data <- left_join(tb_data, (tb_sgr[which(tb_sgr$Stage != 1),] %>% select(FishID, Stage, predict, resid)), by = c("FishID", "Stage"))

#Display all MO2 vs. SGR data by month (including axis breaks for outliers)
Fig3.1.1 <- ggplot() +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
             aes(x = RMR_mass, y = resid, color = as.factor(Category)), size = 2.5) +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & tb_data$FishID == 29),],
             aes(x = RMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  geom_smooth(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
              aes(x = RMR_mass, y = resid), color = "black",linetype = "dashed", method = "lm") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ylim(-0.5, 0.5) +
  scale_color_manual(values = c("limegreen", "orange", "red", "gray"))+
  scale_y_continuous(breaks = seq(0.2, -0.4, by = -0.2), labels = function(x) round(x, 1)) +
  stat_cor(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),], aes(x = RMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 0.4, label.y = 0.19,               
           size = 4) +
  labs(y="SGR Residuals", x="RMR (mg/hr/6.06g)")
              
Fig3.1.2 <- ggplot(data = tb_data[which(tb_data$Stage == 2 & tb_data$FishID == 23),]) +
  geom_point(aes(x = RMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  theme_classic() + 
  ylim(-0.5, 0.5) +
  scale_x_continuous(
    breaks = 2,  # Specify only the break at 2.0
    labels = 2    # Customize label to indicate break
  ) +
  scale_color_manual(values = "gray")+
  labs(y="SGR Residuals", x="RMR (mg/hr/6.06g)") +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(),         # Add break label
        axis.line.x = element_line(color = "red"),
        axis.ticks.x = element_line(color = "red"),
        axis.text.x = element_text(color = "red"))

Fig3.1 <- ggarrange(Fig3.1.1 + rremove("ylab") + rremove("xlab"), 
                    Fig3.1.2 + rremove("ylab") + rremove("xlab"), 
                    nrow = 1, common.legend = TRUE, 
                    legend = "none", widths = c(0.9, 0.1))

Fig3.2 <- ggplot(data=tb_data[which(tb_data$Stage == 3),]) +
  geom_point(aes(x = RMR_mass, y = resid, color = as.factor(Category)), size = 2.5)+
  geom_smooth(aes(x = RMR_mass, y = resid), colour = "black", linetype = "dashed", method = "lm", size = 1) +
  theme_classic() +  
  scale_color_manual(values = c("limegreen", "orange", "red")) +
  stat_cor(aes(x = RMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 0.31, label.y = 0.4,                
           size = 4) +
  labs(y="SGR Residuals", x="RMR (mg/hr/6.06g)") 

Fig3.3.1 <- ggplot() +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
             aes(x = SMR_mass, y = resid, color = as.factor(Category)), size = 2.5) +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & tb_data$FishID == 29),],
             aes(x = SMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  geom_smooth(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
              aes(x = SMR_mass, y = resid), color = "black",linetype = "dashed", method = "lm") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ylim(-0.5, 0.5) +
  scale_color_manual(values = c("limegreen", "orange", "red", "gray"))+
  scale_y_continuous(breaks = seq(0.2, -0.4, by = -0.2), labels = function(x) round(x, 1)) +
  stat_cor(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),], aes(x = SMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 0.3, label.y = 0.19,               
           size = 4) +
  labs(y="SGR Residuals", x="SMR (mg/hr/6.06g)")

Fig3.3.2 <- ggplot(data = tb_data[which(tb_data$Stage == 2 & tb_data$FishID == 23),]) +
  geom_point(aes(x = SMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  theme_classic() + 
  ylim(-0.5, 0.5) +
  scale_x_continuous(
    breaks = 1.2,  # Specify only the break at 2.0
    labels = 1.2    # Customize label to indicate break
  ) +
  scale_color_manual(values = "gray")+
  labs(y="SGR Residuals", x="SMR (mg/hr/6.06g)") +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank(),         # Add break label
        axis.line.x = element_line(color = "red"),
        axis.ticks.x = element_line(color = "red"),
        axis.text.x = element_text(color = "red"))

Fig3.3 <- ggarrange(Fig3.3.1 + rremove("ylab") + rremove("xlab"), 
                    Fig3.3.2 + rremove("ylab") + rremove("xlab"), 
                    nrow = 1, common.legend = TRUE, 
                    legend = "none", widths = c(0.9, 0.1))

Fig3.4 <- ggplot(data=tb_data[which(tb_data$Stage == 3),]) +
  geom_point(aes(x = SMR_mass, y = resid, color = as.factor(Category)), size = 2.5)+
  geom_smooth(aes(x = SMR_mass, y = resid), colour = "black", linetype = "dashed", method = "lm", size = 1) +
  theme_classic() +  
  scale_color_manual(values = c("limegreen", "orange", "red")) +
  stat_cor(aes(x = SMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 0.2, label.y = 0.4,                 
           size = 4) +
  labs(y="SGR Residuals", x="SMR (mg/hr/6.06g)") 

Fig3.5 <- ggplot() +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
             aes(x = MMR_mass, y = resid, color = as.factor(Category)), size = 2.5) +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & !is.na(tb_data$Outlier)),],
             aes(x = MMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  geom_smooth(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
              aes(x = MMR_mass, y = resid), color = "black",linetype = "dashed", method = "lm") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ylim(-0.5, 0.5) +
  scale_color_manual(values = c("limegreen", "orange", "red", "gray"))+
  scale_y_continuous(breaks = seq(0.2, -0.4, by = -0.2), labels = function(x) round(x, 1)) +
  stat_cor(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),], aes(x = MMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 1.4, label.y = 0.19,               
           size = 4) +
  labs(y="SGR Residuals", x="MMR (mg/hr/6.06g)")
  
Fig3.6 <- ggplot(data=tb_data[which(tb_data$Stage == 3),]) +
  geom_point(aes(x = MMR_mass, y = resid, color = as.factor(Category)), size = 2.5)+
  geom_smooth(aes(x = MMR_mass, y = resid), colour = "black", linetype = "dashed", method = "lm", size = 1) +
  theme_classic() +  
  scale_color_manual(values = c("limegreen", "orange", "red")) +
  stat_cor(aes(x = MMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 1, label.y = 0.4,                 
           size = 4) +
  labs(y="SGR Residuals", x="MMR (mg/hr/6.06g)") 

theme1 <- theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
theme2 <- theme(plot.margin = unit(c(0.1,0.1,0.3,0), "cm"))

Fig3.1 <- Fig3.1 + theme1
Fig3.3 <- Fig3.3 + theme1
Fig3.5 <- Fig3.5 + theme2

# Arrange plots in the first row with custom widths
top_row <- ggarrange(
  print(Fig3.5 + rremove("ylab") + rremove("xlab")), 
  print(Fig3.1 + rremove("ylab") + rremove("xlab")), 
  print(Fig3.3 + rremove("ylab") + rremove("xlab")), 
  ncol = 3, labels = c("A", "B", "C"), 
  widths = c(0.28, 0.31, 0.325), heights = c(0.2, 0.5, 0.5),
  align = "v", label.y = 0.97, label.x = c(0.1, 0.17, 0.17),
  common.legend = TRUE, legend = "top")

# Arrange plots in the second row
bottom_row <- ggarrange(
  print(Fig3.6 + rremove("ylab") + rremove("xlab")), 
  print(Fig3.2 + rremove("ylab") + rremove("xlab")), 
  print(Fig3.4 + rremove("ylab") + rremove("xlab")), 
  ncol = 3, labels = c("D", "E", "F"), 
  widths = c(0.29, 0.29, 0.28), 
  align = "v", label.y = 1.01, label.x = c(0.1, 0.1, 0.1), 
  common.legend = TRUE, legend = "none")

# Combine both rows into a single ggarrange call
MR_SGR_months <- ggarrange(top_row, bottom_row, nrow = 2, common.legend = TRUE, legend = "right")

# Add annotations
require(grid)
Fig3 <- annotate_figure(MR_SGR_months, 
                left = textGrob("SGR Residuals (% mass increase/day)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)), 
                bottom = textGrob("MR (mg/hr/6.06g)", gp = gpar(cex = 1.1)))

Fig3

