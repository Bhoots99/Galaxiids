# Title: "Social and metabolic mediation of growth performance in a temperate estuarine fish"
# Author: E. Hoots
# Last updated: 12-08-2024

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
tb_months <- read_excel("3 - Growth and MO2 Rates.xlsx", sheet = "1.MR_Growth") %>%
  mutate(
    Category = ifelse(Category == "MED", "INT.", Category)
  )

tb_sgr <- read_excel("3 - Growth and MO2 Rates.xlsx", sheet = "2.SGR_Long") %>%
  mutate(
    Stage = as.factor(Stage),
    Category = as.factor(ifelse(Category == "MED", "INT.", Category))
  )

tb_data <- read_excel("3 - Growth and MO2 Rates.xlsx", sheet = "3.MR_Growth_Long") %>%
  mutate(
    Stage = as.factor(Stage),
    Category = ifelse(Category == "MED", "INT.", Category)
  )

tb_tankdata <- read_excel("Growth and Resp Master Table.xlsx", sheet = "All growth data") 
tb_tankdata <- tb_tankdata[,1:7] %>%
  mutate(
    Date = as.Date(Date)
  )

tb_tankdata_stage1 <- tb_tankdata[which(tb_tankdata$Date == "2022-09-07 UTC"),]
tb_tankdata_stage2 <- tb_tankdata[which(tb_tankdata$Date == "2022-11-22 UTC" | 
                                        tb_tankdata$Date == "2022-11-23 UTC" | 
                                        tb_tankdata$Date == "2022-11-24 UTC"),]


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

tb_sgr <- tb_sgr %>%
  left_join(tb_tankdata_stage1 %>% select(FishID, Tag_location, Tank), by = "FishID") %>%
  rename(
    Tank_Stage3 = Tank
  ) %>%
  left_join(tb_tankdata_stage2 %>% select(FishID, Tank), by = "FishID") %>%
  rename(
    Tank_Stage2 = Tank
  ) %>%
  mutate(
    Tag_location = as.factor(Tag_location),
    Tank_Stage2 = as.factor(Tank_Stage2),
    Tank_Stage3 = as.factor(Tank_Stage3),
    Tank_all = if_else(Stage == 2, Tank_Stage2, Tank_Stage3)
  ) %>%
  mutate(
    Tank = as.factor(Tank_all),
    Tank_all = NULL,
    Category = as.factor(Category),
    fill_var = factor(ifelse(Tag_location == "none", NA, as.character(Category))))

tb_data <- tb_data %>%
  left_join(tb_tankdata_stage1 %>% select(FishID, Tag_location, Tank), by = "FishID") %>%
  rename(
    Tank_Stage3 = Tank
  ) %>%
  left_join(tb_tankdata_stage2 %>% select(FishID, Tank), by = "FishID") %>%
  rename(
    Tank_Stage2 = Tank
  ) %>%
  mutate(
    Tag_location = as.factor(Tag_location),
    Tank_Stage2 = as.factor(Tank_Stage2),
    Tank_Stage3 = as.factor(Tank_Stage3)
  )

#Plot SGR vs. mass with model line and confidence interval
Fig1.1 <- ggplot(tb_sgr, aes(x = Mass, y = SGR)) +
  geom_point(aes(color = factor(Category), shape = factor(Stage), 
                 fill = fill_var), 
             size = 3) +  # Plot original data points
  geom_smooth(aes(y = predict), method = "lm", formula = y ~ log(x), color = "darkgray", 
              linetype = "dashed", se = FALSE) +  # Plot model line
  labs(x = "Mass (g)", y = SGR~("%"~mass~increase~day^-1)) +
  theme_classic() +
  scale_shape_manual(values = c(21, 24, 22)) +  
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c")) +
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
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
  geom_point(aes(y = resid, x = Mass, colour = Category, shape = Stage, 
                 fill = fill_var), size = 3) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", size = 1) +
  labs(x = "Mass (g)", y = expression(SGR~model~residuals~("%"~mass~increase~day^-1))) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c")) +
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
  scale_shape_manual(values = c(21, 24, 22)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 17, colour = "black"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))


Fig1.2

ggarrange(
  Fig1.1 + theme(legend.position = "none"), 
  Fig1.2,
  widths = c(1, 1.2))

tb_data <- left_join(tb_data, (tb_sgr[which(tb_sgr$Stage != 1),] %>% select(FishID, Stage, predict, resid)), by = c("FishID", "Stage"))

#Display all MO2 vs. SGR data by month (including axis breaks for outliers)
Fig3.1.1 <- ggplot() +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
             aes(x = RMR_mass, y = resid, color = as.factor(Category), fill = ifelse(Tag_location == "none", NA, Category)), shape = 21, size = 2.5) +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & tb_data$FishID == 29),],
             aes(x = RMR_mass, y = resid, color = "OUTLIER"), shape = 21, size = 2.5, fill = "gray") +
  geom_smooth(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
              aes(x = RMR_mass, y = resid), color = "black",linetype = "dashed", method = "lm") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12, colour = "black")) +
  ylim(-0.5, 0.5) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c", "gray"))+
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
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
  geom_point(aes(x = RMR_mass, y = resid, color = as.factor(Category), 
                 fill = ifelse(Tag_location == "none", NA, Category)), size = 2.5, shape = 21)+
  geom_smooth(aes(x = RMR_mass, y = resid), colour = "black", linetype = "dashed", method = "lm", size = 1) +
  theme_classic() +  
  theme(axis.text = element_text(size = 12, colour = "black")) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c")) +
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
  stat_cor(aes(x = RMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 0.31, label.y = 0.4,                
           size = 4) +
  labs(y="SGR Residuals", x="RMR (mg/hr/6.06g)") 

Fig3.3.1 <- ggplot() +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
             aes(x = SMR_mass, y = resid, color = as.factor(Category), 
                 fill = ifelse(Tag_location == "none", NA, Category)), size = 2.5, shape = 21) +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & tb_data$FishID == 29),],
             aes(x = SMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  geom_smooth(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
              aes(x = SMR_mass, y = resid), color = "black",linetype = "dashed", method = "lm") +
  theme_classic() +
  theme(legend.title = element_blank(), 
        axis.text = element_text(size = 12, colour = "black")) +
  ylim(-0.5, 0.5) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c", "gray"))+
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
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
  geom_point(aes(x = SMR_mass, y = resid, color = as.factor(Category), 
                 fill = ifelse(Tag_location == "none", NA, Category)), size = 2.5, shape = 21)+
  geom_smooth(aes(x = SMR_mass, y = resid), colour = "black", linetype = "dashed", method = "lm", size = 1) +
  theme_classic() +  
  theme(axis.text = element_text(size = 12, colour = "black")) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c")) +
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
  stat_cor(aes(x = SMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 0.2, label.y = 0.4,                 
           size = 4) +
  labs(y="SGR Residuals", x="SMR (mg/hr/6.06g)") 

Fig3.5 <- ggplot() +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
             aes(x = MMR_mass, y = resid, color = as.factor(Category), 
                 fill = ifelse(Tag_location == "none", NA, Category)), size = 2.5, shape =21) +
  geom_point(data = tb_data[which(tb_data$Stage == 2 & !is.na(tb_data$Outlier)),],
             aes(x = MMR_mass, y = resid, color = "OUTLIER"), size = 2.5) +
  geom_smooth(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),],
              aes(x = MMR_mass, y = resid), color = "black",linetype = "dashed", method = "lm") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12, colour = "black")) +
  ylim(-0.5, 0.5) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c", "gray"))+
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
  scale_y_continuous(breaks = seq(0.2, -0.4, by = -0.2), labels = function(x) round(x, 1)) +
  stat_cor(data = tb_data[which(tb_data$Stage == 2 & is.na(tb_data$Outlier)),], aes(x = MMR_mass, y = resid), 
           method="pearson",                          
           r.accuracy = 0.01,
           label.x = 1.4, label.y = 0.19,               
           size = 4) +
  labs(y="SGR Residuals", x="MMR (mg/hr/6.06g)")
  
Fig3.6 <- ggplot(data=tb_data[which(tb_data$Stage == 3),]) +
  geom_point(aes(x = MMR_mass, y = resid, color = as.factor(Category), 
                 fill = ifelse(Tag_location == "none", NA, Category)), size = 2.5, shape = 21)+
  geom_smooth(aes(x = MMR_mass, y = resid), colour = "black", linetype = "dashed", method = "lm", size = 1) +
  theme_classic() +  
  theme(axis.text = element_text(size = 12, colour = "black")) +
  scale_color_manual(values = c("#66c2a5", "orange", "#e41a1c")) +
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c"), na.value = "white", guide = "none") +
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
  ncol = 3, labels = c("A - MMR", "B - RMR", "C - SMR"), 
  widths = c(0.28, 0.31, 0.325), heights = c(0.2, 0.5, 0.5),
  align = "v", label.y = 0.96, label.x = c(0.03, 0.13, 0.132),
  common.legend = TRUE, legend = "top")

# Arrange plots in the second row
bottom_row <- ggarrange(
  print(Fig3.6 + rremove("ylab") + rremove("xlab")), 
  print(Fig3.2 + rremove("ylab") + rremove("xlab")), 
  print(Fig3.4 + rremove("ylab") + rremove("xlab")), 
  ncol = 3, labels = c("D - MMR", "E - RMR", "F - SMR"), 
  widths = c(0.29, 0.29, 0.28), 
  align = "v", label.y = 0.96, label.x = c(0.03, 0.04, 0.04), 
  common.legend = TRUE, legend = "none")

# Combine both rows into a single ggarrange call
MR_SGR_months <- ggarrange(top_row, bottom_row, nrow = 2, common.legend = TRUE, legend = "right")

# Add annotations
require(grid)
Fig3 <- annotate_figure(MR_SGR_months, 
                left = textGrob(expression(SGR~Residuals~("%"~mass~increase~"*"~day^-1)), rot = 90, vjust =0.25, gp = gpar(cex = 1.1)), 
                bottom = textGrob(expression(Metabolic~rate~(mg~O[2]~"*"~h^-1~"*"~6.06~g^-1)), gp = gpar(cex = 1.1)))

Fig3


### Growth Performance Analysis
library(multcomp)
library(multcompView)

tank_labels <- c(
  "2.01" = "T1",
  "2.02" = "T2",
  "2.12" = "T3",
  "2.11" = "T4",
  "2.13" = "T5",
  "2.14" = "T6",
  "2.15" = "T7"
)

tb_sgr$Tank <- tank_labels[tb_sgr$Tank]
#tb_sgr$Tank_Stage3 <- tank_labels[tb_sgr$Tank_Stage3]
#tb_sgr$Tank_Stage2 <- tank_labels[tb_sgr$Tank_Stage2]


okabe_ito_colors <- c(
  "T4" = "#0072B2",  # Blue
  "T2" = "#e41a1c",  # Vermilion
  "T3" = "orange",  # Orange
  "T1" = "#66c2a5",  # Bluish Green
  "T5" = "#F0E442",  # Yellow
  "T6" = "#56B4E9",  # Sky Blue
  "T7" = "#CC79A7"   # Reddish Purple
)

growth_plot <- ggplot(data = tb_sgr) +
  geom_boxplot(aes(x = as.factor(Stage), y = SGR, fill = Category), col = "black") + 
  labs(y = "SGR (%/day)", x = "Stage") +
  scale_fill_manual(values = c("#66c2a5", "orange", "#e41a1c")) +
  theme_classic() +
  theme(axis.text = element_text(size = 17, colour = "black"), 
      axis.title = element_text(size =20, colour = "black"), 
      legend.text = element_text(size=17, colour = "black"),
      legend.title = element_text(size = 17, colour = "black"),
      legend.position = "right",
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      rect=element_rect(fill = "white"),
      strip.text = element_text(colour = "white"))

sgr_mod <- aov(SGR ~ Stage * Category, data = tb_sgr)
summary(sgr_mod)

sgr_tukey <- TukeyHSD(sgr_mod)
print(sgr_tukey)
sgr_tukey_cld <- multcompLetters4(sgr_mod, sgr_tukey)
print(sgr_tukey_cld)

cld <- as.data.frame.list(sgr_tukey_cld$`Stage:Category`)

cld$Stage <- sapply(strsplit(row.names(cld), ":"), `[`, 1)
cld$Category <- sapply(strsplit(row.names(cld), ":"), `[`, 2)
cld$Letters <- cld$Letters

FigANOVA <- growth_plot + 
  geom_text(data = cld, aes(x = Stage, y = max(tb_sgr$SGR) + 0.1, label = Letters, group = Category),
            size = 5, vjust = -0.5, position = position_dodge(width = 0.75))

FigANOVA

#### By stage/tank individual plots

growth_plot_Stage1 <- ggplot(data = tb_sgr[which(tb_sgr$Stage == 1),]) +
  geom_boxplot(aes(x = Tank, y = SGR, fill = Tank), col = "black") + 
  labs(y = "SGR (%/day)", x = "Tank - Stage 1") +
  theme_classic() +
  ylim(-0.5, 1.9) +
  scale_fill_manual(values = okabe_ito_colors) +
  theme(axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 17, colour = "black"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))

growth_plot_Stage2 <- ggplot(data = tb_sgr[which(tb_sgr$Stage == 2),]) +
  geom_boxplot(aes(x = Tank, y = SGR, fill = Tank), col = "black") + 
  labs(y = "SGR (%/day)", x = "Tank - Stage 2") +
  theme_classic() +
  ylim(-0.5, 1.9) +
  scale_fill_manual(values = okabe_ito_colors) +
  theme(axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 17, colour = "black"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))

growth_plot_Stage3 <- ggplot(data = tb_sgr[which(tb_sgr$Stage == 3),]) +
  geom_boxplot(aes(x = Tank, y = SGR, fill = Tank), col = "black") + 
  labs(y = "SGR (%/day)", x = "Tank - Stage 3") +
  theme_classic() +
  ylim(-0.5, 1.9) +
  scale_fill_manual(values = okabe_ito_colors) +
  theme(axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 17, colour = "black"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))

growth_plot_Tanks <- ggplot(data = tb_sgr) +
  geom_boxplot(aes(x = Stage, y = SGR, fill = Tank), col = "black") + 
  labs(y = "SGR (%/day)", x = "Stage") +
  theme_classic() +
  scale_fill_manual(values = okabe_ito_colors) +
  theme(axis.text = element_text(size = 17, colour = "black"), 
        axis.title = element_text(size =20, colour = "black"), 
        legend.text = element_text(size=17, colour = "black"),
        legend.title = element_text(size = 17, colour = "black"),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        rect=element_rect(fill = "white"),
        strip.text = element_text(colour = "white"))

# ANOVA for Stage 1 

sgrS1_mod <- aov(SGR ~ Tank, data = tb_sgr[which(tb_sgr$Stage == 1),])
summary(sgrS1_mod)

sgrS1_tukey <- TukeyHSD(sgrS1_mod)
print(sgrS1_tukey)
sgrS1_tukey_cld <- multcompLetters4(sgrS1_mod, sgrS1_tukey)
print(sgrS1_tukey_cld)

cld_S1 <- as.data.frame.list(sgrS1_tukey_cld$`Tank`)

cld_S1$Tank <- row.names(cld_S1)
cld_S1$Letters <- cld_S1$Letters

FigANOVA_Stage1 <- growth_plot_Stage1 + 
  geom_text(data = cld_S1, aes(x = Tank, y = max(tb_sgr[which(tb_sgr$Stage == 1),]$SGR) + 0.1, label = Letters),
            size = 5, vjust = -0.5, position = position_dodge(width = 0.75))

FigANOVA_Stage1

#ANOVA for Stage 2

sgrS2_mod <- aov(SGR ~ Tank, data = tb_sgr[which(tb_sgr$Stage == 2),])
summary(sgrS2_mod)

sgrS2_tukey <- TukeyHSD(sgrS2_mod)
print(sgrS2_tukey)
sgrS2_tukey_cld <- multcompLetters4(sgrS2_mod, sgrS2_tukey)
print(sgrS2_tukey_cld)

cld_S2 <- as.data.frame.list(sgrS2_tukey_cld$`Tank`)

cld_S2$Tank <- row.names(cld_S2)
cld_S2$Letters <- cld_S2$Letters

FigANOVA_Stage2 <- growth_plot_Stage2 + 
  geom_text(data = cld_S2, aes(x = Tank, y = max(tb_sgr[which(tb_sgr$Stage == 1),]$SGR) + 0.1, label = Letters),
            size = 5, vjust = -0.5, position = position_dodge(width = 0.75))

FigANOVA_Stage2

#ANOVA for Stage 3

sgrS3_mod <- aov(SGR ~ Tank, data = tb_sgr[which(tb_sgr$Stage == 3),])
summary(sgrS3_mod)

sgrS3_tukey <- TukeyHSD(sgrS3_mod)
print(sgrS3_tukey)
sgrS3_tukey_cld <- multcompLetters4(sgrS3_mod, sgrS3_tukey)
print(sgrS3_tukey_cld)

cld_S3 <- as.data.frame.list(sgrS3_tukey_cld$`Tank`)

cld_S3$Tank <- row.names(cld_S3)
cld_S3$Letters <- cld_S3$Letters

FigANOVA_Stage3 <- growth_plot_Stage3 + 
  geom_text(data = cld_S3, aes(x = Tank, y = max(tb_sgr[which(tb_sgr$Stage == 1),]$SGR) + 0.1, label = Letters),
            size = 5, vjust = -0.5, position = position_dodge(width = 0.75))

FigANOVA_Stage3

legend <- get_legend(FigANOVA_Stage2)

ggarrange(
  FigANOVA_Stage1 + rremove("xlab"), 
  FigANOVA_Stage2 + rremove("ylab") + rremove("xlab"), 
  FigANOVA_Stage3 + rremove("ylab") + rremove("xlab"), 
  legend,
  ncol = 4)

ggarrange(
  FigANOVA_Stage1 + rremove("xlab") + theme(legend.position = "none"), 
  FigANOVA_Stage2 + rremove("ylab") + rremove("xlab") + theme(legend.position = "none"), 
  FigANOVA_Stage3 + rremove("ylab") + rremove("xlab") + theme(legend.position = "none"), 
  legend,  # Add the extracted legend
  ncol = 4,  # 3 plots + 1 legend
  labels = c("Stage 1", "Stage 2", "Stage 3", ""),
  widths = c(1, 1, 1, 0.2),  # Adjust the width of the legend
  align = "v",
  label.x = 0.15)


#### Growth Performance Data

library(readxl)

tb_behav <- read_excel("4 - Behavior Data")

tb_behav <- tb_behav %>%
  mutate(
    BPM = BurstCount/MinsWatched,
    BPM_chase = ChaseCount/MinsWatched,
    Iso = as.factor(`Iso?`),
    `Iso?` = NULL,
    `%_Chase` = as.numeric(`%_Chase`),
    Bin = as.factor(Bin),
    Date = as.Date(Date)
  )

FastSlow_test <- t.test(tb_behav[which(tb_behav$Bin == 1),]$BPM, tb_behav[which(tb_behav$Bin == 2),]$BPM, paired = FALSE)

print(FastSlow_test)
