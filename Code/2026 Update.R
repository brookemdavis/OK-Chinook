# Updates for 2026 Rebuilding Plan rupdate

Abund_Data <- read.csv("DataIn/OK Chinook AUC Abundance.csv", fileEncoding="UTF-8-BOM")
CYER_Data <- read.csv("DataIn/SMK_CYER.csv")


library(tidyverse)
library(ggpubr)

years <- 2020:2025
Pchange.ln.WSP <- NULL
for(yy in 1:length(years)){
  
  Dat3Gen <- Abund_Data %>% filter(Year %in% c((years[yy]-11):years[yy]))
  
  lm<-lm(log(Dat3Gen$Natural) ~ Dat3Gen$Year)
  y<-exp(predict(lm,as.data.frame(Dat3Gen$Year)))
  Pchange.ln.WSP[yy] <- ((y[n]-y[1])/y[1])*100
}
data.frame(years, Pchange.ln.WSP)

# 2025 percent change metric -78%

# Figure from FSAR

# if want to be able to plot hatchery too, need to turn tidy
EscDataLong <- Abund_Data %>% pivot_longer(cols = c("Natural", "Hatchery", "MRC_Est"), names_to = "Type", values_to = "Spawners")

# Read in CYER data

CYERDataLong <-  CYER_Data %>% pivot_longer(cols = c("Canada", "US"), names_to = "Country", values_to = "CYER")

# Code for the FSAR 4 panel plots

## Vector of SMUs/CU 

esc2 <- 
  ggplot(data =EscDataLong %>% filter(Type %in% c("Natural", "Hatchery")),  aes(x = Year, y=Spawners, fill = Type)) +
  geom_bar(stat = "identity", color="black") +
  #geom_point(data = EscData, aes(x=Year, y=MRC_Est)) + # I don't know why this isnt' working
  theme_classic() +
  scale_fill_grey(start = 0.1, end = .9) +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 9),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.8),
        legend.position = c(0.15, 0.85),
        legend.background=element_blank(),
        legend.title = element_text( size=6), legend.text=element_text(size=6))



CYER2 <- CYERDataLong %>%
  ggplot(aes(x = Year, y=CYER, fill = Country)) +
  geom_bar(stat = "identity", color="black") +
  labs(y = "CYER (Adult Equivalents)", x = "Year") + 
  scale_fill_grey(start = 0.1, end = .9) +
  theme_classic() +
  theme(axis.text = element_text(size = 8), 
        axis.title = element_text(size = 9),
        panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        legend.background=element_blank(),
        legend.position = c(0.85, 0.85),
        legend.title = element_text( size=6), legend.text=element_text(size=6)) 

ggarrange(esc2, CYER2, nrow = 1, ncol = 2, align = "hv")

ggsave("Plots/Two_Panel_2026.pdf", units = "in", height = 4, width = 7.5, dpi = 1000)
ggsave("Plots/Two_Panel_2026.png", units = "in", height = 4, width = 7.5, dpi = 1000)
