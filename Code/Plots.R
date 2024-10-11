# Canadian Okanagan Chinook
# Figures for FSRR Report


library(tidyverse)
library(patchwork)
library(scales)
library(ggplot2)
library(ggpubr)

# Read in escapement data

EscData <- read.csv("DataIn/Ok Chinook AUC Abundance.csv")
# if want to be able to plot hatchery too, need to turn tidy
EscDataLong <- EscData %>% pivot_longer(cols = c("Natural", "Hatchery", "MRC_Est"), names_to = "Type", values_to = "Spawners")

# Read in CYER data

CYERData <- read.csv("DataIn/Ok Chinook CYER AEQ.csv")
CYERDataLong <-  CYERData %>% pivot_longer(cols = c("Canada", "US"), names_to = "Country", values_to = "CYER")




# Code for the FSAR 4 panel plots

## Vector of SMUs/CU 

  catch <- 
    ggplot() +
    theme_void() +
    geom_text(aes(0,0,label='Not Available')) +
    xlab(NULL)  +
    labs(y = "Catch") +
    theme_classic() +
    theme(axis.text = element_blank(), 
          axis.title.y = element_text(size = 9),
          axis.ticks = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8))
  

  esc <- EscData %>%
    ggplot(aes(x = Year)) +
    geom_line(aes(y = Natural), linewidth = 0.5) +
    geom_point(aes(y = Natural)) +
    labs(y = "Natural-Origin Spawner Abundance", x = "Year") +
    theme_classic() +
    theme(axis.text = element_text(size = 8), 
          axis.title = element_text(size = 9),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8))
  
  esc2 <- 
     ggplot(data =EscDataLong %>% filter(Type %in% c("Natural", "Hatchery")),  aes(x = Year, y=Spawners, fill = Type)) +
     geom_bar(stat = "identity", color="black") +
     #geom_point(data = EscData, aes(x=Year, y=MRC_Est)) + # I don't know why this isnt' working
     theme_classic() +
     scale_fill_grey(start = 0.1, end = .9) +
     theme(axis.text = element_text(size = 8), 
           axis.title = element_text(size = 9),
           panel.border = element_rect(colour = "black", fill=NA, size=0.8),
           legend.position = c(0.15, 0.85),
           legend.background=element_blank(),
           legend.title = element_text( size=6), legend.text=element_text(size=6))
    
  
  # doesn't seem straightforward to add MRC estimate? Do I bother?
  
  CYER <- CYERData %>%
    ggplot(aes(x = Year)) +
    geom_line(aes(y = Total), linewidth = 0.5) +
    geom_point(aes(y = Total)) +
    labs(y = "Total AEQ-CYER", x = "Year") +
    theme_classic() +
    theme(axis.text = element_text(size = 8), 
          axis.title = element_text(size = 9),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8))
  
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
          legend.position = c(0.15, 0.85),
          legend.title = element_text( size=6), legend.text=element_text(size=6)) 
  
  recruits <- ggplot() +
    theme_void() +
    geom_text(aes(0,0,label='Not Available')) +
    xlab(NULL)  +
    labs(y = "Recruits") +
    theme_classic() +
    theme(axis.text = element_blank(), 
          axis.title = element_text(size = 9),
          axis.ticks = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8))
  
  ggarrange(catch, esc, CYER, recruits, nrow = 2, ncol = 2, align = "hv", common.legend = TRUE)
  
  ggsave("Plots/Four_Panel_Simple.jpg", units = "in", height = 5, width = 7.5, dpi = 500)
  
  ggarrange(catch, esc2, CYER2, recruits, nrow = 2, ncol = 2, align = "hv")
  
  ggsave("Plots/Four_Panel_Bars.pdf", units = "in", height = 5, width = 7.5, dpi = 1000)
  
  # Figure for presentation of just escapement
  
  

    ggplot(data =EscDataLong %>% filter(Type %in% c("Natural", "Hatchery")),  aes(x = Year, y=Spawners, fill = Type)) +
    geom_bar(stat = "identity", color="black") +
    #geom_point(data = EscData, aes(x=Year, y=MRC_Est)) + # I don't know why this isnt' working
    theme_classic() +
    scale_fill_grey(start = 0.1, end = .9) +
    theme(axis.text = element_text(size = 14), 
          axis.title = element_text(size = 14),
          panel.border = element_rect(colour = "black", fill=NA, size=0.8),
          legend.position = c(0.15, 0.85),
          legend.background=element_blank(),
          legend.title = element_text( size=20), legend.text=element_text(size=20))
    
    CYERDataLong %>%
      ggplot(aes(x = Year, y=CYER, fill = Country)) +
      geom_bar(stat = "identity", color="black") +
      labs(y = "CYER (Adult Equivalents)", x = "Year") + 
      scale_fill_grey(start = 0.1, end = .9) +
      theme_classic() +
      theme(axis.text = element_text(size = 14), 
            axis.title = element_text(size = 14),
            panel.border = element_rect(colour = "black", fill=NA, size=0.8),
            legend.background=element_blank(),
            legend.position = c(0.15, 0.85),
            legend.title = element_text( size=20), legend.text=element_text(size=20)) 
  
  