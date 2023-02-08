#### Pearson correlation test on growth rates vs carrying capacities values ####
#
# Create the plots of Pearson correlation tests as presented in Bonal et al, 2022
#
# Authors: Mathias Bonal, with revisions from Karoline Faust
#
# Last modified: 06/02/2023


 

# Set of working directory
setwd("~/R/Data/stats_mu_K")

# Load packages
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggsci)
library(ggsignif)
library(gridExtra)
library(readxl)



#### Compute Pearson correlation test for growth rate in monoculture vs carrying capacity in bi-culture (Fig. 4a) ####

# Load data
mu_K_co <- read_excel("data_for_Pearson.xlsx", sheet="co_cultures")


# Compute Pearson correlation test including colors + shapes + error bars
plot_mu_K_co <- ggscatter(mu_K_co, x = "mu.mono", y = "lnK.co",                    
                          add = "reg.line", conf.int = TRUE, cor.coef.size = 5, # correlation line + interaction coef
                          cor.coef = TRUE, cor.method = "pearson", cor.coef.coord=c(0.25,25), # test = Pearson
                          color="strain", shape="culture", size=4,    # color by strain and shape by type of culture
                          palette=c("darkorange2","gray47","green3","purple4"),
                          add.params=list(color="black", fill="lightgray", shape=18), 
                          xlim=c(0.2,0.67), ylim=c(12,26),    # add limits for x and y axes
                          xlab = expression(bold(paste("growth rate in monoculture (", h^-1, ")",sep=""))),
                          ylab = "carrying capacity in co-culture [ln(cells/mL)]") +
  geom_point(fill=c("darkorange2","gray47","green3","purple4", "darkorange2","gray47","green3","purple4",
                    "darkorange2","gray47","green3","purple4", "darkorange2","gray47","green3","purple4"), alpha=5) +
  
  # choose shapes to be displayed
  scale_shape_manual(
    values = c(5, 7, 25, 15, 16, 17, 11)) +   
  
  # set vertical error bars
  geom_errorbar(aes(x=mu.mono, y=lnK.co, ymin=lnK.co-sdK, ymax=lnK.co+sdK),    
                width=0.0025, colour="cornflowerblue", alpha=0.7, size=0.5) +  
  
  # set horizontal error bars
  geom_errorbarh(aes(xmin=mu.mono-sdmu, xmax=mu.mono+sdmu), colour="cornflowerblue", alpha=0.7, size=0.5) +
  
  # x and y axes labels + legend
  theme(
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.position = c(1,0.01),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

plot_mu_K_co

#ggsave(file="plot_mu_K_co.png", plot=plot_mu_K_co, width=15, height=7.5)




#### Compute Pearson correlation test for growth rate vs carrying capacity in all co-cultures + 20-sp mix (Fig. 4b) ####


# Load data
co_means <- read_excel("data_for_Pearson.xlsx", sheet="bi_quadri_mix")


# Compute Pearson correlation test including colors + shapes + error bars
plot_co_means <- ggscatter(co_means, x = "mu", y = "lnK",
                           add = "reg.line", conf.int = TRUE, cor.coef.size = 5,
                           cor.coef = TRUE, cor.method = "pearson", cor.coef.coord=c(0.25,25),
                           color="strain", shape="culture",
                           palette=c("darkorange2","gray47","green3","purple4"),
                           add.params=list(color="black", fill="lightgray", shape=18),
                           xlim=c(0.1,0.6), ylim=c(12,26),
                           xlab = expression(bold(paste("growth rate (", h^-1, ")",sep=""))),
                           ylab = "carrying capacity [ln(cells/mL)]") +
  geom_errorbar(aes(x=mu, y=lnK, ymin=lnK-sdK, ymax=lnK+sdK), 
                width=0.0025, colour="cornflowerblue", alpha=0.7, size=0.5) +
  geom_errorbarh(aes(xmin=mu-sdmu, xmax=mu+sdmu), colour="cornflowerblue", alpha=0.7, size=0.5) +
  theme(
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.position = c(1,0.01),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))

plot_co_means




#### Arrange both graphs to create Fig. 4 ####

# Put both graphs on same figure
two_graphs <- ggarrange(plot_mu_K_co, plot_co_means, ncol = 2, nrow = 1)
two_graphs

# Add a and b panels
final_graphs <- annotate_figure(two_graphs, top = text_grob(c("a", "b"), size=20, face="bold", x=c(0.07, 0.57)))
final_graphs

ggsave(file="final_graphs_Pearson.png", plot=final_graphs, width=15, height=7.5)
