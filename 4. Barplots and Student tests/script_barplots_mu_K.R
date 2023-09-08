#### Barplots comparing growth rates and yields in each type of culture
#
# Create the barplots comparing growth rates (mu) and carrying capacity (K) values as presented 
#
# in Bonal et al, 2023
#
#
# Authors: Mathias Bonal, with revisions from Karoline Faust
#
# Last modified: 07/09/2023





# Introductory steps ----

# Set of working directory
setwd("~/R/Data/cultures")


# Load packages
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggsignif)
library(ggsci)
library(cowplot)
library(gridExtra)
library(readxl)


# Load data
s78R <- read_excel("mu_and_K_per_culture.xlsx", sheet="78R")
s124Z <- read_excel("mu_and_K_per_culture.xlsx", sheet="124Z")
s515Z <- read_excel("mu_and_K_per_culture.xlsx", sheet="515Z")
s1315Z <- read_excel("mu_and_K_per_culture.xlsx", sheet="1315Z")




# Label vector ----

# Create label vector for x axis
labels_barplots <- c("mono", "bi", "quadri", "mix")




# Barplots mu ----

# Create barplots for growth rates (Fig. 5a). The sgnificance arcs are built based on values of
# Student unpaired tests to assess whether mean values of mu/K for each strain were significantly
# different between mono-, bi-, quadri-culture and 20-strain mix. Detail of computed values of 
# each test can be found in the file "script_Student_barplots.R"


# Barplot of 78R
plot_78R_mu <- ggbarplot(s78R, x="culture", y="mu", xlab=F, ylab=F, ylim=c(0,0.8), color="darkorchid", fill="darkorchid") +
  
  
  # set error bars
  geom_errorbar(aes(x=culture, ymin=mu-stdev.mu, ymax=mu+stdev.mu), width=0.1, colour="black", alpha=0.7, size=0.5) +
  
  
  # set significance arcs (only significant arcs are plotted, i.e. unpaired Student test giving a significant
  # p-value; 1 star for p < 0.05, 2 stars for p < 0.01, 3 stars for p < 0.001)
  #
  # set coordinates of significance arcs so that they do not overlap
  geom_signif(y_position=0.62, xmin=1, xmax=2, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "bi"), y_position=0.62, tip_length=0.05, vjust=0.2, ) +
  geom_signif(y_position=0.68, xmin=1, xmax=3, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "quadri"), y_position=0.68, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=0.74, xmin=1, xmax=4, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "mix"), y_position=0.74, tip_length=0.05, vjust=0.2) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  
  
  # set x axis labels with the created vector
  scale_x_discrete(labels=labels_barplots)


# Barplots of the remaining strains
plot_124Z_mu <- ggbarplot(s124Z, x="culture", y="mu", xlab=F, ylab=F, ylim=c(0,0.6), color="darkorange", fill="darkorange") +
  geom_errorbar(aes(x=culture, ymin=mu-stdev.mu, ymax=mu+stdev.mu), width=0.1, colour="black", alpha=0.7, size=0.5) +
    theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots)


plot_515Z_mu <- ggbarplot(s515Z, x="culture", y="mu", xlab=F, ylab=F, ylim=c(0,0.6), color="darkgreen", fill="darkgreen") +
  geom_errorbar(aes(x=culture, ymin=mu-stdev.mu, ymax=mu+stdev.mu), width=0.1, colour="black", alpha=0.7, size=0.5) +
  geom_signif(y_position=0.5, xmin=1, xmax=3, annotation="*", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "quadri"), y_position=0.5, tip_length=0.05, vjust=0.2) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots)

plot_1315Z_mu <- ggbarplot(s1315Z, x="culture", y="mu", xlab=F, ylab=F, ylim=c(0,0.6), color="gray47", fill="gray47") + 
  geom_errorbar(aes(x=culture, ymin=mu-stdev.mu, ymax=mu+stdev.mu), width=0.1, colour="black", alpha=0.7, size=0.5) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots) 


# Arrange the four barplots for mu
barplots_mu <- ggarrange(plot_78R_mu, plot_124Z_mu, plot_515Z_mu, plot_1315Z_mu,
                         labels = c("78R_mu", "124Z_mu", "515Z_mu", "1315Z_mu"),
                         label.x=0.55, label.y=1,
                         ncol = 2, nrow = 2)
barplots_mu


# Add x and y axes labels
barplots_mu_leg <- annotate_figure(barplots_mu,
                                   bottom = text_grob("culture", size = 15, face="bold"),
                                   left=ggpubr::text_grob(bquote(bold("growth rate ("*h^-1*")")), 
                                                          rot = 90, size = 15))
barplots_mu_leg




# Barplots K ----

# Create barplots for carrying capacities (Fig. 5b)

plot_78R_K <- ggbarplot(s78R, x="culture", y="K", xlab=F, ylab=F, ylim=c(0,25), color="darkorchid", fill="darkorchid") +
  geom_errorbar(aes(x=culture, ymin=K-stdev.K, ymax=K+stdev.K), width=0.1, colour="black", alpha=0.7, size=0.5) +
  geom_signif(y_position=22, xmin=1.05, xmax=1.95, annotation="*", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "bi"), y_position=21.5, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=24, xmin=1, xmax=3, annotation="***", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "quadri"), y_position=23, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=22, xmin=3, xmax=4, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("quadri", "mix"), y_position=22, tip_length=0.05, vjust=0.2) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots)


plot_124Z_K <- ggbarplot(s124Z, x="culture", y="K", xlab=F, ylab=F, ylim=c(0,25), color="darkorange", fill="darkorange") +
  geom_errorbar(aes(x=culture, ymin=K-stdev.K, ymax=K+stdev.K), width=0.1, colour="black", alpha=0.7, size=0.5) +
  geom_signif(y_position=22.25, xmin=1.15, xmax=1.9, annotation="***", tip_length=0.02) +
  geom_signif(comparisons=c("mono", "bi"), y_position=22.25, tip_length=0.02, vjust=0.2) +
  geom_signif(y_position=23.3, xmin=1.1, xmax=2.9, annotation="***", tip_length=0.02) +
  geom_signif(comparisons=c("mono", "quadri"), y_position=23.3, tip_length=0.02, vjust=0.2) +
  geom_signif(y_position=24.75, xmin=1, xmax=4, annotation="***", tip_length=0.02) +
  geom_signif(comparisons=c("mono", "mix"), y_position=24.75, tip_length=0.02, vjust=0.2) +
  geom_signif(y_position=19, xmin=2, xmax=4, annotation="*", tip_length=0.02) +
  geom_signif(comparisons=c("bi", "mix"), y_position=19, tip_length=0.02, vjust=0.2) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots)


plot_515Z_K <- ggbarplot(s515Z, x="culture", y="K", xlab=F, ylab=F, ylim=c(0,25), color="darkgreen", fill="darkgreen") +
  geom_errorbar(aes(x=culture, ymin=K-stdev.K, ymax=K+stdev.K), width=0.1, colour="black", alpha=0.7, size=0.5) +
  geom_signif(y_position=23.25, xmin=1.1, xmax=2.9, annotation="***", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "quadri"), y_position=23.25, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=24.75, xmin=1, xmax=4, annotation="***", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "mix"), y_position=24.75, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=20.5, xmin=2.1, xmax=3, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("bi", "quadri"), y_position=20.5, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=21.75, xmin=2.1, xmax=3.9, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("bi", "mix"), y_position=21.75, tip_length=0.05, vjust=0.2) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots)


plot_1315Z_K <- ggbarplot(s1315Z, x="culture", y="K", xlab=F, ylab=F, ylim=c(0,25), color="gray47", fill="gray47") + 
  geom_errorbar(aes(x=culture, ymin=K-stdev.K, ymax=K+stdev.K), width=0.1, colour="black", alpha=0.7, size=0.5) +
  geom_signif(y_position=21.5, xmin=1.1, xmax=2.9, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "quadri"), y_position=21.5, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=23, xmin=1, xmax=4, annotation="***", tip_length=0.05) +
  geom_signif(comparisons=c("mono", "mix"), y_position=23, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=19, xmin=2.2, xmax=2.90, annotation="*", tip_length=0.05) +
  geom_signif(comparisons=c("bi", "quadri"), y_position=19, tip_length=0.05, vjust=0.2) +
  geom_signif(y_position=20.25, xmin=2.2, xmax=3.9, annotation="**", tip_length=0.05) +
  geom_signif(comparisons=c("bi", "mix"), y_position=20.25, tip_length=0.05, vjust=0.2) +
  theme(axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)) +
  scale_x_discrete(labels=labels_barplots)


# Arrange the four barplots for K
barplots_K <- ggarrange(plot_78R_K, plot_124Z_K, plot_515Z_K, plot_1315Z_K,
                        labels = c("78R_K", "124Z_K", "515Z_K", "1315Z_K"),
                        label.x=0.6, label.y=1,
                        ncol = 2, nrow = 2)
barplots_K


# Add x and y axes labels
barplots_K_leg <- annotate_figure(barplots_K,
                                  bottom = text_grob("culture", size = 15, face="bold"),
                                  left = text_grob("carrying capacity [ln(cells/mL)]", 
                                                   rot = 90, size = 15, face="bold"))
barplots_K_leg




# Arrange both ----

# Arrange all barplots to create Fig. 5


# Arrange mu and K barplots
barplots_all <- ggarrange(barplots_mu_leg, barplots_K_leg)
barplots_all


# Add a and b panels
barplots_final <- annotate_figure(barplots_all, top = text_grob(c("a", "b"), size=20, face="bold", x=c(0.065, 0.56)))
barplots_final

ggsave(file="barplots_final.png", plot=barplots_final, width=15, height=7.5)
