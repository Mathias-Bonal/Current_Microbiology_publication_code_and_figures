#### Time series plotting ####
#
# Create the plots of mono-, bi- and quadri-cultures as presented in Bonal et al, 2022
#
# Authors: Mathias Bonal, with revisions from Karoline Faust
#
# Last modified: 12/09/2022




# Set of working directory
setwd("~/R/Data/cultures")

# Load packages
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggsci)
library(cowplot)
library(gridExtra)
library(readxl)




#### Create plots with monocultures and bi-cultures triplicates with abundances in ln values (Fig. 2) ####


## Monocultures ##

# Load data
m78R <- read_excel("monocultures.xlsx", sheet="78R")
m124Z <- read_excel("monocultures.xlsx", sheet="124Z")
m515Z <- read_excel("monocultures.xlsx", sheet="515Z")
m1315Z <- read_excel("monocultures.xlsx", sheet="1315Z")


# Plot each monoculture triplicate
s78R <- ggscatter(m78R, x = "time", y = "BA", color="strain", xlab=F, ylab=F, 
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
s78R


s124Z <- ggscatter(m124Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                   palette=c('124Z_1'="darkorange1", '124Z_2'="coral2",'124Z_3'="orangered"),
                   xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
s124Z


s515Z <- ggscatter(m515Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                   palette=c('515Z_1'="darkgreen", '515Z_2'="darkolivegreen2",'515Z_3'="green"),
                   xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
s515Z


s1315Z <- ggscatter(m1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                    palette=c('1315Z_1'="slategrey", '1315Z_2'="gray47",'1315Z_3'="ivory3"),
                    xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none") 
s1315Z




## Bi-cultures ## 

# Load data
s78R_124Z <- read_excel("bi-cultures_ln.xlsx", sheet="78R_124Z")
s78R_515Z <- read_excel("bi-cultures_ln.xlsx", sheet="78R_515Z")
s78R_1315Z <- read_excel("bi-cultures_ln.xlsx", sheet="78R_1315Z")
s124Z_515Z <- read_excel("bi-cultures_ln.xlsx", sheet="124Z_515Z")
s124Z_1315Z <- read_excel("bi-cultures_ln.xlsx", sheet="124Z_1315Z")
s515Z_1315Z <- read_excel("bi-cultures_ln.xlsx", sheet="515Z_1315Z")



# Plot each bi-culture triplicate
bi.1 <- ggscatter(s78R_124Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4",
                            '124Z_1'="darkorange1", '124Z_2'="coral2", '124Z_3'="orangered",
                            '515Z_2'="darkolivegreen2", '515Z_3'="green"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
bi.1


bi.2 <- ggscatter(s78R_515Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4",
                            '515Z_1'="darkgreen", '515Z_2'="darkolivegreen2", '515Z_3'="green"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
bi.2


bi.3 <- ggscatter(s78R_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4",
                            '1315Z_1'="slategrey", '1315Z_2'="gray47", '1315Z_3'="ivory3"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
bi.3


bi.4 <- ggscatter(s124Z_515Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('124Z_1'="darkorange1", '124Z_2'="coral2", '124Z_3'="orangered",
                            '515Z_1'="darkgreen", '515Z_2'="darkolivegreen2", '515Z_3'="green"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
bi.4


bi.5 <- ggscatter(s124Z_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('124Z_1'="darkorange1", '124Z_2'="coral2", '124Z_3'="orangered",
                            '1315Z_1'="slategrey", '1315Z_2'="gray47", '1315Z_3'="ivory3"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
bi.5


bi.6 <- ggscatter(s515Z_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('515Z_1'="darkgreen", '515Z_2'="darkolivegreen2", '515Z_3'="green",
                            '1315Z_1'="slategrey", '1315Z_2'="gray47", '1315Z_3'="ivory3",
                            '78R_2'="magenta",'78R_3'="purple4"),
                  xlim=c(0,75), ylim=c(0,22.25)) + theme(legend.position = "none")
bi.6




## Triangular matrix with monocultures in diagonal and bi-cultures in the lower triangle ##

# Create the matrix
plot_matrix_tri <- ggarrange(s78R, NA, NA, NA,     
                             bi.1, s124Z, NA, NA,  
                             bi.2, bi.4, s515Z, NA,
                             bi.3, bi.5, bi.6, s1315Z,
                             labels = c("78R", "", "", "",   
                                        "78R_124Z", "124Z", "", "",  
                                        "78R_515Z", "124Z_515Z", "515Z", "",
                                        "78R_1315Z", "124Z_1315Z", "515Z_1315Z", "1315Z"),
                             label.x=0.05, label.y=1, common.legend=TRUE,
                             ncol = 4, nrow = 4)  
plot_matrix_tri


# Add x and y axes labels
final_matrix_tri <- annotate_figure(plot_matrix_tri,
                              bottom = text_grob("time (h)", face="bold", size=15),
                              left = text_grob("abundance [ln(cells/mL)]", face="bold", size=15, rot = 90))

final_matrix_tri

ggsave(file="triangular_matrix_ln.png", plot=matrix_tri, width=15, height=7.5)








#### Same with abundances in non-log values (Fig. S1) ####

## Monocultures ##

# Load data
BA_78R <- read_excel("monocultures_tot.xlsx", sheet="78R")
BA_124Z <- read_excel("monocultures_tot.xlsx", sheet="124Z")
BA_515Z <- read_excel("monocultures_tot.xlsx", sheet="515Z")
BA_1315Z <- read_excel("monocultures_tot.xlsx", sheet="1315Z")


# Plot each monoculture triplicate
tot78R <- ggscatter(BA_78R, x = "time", y = "BA", color="strain", xlab=F, ylab=F, 
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none") 
tot78R


tot124Z <- ggscatter(BA_124Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                   palette=c('124Z_1'="darkorange1", '124Z_2'="coral2",'124Z_3'="orangered"),
                   xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot124Z


tot515Z <- ggscatter(BA_515Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                   palette=c('515Z_1'="darkgreen", '515Z_2'="darkolivegreen2",'515Z_3'="green"),
                   xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot515Z


tot1315Z <- ggscatter(BA_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                    palette=c('1315Z_1'="slategrey", '1315Z_2'="gray47",'1315Z_3'="ivory3"),
                    xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none") 
tot1315Z




## Bi-cultures ## 

# Load data
BA_78R_124Z <- read_excel("bi-cultures_tot.xlsx", sheet="78R_124Z")
BA_78R_515Z <- read_excel("bi-cultures_tot.xlsx", sheet="78R_515Z")
BA_78R_1315Z <- read_excel("bi-cultures_tot.xlsx", sheet="78R_1315Z")
BA_124Z_515Z <- read_excel("bi-cultures_tot.xlsx", sheet="124Z_515Z")
BA_124Z_1315Z <- read_excel("bi-cultures_tot.xlsx", sheet="124Z_1315Z")
BA_515Z_1315Z <- read_excel("bi-cultures_tot.xlsx", sheet="515Z_1315Z")


# Plot each bi-culture
tot_bi.1 <- ggscatter(BA_78R_124Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F, show.legend=F,
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4",
                            '124Z_1'="darkorange1", '124Z_2'="coral2", '124Z_3'="orangered",
                            '515Z_2'="darkolivegreen2", '515Z_3'="green"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot_bi.1


tot_bi.2 <- ggscatter(BA_78R_515Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4",
                            '515Z_1'="darkgreen", '515Z_2'="darkolivegreen2", '515Z_3'="green"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot_bi.2


tot_bi.3 <- ggscatter(BA_78R_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('78R_1'="darkorchid", '78R_2'="magenta",'78R_3'="purple4",
                            '1315Z_1'="slategrey", '1315Z_2'="gray47", '1315Z_3'="ivory3"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot_bi.3


tot_bi.4 <- ggscatter(BA_124Z_515Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('124Z_1'="darkorange1", '124Z_2'="coral2", '124Z_3'="orangered",
                            '515Z_1'="darkgreen", '515Z_2'="darkolivegreen2", '515Z_3'="green"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot_bi.4


tot_bi.5 <- ggscatter(BA_124Z_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('124Z_1'="darkorange1", '124Z_2'="coral2", '124Z_3'="orangered",
                            '1315Z_1'="slategrey", '1315Z_2'="gray47", '1315Z_3'="ivory3"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot_bi.5


tot_bi.6 <- ggscatter(BA_515Z_1315Z, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                  palette=c('515Z_1'="darkgreen", '515Z_2'="darkolivegreen2", '515Z_3'="green",
                            '1315Z_1'="slategrey", '1315Z_2'="gray47", '1315Z_3'="ivory3",
                            '78R_2'="magenta",'78R_3'="purple4"),
                  xlim=c(0,75), ylim=c(0,4600000000)) + theme(legend.position = "none")
tot_bi.6




## Triangular matrix with monocultures in diagonal and bi-cultures in the lower triangle ##

# Create the matrix
plot_matrix_tri <- ggarrange(tot78R, NA, NA, NA,     
                             tot_bi.1, tot124Z, NA, NA,  
                             tot_bi.2, tot_bi.4, tot515Z, NA,
                             tot_bi.3, tot_bi.5, tot_bi.6, tot1315Z,
                             labels = c("78R", "", "", "",   
                                        "78R_124Z", "124Z", "", "",  
                                        "78R_515Z", "124Z_515Z", "515Z", "",
                                        "78R_1315Z", "124Z_1315Z", "515Z_1315Z", "1315Z"),
                             label.x=0.05, label.y=1, common.legend=TRUE,
                             ncol = 4, nrow = 4)  
plot_matrix_tri


# Add x and y axes labels
final_matrix_tri <- annotate_figure(plot_matrix_tri,
                                    bottom = text_grob("time (h)", face="bold", size=15),
                                    left = text_grob("abundance [ln(cells/mL)]", face="bold", size=15, rot = 90))

final_matrix_tri

ggsave(file="triangular_matrix_tot.png", plot=matrix_tri, width=15, height=7.5)







#### Create plots for total cell counts of bi-cultures and quadricultures (Fig. S2) ####


# Load data
tot78R_124Z <- read_excel("bi-cultures_cell_counts.xlsx", sheet="78R_124Z_tot")
tot78R_515Z <- read_excel("bi-cultures_cell_counts.xlsx", sheet="78R_515Z_tot")
tot78R_1315Z <- read_excel("bi-cultures_cell_counts.xlsx", sheet="78R_1315Z_tot")
tot124Z_515Z <- read_excel("bi-cultures_cell_counts.xlsx", sheet="124Z_515Z_tot")
tot124Z_1315Z <- read_excel("bi-cultures_cell_counts.xlsx", sheet="124Z_1315Z_tot")
tot515Z_1315Z <- read_excel("bi-cultures_cell_counts.xlsx", sheet="515Z_1315Z_tot")
tot_quadri <- read_excel("quadri-culture_ln.xlsx", sheet="quadri_tot")


# Plot each bi-culture and quadriculture
bi.1_tot <- ggscatter(tot78R_124Z, x = "time", y = "BA", color="replicate", xlab=F, ylab=F, show.legend=F,
                  palette="jco",
                  xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
bi.1_tot


bi.2_tot <- ggscatter(tot78R_515Z, x = "time", y = "BA", color="replicate", xlab=F, ylab=F,
                  palette="jco",
                  xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
bi.2_tot


bi.3_tot <- ggscatter(tot78R_1315Z, x = "time", y = "BA", color="replicate", xlab=F, ylab=F,
                  palette="jco",
                  xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
bi.3_tot


bi.4_tot <- ggscatter(tot124Z_515Z, x = "time", y = "BA", color="replicate", xlab=F, ylab=F,
                  palette="jco",
                  xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
bi.4_tot


bi.5_tot <- ggscatter(tot124Z_1315Z, x = "time", y = "BA", color="replicate", xlab=F, ylab=F,
                  palette="jco",
                  xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
bi.5_tot


bi.6_tot <- ggscatter(tot515Z_1315Z, x = "time", y = "BA", color="replicate", xlab=F, ylab=F,
                  palette="jco",
                  xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
bi.6_tot


quadri_tot <- ggscatter(tot_quadri, x = "time", y = "BA", color="replicate", xlab=F, ylab=F,
                    palette="jco",
                    xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
quadri_tot


# Group all time series
time_series_co_tot <- ggarrange(bi.1_tot, bi.2_tot, bi.3_tot, bi.4_tot, bi.5_tot, bi.6_tot, 
                                NA, quadri_tot, NA,
                                labels = c("78R_124Z", "78R_515Z", "78R_1315Z", 
                                           "124Z_515Z", "124Z_1315Z", "515Z_1315Z",
                                           "", "Quadriculture", ""),
                                label.x=0.02, common.legend=TRUE,
                                ncol = 3, nrow = 3)
time_series_co_tot


# Add x and y labels
plot_cocultures_tot <- annotate_figure(time_series_co_tot,
                                       bottom = text_grob("time (h)", face="bold", size=15),
                                       left = text_grob("abundance [ln(cells/mL)]", face="bold", size=15, rot = 90))
plot_cocultures_tot


ggsave(file="cocultures_tot_title.svg", plot=plot_cocultures_tot, width=15, height=7.5)








#### Create plots for each quadri-culture replicate (Fig.S3) ####


# Load data
quadri_1 <- read_excel("quadri-culture_ln.xlsx", sheet="quadri_1")
quadri_2 <- read_excel("quadri-culture_ln.xlsx", sheet="quadri_2")
quadri_3 <- read_excel("quadri-culture_ln.xlsx", sheet="quadri_3")

quadri1 <- ggscatter(quadri_1, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                     palette=c('78R_1'="darkorchid", '124Z_1'="darkorange1", '515Z_1'="darkgreen", '1315Z_1'="slategrey"),
                     xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position="none")
quadri1


quadri2 <- ggscatter(quadri_2, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                     palette=c('78R_2'="magenta", '124Z_2'="coral2", '515Z_2'="darkolivegreen2", '1315Z_2'="gray47"),
                     xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
quadri2


quadri3 <- ggscatter(quadri_3, x = "time", y = "BA", color="strain", xlab=F, ylab=F,
                     palette=c('78R_3'="purple4", '124Z_3'="orangered", '515Z_3'="green", '1315Z_3'="ivory3"),
                     xlim=c(0,75), ylim=c(0,21.15)) + theme(legend.position = "none")
quadri3


time_series_quadri <- ggarrange(quadri1, quadri2, quadri3,
                                labels = c("Quadri_1", "Quadri_2", "Quadri_3"),
                                label.x=0.05,
                                ncol = 3, nrow = 1)
time_series_quadri


plot_quadricultures <- annotate_figure(time_series_quadri,
                                       #top = text_grob("quadricultures", color = "blue", face="bold", size = 14),
                                       bottom = text_grob("time (h)", face="bold", size=15),
                                       left = text_grob("abundance [ln(cells/mL)]", face="bold", size=15, rot = 90))
plot_quadricultures

ggsave(file="quadricultures_title.png", plot=plot_quadricultures, width=15, height=7.5)
