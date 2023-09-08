# Student mu/K mono vs bi vs quadri vs mix (barplots Fig. 5 in Bonal et al, 2023)
#
# Student unpaired test to assess whether mean values for growth rates (mu) and carrying
#
# (K) in monoculture vs bi-culture/quadri-culture/mix of 20 strains are significantly different
#
#
# Author: Mathias Bonal
#
# Last modified: 07/09/2023




# Introductory steps ----


# Set of working directory
setwd("~/R/Data/stats_mu_K")


# Load packages
library(dplyr)
library(devtools)
library(ggplot2)
library(ggpubr)
library(readxl)




# All strains ----

# First try: mixing all strains' values in 4 groups (mono/bi/quadri/mix)


# Load data
mu_mono <- read_excel("mu_K_per_culture.xlsx", sheet="mono", range="B1:B5")
K_mono <- read_excel("mu_K_per_culture.xlsx", range="mono!C1:C5")
mu_bi <- read_excel("mu_K_per_culture.xlsx", range="bi!B1:B5")
K_bi <- read_excel("mu_K_per_culture.xlsx", range="bi!C1:C5")
mu_quadri <- read_excel("mu_K_per_culture.xlsx", range="quadri!B1:B5")
K_quadri <- read_excel("mu_K_per_culture.xlsx", range="quadri!C1:C5")
mu_mix <- read_excel("mu_K_per_culture.xlsx", range="mix!B1:B5")
K_mix <- read_excel("mu_K_per_culture.xlsx", range="mix!C1:C5")


# Compute Student test for growth rates

res_1 <- t.test(mu_mono, mu_bi)
res_1
# p = 0.39

res_2 <- t.test(mu_mono, mu_quadri)
res_2
# p = 0.34

res_3 <- t.test(mu_mono, mu_mix)
res_3
# p = 0.31

res_4 <- t.test(mu_bi, mu_quadri)
res_4
# p = 0.74

res_5 <- t.test(mu_bi, mu_mix)
res_5
# p = 0.97

res_6 <- t.test(mu_quadri, mu_mix)
res_6
# p = 0.80



# Compute Student test for carrying capacities

res_1 <- t.test(K_mono, K_bi)
res_1
# p = 0.06

res_2 <- t.test(K_mono, K_quadri)
res_2
# p = 0.02

res_3 <- t.test(K_mono, K_mix)
res_3
# p = 0.06

res_4 <- t.test(K_bi, K_quadri)
res_4
# p = 0.20

res_5 <- t.test(K_bi, K_mix)
res_5
# p = 0.63

res_6 <- t.test(K_quadri, K_mix)
res_6
# p = 0.43




# Strain by strain ----

# Second try: comparing values strain by strain


# Load data for 78R
s78R_mu_mono <- read_excel("mu_K_per_culture.xlsx", range="78R!A2:A4")
s78R_mu_bi <- read_excel("mu_K_per_culture.xlsx", range="78R!B2:B4")
s78R_mu_quadri <- read_excel("mu_K_per_culture.xlsx", range="78R!C2:C4")
s78R_mu_mix <- read_excel("mu_K_per_culture.xlsx", range="78R!D2:D4")
s78R_K_mono <- read_excel("mu_K_per_culture.xlsx", range="78R!E2:E4")
s78R_K_bi <- read_excel("mu_K_per_culture.xlsx", range="78R!F2:F4")
s78R_K_quadri <- read_excel("mu_K_per_culture.xlsx", range="78R!G2:G4")
s78R_K_mix <- read_excel("mu_K_per_culture.xlsx", range="78R!H2:H4")

# Load data for 124Z
s124Z_mu_mono <- read_excel("mu_K_per_culture.xlsx", range="124Z!A2:A4")
s124Z_mu_bi <- read_excel("mu_K_per_culture.xlsx", range="124Z!B2:B4")
s124Z_mu_quadri <- read_excel("mu_K_per_culture.xlsx", range="124Z!C2:C4")
s124Z_mu_mix <- read_excel("mu_K_per_culture.xlsx", range="124Z!D2:D4")
s124Z_K_mono <- read_excel("mu_K_per_culture.xlsx", range="124Z!E2:E4")
s124Z_K_bi <- read_excel("mu_K_per_culture.xlsx", range="124Z!F2:F4")
s124Z_K_quadri <- read_excel("mu_K_per_culture.xlsx", range="124Z!G2:G4")
s124Z_K_mix <- read_excel("mu_K_per_culture.xlsx", range="124Z!H2:H4")

# Load data for 515Z
s515Z_mu_mono <- read_excel("mu_K_per_culture.xlsx", range="515Z!A2:A4")
s515Z_mu_bi <- read_excel("mu_K_per_culture.xlsx", range="515Z!B2:B4")
s515Z_mu_quadri <- read_excel("mu_K_per_culture.xlsx", range="515Z!C2:C4")
s515Z_mu_mix <- read_excel("mu_K_per_culture.xlsx", range="515Z!D2:D4")
s515Z_K_mono <- read_excel("mu_K_per_culture.xlsx", range="515Z!E2:E4")
s515Z_K_bi <- read_excel("mu_K_per_culture.xlsx", range="515Z!F2:F4")
s515Z_K_quadri <- read_excel("mu_K_per_culture.xlsx", range="515Z!G2:G4")
s515Z_K_mix <- read_excel("mu_K_per_culture.xlsx", range="515Z!H2:H4")

# Load data for 1315Z
s1315Z_mu_mono <- read_excel("mu_K_per_culture.xlsx", range="1315Z!A2:A4")
s1315Z_mu_bi <- read_excel("mu_K_per_culture.xlsx", range="1315Z!B2:B4")
s1315Z_mu_quadri <- read_excel("mu_K_per_culture.xlsx", range="1315Z!C2:C4")
s1315Z_mu_mix <- read_excel("mu_K_per_culture.xlsx", range="1315Z!D2:D4")
s1315Z_K_mono <- read_excel("mu_K_per_culture.xlsx", range="1315Z!E2:E4")
s1315Z_K_bi <- read_excel("mu_K_per_culture.xlsx", range="1315Z!F2:F4")
s1315Z_K_quadri <- read_excel("mu_K_per_culture.xlsx", range="1315Z!G2:G4")
s1315Z_K_mix <- read_excel("mu_K_per_culture.xlsx", range="1315Z!H2:H4")


# Compute Student test for growth rates

# 78R

res <- t.test(s78R_mu_mono, s78R_mu_bi)
res$p.value
# p = 0.001

res <- t.test(s78R_mu_mono, s78R_mu_quadri)
res$p.value
# p = 0.017

res <- t.test(s78R_mu_mono, s78R_mu_mix)
res$p.value
# p = 0.002 (but used to be 0.059...)

res <- t.test(s78R_mu_bi, s78R_mu_quadri)
res$p.value
# p = 0.051

res <- t.test(s78R_mu_bi, s78R_mu_mix)
res$p.value
# p = 0.208

res <- t.test(s78R_mu_quadri, s78R_mu_mix)
res$p.value
# p = 0.350


# 124Z

res <- t.test(s124Z_mu_mono, s124Z_mu_bi)
res$p.value
# p = 0.421

res <- t.test(s124Z_mu_mono, s124Z_mu_quadri)
res$p.value
# p = 0.027

res <- t.test(s124Z_mu_mono, s124Z_mu_mix)
res$p.value
# p = 0.021

res <- t.test(s124Z_mu_bi, s124Z_mu_quadri)
res$p.value
# p = 0.208

res <- t.test(s124Z_mu_bi, s124Z_mu_mix)
res$p.value
# p = 0.146

res <- t.test(s124Z_mu_quadri, s124Z_mu_mix)
res$p.value
# p = 0.038


# 515Z

res <- t.test(s515Z_mu_mono, s515Z_mu_bi)
res$p.value
# p = 0.55

res <- t.test(s515Z_mu_mono, s515Z_mu_quadri)
res$p.value
# p = 0.006

res <- t.test(s515Z_mu_mono, s515Z_mu_mix)
res$p.value
# p = 0.129

res <- t.test(s515Z_mu_bi, s515Z_mu_quadri)
res$p.value
# p = 0.310

res <- t.test(s515Z_mu_bi, s515Z_mu_mix)
res$p.value
# p = 0.876

res <- t.test(s515Z_mu_quadri, s515Z_mu_mix)
res$p.value
# p = 0.069


# 1315Z

res <- t.test(s1315Z_mu_mono, s1315Z_mu_bi)
res$p.value
# p = 0.372

res <- t.test(s1315Z_mu_mono, s1315Z_mu_quadri)
res$p.value
# p = 1.000

res <- t.test(s1315Z_mu_mono, s1315Z_mu_mix)
res$p.value
# p = 0.129

res <- t.test(s1315Z_mu_bi, s1315Z_mu_quadri)
res$p.value
# p = 0.372

res <- t.test(s1315Z_mu_bi, s1315Z_mu_mix)
res$p.value
# p = 0.053

res <- t.test(s1315Z_mu_quadri, s1315Z_mu_mix)
res$p.value
# p = 0.129



# Compute Student test for carrying capacities

# 78R

res <- t.test(s78R_K_mono, s78R_K_bi)
res$p.value
# p = 0.004 (but used to be 0.064...)

res <- t.test(s78R_K_mono, s78R_K_quadri)
res$p.value
# p = 0.037

res <- t.test(s78R_K_mono, s78R_K_mix)
res$p.value
# p = 0.019 (but used to be 0.008...)

res <- t.test(s78R_K_bi, s78R_K_quadri)
res$p.value
# p = 0.148

res <- t.test(s78R_K_bi, s78R_K_mix)
res$p.value
# p = 0.126

res <- t.test(s78R_K_quadri, s78R_K_mix)
res$p.value
# p = 0.059


# 124Z

res <- t.test(s124Z_K_mono, s124Z_K_bi)
res$p.value
# p = 0.013 (but used to be 0.064...)

res <- t.test(s124Z_K_mono, s124Z_K_quadri)
res$p.value
# p = 0.047

res <- t.test(s124Z_K_mono, s124Z_K_mix)
res$p.value
# p = 0.002

res <- t.test(s124Z_K_bi, s124Z_K_quadri)
res$p.value
# p = 0.099

res <- t.test(s124Z_K_bi, s124Z_K_mix)
res$p.value
# p = 0.110

res <- t.test(s124Z_K_quadri, s124Z_K_mix)
res$p.value
# p = 0.288


# 515Z

res <- t.test(s515Z_K_mono, s515Z_K_bi)
res$p.value
# p = 0.021

res <- t.test(s515Z_K_mono, s515Z_K_quadri)
res$p.value
# p = 0.001

res <- t.test(s515Z_K_mono, s515Z_K_mix)
res$p.value
# p = 0.002

res <- t.test(s515Z_K_bi, s515Z_K_quadri)
res$p.value
# p = 0.003

res <- t.test(s515Z_K_bi, s515Z_K_mix)
res$p.value
# p = 0.006

res <- t.test(s515Z_K_quadri, s515Z_K_mix)
res$p.value
# p = 0.267


# 1315Z

res <- t.test(s1315Z_K_mono, s1315Z_K_bi)
res$p.value
# p = 0.558

res <- t.test(s1315Z_K_mono, s1315Z_K_quadri)
res$p.value
# p = 0.001 (but used to be 0.045...)

res <- t.test(s1315Z_K_mono, s1315Z_K_mix)
res$p.value
# p = 0.004 (but used to be 0.055...)

res <- t.test(s1315Z_K_bi, s1315Z_K_quadri)
res$p.value
# p = 0.057

res <- t.test(s1315Z_K_bi, s1315Z_K_mix)
res$p.value
# p = 0.055

res <- t.test(s1315Z_K_quadri, s1315Z_K_mix)
res$p.value
# p = 0.367




# + Correction ----

# We chose to compare the mean values strain by strain and here we add multiple pair testing 
# correction (Bonferroni's method).
#
# The following computed values will be those used to add significance stars in the barplot in
# Fig 5 in Bonal et al, 2023: p-values < 0.05 / 0.01 / 0.001 will be represented with 1, 2 or 3
# stars (asterisks), respectively. See file "script_barplots_mu_K.R"


# Load data
s78R <- read_excel("mu_K_by_culture.xlsx", sheet="78R")
s124Z <- read_excel("mu_K_by_culture.xlsx", sheet="124Z")
s515Z <- read_excel("mu_K_by_culture.xlsx", sheet="515Z")
s1315Z <- read_excel("mu_K_by_culture.xlsx", sheet="1315Z")


s78R_mu <- pairwise.t.test(s78R$mu, s78R$culture, p.adjust.method="bonferroni")
s78R_mu

s124Z_mu <- pairwise.t.test(s124Z$mu, s124Z$culture, p.adjust.method="bonferroni")
s124Z_mu

s515Z_mu <- pairwise.t.test(s515Z$mu, s515Z$culture, p.adjust.method="bonferroni")
s515Z_mu

s1315Z_mu <- pairwise.t.test(s1315Z$mu, s1315Z$culture, p.adjust.method="bonferroni")
s1315Z_mu


s78R_K <- pairwise.t.test(s78R$K, s78R$culture, p.adjust.method="bonferroni")
s78R_K

s124Z_K <- pairwise.t.test(s124Z$K, s124Z$culture, p.adjust.method="bonferroni")
s124Z_K

s515Z_K <- pairwise.t.test(s515Z$K, s515Z$culture, p.adjust.method="bonferroni")
s515Z_K

s1315Z_K <- pairwise.t.test(s1315Z$K, s1315Z$culture, p.adjust.method="bonferroni")
s1315Z_K



# Wilcoxon ---- 

# Do the same calculations with non-parametric Wilcoxon test
#
# This was done to compare the relevance of this test from Student, but as there is no correction
# applied for multiple-pair testing, we did not use this for the article in the end. I deliberately
# kept the following line codes anyway so that the structure of the code could be re-used in more
# accurate applications by me or other persons in the future.


# Load data for 78R
s78R_mono_bi <- read_excel("data_for_wilcoxon_78R.xlsx", sheet="mono-bi")
s78R_mono_quadri <- read_excel("data_for_wilcoxon_78R.xlsx", sheet="mono-quadri")
s78R_mono_mix <- read_excel("data_for_wilcoxon_78R.xlsx", sheet="mono-mix")
s78R_bi_quadri <- read_excel("data_for_wilcoxon_78R.xlsx", sheet="bi-quadri")
s78R_bi_mix <- read_excel("data_for_wilcoxon_78R.xlsx", sheet="bi-mix")
s78R_quadri_mix <- read_excel("data_for_wilcoxon_78R.xlsx", sheet="quadri-mix")

# Load data for 124Z
s124Z_mono_bi <- read_excel("data_for_wilcoxon_124Z.xlsx", sheet="mono-bi")
s124Z_mono_quadri <- read_excel("data_for_wilcoxon_124Z.xlsx", sheet="mono-quadri")
s124Z_mono_mix <- read_excel("data_for_wilcoxon_124Z.xlsx", sheet="mono-mix")
s124Z_bi_quadri <- read_excel("data_for_wilcoxon_124Z.xlsx", sheet="bi-quadri")
s124Z_bi_mix <- read_excel("data_for_wilcoxon_124Z.xlsx", sheet="bi-mix")
s124Z_quadri_mix <- read_excel("data_for_wilcoxon_124Z.xlsx", sheet="quadri-mix")

# Load data for 515Z
s515Z_mono_bi <- read_excel("data_for_wilcoxon_515Z.xlsx", sheet="mono-bi")
s515Z_mono_quadri <- read_excel("data_for_wilcoxon_515Z.xlsx", sheet="mono-quadri")
s515Z_mono_mix <- read_excel("data_for_wilcoxon_515Z.xlsx", sheet="mono-mix")
s515Z_bi_quadri <- read_excel("data_for_wilcoxon_515Z.xlsx", sheet="bi-quadri")
s515Z_bi_mix <- read_excel("data_for_wilcoxon_515Z.xlsx", sheet="bi-mix")
s515Z_quadri_mix <- read_excel("data_for_wilcoxon_515Z.xlsx", sheet="quadri-mix")

# Load data for 1315Z
s1315Z_mono_bi <- read_excel("data_for_wilcoxon_1315Z.xlsx", sheet="mono-bi")
s1315Z_mono_quadri <- read_excel("data_for_wilcoxon_1315Z.xlsx", sheet="mono-quadri")
s1315Z_mono_mix <- read_excel("data_for_wilcoxon_1315Z.xlsx", sheet="mono-mix")
s1315Z_bi_quadri <- read_excel("data_for_wilcoxon_1315Z.xlsx", sheet="bi-quadri")
s1315Z_bi_mix <- read_excel("data_for_wilcoxon_1315Z.xlsx", sheet="bi-mix")
s1315Z_quadri_mix <- read_excel("data_for_wilcoxon_1315Z.xlsx", sheet="quadri-mix")


## mu ----

# Compute Wilcoxon test for growth rates

# 78R

res <- pairwise.wilcox.test(s78R_mono_bi$mu, s78R_mono_bi$culture)
res$p.value
# p = 0.081

res <- wilcox.test(mu ~ culture, data = s78R_mono_quadri, exact = FALSE)
res$p.value
# p = 0.376

res <- wilcox.test(mu ~ culture, data = s78R_mono_mix, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(mu ~ culture, data = s78R_bi_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(mu ~ culture, data = s78R_bi_mix, exact = FALSE)
res$p.value
# p = 0.376

res <- wilcox.test(mu ~ culture, data = s78R_quadri_mix, exact = FALSE)
res$p.value
# p = 0.261


# 124Z

res <- wilcox.test(mu ~ culture, data = s124Z_mono_bi, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(mu ~ culture, data = s124Z_mono_quadri, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(mu ~ culture, data = s124Z_mono_mix, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(mu ~ culture, data = s124Z_bi_quadri, exact = FALSE)
res$p.value
# p = 0.383

res <- wilcox.test(mu ~ culture, data = s124Z_bi_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(mu ~ culture, data = s124Z_quadri_mix, exact = FALSE)
res$p.value
# p = 0.663


# 515Z

res <- wilcox.test(mu ~ culture, data = s515Z_mono_bi, exact = FALSE)
res$p.value
# p = 0.507

res <- wilcox.test(mu ~ culture, data = s515Z_mono_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(mu ~ culture, data = s515Z_mono_mix, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(mu ~ culture, data = s515Z_bi_quadri, exact = FALSE)
res$p.value
# p = 0.383

res <- wilcox.test(mu ~ culture, data = s515Z_bi_mix, exact = FALSE)
res$p.value
# p = 0.658

res <- wilcox.test(mu ~ culture, data = s515Z_quadri_mix, exact = FALSE)
res$p.value
# p = 0.077


# 1315Z

res <- wilcox.test(mu ~ culture, data = s1315Z_mono_bi, exact = FALSE)
res$p.value
# p = 0.663

res <- wilcox.test(mu ~ culture, data = s1315Z_mono_quadri, exact = FALSE)
res$p.value
# p = 0.369

res <- wilcox.test(mu ~ culture, data = s1315Z_mono_mix, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(mu ~ culture, data = s1315Z_bi_quadri, exact = FALSE)
res$p.value
# p = 0.663

res <- wilcox.test(mu ~ culture, data = s1315Z_bi_mix, exact = FALSE)
res$p.value
# p = 0.658

res <- wilcox.test(mu ~ culture, data = s1315Z_quadri_mix, exact = FALSE)
res$p.value
# p = 0.658




## K ----
# Compute Wilcoxon test for carrying capacities

# 78R

res <- wilcox.test(K ~ culture, data = s78R_mono_bi, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s78R_mono_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s78R_mono_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s78R_bi_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s78R_bi_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s78R_quadri_mix, exact = FALSE)
res$p.value
# p = 0.081


# 124Z

res <- wilcox.test(K ~ culture, data = s124Z_mono_bi, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s124Z_mono_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s124Z_mono_mix, exact = FALSE)
res$p.value
# p = 0.077

res <- wilcox.test(K ~ culture, data = s124Z_bi_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s124Z_bi_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s124Z_quadri_mix, exact = FALSE)
res$p.value
# p = 0.376


# 515Z

res <- wilcox.test(K ~ culture, data = s515Z_mono_bi, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s515Z_mono_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s515Z_mono_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s515Z_bi_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s515Z_bi_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s515Z_quadri_mix, exact = FALSE)
res$p.value
# p = 0.383


# 1315Z

res <- wilcox.test(K ~ culture, data = s1315Z_mono_bi, exact = FALSE)
res$p.value
# p = 0.190

res <- wilcox.test(K ~ culture, data = s1315Z_mono_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s1315Z_mono_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s1315Z_bi_quadri, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s1315Z_bi_mix, exact = FALSE)
res$p.value
# p = 0.081

res <- wilcox.test(K ~ culture, data = s1315Z_quadri_mix, exact = FALSE)
res$p.value
# p = 0.081