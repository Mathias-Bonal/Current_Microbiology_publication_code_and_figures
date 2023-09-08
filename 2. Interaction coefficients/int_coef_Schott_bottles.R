# Calculation of alpha and kappa values for interaction coefficients as used to build interaction 
#
# matrices as presented in Bonal et al, 2023 (Schott bottle experiments mono vs bi vs quadricultures)
#
#
# Author: Mathias Bonal
#
# Last modified: 06/09/2023



# Introductory steps ----


# Set of working directory
setwd("~/R/Data/cultures")


# Load packages
library(readxl)
library(writexl)
library(dplyr)



# Alpha ----

# Calculation of interaction coefficient for growth rates = alpha


# Load data
mu_78R <- read_excel("interaction_mu.xlsx", sheet="78R")
mu_124Z <- read_excel("interaction_mu.xlsx", sheet="124Z")
mu_515Z <- read_excel("interaction_mu.xlsx", sheet="515Z")
mu_1315Z <- read_excel("interaction_mu.xlsx", sheet="1315Z")



## Bi-cultures ----

# initialize the vector that has to be empty before running the loop
ln <-c() 


# make a vector with all the values in the bi column
bi.1 <- mu_78R$bi.1 


# make the same with the mono values 
mono.78R <- mu_78R$mono 


# calculate values of alpha for 1st bi-culture
for (i in bi.1) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


# collect the mean and standard deviation
ln.mean.78R.1 <- mean(ln)  
sd.mean.78R.1 <- sd(ln) 


# repeat the same steps for 2nd and 3rd bi-cultures and for the 3 other strains


# 2nd bi-culture
ln <-c() 

bi.2 <- mu_78R$bi.2

for (i in bi.2) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.78R.2 <- mean(ln)  
sd.mean.78R.2 <- sd(ln) 


# 3rd bi-culture
ln <-c() 
bi.3 <- mu_78R$bi.3

for (i in bi.3) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.78R.3 <- mean(ln)  
sd.mean.78R.3 <- sd(ln)




## 124Z ##

# 1st bi-culture
ln <-c() 
bi.1 <- mu_124Z$bi.1 
mono.124Z <- mu_124Z$mono 


for (i in bi.1) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.124Z.1 <- mean(ln)  
sd.mean.124Z.1 <- sd(ln) 

# 2nd bi-culture
ln <-c()
bi.2 <- mu_124Z$bi.2

for (i in bi.2) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.124Z.2 <- mean(ln)  
sd.mean.124Z.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- mu_124Z$bi.3

for (i in bi.3) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.124Z.3 <- mean(ln)  
sd.mean.124Z.3 <- sd(ln)




## 515Z ##

# 1st bi-culture
ln <-c()
bi.1 <- mu_515Z$bi.1 
mono.515Z <- mu_515Z$mono 


for (i in bi.1) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.515Z.1 <- mean(ln)  
sd.mean.515Z.1 <- sd(ln) 

# 2nd bi-culture
ln <-c()
bi.2 <- mu_515Z$bi.2

for (i in bi.2) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.515Z.2 <- mean(ln)  
sd.mean.515Z.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- mu_515Z$bi.3

for (i in bi.3) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.515Z.3 <- mean(ln)  
sd.mean.515Z.3 <- sd(ln)




## 1315Z ##

# 1st bi-culture
ln <-c()
bi.1 <- mu_1315Z$bi.1 
mono.1315Z <- mu_1315Z$mono 


for (i in bi.1) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.1315Z.1 <- mean(ln)  
sd.mean.1315Z.1 <- sd(ln) 

# 2nd bi-culture
ln <-c()
bi.2 <- mu_1315Z$bi.2

for (i in bi.2) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.1315Z.2 <- mean(ln)  
sd.mean.1315Z.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- mu_1315Z$bi.3

for (i in bi.3) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.1315Z.3 <- mean(ln)  
sd.mean.1315Z.3 <- sd(ln)




## Quadriculture ----

# 78R
ln <-c()
quadri.78R <- mu_78R$quadri

for (i in quadri.78R) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.78R <- mean(ln)  
sd.mean.quadri.78R <- sd(ln) 


# 124Z
ln <-c()
quadri.124Z <- mu_124Z$quadri

for (i in quadri.124Z) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.124Z <- mean(ln)  
sd.mean.quadri.124Z <- sd(ln) 


# 515Z
ln <-c()
quadri.515Z <- mu_515Z$quadri

for (i in quadri.515Z) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.515Z <- mean(ln)  
sd.mean.quadri.515Z <- sd(ln) 


# 1315Z
ln <-c()
quadri.1315Z <- mu_1315Z$quadri

for (i in quadri.1315Z) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.1315Z <- mean(ln)  
sd.mean.quadri.1315Z <- sd(ln) 



## Data frames ----

# create dataframe with alpha values for bi-cultures

strain <- c("78R.1", "78R.2", "78R.3", "124Z.1", "124Z.2", "124Z.3", "515Z.1", "515Z.2", "515Z.3", 
            "1315Z.1", "1315Z.2", "1315Z.3")

alpha <- c(ln.mean.78R.1, ln.mean.78R.2, ln.mean.78R.3, ln.mean.124Z.1, ln.mean.124Z.2, ln.mean.124Z.3,
           ln.mean.515Z.1, ln.mean.515Z.2, ln.mean.515Z.3, ln.mean.1315Z.1, ln.mean.1315Z.2, ln.mean.1315Z.3)

sd.alpha <- c(sd.mean.78R.1, sd.mean.78R.2, sd.mean.78R.3, sd.mean.124Z.1, sd.mean.124Z.2, sd.mean.124Z.3,
              sd.mean.515Z.1, sd.mean.515Z.2, sd.mean.515Z.3, sd.mean.1315Z.1, sd.mean.1315Z.2, sd.mean.1315Z.3)

table_alpha <- data.frame(strain, alpha, sd.alpha)



# create dataframe with alpha values for quadriculture

strain_quadri <- c("78R.quadri", "124Z.quadri", "515Z.quadri", "1315Z.quadri")

kappa_quadri <- c(ln.mean.quadri.78R, ln.mean.quadri.124Z, ln.mean.quadri.515Z, ln.mean.quadri.1315Z)

sd.kappa_quadri <- c(sd.mean.quadri.78R, sd.mean.quadri.124Z, sd.mean.quadri.515Z, sd.mean.quadri.1315Z)

table_kappa_quadri <- data.frame(strain_quadri, kappa_quadri, sd.kappa_quadri)




# Kappa ----

# Calculation of interaction coefficient for growth rates = kappa
  

# Load data
K_78R <- read_excel("interaction_K.xlsx", sheet="78R")
K_124Z <- read_excel("interaction_K.xlsx", sheet="124Z")
K_515Z <- read_excel("interaction_K.xlsx", sheet="515Z")
K_1315Z <- read_excel("interaction_K.xlsx", sheet="1315Z")


## Bi-cultures ----

## 78R ##

# 1st bi-culture
ln <-c()
bi.1 <- K_78R$bi.1 
mono.78R <- K_78R$mono 


for (i in bi.1) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.78R.1 <- mean(ln)  
sd.mean.78R.1 <- sd(ln) 


# 2nd bi-culture
ln <-c()
bi.2 <- K_78R$bi.2

for (i in bi.2) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.78R.2 <- mean(ln)  
sd.mean.78R.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- K_78R$bi.3

for (i in bi.3) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.78R.3 <- mean(ln)  
sd.mean.78R.3 <- sd(ln)





## 124Z ##

# 1st bi-culture
ln <-c()
bi.1 <- K_124Z$bi.1 
mono.124Z <- K_124Z$mono 


for (i in bi.1) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.124Z.1 <- mean(ln)  
sd.mean.124Z.1 <- sd(ln) 

# 2nd bi-culture
ln <-c()
bi.2 <- K_124Z$bi.2

for (i in bi.2) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.124Z.2 <- mean(ln)  
sd.mean.124Z.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- K_124Z$bi.3

for (i in bi.3) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.124Z.3 <- mean(ln)  
sd.mean.124Z.3 <- sd(ln)




## 515Z ##

# 1st bi-culture
ln <-c()
bi.1 <- K_515Z$bi.1 
mono.515Z <- K_515Z$mono 


for (i in bi.1) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.515Z.1 <- mean(ln)  
sd.mean.515Z.1 <- sd(ln) 

# 2nd bi-culture
ln <-c()
bi.2 <- K_515Z$bi.2

for (i in bi.2) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.515Z.2 <- mean(ln)  
sd.mean.515Z.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- K_515Z$bi.3

for (i in bi.3) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.515Z.3 <- mean(ln)  
sd.mean.515Z.3 <- sd(ln)




## 1315Z ##

# 1st bi-culture
ln <-c()
bi.1 <- K_1315Z$bi.1 
mono.1315Z <- K_1315Z$mono 


for (i in bi.1) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}


ln.mean.1315Z.1 <- mean(ln)  
sd.mean.1315Z.1 <- sd(ln) 

# 2nd bi-culture
ln <-c()
bi.2 <- K_1315Z$bi.2

for (i in bi.2) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.1315Z.2 <- mean(ln)  
sd.mean.1315Z.2 <- sd(ln) 


# 3rd bi-culture
ln <-c()
bi.3 <- K_1315Z$bi.3

for (i in bi.3) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.1315Z.3 <- mean(ln)  
sd.mean.1315Z.3 <- sd(ln)




## Quadriculture ----

# 78R
ln <-c()
quadri.78R <- K_78R$quadri

for (i in quadri.78R) {
  for (j in mono.78R) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.78R <- mean(ln)  
sd.mean.quadri.78R <- sd(ln) 


# 124Z
ln <-c()
quadri.124Z <- K_124Z$quadri

for (i in quadri.124Z) {
  for (j in mono.124Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.124Z <- mean(ln)  
sd.mean.quadri.124Z <- sd(ln) 


# 515Z
ln <-c()
quadri.515Z <- K_515Z$quadri

for (i in quadri.515Z) {
  for (j in mono.515Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.515Z <- mean(ln)  
sd.mean.quadri.515Z <- sd(ln) 


# 1315Z
ln <-c()
quadri.1315Z <- K_1315Z$quadri

for (i in quadri.1315Z) {
  for (j in mono.1315Z) {
    a <- log(i/j)
    ln <- rbind(ln,a)
  }
}

ln.mean.quadri.1315Z <- mean(ln)  
sd.mean.quadri.1315Z <- sd(ln) 


## Data frames ----

# create dataframe with kappa values for bi-cultures

strain <- c("78R.1", "78R.2", "78R.3", "124Z.1", "124Z.2", "124Z.3", "515Z.1", "515Z.2", "515Z.3", 
            "1315Z.1", "1315Z.2", "1315Z.3")

kappa <- c(ln.mean.78R.1, ln.mean.78R.2, ln.mean.78R.3, ln.mean.124Z.1, ln.mean.124Z.2, ln.mean.124Z.3,
           ln.mean.515Z.1, ln.mean.515Z.2, ln.mean.515Z.3, ln.mean.1315Z.1, ln.mean.1315Z.2, ln.mean.1315Z.3)

sd.kappa <- c(sd.mean.78R.1, sd.mean.78R.2, sd.mean.78R.3, sd.mean.124Z.1, sd.mean.124Z.2, sd.mean.124Z.3,
              sd.mean.515Z.1, sd.mean.515Z.2, sd.mean.515Z.3, sd.mean.1315Z.1, sd.mean.1315Z.2, sd.mean.1315Z.3)

table_kappa <- data.frame(strain, kappa, sd.kappa)



# create dataframe with kappa values for quadriculture

strain_quadri <- c("78R.quadri", "124Z.quadri", "515Z.quadri", "1315Z.quadri")

kappa_quadri <- c(ln.mean.quadri.78R, ln.mean.quadri.124Z, ln.mean.quadri.515Z, ln.mean.quadri.1315Z)

sd.kappa_quadri <- c(sd.mean.quadri.78R, sd.mean.quadri.124Z, sd.mean.quadri.515Z, sd.mean.quadri.1315Z)

table_kappa_quadri <- data.frame(strain_quadri, kappa_quadri, sd.kappa_quadri)




# Final data frames ----

## Bi-cultures ----

alpha_kappa_bi <- left_join(table_alpha, table_kappa, by="strain")
alpha_kappa_bi

write_xlsx(alpha_kappa_bi, 'C:\\Users\\Mathias Bonal\\Documents\\R\\Data\\cultures\\int_coef_bi-cultures.xlsx')


## Quadriculture ----

alpha_kappa_quadri <- left_join(table_kappa_quadri, table_kappa_quadri, by="strain_quadri")
alpha_kappa_quadri

write_xlsx(kappa_kappa_quadri, 'C:\\Users\\Mathias Bonal\\Documents\\R\\Data\\cultures\\int_coef_quadriculture.xlsx')







