#To start off we can clear all the variables from the current environment and close all the plots.

rm(list = ls())
graphics.off()


#We will need to make use of the vars and tsm packages
library(tsm)
library(vars)
library("svars")
library(mFilter)
library (readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(kableExtra)



#To load the data and store the time series objects
setwd("C:/Users/akhud/OneDrive/Рабочий стол/Finish it/Data")

data<-read_csv("C:/Users/akhud/OneDrive/Рабочий стол/Finish it/Data/data.csv", show_col_types = FALSE)

#This allows us to load the data and store the time series objects as gdp and une.

gdp <- ts(diff(log(data$GDP)), start= c(2019, 1), freq = 4) # Correct way to construct the data
cpi <- ts(diff(log(data$CPI)), start= c(2019, 1), freq = 4)
prate <- ts(diff(log(data$FEDFUNDS)), start= c(2019, 1), freq = 4) # Not necessesary to take to take log and diff
empration <- ts(data$EMRATIO, start= c(2019, 1), freq = 4) # Not necessesary to take to take log and diff
wti_oil <- ts(diff(log(data$WTI_OIL)), start= c(2019, 1), freq = 4)
divisia <- ts(diff(log(data$DIVISIA_M3)), start= c(2019, 1), freq = 4)
gscpi <- ts(diff(log(data$GSCPI)), start= c(2019, 1), freq = 4) # Not necessesary to take to take log and diff
gov_total <- ts(diff(log(data$GOV_SPEND)), start= c(2019, 1), freq = 4)
total_market <- ts(diff(log(data$GOV_DEBT)), start= c(2019, 1), freq = 4)


gdp <- ts(c(gdp, 0.01479994), start = c(2019, 1), frequency = 4)
cpi <- ts(c(cpi, 0.01028919), start = c(2019, 1), frequency = 4)
wti_oil <- ts(c(wti_oil, 0.01882705), start = c(2019, 1), frequency = 4)
divisia <- ts(c(divisia, 0.01204757), start = c(2019, 1), frequency = 4)
gov_total <- ts(c(gov_total, 0.01691471), start = c(2019, 1), frequency = 4)
total_market <- ts(c(total_market, 0.02069587), start = c(2019, 1), frequency = 4)
prate <- ts(c(prate, 0.04199382), start = c(2019, 1), frequency = 4)
gscpi <- ts(c(gscpi, 0.05534446), start = c(2019, 1), frequency = 4)

#Plot the data
plot(cbind(gdp, cpi, prate, empration, wti_oil, divisia, gov_total, total_market))


##################################### 1. GDP ########################################
# To graph the log of the variables 
plot(gdp)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gdp.ac <- acf(gdp, main = "GDP")

#To check for a unit root
adf.gdp <- ur.df(gdp,lags = 4, type = "trend",  selectlags = "AIC")
summary(adf.gdp)


##################################### 2. CPI ########################################
# To graph the log of the variables 
plot(cpi)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
cpi.acf <- acf(cpi, main = "CPI")

#To check for a unit root
adf.cpi <- ur.df(cpi, lags = 4,type = "trend",  selectlags = "AIC")
summary(adf.cpi)


##################################### 3. POLICY RATE ########################################
# To graph the log of the variables 
plot(prate)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
prate.acf <- acf(prate, main = "POLICY RATE")

#To check for a unit root
adf.prate <- ur.df(prate,lags = 4, type = "trend", selectlags = "AIC")
summary(adf.prate)


##################################### 4. EM RATIO ########################################
# To graph the log of the variables 
plot(empration)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
empration.acf <- acf(empration, main = "EM RATIO")

#To check for a unit root
adf.empration <- ur.df(empration,lags = 4, type = "trend", selectlags = "AIC")
summary(adf.empration)


##################################### 5. WTI OIL TR ########################################
# To graph the log of the variables 
plot(wti_oil)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
wti_oil.acf <- acf(wti_oil, main = "WTI OIL")

#To check for a unit root
adf.wti_oil <- ur.df(wti_oil,lags = 4, type = "trend", selectlags = "AIC")
summary(adf.wti_oil)


##################################### 6. DIVISIA ########################################
# To graph the log of the variables 
plot(divisia)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
divisia.acf <- acf(divisia, main = "DIVISIA")

#To check for a unit root
adf.divisia <- ur.df(divisia,lags = 4, type = "trend", selectlags = "AIC")
summary(adf.divisia)

##################################### 7. GOV TOTAL ########################################
# To graph the log of the variables 
plot(gov_total)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gov_total.acf <- acf(gov_total, main = "GOV TOTAL")

#To check for a unit root
adf.gov_total <- ur.df(gov_total,lags = 2, type = "trend", selectlags = "AIC")
summary(adf.gov_total)

##################################### 8. TORAL MARKET ########################################
# To graph the log of the variables 
plot(total_market)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
total_market.acf <- acf(total_market, main = "TORAL MARKET")

#To check for a unit root
adf.total_market <- ur.df(total_market,lags = 4, type = "trend", selectlags = "AIC")
summary(adf.total_market)

##################################### 9. GSCPI ########################################
# To graph the log of the variables 
plot(gscpi)

#To consider the degree of persistence in the data we make use of the autocorrelation function.
gscpi.acf <- acf(gscpi, main = "GSCPI")

#To check for a unit root
adf.gscpi <- ur.df(gscpi,lags = 4, type = "trend",  selectlags = "AIC")
summary(adf.gscpi)



# MODEL SELECTION AND ESTIMATION

data.bv <- cbind(gscpi, wti_oil, prate, divisia, gov_total, total_market, gdp, empration, cpi)
data.bv

colnames(data.bv) <- c("gscpi","wti_oil","prate", "divisia", "gov_total", "total_market", "gdp","empration", "cpi")

info.bv <- VARselect(data.bv, lag.max = 8, type = "both")
info.bv$selection

var_model <- VAR(data.bv, p = 1, type = "both")
var_model

# To set-up the matrix for the contemporaneous coefficients, we need to make use of a matrix that has the appropriate 
# dimensions. This is easily achieved with the aid of the diagnol matrix. To code this appropriately we need to insert 
# zeros for restrictions and NA in all those places that would not pertain to a restriction. 
# Hence,

# Number of variables
n <- 9

# Create an identity matrix of size 9x9
a.mat <- diag(n)

# Set diagonal elements to NA (these will be estimated)
diag(a.mat) <- NA

# Implement Cholesky-type restrictions:
# wti_oil (2nd variable) is contemporaneously affected by gscpi (1st variable)
a.mat[2, 1] <- NA  # wti_oil affected by gscpi

# prate (3rd variable) is affected by gscpi and wti_oil
a.mat[3, 1] <- NA  # prate affected by gscpi
a.mat[3, 2] <- NA  # prate affected by wti_oil

# divisia (4th variable) is affected by gscpi, wti_oil, and prate
a.mat[4, 1] <- NA  # divisia affected by gscpi
a.mat[4, 2] <- NA  # divisia affected by wti_oil
a.mat[4, 3] <- NA  # divisia affected by prate

# gov_total (5th variable) is affected by gscpi, wti_oil, prate, and divisia
a.mat[5, 1] <- NA  # gov_total affected by gscpi
a.mat[5, 2] <- NA  # gov_total affected by wti_oil
a.mat[5, 3] <- NA  # gov_total affected by prate
a.mat[5, 4] <- NA  # gov_total affected by divisia

# total_market (6th variable) is affected by all preceding variables
a.mat[6, 1] <- NA  # total_market affected by gscpi
a.mat[6, 2] <- NA  # total_market affected by wti_oil
a.mat[6, 3] <- NA  # total_market affected by prate
a.mat[6, 4] <- NA  # total_market affected by divisia
a.mat[6, 5] <- NA  # total_market affected by gov_total

# gdp (7th variable) is affected by all preceding variables
a.mat[7, 1] <- NA  # gdp affected by gscpi
a.mat[7, 2] <- NA  # gdp affected by wti_oil
a.mat[7, 3] <- NA  # gdp affected by prate
a.mat[7, 4] <- NA  # gdp affected by divisia
a.mat[7, 5] <- NA  # gdp affected by gov_total
a.mat[7, 6] <- NA  # gdp affected by total_market

# empration (8th variable) is affected by all preceding variables
a.mat[8, 1] <- NA  # empration affected by gscpi
a.mat[8, 2] <- NA  # empration affected by wti_oil
a.mat[8, 3] <- NA  # empration affected by prate
a.mat[8, 4] <- NA  # empration affected by divisia
a.mat[8, 5] <- NA  # empration affected by gov_total
a.mat[8, 6] <- NA  # empration affected by total_market
a.mat[8, 7] <- NA  # empration affected by gdp

# cpi (9th variable) is affected by all preceding variables
a.mat[9, 1] <- NA  # cpi affected by gscpi
a.mat[9, 2] <- NA  # cpi affected by wti_oil
a.mat[9, 3] <- NA  # cpi affected by prate
a.mat[9, 4] <- NA  # cpi affected by divisia
a.mat[9, 5] <- NA  # cpi affected by gov_total
a.mat[9, 6] <- NA  # cpi affected by total_market
a.mat[9, 7] <- NA  # cpi affected by gdp
a.mat[9, 8] <- NA  # cpi affected by empration

# Print the matrix to check it
print(a.mat)

# We then need to set-up the matrix for the identification of individual shocks. 
# Once again starting with the diagnol matrix, we need to insert zeros into the covariance terms,
# while the variance for each of the individual shocks is to be retrieved.
# Hence,

# Number of variables (K = 9)
K <- 9

# Create a diagonal matrix of size 9x9
b.mat <- diag(K)

# Set diagonal elements to NA (these will be estimated)
diag(b.mat) <- NA

# Set upper triangular elements to 0 (since we're using Cholesky decomposition)
b.mat[upper.tri(b.mat)] <- 0

# Print the matrix to check it
print(b.mat)

# We are finally at a point where we can estimate the SVAR(1) model. 

svar.one <- SVAR(var_model, Amat = a.mat)
svar.one

#Creating the coefficient matrix from the printed output
A_matrix <- matrix(c(0.72297, 0.000, 0.0000, 0.000, 0.000, 0.000, 0.000, 0.0000, 0.00,
                     0.58440, 0.823, 0.0000, 0.000, 0.000, 0.000, 0.000, 0.0000, 0.00,
                     0.35111, -4.949, 0.2279, 0.000, 0.000, 0.000, 0.000, 0.0000, 0.00,
                     -0.39419, -8.146, 60.188, 0.000, 0.000, 0.000, 0.000, 0.0000, 0.00,
                     0.10619, -0.326, 1.4308, -5.509, 8.368, 0.000, 0.000, 0.0000, 0.00,
                     -0.44859, -12.908, -8.547, -1.271, 67.224, 0.000, 0.000, 0.0000, 0.00,
                     -0.18910, -5.523, 0.3547, 16.059, 8.774, 43.087, 93.332, 0.0000, 0.00,
                     -0.66541, -3.393, -1.0482, -24.671, -8.867, 18.389, 20.174, 5.2544, 0.00,
                     0.09806, -1.832, 0.3349, 1.104, 2.493, 5.231, 1.081, -0.8923, 82.49),
                   nrow = 9, byrow = TRUE)

colnames(A_matrix) <- c("gscpi", "wti_oil", "prate", "divisia", "gov_total", "total_market", "gdp", "empration", "cpi")
rownames(A_matrix) <- colnames(A_matrix)

# Convert matrix to a data frame for better presentation
A_matrix_df <- as.data.frame(A_matrix)
print(A_matrix_df)

# Assuming coef_matrix is your matrix
coef_matrix <- matrix(c(
  0.72297, 0, 0, 0, 0, 0, 0, 0, 0,
  0.58440, 0.823, 0, 0, 0, 0, 0, 0, 0,
  0.35111, -4.949, 0.2279, 0, 0, 0, 0, 0, 0,
  -0.39419, -8.146, 60.1880, 0, 0, 0, 0, 0, 0,
  0.10619, -0.326, 1.4308, -5.509, 8.368, 0, 0, 0, 0,
  -0.44859, -12.908, -8.5470, -1.271, 67.224, 0, 0, 0, 0,
  -0.18910, -55.523, 0.3547, 16.059, 8.774, 43.087, 93.332, 0, 0,
  -0.66541, -3.393, -1.0482, -24.671, -8.867, 18.389, 20.174, 5.2544, 0,
  0.09806, -1.832, 0.3349, 1.104, 2.493, 5.231, 1.081, -0.8923, 82.49
), nrow = 9, byrow = TRUE)

# Adding row and column names
rownames(coef_matrix) <- colnames(coef_matrix) <- c("gscpi", "wti_oil", "prate", "divisia", "gov_total", "total_market", "gdp", "empration", "cpi")

# Creating the table
kable(coef_matrix, format = "html", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE, position = "left") %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)





# MODEL SELECTION AND ESTIMATION - Impulse response functions

# Number of periods ahead for forecasting
n.ahead <- 15  

##################################### GSCPI ########################################

# IRF for CPI in response to a shock in GSCPI
irf_gscpi <- irf(svar.one, response = "cpi", impulse = "gscpi", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_gscpi$irf$gscpi[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: GSCPI to CPI",
     ylim = range(c(irf_gscpi$irf$gscpi[, "cpi"], irf_gscpi$Lower$gscpi[, "cpi"], irf_gscpi$Upper$gscpi[, "cpi"])))

# Adding confidence intervals
lines(irf_gscpi$Lower$gscpi[, "cpi"], col = "red", lty = 2)
lines(irf_gscpi$Upper$gscpi[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


##################################### WTI OIL ########################################

# IRF for CPI in response to a shock in WTI Oil Prices
irf_wti_oil <- irf(svar.one, response = "cpi", impulse = "wti_oil", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_wti_oil$irf$wti_oil[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: WTI Oil Prices to CPI",
     ylim = range(c(irf_wti_oil$irf$wti_oil[, "cpi"], irf_wti_oil$Lower$wti_oil[, "cpi"], irf_wti_oil$Upper$wti_oil[, "cpi"])))

# Adding confidence intervals
lines(irf_wti_oil$Lower$wti_oil[, "cpi"], col = "red", lty = 2)
lines(irf_wti_oil$Upper$wti_oil[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")



##################################### POLICY RATE ########################################

# IRF for CPI in response to a shock in Policy Rate
irf_prate <- irf(svar.one, response = "cpi", impulse = "prate", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_prate$irf$prate[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: Policy Rate to CPI",
     ylim = range(c(irf_prate$irf$prate[, "cpi"], irf_prate$Lower$prate[, "cpi"], irf_prate$Upper$prate[, "cpi"])))

# Adding confidence intervals
lines(irf_prate$Lower$prate[, "cpi"], col = "red", lty = 2)
lines(irf_prate$Upper$prate[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


##################################### DIVISIA ########################################

# IRF for CPI in response to a shock in Divisia
irf_divisia <- irf(svar.one, response = "cpi", impulse = "divisia", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_divisia$irf$divisia[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: Divisia to CPI",
     ylim = range(c(irf_divisia$irf$divisia[, "cpi"], irf_divisia$Lower$divisia[, "cpi"], irf_divisia$Upper$divisia[, "cpi"])))

# Adding confidence intervals
lines(irf_divisia$Lower$divisia[, "cpi"], col = "red", lty = 2)
lines(irf_divisia$Upper$divisia[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


#####################################  GOV TOTAL ########################################


# IRF for CPI in response to a shock in Government Total Spending
irf_gov_total <- irf(svar.one, response = "cpi", impulse = "gov_total", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_gov_total$irf$gov_total[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: Government Total Spending to CPI",
     ylim = range(c(irf_gov_total$irf$gov_total[, "cpi"], irf_gov_total$Lower$gov_total[, "cpi"], irf_gov_total$Upper$gov_total[, "cpi"])))

# Adding confidence intervals
lines(irf_gov_total$Lower$gov_total[, "cpi"], col = "red", lty = 2)
lines(irf_gov_total$Upper$gov_total[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


#####################################  TORAL MARKET ########################################

# IRF for CPI in response to a shock in Government Debt (Total Market)
irf_total_market <- irf(svar.one, response = "cpi", impulse = "total_market", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_total_market$irf$total_market[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: Government Debt to CPI",
     ylim = range(c(irf_total_market$irf$total_market[, "cpi"], irf_total_market$Lower$total_market[, "cpi"], irf_total_market$Upper$total_market[, "cpi"])))

# Adding confidence intervals
lines(irf_total_market$Lower$total_market[, "cpi"], col = "red", lty = 2)
lines(irf_total_market$Upper$total_market[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")



##################################### GDP ########################################

# IRF for CPI in response to a shock in GDP
irf_gdp <- irf(svar.one, response = "cpi", impulse = "gdp", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_gdp$irf$gdp[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: GDP to CPI",
     ylim = range(c(irf_gdp$irf$gdp[, "cpi"], irf_gdp$Lower$gdp[, "cpi"], irf_gdp$Upper$gdp[, "cpi"])))

# Adding confidence intervals
lines(irf_gdp$Lower$gdp[, "cpi"], col = "red", lty = 2)
lines(irf_gdp$Upper$gdp[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")



##################################### EM RATIO ########################################

# IRF for CPI in response to a shock in Employment Ratio
irf_empration <- irf(svar.one, response = "cpi", impulse = "empration", n.ahead = n.ahead, ortho = TRUE, boot = TRUE)

# Custom Plot
plot(irf_empration$irf$empration[, "cpi"], type = "l", col = "black", lwd = 2, 
     xlab = "Periods", ylab = "Response of CPI", 
     main = "Impulse Response Function: Employment Ratio to CPI",
     ylim = range(c(irf_empration$irf$empration[, "cpi"], irf_empration$Lower$empration[, "cpi"], irf_empration$Upper$empration[, "cpi"])))

# Adding confidence intervals
lines(irf_empration$Lower$empration[, "cpi"], col = "red", lty = 2)
lines(irf_empration$Upper$empration[, "cpi"], col = "red", lty = 2)

# Adding a horizontal line at 0 for reference
abline(h = 0, col = "black", lty = 3)

# Adding a legend
legend("topright", legend = c("IRF", "95% CI"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Adding grid lines for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
