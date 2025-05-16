# ------------------------------------------------------------------------------
# Effect of Trump Administration's Tariff Changes(2018)
# Objective: Estimate how trade volumes are influenced by GDP and tariff rates
# accounting for spatial dependencies using the Spatial Durbin Model.
# ------------------------------------------------------------------------------


# set working directory to desired folder
getwd()
setwd("~/Wilson John/learning programming/R/trump_tariffs_project")
options(scipen = 999) #Turn Off Scientific Notation Globally

# Load necessary libraries (uncomment line below if need to install pckgs)
# Install.packages(c("readxl", "dplyr", "spdep", "spatialreg", "sf", "rgdal", "writexl", "ggplot2",
                      "ggmap", "fuzzyjoin", "stringr", "car", "stargazer"))

library(readxl)        # For reading Excel files
library(dplyr)         # For data manipulation
library(spdep)         # For spatial weight matrices
library(spatialreg)    # For spatial regression including SDM
library(sf)            # For spatial data handling
library(ggplot2)       # For plotting
library(writexl)       # For saving output
library(fuzzyjoin)     # For name-matching or fuzzy-join routine (country name are slightly mismatched)
library(stringr)       # Clean Whitespace from All Character Columns
library(car)           # VIF
library(stargazer)     # For summarizing model outputs

#sessionInfo()

# Import trade balance data (in usd) for graphical
net_trade <- read_excel("data/API_BN.GSR.MRCH.CD_DS2_en_excel_v2_85628.xls",skip = 3)
us_net_trade <- net_trade %>% 
  filter(`Country Name` == "United States")
us_net_trade <- us_net_trade[,-c(1:4)]
us_net_trade_t <- data.frame(
  Year = as.numeric(names(us_net_trade)),
  XM = as.numeric(t(us_net_trade))
  )
presidents <- c(1974, 1977, 1981, 1989, 1993, 2001, 2009, 2017, 2021)
ggplot(us_net_trade_t, aes(x = Year, y = XM)) +
  geom_line(color = "blue", size = 1.2) +
  geom_vline(xintercept = presidents, linetype = "dashed", color = "red") +
  labs(title = "US Trade Flow (1970–2023)",
       x = "Year",
       y = "Trade Value (USD)",
       caption = "https://data.worldbank.org/indicator/BN.GSR.MRCH.CD?locations=US, accessed May 8, 2025") +
  theme_minimal()

pres_term_df <- data.frame(
  start = c(1969, 1974, 1977, 1981, 1989, 1993, 2001, 2009, 2017, 2021),
  end   = c(1974, 1977, 1981, 1989, 1993, 2001, 2009, 2017, 2021, 2025),
  president = c("Nixon", "Ford", "Carter", "Reagan", "Bush Sr.", "Clinton", 
                "Bush Jr.", "Obama", "Trump", "Biden"))
pres_term_df$president <- factor(pres_term_df$president, levels = pres_term_df$president)
ggplot(us_net_trade_t, aes(x = Year, y = XM)) +
  geom_rect(data = pres_term_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = president),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_line(color = "blue", size = 1.2) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "US Trade Flow by Presidential Term",
       x = "Year", y = "Balance of Trade (USD)",
       fill = "President",
       caption = "https://data.worldbank.org/indicator/BN.GSR.MRCH.CD?locations=US, accessed May 8, 2025") +
  theme_minimal()

# Import GDP
gdp <- read_excel("data/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_19310.xls",skip = 3)
us_gdp <- net_trade %>% 
  filter(`Country Name` == "United States")
us_net_trade <- us_net_trade[,-c(1:4)]
us_net_trade_t <- data.frame(
  Year = as.numeric(names(us_net_trade)),
  XM = as.numeric(t(us_net_trade))
)

# Import tariff rate data for graphical
tr <- read_excel("data/API_TM.TAX.MRCH.WM.AR.ZS_DS2_en_excel_v2_85076.xls",skip = 3)
us_tr <- tr %>% 
  filter(`Country Name` == "United States")
us_tr <- us_tr[,-c(1:4)]
us_tr_t <- data.frame(
  Year = as.numeric(names(us_tr)),
  TR = as.numeric(t(us_tr))
  )
ggplot(us_tr_t, aes(x = Year, y = TR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_vline(xintercept = presidents, linetype = "dashed", color = "red") +
  labs(title = "US Tariff Rate (1970–2023)",
       x = "Year",
       y = "Tariff Rate (all products)",
       caption = "https://data.worldbank.org/indicator/TM.TAX.MRCH.WM.AR.ZS?locations=US, accessed May 8, 2025") +
  theme_minimal()
ggplot(us_tr_t, aes(x = Year, y = TR)) +
  geom_rect(data = pres_term_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = president),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_line(color = "blue", size = 1.2) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "US Tariff Rate by Presidential Term",
       x = "Year", y = "Tariff Rate (all products)",
       fill = "President",
       caption = "https://data.worldbank.org/indicator/BN.GSR.MRCH.CD?locations=US, accessed May 8, 2025") +
  theme_minimal()

rm("pres_term_df","presidents","president")


# Import GDP (in usd)
gdp_data <- read_excel("data/gdp_clean_rev00.xlsx", skip = 0)

# Extract 2017 to 2019
for (year in 2017:2019) {
  year_col <- as.character(year)
  gdp_year <- gdp_data[,c("country_name",year)]
  assign(paste0("gdp_", year), gdp_year)
  }

rm("year","year_col","gdp_year")


# Upload tariff rate and trade flow (in usd)
for (year in 2017:2019) {
  file_name <- paste0("data/tr_xm_",year,"_clean.xlsx")
  
  # Read the sheet 'Partner' into a data frame, col_types argument is used to remove unneeded columns
  trxm_year <- read_excel(file_name, sheet = "Partner")
  
  # Dynamically assign each year's data to a variable
  assign(paste0("trxm_", year), trxm_year)
  }

rm("year","file_name","trxm_year")


# merge gdp and trxm, full join to keep all data
gdp_trxm_2017 <- full_join(
  trxm_2017, gdp_2017,
  by = "country_name")

colnames(gdp_trxm_2017)[length(colnames(gdp_trxm_2017))] <- "gdp_2017"

# Rows where GDP data is missing
missing_gdp_2017 <- gdp_trxm_2017 %>% filter(is.na(`gdp_2017`))

# Rows where trade data is missing
missing_trade_2017 <- gdp_trxm_2017 %>% filter(is.na(`trade_bal`))

# Remove Unmatched Rows
gdp_trxm_2017 <- gdp_trxm_2017 %>%
  filter(!is.na(`gdp_2017`) & !is.na(`trade_bal`))

# Descriptive Statistics
summary(gdp_trxm_2017[, c("trade_bal", "gdp_2017", "ahs_weighted")])
cor(gdp_trxm_2017[, c("trade_bal", "gdp_2017", "ahs_weighted")])


# Add country coordinates for spatial matrix
geo_coor <- read_excel("data/geo_data_clean_rev01.xlsx")
data_2017 <- gdp_trxm_2017 %>% left_join(geo_coor, by = "country_name")


# Clean NA's
data_2017 <- data_2017 %>% 
  filter(!is.na(`latitude`) & !is.na(`longitude`))

# Prepare spatial weight matrix

# Contiguity (k-nearest neighbors, k = 5)
coor_matrix <- st_as_sf(data_2017, coords = c("longitude", "latitude"), crs = 4326)
coords <- st_coordinates(coor_matrix)
knn_nb <- knearneigh(coords, k = 5)  # Create neighbors based on 5 nearest countries
knn_list <- knn2nb(knn_nb)
W_knn <- nb2listw(knn_list, style = "W")

# Run basic OLS to get residuals
ols_model <- lm(trade_bal ~ ahs_weighted, data = data_2017)
residuals_ols <- residuals(ols_model)
summary(ols_model)

# Moran's I test on OLS residuals
moran.test(residuals_ols, W_knn)

# Lagrange Multiplier Tests
lm.LMtests(ols_model, listw = W_knn, test = "all")


# Estimate Spatial Models (SAR, SEM, SDM)
# Scale data to avoid numerical stability of the model (failure in the inversion of the asymptotic covariance matrix)
# Avoid problem with collinearity or redundancy in the data or the spatial structure
vif(lm(form, data = data_2017))
data_2017$trade_bal <- scale(data_2017$trade_bal)
data_2017$exp <- scale(data_2017$exp)
data_2017$gdp_2017 <- scale(data_2017$gdp_2017)
data_2017$ahs_weighted <- scale(data_2017$ahs_weighted)

# Alternatively, logging the data to avoid numerical stability of the model
data_2017$log_exp <- log(data_2017$exp)

# Scaling
data_2017$gdp_2017_z <- scale(data_2017$gdp_2017)
data_2017$ahs_weighted_z <- scale(data_2017$ahs_weighted)
data_2017$log_exp_z <- scale(log(data_2017$exp))

# Define formula
form <- trade_bal ~ ahs_weighted

# SDM with different weights
sdm_knn <- lagsarlm(form, data = data_2017, listw = W_knn, type = "mixed")
summary(sdm_knn)

# Compare with SAR
sar_knn <- lagsarlm(form, data = data_2017, listw = W_knn)
summary(sar_knn)

# Compare with SEM
sem_knn <- errorsarlm(form, data = data_2017, listw = W_knn)
summary(sem_knn)

# -----------------test for gdp and ahs tariff---------------------------
# Run basic OLS to get residuals
gdp_ols_model <- lm(gdp_2017_z ~ ahs_weighted, data = data_2017)
gdp_residuals_ols <- residuals(gdp_ols_model)
summary(gdp_ols_model)

# Moran's I test on OLS residuals
moran.test(gdp_residuals_ols, W_knn)

# Lagrange Multiplier Tests
lm.LMtests(gdp_ols_model, listw = W_knn, test = "all")

# Define formula
form_gdp <- gdp_2017_z ~ ahs_weighted

# SDM with different weights
sdm_knn <- lagsarlm(form_gdp, data = data_2017, listw = W_knn, type = "mixed")
summary(sdm_knn)


# ====2018=====


# merge gdp and trxm, full join to keep all data
gdp_trxm_2018 <- full_join(
  trxm_2018, gdp_2018,
  by = "country_name")

colnames(gdp_trxm_2018)[length(colnames(gdp_trxm_2018))] <- "gdp_2018"

# Rows where GDP data is missing
missing_gdp_2018 <- gdp_trxm_2018 %>% filter(is.na(`gdp_2018`))

# Rows where trade data is missing
missing_trade_2018 <- gdp_trxm_2018 %>% filter(is.na(`trade_bal`))

# Remove Unmatched Rows
gdp_trxm_2018 <- gdp_trxm_2018 %>%
  filter(!is.na(`gdp_2018`) & !is.na(`trade_bal`))

# Descriptive Statistics
summary(gdp_trxm_2018[, c("trade_bal", "gdp_2018", "ahs_weighted")])
cor(gdp_trxm_2018[, c("trade_bal", "gdp_2018", "ahs_weighted")])

# Add country coordinates for spatial matrix
geo_coor <- read_excel("data/geo_data_clean_rev01.xlsx")
data_2018 <- gdp_trxm_2018 %>% left_join(geo_coor, by = "country_name")

# Clean NA's
data_2018 <- data_2018 %>% 
  filter(!is.na(`latitude`) & !is.na(`longitude`))

# Prepare spatial weight matrix
# Contiguity (k-nearest neighbors, k = 5)
coor_matrix <- st_as_sf(data_2018, coords = c("longitude", "latitude"), crs = 4326)
coords <- st_coordinates(coor_matrix)
knn_nb <- knearneigh(coords, k = 5)  # Create neighbors based on 5 nearest countries
knn_list <- knn2nb(knn_nb)
W_knn <- nb2listw(knn_list, style = "W")

# Run basic OLS to get residuals
ols_model <- lm(trade_bal ~ ahs_weighted, data = data_2018)
residuals_ols <- residuals(ols_model)
summary(ols_model)

# Moran's I test on OLS residuals
moran.test(residuals_ols, W_knn)

# Lagrange Multiplier Tests
lm.LMtests(ols_model, listw = W_knn, test = "all")

# Estimate Spatial Models (SAR, SEM, SDM)
# Scale data to avoid numerical stability of the model (failure in the inversion of the asymptotic covariance matrix)
# Avoid problem with collinearity or redundancy in the data or the spatial structure
vif(lm(form, data = data_2018))

data_2018$trade_bal <- scale(data_2018$trade_bal)
data_2018$exp <- scale(data_2018$exp)
data_2018$gdp_2018 <- scale(data_2018$gdp_2018)
data_2018$ahs_weighted <- scale(data_2018$ahs_weighted)

# Alternatively, logging the data to avoid numerical stability of the model
data_2018$log_exp <- log(data_2018$exp)

# Scaling
data_2018$gdp_2018_z <- scale(data_2018$gdp_2018)
data_2018$ahs_weighted_z <- scale(data_2018$ahs_weighted)
data_2018$log_exp_z <- scale(log(data_2018$exp))

# Define formula
form <- trade_bal ~ ahs_weighted

# SDM with different weights
sdm_knn <- lagsarlm(form, data = data_2018, listw = W_knn, type = "mixed")
summary(sdm_knn)

# Compare with SAR
sar_knn <- lagsarlm(form, data = data_2018, listw = W_knn)
summary(sar_knn)

# Compare with SEM
sem_knn <- errorsarlm(form, data = data_2018, listw = W_knn)
summary(sem_knn)

# -----------------test for gdp and ahs tariff---------------------------
# Run basic OLS to get residuals
gdp_ols_model <- lm(gdp_2018_z ~ ahs_weighted, data = data_2018)
gdp_residuals_ols <- residuals(gdp_ols_model)
summary(gdp_ols_model)

# Moran's I test on OLS residuals
moran.test(gdp_residuals_ols, W_knn)

# Lagrange Multiplier Tests
lm.LMtests(gdp_ols_model, listw = W_knn, test = "all")

# Define formula
form_gdp <- gdp_2018_z ~ ahs_weighted

# SDM with different weights
sdm_knn <- lagsarlm(form_gdp, data = data_2018, listw = W_knn, type = "mixed")
summary(sdm_knn)

# ====2019=====

# merge gdp and trxm, full join to keep all data
gdp_trxm_2019 <- full_join(
  trxm_2019, gdp_2019,
  by = "country_name")

colnames(gdp_trxm_2019)[length(colnames(gdp_trxm_2019))] <- "gdp_2019"

# Rows where GDP data is missing
missing_gdp_2019 <- gdp_trxm_2019 %>% filter(is.na(`gdp_2019`))

# Rows where trade data is missing
missing_trade_2019 <- gdp_trxm_2019 %>% filter(is.na(`trade_bal`))

# Remove Unmatched Rows
gdp_trxm_2019 <- gdp_trxm_2019 %>%
  filter(!is.na(`gdp_2019`) & !is.na(`trade_bal`))

# Descriptive Statistics
summary(gdp_trxm_2019[, c("trade_bal", "gdp_2019", "ahs_weighted")])
cor(gdp_trxm_2019[, c("trade_bal", "gdp_2019", "ahs_weighted")])


# Add country coordinates for spatial matrix
geo_coor <- read_excel("data/geo_data_clean_rev01.xlsx")
data_2019 <- gdp_trxm_2019 %>% left_join(geo_coor, by = "country_name")

# Clean NA's
data_2019 <- data_2019 %>% 
  filter(!is.na(`latitude`) & !is.na(`longitude`))

# Prepare spatial weight matrix
# Contiguity (k-nearest neighbors, k = 5)
coor_matrix <- st_as_sf(data_2019, coords = c("longitude", "latitude"), crs = 4326)
coords <- st_coordinates(coor_matrix)
knn_nb <- knearneigh(coords, k = 5)  # Create neighbors based on 5 nearest countries
knn_list <- knn2nb(knn_nb)
W_knn <- nb2listw(knn_list, style = "W")

# Run basic OLS to get residuals
ols_model <- lm(trade_bal ~ ahs_weighted, data = data_2019)
residuals_ols <- residuals(ols_model)
summary(ols_model)

# Moran's I test on OLS residuals
moran.test(residuals_ols, W_knn)

# Lagrange Multiplier Tests
lm.LMtests(ols_model, listw = W_knn, test = "all")

# Estimate Spatial Models (SAR, SEM, SDM)
# Scale data to avoid numerical stability of the model (failure in the inversion of the asymptotic covariance matrix)
# Avoid problem with collinearity or redundancy in the data or the spatial structure
vif(lm(form, data = data_2019))

data_2019$trade_bal <- scale(data_2019$trade_bal)
data_2019$exp <- scale(data_2019$exp)
data_2019$gdp_2019 <- scale(data_2019$gdp_2019)
data_2019$ahs_weighted <- scale(data_2019$ahs_weighted)

# Alternatively, logging the data to avoid numerical stability of the model
data_2019$log_exp <- log(data_2019$exp)

# Scaling
data_2019$gdp_2019_z <- scale(data_2019$gdp_2019)
data_2019$ahs_weighted_z <- scale(data_2019$ahs_weighted)
data_2019$log_exp_z <- scale(log(data_2019$exp))

# Define formula
form <- trade_bal ~ ahs_weighted

# SDM with different weights
sdm_knn <- lagsarlm(form, data = data_2019, listw = W_knn, type = "mixed")
summary(sdm_knn)

# Compare with SAR
sar_knn <- lagsarlm(form, data = data_2019, listw = W_knn)
summary(sar_knn)

# Compare with SEM
sem_knn <- errorsarlm(form, data = data_2019, listw = W_knn)
summary(sem_knn)

# -----------------test for gdp and ahs tariff---------------------------
# Run basic OLS to get residuals
gdp_ols_model <- lm(gdp_2019_z ~ ahs_weighted, data = data_2019)
gdp_residuals_ols <- residuals(gdp_ols_model)
summary(gdp_ols_model)

# Moran's I test on OLS residuals
moran.test(gdp_residuals_ols, W_knn)

# Lagrange Multiplier Tests
lm.LMtests(gdp_ols_model, listw = W_knn, test = "all")

# Define formula
form_gdp <- gdp_2019_z ~ ahs_weighted

# SDM with different weights
sdm_knn <- lagsarlm(form_gdp, data = data_2019, listw = W_knn, type = "mixed")
summary(sdm_knn)