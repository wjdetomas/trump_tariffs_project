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
# Install.packages(c("readxl", "dplyr", "spdep", "spatialreg", "sf", "rgdal", "writexl", "ggplot2", "ggmap","fuzzyjoin","stringr","car"))

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

#sessionInfo()

# Load trade balance data (in usd) for graphical
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
       x = "Year", y = "Trade Value (USD)",
       fill = "President",
       caption = "https://data.worldbank.org/indicator/BN.GSR.MRCH.CD?locations=US, accessed May 8, 2025") +
  theme_minimal()

# Load tariff rate data for graphical
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
ggplot(us_net_trade_t, aes(x = Year, y = XM)) +
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

# Distance-based weights (inverse distance)
dist_matrix <- as.matrix(dist(coords))
dist_inv <- 1 / dist_matrix
diag(dist_inv) <- 0  # remove self-weight
W_dist <- mat2listw(dist_inv, style = "W")

# Economic proximity (GDP similarity)
gdp_dist_2017 <- dist(data_2017$gdp_2017)
gdp_inv_2017 <- 1 / as.matrix(gdp_dist_2017)
diag(gdp_inv_2017) <- 0

# Ensure no zero distances (if any distance is zero, set it to a small value)
gdp_inv_2017[gdp_inv_2017 < 1e-10] <- 1e-10
row_sums <- rowSums(gdp_inv_2017)
small_row_sums <- which(abs(row_sums) < 1e-10)
if(length(small_row_sums) > 0) {
  gdp_inv_2017[small_row_sums, ] <- 1e-10  # Replace entire rows with small value if necessary
}

# Compute the GDP spatial weight matrix 
W_gdp <- mat2listw(gdp_inv_2017, style = "W")

# Run basic OLS to get residuals
ols_model <- lm(exp ~ gdp_2017 + ahs_weighted, data = data_2017)
residuals_ols <- residuals(ols_model)
summary(ols_model)

# Moran's I test on OLS residuals
moran.test(residuals_ols, W_knn)
moran.test(residuals_ols, W_dist)
moran.test(residuals_ols, W_gdp)

# -------------------------------
# 4. Lagrange Multiplier Tests
# -------------------------------

lm.LMtests(ols_model, listw = W_knn, test = "all")
lm.LMtests(ols_model, listw = W_dist, test = "all")
lm.LMtests(ols_model, listw = W_gdp, test = "all")

# -------------------------------
# 5. Estimate Spatial Models (SAR, SEM, SDM)
# -------------------------------

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
form <- exp ~ ahs_weighted
form_log <- log_exp ~ gdp_2017 + ahs_weighted
form_std <- log_exp_z ~ gdp_2017_z + ahs_weighted_z

# SDM with different weights
sdm_knn <- lagsarlm(form_std, data = data_2017, listw = W_knn, type = "mixed")
sdm_dist <- lagsarlm(form_std, data = data_2017, listw = W_dist, type = "mixed")
sdm_gdp <- lagsarlm(form_std, data = data_2017, listw = W_gdp, type = "mixed")
summary(sdm_knn)
summary(sdm_dist)
summary(sdm_gdp)

# Compare with SAR
sar_knn <- lagsarlm(form_std, data = data_2017, listw = W_knn)
summary(sar_knn)

# Compare with SEM
sem_knn <- errorsarlm(form, data = data_2017, listw = W_knn)
summary(sem_knn)

# Extract spatial effects (direct, indirect, total)
impacts <- impacts(sdm_gdp, listw = W_gdp, R = 1000)  # Bootstrapping for inference
summary(impacts)

# Save results
write_xlsx(data_2017, "data/data_2017.xlsx")
saveRDS(sdm_gdp, "SDM_model_result.rds")

# Visual check of residuals
data_2017$resid <- residuals(sdm_gdp)
ggplot(data_2017, aes(x = longitude, y = latitude, fill = resid)) +
  geom_tile() + theme_minimal() + scale_fill_gradient2() +
  labs(title = "Spatial Distribution of SDM Residuals")
