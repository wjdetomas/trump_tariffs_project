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
# install.packages(c("readxl", "dplyr", "spdep", "spatialreg", "sf", "rgdal", "writexl", "ggplot2", "ggmap","fuzzyjoin","stringr"))

library(readxl)        # For reading Excel files
library(dplyr)         # For data manipulation
library(spdep)         # For spatial weight matrices
library(spatialreg)    # For spatial regression including SDM
library(sf)            # For spatial data handling
library(ggplot2)       # For plotting
library(writexl)       # For saving output
library(fuzzyjoin)     # For name-matching or fuzzy-join routine (country name are slightly mismatched)
library(stringr)       # Clean Whitespace from All Character Columns

#sessionInfo()

# load trade balance data (in usd) for graphical
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

# load tariff rate data for graphical
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

# import GDP (in usd)
gdp_data <- read_excel("data/gdp_clean_rev00.xlsx", skip = 0)

# segregate gdp per year
for (year in 2017:2019) {
  year_col <- as.character(year)
  gdp_year <- gdp_data[,c("country_name",year)]
  assign(paste0("gdp_", year), gdp_year)
  }

rm("year","year_col","gdp_year")


# upload tariff rate and trade flow (in usd)
for (year in 2017:2019) {
  file_name <- paste0("data/tr_xm_",year,"_clean.xlsx")
  
  # Read the sheet 'Partner' into a data frame, col_types argument is used to remove unneeded columns
  trxm_year <- read_excel(file_name, sheet = "Partner",col_types = c("text","text","numeric",
                                                                     "text", "skip","skip",
                                                                     "skip", "skip","numeric",
                                                                     "numeric","numeric","skip",
                                                                     "skip","numeric","numeric",
                                                                     "skip","skip","skip",
                                                                     "skip","skip","skip",
                                                                     "skip","skip","skip",
                                                                     "skip","numeric","numeric",
                                                                     "skip","skip","skip",
                                                                     "skip","skip","skip",
                                                                     "skip","skip","skip",
                                                                     "skip","numeric"))
  
  # Dynamically assign each year's data to a variable
  assign(paste0("trxm_", year), trxm_year)
  }

rm("year","file_name","trxm_year")


# merge gdp and trxm, full join to keep all data
gdp_trxm_2017 <- full_join(
  trxm_2017, gdp_2017,
  by = c("Partner Name" = "Country Name")
  )

# Rows where GDP data is missing
missing_gdp_2017 <- gdp_trxm_2017 %>% filter(is.na(`2017`))

# Rows where trade data is missing
missing_trade_2017 <- gdp_trxm_2017 %>% filter(is.na(`Trade Flow`))

# Remove Unmatched Rows
gdp_trxm_2017 <- gdp_trxm_2017 %>%
  filter(!is.na(`2017`) & !is.na(`Trade Flow`))


# Trying Fuzzy join Partner Name (trade) to Country Name (GDP) to increase obs.
fuzzy_gdp_trxm_2017 <- stringdist_left_join(
  trxm_2017, gdp_2017,
  by = c("Partner Name" = "Country Name"),
  method = "jw",     # Jaro-Winkler distance (good for short strings)
  max_dist = 0.11,    # Tolerance (adjust between 0.1–0.3 if needed)
  distance_col = "dist"
)

# View highest distance (worst match) and inspect before filtering
fuzzy_gdp_trxm_2017 %>%
  arrange(desc(dist)) %>%
  select(`Partner Name`, `Country Name`, dist) %>%
  head(100)

# Keep matches with a good similarity score
clean_gdp_trxm_2017 <- fuzzy_gpd_trxm_2017 %>%
  filter(!is.na(`2017`) & dist < 0.11)

# outcome from fuzzy join might be ambiguous like interchanging Ireland and Iceland
# will use previous merged data set

# Add country coordinates for spatial matrix
geo_coor <- read_excel("data/geo_dist_per_country.xls")
data_2017 <- gdp_trxm_2017 %>% left_join(geo_coor, by = c("Partner Name" = "country"))


# Update values and column names
colnames(data_2017)[5] <- "trade_bal"
data_2017$trade_bal <- data_2017$trade_bal * 1000
colnames(data_2017)[7] <- "trade_imp"
data_2017$trade_imp <- data_2017$trade_imp * 1000
colnames(data_2017)[13] <- "gdp_2017"
colnames(data_2017)[8] <- "ahs_tr"

# Clean NA's
data_2017 <- data_2017 %>% 
  filter(!is.na(`lat`) & !is.na(`lon`))

# Prepare spatial weight matrix

# Contiguity (k-nearest neighbors, k = 5)
coor_matrix <- st_as_sf(data_2017, coords = c("lon", "lat"), crs = 4326)
nb <- knn2nb(knearneigh(st_coordinates(geo_coor_sf), k = 5))  # Create neighbors based on 5 nearest countries
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Distance-based weights (inverse distance)
dist_matrix <- as.matrix(dist(coords_matrix))
dist_inv <- 1 / dist_matrix
diag(dist_inv) <- 0  # remove self-weight
W_dist <- mat2listw(dist_inv, style = "W")

# Convert to spatial data frame
data_2017$log_trade <- log(data_2017$trade_imp + 1)
data_2017$log_gdp <- log(data_2017$gdp_2017 + 1)

# Run Moran’s I test on trade to detect spatial autocorrelation
moran_test <- moran.test(data_2017$log_trade, listw = lw)
print(moran_test)

# Fit OLS model as baseline
ols_model <- lm(log_trade ~ log_gdp + ahs_tr, data = data_2017)
summary(ols_model)

# Run Lagrange Multiplier tests to justify spatial model
lm_tests <- lm.LMtests(ols_model, listw = lw, test = "all")
print(lm_tests)

# Fit Spatial Durbin Model (SDM)
sdm_model <- lagsarlm(log_trade ~ log_gdp + ahs_tr, data = data_2017, listw = lw,
                      type = "mixed", method = "eigen", zero.policy = TRUE)
summary(sdm_model)

# Extract spatial effects (direct, indirect, total)
impacts <- impacts(sdm_model, listw = lw, R = 1000)  # Bootstrapping for inference
summary(impacts)

# Save results
write_xlsx(data_2017, "data/data_2017.xlsx")
saveRDS(sdm_model, "SDM_model_result.rds")

# Visual check of residuals
data_2017$resid <- residuals(sdm_model)
ggplot(merged_data, aes(x = lon, y = lat, fill = resid)) +
  geom_tile() + theme_minimal() + scale_fill_gradient2() +
  labs(title = "Spatial Distribution of SDM Residuals")