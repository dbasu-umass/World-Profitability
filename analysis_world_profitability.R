
# ---- Libraries to use
library(foreign)
library(ggplot2)
library(readxl)
library(tidyverse)
library(stargazer)
library(plm)
library(xtable)


# ---- Function to carry out medium run decomposition ---- #
decomp_mr <- function(x){
  
  # x is a data frame with four columns arranged as
  # follows: year, rop, pshare, rho
  
  n <- nrow(x)
  
  # Growth rate of profit rate
  g_rop <- coef(
    lm(log(x[,2]) ~ x[,1])
  )[2]
  
  # Growth rate of profit share
  g_pshare <- coef(
    lm(log(x[,3]) ~ x[,1])
  )[2]
  
  # Growth rate of output-capital ratio
  g_rho <- coef(
    lm(log(x[,4]) ~ x[,1])
  )[2]
  
  # Result
  return(
    c(x[1,1], x[n,1],
      round(100*g_rop, digits = 2),
      round(100*g_pshare, digits = 2),
      round(100*g_rho, digits = 2)
    )
  )
  
}


# -------------------------------------------------------------- #
# ---------------------- EPWT 7.0 data set  -------------------- #

# Read data (Preliminary version)
d1_e <- read_excel("EPWT 7.0 FV.xlsx", 
                   sheet =  "EPWT7.0"
)


# ----- All countries ---------- #
# Create variables using national currency values
d2 <- d1_e %>%
  dplyr::select(
    Country,Year,LabShare,rhonatcur,rnatcur
  ) %>%
  dplyr::mutate(
    pshare = 1-LabShare,
    # net rate of profit (measured in national currency units)
    rop = 100*rnatcur
  ) %>%
  dplyr::rename(
    ps_epwt7 = pshare,
    ocr_epwt7 = rhonatcur,
    rop_epwt7 = rop) %>%
  dplyr::select(Country,Year,ps_epwt7,ocr_epwt7,rop_epwt7) %>%
  group_by(Year) %>%
  summarise(
    # mean
    mean_rop = mean(rop_epwt7, na.rm = TRUE),
    # standard deviation
    sd_rop = sd(rop_epwt7, na.rm = TRUE),
    # median        
    med_rop =  median(rop_epwt7, na.rm = TRUE),
    # interquartile range        
    iqr_rop = IQR(rop_epwt7, na.rm = TRUE),
    # number of non-missing observations        
    n_rop = sum(!is.na(rop_epwt7))) %>%
  pivot_longer(!c("Year"),
               names_to="vars",
               values_to="vals") %>%
  as.data.frame()



# Mean and Median
p1_ctry <- ggplot(d2[d2$vars=="mean_rop"|d2$vars=="med_rop",], 
                  aes(x=Year, y=vals, group=vars)) +
  geom_line(aes(linetype=vars)) +
  labs(x="",y="",
       title = "Net Rate of Profit Across Countries, 1950-2019",
       subtitle = "Mean and Median",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal() +
  theme(plot.caption=element_text(hjust = 0)) +
  scale_linetype_discrete(
    name = "Statistic", labels = c("Mean", "Median")
  )



# SD and IQR
p2_ctry <- ggplot(d2[d2$vars=="sd_rop"|d2$vars=="iqr_rop",], 
                  aes(x=Year, y=vals, group=vars)) +
  geom_line(aes(linetype=vars)) +
  labs(x="",y="",
       title = "Net Rate of Profit Across Countries, 1950-2019",
       subtitle = "Standard Deviation and IQR",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal() +
  theme(plot.caption=element_text(hjust = 0)) +
  scale_linetype_discrete(
    name = "Statistic", labels = c("IQR", "SD")
  )



# Number of Non-Missing Observations
p3_ctry <- ggplot(d2[d2$vars=="n_rop",], 
                  aes(x=Year, y=vals)) +
  geom_line(color="blue") +
  labs(x="",y="",
       title = "Net Rate of Profit Across Countries, 1950-2019",
       subtitle = "Number of Countries with Data",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()



# ----------------------------------------------------- #
# ------------ World Rate of Profit ------------------ #

# ------------- PPP Conversion factors 
# (source: OECD)
d2_ppp <- read_excel("OECD_PPP.xlsx")



# (source: OECD)
d2_nom2 <- read_excel("Exchange_Rates_OECD.xlsx")


# Merge exchange rate data sets
d3_1 <- left_join(d2_nom2, d2_ppp, by = c("LOCATION", "TIME"))

# Merge with EPWT 7.0 data set
d3 <- left_join(d1_e, d3_1, 
                by=c("Countrycode" = "LOCATION", "Year" = "TIME")
) %>%
  as.data.frame()


# Create variables using: 
# (a) current PPP conversion  
# (b) nominal exchange rates
d4 <- d3 %>%
  dplyr::rename(nom_conv = Value.x) %>%
  dplyr::rename(ppp_conv = Value.y) %>%
  dplyr::filter(!is.na(ppp_conv)) %>%
  mutate(
    
    # ------ PPP --------- #
    # capital stock, PPP
    Kppp = (Knatcur/ppp_conv),
    # gross output, PPP
    Xppp = XGDPnatcur/ppp_conv,
    # net output, PPP
    Xppp_net = (XGDPnatcur - delta*Knatcur)/ppp_conv,
    # gross profit income, PPP
    profit = (1-LabShare)*Xppp,
    # net profit income, PPP
    profit_net = (Xppp_net-LabShare*Xppp),
    
    # ------ Nominal Exchange Rate ----- #
    # capital stock in US dollars
    K_nom = (Knatcur/nom_conv),
    # gross output in US dollars
    X_nom = XGDPnatcur/nom_conv,
    # net output in US dollars
    X_nom_net = (XGDPnatcur - delta*Knatcur)/nom_conv,
    # gross profit income in US dollars
    profit_nom = (1-LabShare)*X_nom,
    # net profit income in US dollars
    profit_nom_net = X_nom_net-LabShare*X_nom
    
  ) %>%
  as.data.frame()


# ------------------------------------------------------------------ #
# ---------- PPP: World Rate of profit using all observations ----- #

d5 <- d4 %>%
  group_by(Year) %>%
  summarise(
    # gross profit
    p_tot = sum(profit, na.rm = TRUE),
    # net profit
    p_tot_net = sum(profit_net, na.rm = TRUE),
    p_obs = sum(!is.na(profit)),
    K_tot = sum(Kppp, na.rm = TRUE),
    K_obs = sum(!is.na(Kppp)),
    # gross output
    X_tot = sum(Xppp, na.rm = TRUE),
    # net output
    X_tot_net = sum(Xppp_net, na.rm = TRUE),
    X_obs = sum(!is.na(Xppp)),
  ) %>%
  mutate(
    # gross profit
    rop_global = 100*(p_tot/K_tot),
    pshare_global = 100*(p_tot/X_tot),
    rho_global = X_tot/K_tot,
    # net profit
    rop_global_net = 100*(p_tot_net/K_tot),
    pshare_global_net = 100*(p_tot_net/X_tot_net),
    rho_global_net = X_tot_net/K_tot
  ) %>%
  as.data.frame()




# World Rate of net profit
p1_global <- ggplot(d5, aes(x=Year, y=rop_global_net)) +
  geom_line(color="blue") +
  labs(x="",y="percentage",
       title = "World Rate of Net Profit, 1960-2019",
       subtitle = "Aggregating Country Profit Rates Using PPP Exch Rates, All Obs",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()

# Nonmissing Observations
p1_global_pppobs <- ggplot(d5, aes(x=Year, y=p_obs)) +
  geom_line(color="blue") +
  labs(x="",y="number",
       title = "Countries with Non-Missing Observations, 1960-2019",
       subtitle = "Aggregating Country Profit Rates Using PPP Exch Rates, All Obs",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()




# --- Decomposition
# Full sample period
dp0 <- decomp_mr(
  d5[d5$Year>=1960,c("Year","rop_global_net","pshare_global_net","rho_global_net")]
)

# Regulated period
dp1 <- decomp_mr(
  d5[d5$Year<=1980,c("Year","rop_global_net","pshare_global_net","rho_global_net")]
)

# Neoliberal period
dp2 <- decomp_mr(
  d5[d5$Year>=1981,c("Year","rop_global_net","pshare_global_net","rho_global_net")]
)


# -----------------------------------------------------
# ---------- World Rate of profit using balanced panel

# Set as panel
d6 <- pdata.frame(d4, index = c("Countrycode", "Year"))

# Choose balanced panel
d7 <- make.pbalanced(d6, balance.type="shared.individuals")

# Dimension of balanced panel
pdim(d7)

# Which countries are in the balanced panel
x <- cbind(table(d7$Country))
x[x>0,]

# Compute global values
d8 <- d7 %>%
  group_by(Year) %>%
  summarise(
    # Net profit
    p_tot = sum(profit_net, na.rm = TRUE),
    K_tot = sum(Kppp, na.rm = TRUE),
    # Net output
    X_tot = sum(Xppp_net, na.rm = TRUE)
  ) %>%
  dplyr::rename(year=Year)%>%
  mutate(
    # covert year to numeric
    Year = as.numeric(as.character(year)),
    # Net rate of profit
    rop_global = 100*(p_tot/K_tot),
    # Net profit share
    pshare_global = 100*(p_tot/X_tot),
    # Net output-capital stock
    rho_global = X_tot/K_tot
  ) %>%
  dplyr::select(-year) %>%
  as.data.frame()



# World Rate of profit
p2_global <- ggplot(d8, aes(x=Year, y=rop_global)) +
  geom_line(color="blue") +
  labs(x="",y="percentage",
       title = "World Rate of Net Profit, 1960-2019",
       subtitle = "Aggregating Country Profit Rates Using PPP Exch Rates, Balanced Panel",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()



# ---- Decomposition
# Full sample period
dp3_0 <- round(decomp_mr(
  d8[d8$Year>=1960,c("Year","rop_global","pshare_global","rho_global")]
), digits = 2)

# Regulated period
dp3 <- round(decomp_mr(
  d8[d8$Year<=1980,c("Year","rop_global","pshare_global","rho_global")]
), digits = 2)

# Neoliberal period
dp4 <- round(decomp_mr(
  d8[d8$Year>=1981,c("Year","rop_global","pshare_global","rho_global")]
), digits = 2)






# ------------------------------------------------------------------------- #
# -- Nominal Exchange Rate: World Rate of profit using all observations -- #

d5 <- d4 %>%
  group_by(Year) %>%
  summarise(
    # net profit 
    p_tot = sum(profit_nom_net, na.rm = TRUE),
    p_obs = sum(!is.na(profit_nom_net)),
    K_tot = sum(K_nom, na.rm = TRUE),
    K_obs = sum(!is.na(K_nom)),
    # net output
    X_tot = sum(X_nom_net, na.rm = TRUE),
    X_obs = sum(!is.na(X_nom_net)),
  ) %>%
  mutate(
    rop_global = 100*(p_tot/K_tot),
    pshare_global = 100*(p_tot/X_tot),
    rho_global = X_tot/K_tot
  ) %>%
  as.data.frame()



# World Rate of profit
p1_global_nom <- ggplot(d5, aes(x=Year, y=rop_global)) +
  geom_line(color="blue") +
  labs(x="",y="percentage",
       title = "World Rate of Net Profit, 1960-2019",
       subtitle = "Aggregating Country Profit Rates Using Nom Exch Rates, All Obs",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()


# Nonmissing Observations
p1_global_nomobs <- ggplot(d5, aes(x=Year, y=p_obs)) +
  geom_line(color="blue") +
  labs(x="",y="number",
       title = "Countries with Non-Missing Observations, 1960-2019",
       subtitle = "Aggregating Country Profit Rates Using Nom Exch Rates, All Obs",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()


# --- Decomposition
# Full sample period
dp0_nom <- decomp_mr(
  d5[d5$Year>=1960,c("Year","rop_global","pshare_global","rho_global")]
)

# Regulated period
dp1_nom <- decomp_mr(
  d5[d5$Year<=1980,c("Year","rop_global","pshare_global","rho_global")]
)

# Neoliberal period
dp2_nom <- decomp_mr(
  d5[d5$Year>=1981,c("Year","rop_global","pshare_global","rho_global")]
)


# -----------------------------------------------------
# ---------- World Rate of profit using balanced panel

# Set as panel
d6 <- pdata.frame(d4, index = c("Countrycode", "Year"))

# Choose balanced panel
d7 <- make.pbalanced(d6, balance.type="shared.individuals")

# Compute global values
d8 <- d7 %>%
  group_by(Year) %>%
  summarise(
    # net profit
    p_tot = sum(profit_nom_net, na.rm = TRUE),
    K_tot = sum(K_nom, na.rm = TRUE),
    # net output
    X_tot = sum(X_nom_net, na.rm = TRUE)
  ) %>%
  dplyr::rename(year=Year) %>%
  mutate(
    # covert year to numeric
    Year = as.numeric(as.character(year)),
    rop_global = 100*(p_tot/K_tot),
    pshare_global = 100*(p_tot/X_tot),
    rho_global = X_tot/K_tot
  ) %>%
  dplyr::select(-year) %>%
  as.data.frame()



# World Rate of profit
p2_global_nom <- ggplot(d8, aes(x=Year, y=rop_global)) +
  geom_line(color="blue") +
  labs(x="",y="percentage",
       title = "World Rate of Net Profit, 1960-2019",
       subtitle = "Aggregating Country Profit Rates Using Nom Exch Rates, Balanced Panel",
       caption = "Data Source: Extended Penn World Table 7.0") +
  theme_minimal()



# ---- Decompositon
# Full sample period
dp3_0_nom <- round(decomp_mr(
  d8[d8$Year>=1960,c("Year","rop_global","pshare_global","rho_global")]
), digits = 2)

# Regulated period
dp3_nom <- round(decomp_mr(
  d8[d8$Year<=1980,c("Year","rop_global","pshare_global","rho_global")]
), digits = 2)

# Neoliberal period
dp4_nom <- round(decomp_mr(
  d8[d8$Year>=1981,c("Year","rop_global","pshare_global","rho_global")]
), digits = 2)






# -------------------------------------------------------------- #
# ---------------------- WIOD data set  ------------------------ #

# Read data
d1_w <- read_excel("WIOD_SEA_Nov16.xlsx", 
                   sheet =  "DATA"
)

# Value added
d2_1 <- d1_w %>%
  pivot_longer(!c("country","variable","description","code"),
               names_to="year",
               values_to="value") %>%
  dplyr::select(-c(description)) %>%
  dplyr::filter(
    variable=="VA"|variable=="LAB"|variable=="K"
  ) %>%
  mutate(
    Year = as.numeric(as.character(year))
  ) %>%
  dplyr::select(-year) %>%
  dplyr::filter(variable=="VA") %>%
  dplyr::rename(vad = value) %>%
  dplyr::select(-variable) %>%
  as.data.frame()


# Capital stock
d2_2 <- d1_w %>%
  pivot_longer(!c("country","variable","description","code"),
               names_to="year",
               values_to="value") %>%
  dplyr::select(-c(description)) %>%
  dplyr::filter(
    variable=="VA"|variable=="LAB"|variable=="K"
  ) %>%
  mutate(
    Year = as.numeric(as.character(year))
  ) %>%
  dplyr::select(-year) %>%
  dplyr::filter(variable=="K") %>%
  dplyr::rename(K = value) %>%
  dplyr::select(-variable) %>%
  as.data.frame()

# Merge
d2_3 <- left_join(d2_1, d2_2)

# Labor share
d2_4 <- d1_w %>%
  pivot_longer(!c("country","variable","description","code"),
               names_to="year",
               values_to="value") %>%
  dplyr::select(-c(description)) %>%
  dplyr::filter(
    variable=="VA"|variable=="LAB"|variable=="K"
  ) %>%
  mutate(
    Year = as.numeric(as.character(year))
  ) %>%
  dplyr::select(-year) %>%
  dplyr::filter(variable=="LAB") %>%
  dplyr::rename(lab_income = value) %>%
  dplyr::select(-variable) %>%
  as.data.frame()

# Merge again
d2_5 <- left_join(d2_3, d2_4)


# Merge with exchange rate data set and create
# industry level variables
d3 <- left_join(
  d2_5, d3_1, 
  by = c("country" = "LOCATION", "Year" = "TIME")
) %>%
  dplyr::rename(nom_conv = Value.x) %>%
  dplyr::rename(ppp_conv = Value.y) %>%
  dplyr::select(-c(INDICATOR.x, SUBJECT.x, MEASURE.x, FREQUENCY.x,
                   INDICATOR.y, SUBJECT.y, MEASURE.y, FREQUENCY.y)) %>%
  # Variables expressed in current PPP dollars
  mutate(
    
    # ---- PPP ---- #
    vad_ppp = (vad)/ppp_conv,
    K_ppp = K/ppp_conv,
    profit_ppp = (vad_ppp - (lab_income/ppp_conv)),
    
    # ---- Nominal Exchange Rate ---- #
    vad_nom = (vad)/nom_conv,
    K_nom = K/nom_conv,
    profit_nom = (vad_nom - (lab_income/nom_conv))
  ) %>%
  # Add across countries
  group_by(code, Year) %>%
  summarise(
    # ----- PPP ------ #
    vad_ind = sum(vad_ppp, na.rm = TRUE),
    profit_ind = sum(profit_ppp, na.rm = TRUE),
    K_ind = sum(K_ppp, na.rm = TRUE),
    
    # ----- Nominal Exchange Rate ------ #
    vad_ind_nom = sum(vad_nom, na.rm = TRUE),
    profit_ind_nom = sum(profit_nom, na.rm = TRUE),
    K_ind_nom = sum(K_nom, na.rm = TRUE)
    
  ) %>%
  as.data.frame()

# Industry level profit rate variables
d4 <- d3 %>%
  mutate(
    
    # ------ PPP ------- #
    rop_ind = 100*(profit_ind/K_ind),
    pshare_ind = 100*(profit_ind/vad_ind),
    rho_ind = vad_ind/K_ind,
    
    # ------ Nominal Exchange Rate ------- #
    rop_ind_nom = 100*(profit_ind_nom/K_ind_nom),
    pshare_ind_nom = 100*(profit_ind_nom/vad_ind_nom),
    rho_ind_nom = vad_ind_nom/K_ind_nom
    
  ) %>%
  group_by(Year) %>%
  summarise(
    # --------- PPP ---------- #
    # mean
    mean_rop = mean(rop_ind, na.rm = TRUE),
    # standard deviation
    sd_rop = sd(rop_ind, na.rm = TRUE),
    # median        
    med_rop =  median(rop_ind, na.rm = TRUE),
    # interquartile range        
    iqr_rop = IQR(rop_ind, na.rm = TRUE),
    
    # --------- Nominal Exchange Rate ---------- #
    # mean
    mean_rop_nom = mean(rop_ind_nom, na.rm = TRUE),
    # standard deviation
    sd_rop_nom = sd(rop_ind_nom, na.rm = TRUE),
    # median        
    med_rop_nom =  median(rop_ind_nom, na.rm = TRUE),
    # interquartile range        
    iqr_rop_nom = IQR(rop_ind_nom, na.rm = TRUE)
  ) %>%
  pivot_longer(!c("Year"),
               names_to="vars",
               values_to="vals") %>%
  as.data.frame()


# Mean and Median (PPP)
p1_indtry <- ggplot(d4[d4$vars=="mean_rop"|d4$vars=="med_rop",], 
                    aes(x=Year, y=vals, group=vars)) +
  geom_line(aes(linetype=vars)) +
  labs(x="",y="",
       title = "Rate of Profit Across Countries, 2000-2014",
       subtitle = "Mean and Median",
       caption = "Data Source: World Input Output Database") +
  theme_minimal() +
  theme(plot.caption=element_text(hjust = 0)) +
  scale_linetype_discrete(
    name = "Statistic", labels = c("Mean", "Median")
  )


# Mean and Median (Nominal Exchange Rate)
p1_indtry_nom <- ggplot(d4[d4$vars=="mean_rop_nom"|d4$vars=="med_rop_nom",], 
                        aes(x=Year, y=vals, group=vars)) +
  geom_line(aes(linetype=vars)) +
  labs(x="",y="",
       title = "Rate of Profit Across Countries, 2000-2014",
       subtitle = "Mean and Median",
       caption = "Data Source: World Input Output Database") +
  theme_minimal() +
  theme(plot.caption=element_text(hjust = 0)) +
  scale_linetype_discrete(
    name = "Statistic", labels = c("Mean", "Median")
  )

# SD and IQR (PPP)
p2_indtry <- ggplot(d4[d4$vars=="sd_rop"|d4$vars=="iqr_rop",], 
                    aes(x=Year, y=vals, group=vars)) +
  geom_line(aes(linetype=vars)) +
  labs(x="",y="",
       title = "Rate of Profit Across Countries, 2000-2014",
       subtitle = "Standard Deviation and IQR",
       caption = "Data Source: World Input Output Database") +
  theme_minimal() +
  theme(plot.caption=element_text(hjust = 0)) +
  scale_linetype_discrete(
    name = "Statistic", labels = c("IQR", "SD")
  )


# SD and IQR (Nominal Exchange Rate)
p2_indtry_nom <- ggplot(d4[d4$vars=="sd_rop_nom"|d4$vars=="iqr_rop_nom",], 
                        aes(x=Year, y=vals, group=vars)) +
  geom_line(aes(linetype=vars)) +
  labs(x="",y="",
       title = "Rate of Profit Across Countries, 2000-2014",
       subtitle = "Standard Deviation and IQR",
       caption = "Data Source: World Input Output Database") +
  theme_minimal() +
  theme(plot.caption=element_text(hjust = 0)) +
  scale_linetype_discrete(
    name = "Statistic", labels = c("IQR", "SD")
  )


# ------------ Global profit rate
d5 <- d3 %>%
  group_by(Year) %>%
  summarise(
    # ------ PPP ------------ #
    vad_global = sum(vad_ind, na.rm = TRUE),
    profit_global = sum(profit_ind, na.rm = TRUE),
    K_global = sum(K_ind, na.rm = TRUE),
    
    # ------ Nominal Exchange Rate ------------ #
    vad_global_nom = sum(vad_ind_nom, na.rm = TRUE),
    profit_global_nom = sum(profit_ind_nom, na.rm = TRUE),
    K_global_nom = sum(K_ind_nom, na.rm = TRUE)
  ) %>%
  mutate(
    # -------- PPP -------- #
    rop_global = 100*(profit_global/K_global),
    pshare_global = 100*(profit_global/vad_global),
    rho_global = vad_global/K_global,
    
    # -------- Nominal Exchange Rate -------- #
    rop_global_nom = 100*(profit_global_nom/K_global_nom),
    pshare_global_nom = 100*(profit_global_nom/vad_global_nom),
    rho_global_nom = vad_global_nom/K_global_nom
  ) %>%
  as.data.frame()


# Time series plot (PPP)
p3_global <- ggplot(data = d5, aes(x=Year, y=rop_global)) +
  geom_line(color="blue") +
  labs(x="", y="percentage",
       title = "Global Profit Rate, 2000-2014",
       subtitle = "Aggregating Industry Profit Rates Using PPP Exch Rates",
       caption = "Data Source: World Input Output Database") +
  theme_minimal()


# Time series plot (Nominal Exchange Rate)
p3_global_nom <- ggplot(data = d5, aes(x=Year, y=rop_global_nom)) +
  geom_line(color="blue") +
  labs(x="", y="percentage",
       title = "Global Profit Rate, 2000-2014",
       subtitle = "Aggregating Industry Profit Rates Using Nom Exch Rates",
       caption = "Data Source: World Input Output Database") +
  theme_minimal()


# Decompositon (PPP)
dp5 <- round(
  decomp_mr(d5[,c("Year","rop_global","pshare_global","rho_global")]),
  digits = 2)


# Decompositon (Nominal Exchange Rate)
dp5_nom <- round(
  decomp_mr(d5[,c("Year","rop_global_nom","pshare_global_nom","rho_global_nom")]),
  digits = 2)



# ----------------------------------------------------------- #
# -------------------------- SAVE RESULTS ------------------- #

# ---- Plots using country level data
# Mean and Median 
jpeg("C1.jpeg")
print(p1_ctry)
dev.off()

# ggsave(
#   filename = paste(a1,"C1.eps",sep=""),
#   plot = p1_ctry
# )

# SD and IQR 
jpeg("C2.jpeg")
print(p2_ctry)
dev.off()

# ggsave(
#   filename = paste(a1,"C2.eps",sep=""),
#   plot = p2_ctry
# )


# Number of nonmissing obs 
jpeg("C3.jpeg")
print(p3_ctry)
dev.off()

# ggsave(
#   filename = paste(a1,"C3.eps",sep=""),
#   plot = p3_ctry
# )


# Global profit rate using all obs (PPP)
jpeg("C4.jpeg")
print(p1_global)
dev.off()

# ggsave(
#   filename = paste(a1,"C4.eps",sep=""),
#   plot = p1_global
# )


# Global profit rate using all obs (PPP), number of obs
jpeg("C4_obs.jpeg")
print(p1_global_pppobs)
dev.off()

# ggsave(
#   filename = paste(a1,"C4_obs.eps",sep=""),
#   plot = p1_global_pppobs
# )


# Global profit rate using all obs (Nominal Exchange Rate)
jpeg("C4_nom.jpeg")
print(p1_global_nom)
dev.off()

# ggsave(
#   filename = paste(a1,"C4_nom.eps",sep=""),
#   plot = p1_global_nom
# )


# Global profit rate using all obs (Nominal Exchange Rate), number of obs
jpeg("C4_nom_obs.jpeg")
print(p1_global_nomobs)
dev.off()

# ggsave(
#   filename = paste(a1,"C4_nom_obs.eps",sep=""),
#   plot = p1_global_nomobs
# )

# Global profit rate using balanced panel (PPP)
jpeg("C5.jpeg")
print(p2_global)
dev.off()

# ggsave(
#   filename = paste(a1,"C5.eps",sep=""),
#   plot = p2_global
# )

# Global profit rate using balanced panel (Nominal Exchange Rate)
jpeg("C5_nom.jpeg")
print(p2_global_nom)
dev.off()

# ggsave(
#   filename = paste(a1,"C5_nom.eps",sep=""),
#   plot = p2_global_nom
# )

# ----- Plots using industry level data

# Mean and Median (PPP)
jpeg("I1.jpeg")
print(p1_indtry)
dev.off()

# ggsave(
#   filename = paste(a1,"I1.eps",sep=""),
#   plot = p1_indtry
# )

# Mean and Median (Nominal Exchange Rate)
jpeg("I1_nom.jpeg")
print(p1_indtry_nom)
dev.off()

# ggsave(
#   filename = paste(a1,"I1_nom.eps",sep=""),
#   plot = p1_indtry_nom
# )

# SD and IQR (PPP)
jpeg("I2.jpeg")
print(p2_indtry)
dev.off()

# ggsave(
#   filename = paste(a1,"I2.eps",sep=""),
#   plot = p2_indtry
# )

# SD and IQR (Nominal Exchange Rate)
jpeg("I2_nom.jpeg")
print(p2_indtry_nom)
dev.off()

# ggsave(
#   filename = paste(a1,"I2_nom.eps",sep=""),
#   plot = p2_indtry_nom
# )

# Global profit rate using all obs (PPP)
jpeg("I3.jpeg")
print(p3_global)
dev.off()

# ggsave(
#   filename = paste(a1,"I3.eps",sep=""),
#   plot = p3_global
# )

# Global profit rate using all obs (Nominal Exchange Rate)
jpeg("I3_nom.jpeg")
print(p3_global_nom)
dev.off()

# ggsave(
#   filename = paste(a1,"I3_nom.eps",sep=""),
#   plot = p3_global_nom
# )

# ---- Table of medium run decomposition (PPP)
decomp_table <- rbind(dp0, dp1, dp2, dp3_0, dp3, dp4, dp5)
colnames(decomp_table) <- c("Start","End","ROP", "PSHARE", "OUTCAP")
rownames(decomp_table) <- c("Global 1C","Global 1C","Global 1C",
                            "Global 2C","Global 2C","Global 2C",
                            "Global I1")
print(decomp_table)
xtable(decomp_table)

# ---- Table of medium run decomposition (Nominal Exchange Rate)
decomp_table_nom <- rbind(dp0_nom, dp1_nom, dp2_nom, dp3_0_nom, dp3_nom, dp4_nom, dp5_nom)
colnames(decomp_table_nom) <- c("Start","End","ROP", "PSHARE", "OUTCAP")
rownames(decomp_table_nom) <- c("Global 1C","Global 1C","Global 1C",
                                "Global 2C","Global 2C","Global 2C",
                                "Global I1")
print(decomp_table_nom)
xtable(decomp_table_nom)
