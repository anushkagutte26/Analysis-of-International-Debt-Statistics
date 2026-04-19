install.packages(c("WDI", "stargazer", "tidyverse", "ggplot2", "scales", "countrycode", "ggrepel", "patchwork"))
install.packages("modelsummary")
library(WDI)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(stargazer)

WDI_df <- read.csv("~/Downloads/P_Data_Extract_From_International_Debt_Statistics-3/17c5c885-cb1b-4561-b24a-f97470257834_Data.csv")
View(WDI_df)

df <- head(WDI_df, -5)

df <- df %>%
  select(
    Country.Name,
    Country.Code,
    Series.Code,
    X2023..YR2023.
  )

head(df)

## Since the data fram is in long format, downloaded from the WB database, I will be converting it into a wide format.

df_wide <- df %>%
  pivot_wider(
    names_from = Series.Code,
    values_from = X2023..YR2023.
  )

colnames(df_wide)

final_df <- df_wide %>%
  select(
    country        = Country.Name,
    country_code          = Country.Code,
  # --- CORE DEBT BURDEN ---
    debt_gni = DT.DOD.DECT.GN.ZS,          # External debt (% GNI)
    debt_exports = DT.DOD.DECT.EX.ZS,      # External debt (% exports)
    debt_service = DT.TDS.DECT.EX.ZS,      # Debt service (% exports)
    interest_exports = DT.INT.DECT.EX.ZS,  # Interest (% exports)
  
  # --- TOTAL STOCKS ---
  total_external_debt = DT.DOD.DECT.CD,
  long_term_debt = DT.DOD.DLXF.CD,
  
  # --- CREDITOR COMPOSITION ---
  multilateral_debt = DT.DOD.MLAT.CD,
  bilateral_debt = DT.DOD.BLAT.CD,
  bond_debt = DT.DOD.PBND.CD,
  commercial_bank_debt = DT.DOD.PCBK.CD,
  
  # --- STRUCTURE ---
  short_term_share = DT.DOD.DSTC.ZS,
  concessional_debt = DT.DOD.ALLC.CD,
  
  # --- LIQUIDITY ---
  reserves_months = FI.RES.TOTL.MO,
  reserves_debt_ratio = FI.RES.TOTL.DT.ZS,
  reserves_usd = FI.RES.TOTL.CD,
  
  # --- STRESS / IMF ---
  imf_credit = DT.DOD.DIMF.CD,
  debt_restructured = DT.TXR.DPPG.CD,
  
  # --- TERMS ---
  grant_element = DT.GRE.DPPG,
  avg_interest = DT.INR.DPPG,
  avg_maturity = DT.MAT.DPPG,
  grace_period = DT.GPA.DPPG,
  
  # --- MACRO CONTEXT ---
  current_account = BN.CAB.XOKA.CD,
  exports = BX.GSR.TOTL.CD,
  population = SP.POP.TOTL,
  gni = NY.GNP.MKTP.CD
)

final_df <- final_df %>%
  mutate(across(-c(country, country_code), ~as.numeric(na_if(., ".."))))

final_df <- final_df %>%
  mutate(
    # Creditor composition shares
    total_creditors     = multilateral_debt + bilateral_debt + bond_debt + commercial_bank_debt,
    sh_multilateral     = multilateral_debt / total_creditors,
    sh_bilateral        = bilateral_debt    / total_creditors,
    sh_bonds            = bond_debt         / total_creditors,
    sh_commercial       = commercial_bank_debt / total_creditors,
    
    # Debt per capita (USD)
    debt_per_capita     = total_external_debt / population,
    
    # IMF reliance as share of total debt
    imf_share           = imf_credit / total_external_debt,
    
    # Concessional share
    concessional_share  = concessional_debt / total_external_debt,
    
    # Risk classification
    risk_flag = case_when(
      debt_gni > 60 & debt_service > 20 ~ "High Risk",
      debt_gni > 40 | debt_service > 15 ~ "Elevated Risk",
      TRUE                               ~ "Moderate"
    ),
    risk_flag = factor(risk_flag, levels = c("Moderate", "Elevated Risk", "High Risk"))
  )

final_df %>%
  summarise(across(debt_gni:gni, ~round(mean(!is.na(.)) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "pct_nonmissing") %>%
  arrange(pct_nonmissing) %>%
  print(n = Inf)

p1 <- ggplot(final_df, aes(x = debt_gni, y = debt_service,
                            label = country_code)) +
  annotate("rect", xmin = 60, xmax = Inf, ymin = 20, ymax = Inf,
           fill = "#e74c3c", alpha = 0.08) +
  annotate("text", x = 200, y = Inf, label = "High risk zone",
           vjust = 2, hjust = 0, size = 3, color = "#e74c3c") +
  geom_point(alpha = 0.75, size = 2.5) +
  geom_text_repel(
    data        = filter(final_df, risk_flag == "High Risk"),
    size        = 2.8,
    max.overlaps= 20,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  geom_hline(yintercept = 15, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "External Debt Vulnerability, 2023",
    subtitle = "Dashed lines: IMF/WB stress thresholds (40% GNI; 15% of exports)",
    x        = "External Debt Stock (% of GNI)",
    y        = "Total Debt Service (% of Exports)",
    color    = NULL,
    caption  = "Source: World Bank IDS, 2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p1)


###############################

creditor_long <- final_df %>%
  filter(!is.na(total_creditors), total_creditors > 0) %>%
  select(country, country_code,
         Multilateral = sh_multilateral,
         Bilateral    = sh_bilateral,
         Bonds        = sh_bonds,
         `Comm. Banks`= sh_commercial) %>%
  pivot_longer(Multilateral:`Comm. Banks`,
               names_to = "creditor", values_to = "share")

# Top 30 most indebted countries for readability
top30 <- final_df %>%
  filter(!is.na(total_external_debt)) %>%
  slice_max(total_external_debt, n = 30) %>%
  pull(country_code)

p2 <- creditor_long %>%
  filter(country_code %in% top30) %>%
  ggplot(aes(x = reorder(country_code, share), y = share, fill = creditor)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Multilateral" = "#2ecc71",
    "Bilateral"    = "#e67e22",
    "Bonds"        = "#9b59b6",
    "Comm. Banks"  = "#3498db"
  )) +
  scale_y_continuous(labels = label_percent()) +
  coord_flip() +
  labs(
    title    = "Creditor Composition of External Debt, 2023",
    subtitle = "Top 30 countries by total external debt stock",
    x        = NULL,
    y        = "Share of PPG External Debt",
    fill     = NULL,
    caption  = "Source: World Bank IDS, 2023."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p2)


#########################################

p3 <- ggplot(final_df, aes(x = reserves_months, y = short_term_share,
                           color = risk_flag, label = country_code)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = 40, ymax = Inf,
           fill = "#e74c3c", alpha = 0.08) +
  annotate("text", x = 0.3, y = Inf, label = "Acute liquidity risk",
           vjust = 2, hjust = 0, size = 3, color = "#e74c3c") +
  geom_point(alpha = 0.75, size = 2.5) +
  geom_text_repel(
    data        = filter(final_df, !is.na(reserves_months),
                         reserves_months < 3 & short_term_share > 40),
    size        = 2.8,
    max.overlaps= 15,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  scale_color_manual(values = c(
    "Moderate"       = "#3498db",
    "Elevated Risk"  = "#f39c12",
    "High Risk"      = "#e74c3c"
  )) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    title    = "Liquidity Risk: Reserve Coverage vs Short-Term Debt, 2023",
    subtitle = "Vertical dashed line = IMF 3-month reserve benchmark",
    x        = "Reserves (months of imports)",
    y        = "Short-Term Debt (% of total external debt)",
    color    = NULL,
    caption  = "Source: World Bank IDS, 2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p3)




p4 <- final_df %>%
  filter(!is.na(grant_element), !is.na(avg_interest)) %>%
  ggplot(aes(x = avg_interest, y = grant_element,
             color = risk_flag, label = country_code)) +
  geom_point(alpha = 0.75, size = 2.5) +
  geom_text_repel(
    data = filter(final_df, !is.na(grant_element), !is.na(avg_interest),
                  avg_interest > 5 | grant_element < 10),
    size = 2.8, max.overlaps = 15, show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Moderate"       = "#3498db",
    "Elevated Risk"  = "#f39c12",
    "High Risk"      = "#e74c3c"
  )) +
  labs(
    title    = "Borrowing Terms on New External Debt Commitments, 2023",
    subtitle = "Higher interest & lower grant element = costlier, less concessional financing",
    x        = "Average Interest Rate on New Commitments (%)",
    y        = "Average Grant Element (%)",
    color    = NULL,
    caption  = "Source: World Bank IDS, 2023."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p4)




model_df <- final_df %>%
  filter(!is.na(debt_service), !is.na(debt_gni), !is.na(short_term_share),
         !is.na(reserves_months), !is.na(concessional_share), !is.na(grant_element))

model <- lm(
  debt_service ~ debt_gni + short_term_share + reserves_months +
    concessional_share + grant_element,
  data = model_df
)

summary(model)

stargazer(model, type="latex", out="debt_results.tex")


model_df2 <- final_df %>%
  mutate(
    # Log-transform skewed variables to reduce outlier influence
    log_debt_service      = log(debt_service + 1),
    log_debt_gni          = log(debt_gni + 1),
    log_imf_share         = log(imf_share * 100 + 1),   # rescale before log
    
    # Interaction: debt burden matters more when reserves are low
    debt_x_lowreserves    = debt_gni * (reserves_months < 3),
    
    # Dummy: below IMF reserve threshold
    low_reserves_dummy    = as.numeric(reserves_months < 3),
    
    # Rescale shares to 0-100 for interpretability
    short_term_pct        = short_term_share,
    concessional_pct      = concessional_share * 100,
    grant_element_c       = grant_element,
    bilateral_share_pct   = sh_bilateral * 100,
    bond_share_pct        = sh_bonds * 100
  ) %>%
  filter(
    !is.na(debt_service),
    !is.na(debt_gni),
    !is.na(short_term_share),
    !is.na(reserves_months),
    !is.na(grant_element),
    !is.na(sh_bilateral),
    !is.na(sh_bonds),
    !is.na(imf_share)
  )

nrow(model_df2)  # check sample size after filtering



# Model 1: Your original baseline
m1 <- lm(debt_service ~ debt_gni + short_term_share + reserves_months +
           concessional_share + grant_element,
         data = model_df2)
summary(m1)

# Model 2: Add creditor composition and IMF reliance
m2 <- lm(debt_service ~ debt_gni + short_term_share + reserves_months +
           grant_element + bilateral_share_pct + bond_share_pct + log_imf_share,
         data = model_df2)
summary(m2)

# Model 3: Log-transform DV to handle skew and outliers
m3 <- lm(log_debt_service ~ debt_gni + short_term_share + reserves_months +
           grant_element + bilateral_share_pct + bond_share_pct + log_imf_share,
         data = model_df2)
summary(m3)

# Model 4: Log-log + interaction (debt burden × low reserves)
m4 <- lm(log_debt_service ~ log_debt_gni + short_term_share + reserves_months +
           grant_element + bilateral_share_pct + bond_share_pct +
           log_imf_share + low_reserves_dummy + debt_x_lowreserves,
         data = model_df2)
summary(m4)

# Compare all models
library(broom)
library(modelsummary)

modelsummary(
  list("Baseline" = m1, "+ Creditors" = m2,
       "Log DV" = m3, "Log-Log + Interaction" = m4),
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse", "aic"),
  title = "Competing Models: Determinants of Debt Service Burden"
)



library(broom)
tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       debt_gni           = "External debt (% GNI)",
                       short_term_share   = "Short-term debt share (%)",
                       reserves_months    = "Reserves (months of imports)",
                       concessional_share = "Concessional debt share",
                       grant_element      = "Grant element on new loans"
  )) %>%
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high,
             y = reorder(term, estimate))) +
  geom_pointrange(color = "#2c3e50", linewidth = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#e74c3c") +
  labs(
    title    = "Determinants of Debt Service Burden, 2023",
    subtitle = "OLS. Dependent variable: total debt service (% of exports)",
    x        = "Coefficient (95% CI)",
    y        = NULL,
    caption  = "Source: World Bank IDS, 2023."
  ) +
  theme_minimal(base_size = 12)


ggsave("p1_vulnerability.png", p1, width = 10, height = 7, dpi = 300)
ggsave("p2_creditors.png",     p2, width = 10, height = 8, dpi = 300)
ggsave("p3_liquidity.png",     p3, width = 10, height = 7, dpi = 300)
ggsave("p4_loan_terms.png",    p4, width = 10, height = 7, dpi = 300)
