# Analysis-of-International-Debt-Statistics
This repository contains the data pipeline, analysis code, and policy brief for a cross-sectional study of external debt vulnerability across low- and middle-income countries, using the World Bank's International Debt Statistics (IDS) for 2023.

## What This Project Does

- Downloads and cleans sovereign debt data from the World Bank IDS database
- Classifies countries into debt risk tiers based on IMF/World Bank stress thresholds
- Produces four visualisations covering debt sustainability, creditor composition, liquidity risk, and borrowing terms
- Estimates an OLS regression identifying the key drivers of debt service burden

## Key Findings

- Debt service pressure is more widespread than stock ratios suggest. Many countries exceed the IMF's 15% debt-service-to-exports threshold even at moderate debt levels, driven by unfavourable loan terms and compressed maturities.
- Creditor composition is highly fragmented. The shift from multilateral to bilateral and bond financing — particularly pronounced in Sub-Saharan Africa and Latin America — complicates future restructuring by multiplying the number of creditor classes requiring coordination.
- Liquidity risk is concentrated. Several countries combine reserve coverage below the IMF's 3-month benchmark with high short-term debt shares, creating acute rollover vulnerability.
- Concessional finance is in retreat. Several countries are committing to new debt at negative grant elements — borrowing on terms worse than market rates — at precisely the moment their debt positions are most fragile.
- Regression results. External debt stock (% GNI) and the grant element on new loans are the two statistically significant drivers of debt service burden (both p < 0.01). A 10pp increase in debt/GNI raises debt service by ~0.84pp of exports; a 10pp increase in the grant element reduces it by ~1.93pp. Adjusted R² = 0.24.
