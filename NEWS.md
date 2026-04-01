# medusa 0.2.0

## New features: EU module

New module supporting distributional, energy poverty, and transport poverty analysis for all 27 EU countries using Eurostat Household Budget Survey (HBS) microdata (waves 2010, 2015 and 2020).

- `hbs_eu()`: Process Eurostat HBS microdata into the format required by `medusa` (supports 27 EU countries, waves 2010, 2015 and 2020).
- `calc_di_eu()`: Calculate distributional impacts for EU countries with support for two price shock scenarios, country-level disaggregation and intersectional analysis.
- `ex_shocks_eu()`: Download the two-scenario price shock template (columns `CC_s1`/`CC_s2` per country).
- `available_var_eu()`: List variables available for distributional analysis in the EU module.
- `ex_var_intersec_eu()`: Download the full list of 211 intersectional variable pairs available for EU analysis.
- `check_var_eu()`: Validate that requested variables are present in the data.
- `calc_ep_eu()`: Calculate energy poverty indices by country for EU data.
- `calc_tp_eu()`: Calculate transport poverty indices by country for EU data.

# medusa 0.1.0

## Initial release

First release of the `medusa` package for Spain (INE microdata).

### Spain module

- `calc_di()`: Calculate distributional impacts of price shocks across socioeconomic groups (income deciles/quintiles, gender, household type, etc.).
- `ex_shocks()`: Download the price shock template file.
- `available_var_impact()`: List variables available for distributional analysis.
- `ex_var_intersec()`, `available_var_intersec()`: Manage intersectional variable pairs for combined analysis (e.g. income x gender).
- `calc_ep()`: Calculate five energy poverty indices (10%, 2M, LIHC, HEP, HEP_LI).
- `calc_tp()`: Calculate four transport poverty indices (10%, 2M, LIHC, VTU).
