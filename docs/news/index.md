# Changelog

## medusa 0.2.0

### New features: EU module

New module supporting distributional, energy poverty, and transport
poverty analysis for all 27 EU countries using Eurostat Household Budget
Survey (HBS) microdata (waves 2010, 2015 and 2020).

- [`hbs_eu()`](../reference/hbs_eu.md): Process Eurostat HBS microdata
  into the format required by `medusa` (supports 27 EU countries, waves
  2010, 2015 and 2020).
- [`calc_di_eu()`](../reference/calc_di_eu.md): Calculate distributional
  impacts for EU countries with support for two price shock scenarios,
  country-level disaggregation and intersectional analysis.
- [`ex_shocks_eu()`](../reference/ex_shocks_eu.md): Download the
  two-scenario price shock template (columns `CC_s1`/`CC_s2` per
  country).
- [`available_var_eu()`](../reference/available_var_eu.md): List
  variables available for distributional analysis in the EU module.
- [`ex_var_intersec_eu()`](../reference/ex_var_intersec_eu.md): Download
  the full list of 211 intersectional variable pairs available for EU
  analysis.
- [`check_var_eu()`](../reference/check_var_eu.md): Validate that
  requested variables are present in the data.
- [`calc_ep_eu()`](../reference/calc_ep_eu.md): Calculate energy poverty
  indices by country for EU data.
- [`calc_tp_eu()`](../reference/calc_tp_eu.md): Calculate transport
  poverty indices by country for EU data.

## medusa 0.1.0

### Initial release

First release of the `medusa` package for Spain (INE microdata).

#### Spain module

- [`calc_di()`](../reference/calc_di.md): Calculate distributional
  impacts of price shocks across socioeconomic groups (income
  deciles/quintiles, gender, household type, etc.).
- [`ex_shocks()`](../reference/ex_shocks.md): Download the price shock
  template file.
- [`available_var_impact()`](../reference/available_var_impact.md): List
  variables available for distributional analysis.
- [`ex_var_intersec()`](../reference/ex_var_intersec.md),
  [`available_var_intersec()`](../reference/available_var_intersec.md):
  Manage intersectional variable pairs for combined analysis
  (e.g. income x gender).
- [`calc_ep()`](../reference/calc_ep.md): Calculate five energy poverty
  indices (10%, 2M, LIHC, HEP, HEP_LI).
- [`calc_tp()`](../reference/calc_tp.md): Calculate four transport
  poverty indices (10%, 2M, LIHC, VTU).
