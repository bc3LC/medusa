# Changelog

## medusa 0.1.0

### Initial release

First public release of the `medusa` package, providing a complete
framework for distributional, energy poverty, and transport poverty
analysis for both Spain (INE microdata) and all EU countries (Eurostat
HBS microdata).

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

#### EU module (new in this release)

- [`hbs_eu()`](../reference/hbs_eu.md): Process Eurostat Household
  Budget Survey microdata into the format required by `medusa` (supports
  27 EU countries, waves 2010, 2015 and 2020).
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

#### Bug fixes

- Fixed [`rawhbs_eu()`](../reference/rawhbs_eu.md) to correctly handle
  multi-country requests (vector `country` argument was incorrectly
  evaluated in `if()` condition).
- Fixed [`ex_shocks_eu()`](../reference/ex_shocks_eu.md) to export the
  two-scenario template (`shocks_eu_s`) instead of the single-scenario
  dataset.
- Cleaned `shocks_eu_s` package data: all shock values initialised to
  `1` (neutral) so the exported template is always a clean starting
  point.
