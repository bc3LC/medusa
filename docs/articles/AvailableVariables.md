# Available Variables

`medusa` allows for the calculation of distributional impacts according
to numerous socio-economic and demographic variables of households. The
variables available differ between the Spain and EU data.

## Spain

The variables available for Spain are detailed below. To see them in R,
run [`available_var_impact()`](../reference/available_var_impact.md).

[TABLE]

## EU

The variables available for EU countries are detailed below. To see them
in R, run [`available_var_eu()`](../reference/available_var_eu.md).
Income groups (quintile, decile, ventile, percentile) are available both
at the **national level** (calculated within each country) and at the
**EU level** (calculated across all EU households jointly).

| Variable | Description | Labels |
|----|----|----|
| quintile | Income quintile at national level [\[1\]](#id_1). | 1–5 |
| quintile_eu | Income quintile at EU level [\[1\]](#id_1). | 1–5 |
| decile | Income decile at national level [\[1\]](#id_1). | 1–10 |
| decile_eu | Income decile at EU level [\[1\]](#id_1). | 1–10 |
| ventile | Income ventile at national level [\[1\]](#id_1). | 1–20 |
| ventile_eu | Income ventile at EU level [\[1\]](#id_1). | 1–20 |
| percentile | Income percentile at national level [\[1\]](#id_1). | 1–100 |
| percentile_eu | Income percentile at EU level [\[1\]](#id_1). | 1–100 |
| country | Country of residence. | AT, BE, BG, CY, CZ, DE, DK, EE, EL, ES, FI, FR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK |
| zone | Zone of residence. | Urban, Semi-urban, Rural |
| household_type | Type of household. | Adult alone, Couple, Couple with children, More than 2 adults, More than 2 adults with children, Single parent |
| income_source | Main income source of the household. | Wages, Self-employment, Property, Pensions, Unemployment |
| gender | Gender of the reference person. | Male, Female |
| feminization_degree | Feminization degree of the household. | FD1 (\<20% women), FD2 (20–40%), FD3 (40–60%), FD4 (60–80%), FD5 (\>80%) |
| age | Age of the reference person. | Young (≤30), Adult (30–65), Elder (≥65) |
| birth_country | Country of birth of the reference person. | National, EU, Non-EU, Non-national |
| education | Education level of the reference person. | Early childhood education, Primary, Secondary, Post-secondary, Tertiary, Higher education |
| activity | Professional status of the reference person. | Employed, Unemployed, Retired, Student, Domestic tasks, Disabled, Military service |
| contract_type | Type of employment contract of the reference person. | Permanent, Fixed-term, Not applicable |
| workday | Type of working hours of the reference person. | Full time, Part time, Not apply |
| employment_sector | Employment sector of the reference person. | Private sector, Public sector |
| REGMR | Tenure status of the main residence. | Ownership, Rented, Relinquish |

*\[1\] Equivalent consumption expenditure is used instead of income as
it is considered a better proxy for permanent household income since it
fluctuates less in the long run [(Goodman & Oldfield, 2004)](#R1). The
equivalent spending is calculated based on household spending
relativised by the modified OECD equivalence scale, thus considering the
economies of scale generated in households based on their size. The
modified OECD scale values 1 for the reference person in the household,
0.5 for other people aged 14 or over, and 0.3 for other people under 14
years of age.*

\[R1\] Goodman, A., & Oldfield, Z. (2004). *Permanent differences?
Income and expenditure inequality in the 1990s and 2000s* (Research
Report R66). IFS Report. <https://doi.org/10.1920/re.ifs.2004.0066>
