# TheModel

## The microsimulation model

MEDUSA allows to develop distributional analyses based in the following
overnight effect microsimulation model. The model is built up with the
microdata from the Household Budget Survey, a common statistic in all EU
countries which is increasingly standardized and which has relevant
potential due the large amount of socioeconomic information that it
collects. The HBS provides information about household final consumption
expenditure on goods and services and information on some socioeconomic
and demographic characteristics of each household. The HBS provides
information at two levels: one for households and their expenditures and
the other for household members.

In order to have a suitable database for the model, some adjustments are
necessary. Firstly, some socio-economic variables have been renamed and
new ones have been created based on the information collected in the
survey (e.g. income variables: decile and quintile). Secondly, it is
usual that expenditure variables by COICOP categories have to be
aggregated because it is sometimes difficult to have price impacts at
such a disaggregated level. To adjust the consumption expenditure data
to the inputs provided by the price model, we include in the package a
function that allows the aggregation of variables (the aggregation is
determined by default but can also be customized according to the user’s
needs). Moreover, if the inputs of the price shock are determined by a
macro model, an additional adjustment is necessary. In fact, despite the
fact that the HBS covers a representative sample of the population and
provides a very detailed image of the annual consumption of households,
the aggregate costs of the survey are not aligned with the principles
and data of the National Accounting, which builds its macroeconomic
aggregates based on more complete sources of information. Therefore,
before the simulation, the HBS data should be adjusted to make them
consistent with the macroeconomic dimension. For this purpose, the
package has a function that allows the survey to be elevated to make it
consistent with the National Accounts. The following adjustments are
made within the function: i) Scaling the HBS population to be consistent
with the reference population of the National Accounts and ii) Scaling
the HBS consumption data according to the final consumption per energy
good of the National Accounts.

The model simulates the changes in spending in the COICOP categories,
multiplying the changes in prices by the current levels of spending on
the different products consumed by households that are part of the HBS
dataset. Thus, the model reflects the direct impacts of the selected
price shock before assuming any changes in behaviour related to the new
prices. In other words, the microsimulation model does not reflect the
reaction of the different types of households to the expected changes in
prices. In order to carry out a “behavioural” impact study, it would be
necessary to collect the direct reactions of consumers (through the
price elasticities of demand for goods) and the induced reactions
(through cross-elasticities and income elasticities). At the
distributional level the lack of elasticities would not have a large
effect because if the elasticity introduced is homogeneous for all
households it will not affect the differences between different types of
households. However, in future updates of the model it would be
interesting to introduce heterogeneous household elasticities. Also, in
the case of energy goods and transportation services, these effects are
known to be small in the short and medium term, since households do not
easily change their behaviour as far as energy consumption is concerned
[(Labandeira et al., 2017)](#R1).

The results derived from the model are presented as the relative impact
(%) on total equivalent consumption expenditure [\[1\]](#id_1). The
relative impact, $\Delta e_{h}^{s}$, shows the additional cost that
household *h* would assume in a proposed scenario in relative terms (%),
compared to the initial household expenditure, and it is calculated as:

$$\Delta e_{h}^{s} = \frac{\sum\limits_{c}e_{c,h}\left( 1 + \Delta p_{c}^{s} \right) - \sum\limits_{c}e_{c,h}}{\sum\limits_{c}e_{c,h}} \times 100$$

here $e_{c,h}$ refers to the total spending on each consumption
category, *c* , consumed by each of the household, in the baseline
scenario and $\Delta p_{c}^{s}$ is the increase in prices by consumption
category and scenario obtained with the price model.

The great granularity of the data contained in the HBS allows the
integration of the intersectional approach [\[2\]](#id_2) in the
analysis. The results can therefore be calculated according to numerous
socioeconomic characteristics of the households (such as, class, gender,
race…). Furthermore, in order to facilitate the analysis of the results,
the package allows the generation of summary graphs of the
distributional impacts either for one or several socio-economic
variables.

*\[1\]Equivalent consumption expenditure is used instead of income as it
is considered a better proxy for permanent household income since it
fluctuates less in the long run [(Goodman & Oldfield, 2004)](#R2). The
equivalent spending is calculated based on household spending
relativized by the modified OECD equivalence scale, thus considering the
economies of scale generated in households based on their size. The
modified OECD scale values 1 for the reference person in the household,
0.5 for other people aged 14 or over, and 0.3 for other people under 14
years of age.*

*\[2\] Intersectionality refers to the fact that the privileges or
oppression of each individual depend on the multiple social categories
to which he or she belongs, which are social constructs and can change
over time ([Cho et al., 2013](#R3); [Crenshaw, 1994](#R4); [Davis,
2008](#R5); [Djoudi et al., 2016](#R6); [Kaijser & Kronsell,
2014](#R7)). Intersectionality is therefore also a tool for analysing
the articulation of different socio-economic categories (e.g. class,
gender, race, etc.) rather than considering them as independent forms of
power relations (Colombo & Rebughini, 2016).*

## References

\[R1\] Labandeira, X., Labeaga, J., & López-Otero, X. (2017). A
meta-analysis on the price elasticity of energy demand. *Energy Policy*
, 102 ( C), 549–568.

\[R2\] Goodman, A., & Oldfield, Z. (2004). *Permanent differences?
Income and expenditure inequality in the 1990s and 2000s* (Research
Report R66). IFS Report. <https://doi.org/10.1920/re.ifs.2004.0066>

\[R3\] Cho, S., Crenshaw, K. W., & McCall, L. (2013). Toward a Field of
Intersectionality Studies: Theory, Applications, and Praxis. *Signs:
Journal of Women in Culture and Society, 38(4), 785–810.*
<https://doi.org/10.1086/669608>

\[R4\] Crenshaw, K. W. (1994). Mapping the Margins: Intersectionality,
Identity Politics, and Violence Against Women of Color. In *The Public
Nature of Private Violence*. Routledge. \[R5\] Davis, K. (2008).
Intersectionality as buzzword: A sociology of science perspective on
what makes a feminist theory successful. *Feminist Theory*, 9(1), 67–85.
<https://doi.org/10.1177/1464700108086364>

\[R6\] Djoudi, H., Locatelli, B., Vaast, C., Asher, K., Brockhaus, M., &
Basnett Sijapati, B. (2016). Beyond dichotomies: Gender and intersecting
inequalities in climate change studies. *Ambio*, 45(3), 248–262.
<https://doi.org/10.1007/s13280-016-0825-2>

\[R7\] Kaijser, A., & Kronsell, A. (2014). Climate change through the
lens of intersectionality. *Environmental Politics*, 23(3), 417–433.
<https://doi.org/10.1080/09644016.2013.835203>
