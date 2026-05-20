## DOE Desirability Optimization

This module performs multi-response optimization using the **Derringer-Suich desirability function** approach (**Derringer & Suich, 1980**). It finds factor settings that simultaneously satisfy objectives for multiple response variables from previously analyzed DOE models.

### Workflow

**1. Select Analyses:** Choose one or more saved DOE Analysis results. Each analysis provides a fitted model for one response variable.

**2. Configure Desirability Settings:** For each response, set the objective (maximize, minimize, or target a specific value), the acceptable range (low and high limits), and the importance weight. These define the individual desirability functions.

**3. Set Factor Ranges:** Define the search space for each factor. Use the default design ranges or specify custom ranges.

**4. Run Optimization:** The optimizer searches for factor settings that maximize the **overall desirability** *D*, defined as the weighted geometric mean of individual desirabilities:

$$D = \left(\prod_{i=1}^{n} d_i^{w_i}\right)^{1/\sum w_i}$$

where *d*ᵢ is the individual desirability (0–1) and *w*ᵢ is the weight for the *i*-th response.

**5. Results:** Optimal factor settings, predicted responses, individual and overall desirability values, contour plots, and optionally k-medoids clustering of solutions.

### References

**Cardoso, R.P., da Motta Reis, J.S., Silva, D.E.W., de Barros, J.G.M., Sampaio, N.A.S.** (2023), 'How to perform a simultaneous optimization with several response variables', *Management and Administrative Professional Review* **14** (1), pp. 564-578. [DOI](http://dx.doi.org/10.7769/gesec.v14i1.1536)

**Cojocaru, C., Khayet, M., Zakrzewska-Trznadel, G., Jaworska, A.** (2009), 'Modeling and multi-response optimization of pervaporation of organic aqueous solutions using desirability function approach', *Journal of Hazardous Materials* **167**, pp. 52-63. [DOI](http://dx.doi.org/10.1016/j.jhazmat.2008.12.078)

**Derringer, G., Suich, R.** (1980), 'Simultaneous Optimization of Several Response Variables', *Journal of Quality Technology* **12** (4), pp. 214-219. [DOI](https://doi.org/10.1080/00224065.1980.11980968)
