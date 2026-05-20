## Mixture Critical Parameters

This tool estimates the critical temperature (Tc), critical pressure (Pc), and critical density of solvent mixtures at user-specified compositions, pressures, and temperatures. It also determines whether the mixture is in a supercritical state at the given conditions.

### CO₂ + Ethanol

Uses the **Chueh-Prausnitz** and **Redlich-Kister** methods specific to CO₂–Ethanol binary mixtures (**Gil et al., 2012**; **Chueh & Prausnitz, 1967**; **Redlich & Kister, 1948**). These cover the entire CO₂ molar fraction range (0–1). The Redlich-Kister method additionally corrects the overestimation of Chueh-Prausnitz at high CO₂ molar fractions (> 0.5).

### General Mixture

Supports arbitrary binary and ternary solvent combinations. Available methods for critical temperature include **Kay (1938)** (molar averaging), **Li (1971)** (accounts for molecular size), **First Extended Chueh-Prausnitz** (**Najafi et al., 2014**), **He et al. (2017)**, and four variants of **Tang et al. (2025)**. Critical pressure methods are the same except FECP, which is not available for pressure.

### References

**Chueh, P.L., Prausnitz, J.M.** (1967), 'Vapor-Liquid Equilibria at High Pressures: Calculation of Critical Temperatures, Volumes, and Pressures of Nonpolar Mixtures', *AIChE Journal* **13** (6), pp. 1107-1113. [DOI](https://doi.org/10.1002/aic.690130613)

**Gil, L., Blanco, S.T., Rivas, C., Laga, E., Fernandez, J., Artal, M., Velasco, I.** (2012), 'Experimental determination of the critical loci for {n-C6H14 or CO2 + alkan-1-ol} mixtures. Evaluation of their critical and subcritical behavior using PC-SAFT EoS', *Journal of Supercritical Fluids* **71**, pp. 26-44. [DOI](https://www.doi.org/10.1016/j.supflu.2012.07.008)

**He, M., Liu, Y., Liu, X.** (2017), 'Prediction of critical temperature and critical pressure of multicomponent mixtures', *Fluid Phase Equilibria* **441**, pp. 2-8. [DOI](http://dx.doi.org/10.1016/j.fluid.2016.11.017)

**Kay, W.B.** (1938), 'Liquid-Vapor Phase Equilibrium Relations in the Ethane-n-Heptane System', *Industrial & Engineering Chemistry* **30** (4), pp. 459-465. [DOI](https://doi.org/10.1021/ie50340a023)

**Li, C.C.** (1971), 'Critical temperature estimation for simple mixtures', *The Canadian Journal of Chemical Engineering* **49** (5), pp. 709-710. [DOI](https://doi.org/10.1002/cjce.5450490529)

**Redlich, O., Kister, A.T.** (1948), 'Algebraic Representation of Thermodynamic Properties and the Classification of Solutions', *Industrial & Engineering Chemistry* **40** (2), pp. 345-348. [DOI](https://doi.org/10.1021/ie50458a036)
