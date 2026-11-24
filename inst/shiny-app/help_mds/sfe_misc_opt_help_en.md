## Miscibility Optimization

This module evaluates which co-solvents best enhance the miscibility of a target solute in supercritical CO₂, based on **Hansen Solubility Parameters (HSPs)** and the work of **Tirado et al. (2018, 2019)**.

### Method

Solute HSPs (estimated in the Solute Characterization module) are compared against the HSPs of pure CO₂ and CO₂ + co-solvent mixtures across a grid of pressures and temperatures. The HSP distance *Rₐ* between solvent (subscript 1) and solute (subscript 2) is:

$$R_a = \sqrt{4 \times (\delta_{d1} - \delta_{d2})^2 + (\delta_{p1} - \delta_{p2})^2 + (\delta_{HB1} - \delta_{HB2})^2}$$

where *δ_d*, *δ_p*, and *δ_HB* are the dispersion, polarity, and hydrogen bonding HSP components. The **Miscibility Enhancement (ME%)** is then calculated as:

$$ME\ (\%) = \left(1 - \frac{R_{a,\ scCO_2 + cosolvent}}{R_{a,\ pure\ scCO_2}}\right) \times 100$$

A positive ME% indicates that the co-solvent brings the solvent mixture closer to the solute in HSP space, improving miscibility. The evaluation is repeated at every pressure–temperature combination in the specified range.

### Solvent Blends

Custom co-solvent blends (two or more solvents at specified proportions) can be built and screened, and the **Automatic Solvent Finder** can additionally recommend enhancing blends when *"Also screen enhancing blends"* is ticked. How a blend is evaluated depends on the model:

- **HSP (Miscibility)** — the blend HSPs are the volume-weighted averages of the individual solvent HSPs (Hansen-distance blending).
- **CSP-2021 / openCOSMO-RS-Phi** — each blend is evaluated directly as a genuine *n*-component COSMO-SAC-Phi solve (the solute in CO₂ + the co-solvents at their mole fractions), not an extrapolation.

Both Solubility-mode engines (**CSP-2021** and **openCOSMO-RS-Phi**) are genuine *n*-component models, so blends are screened directly in either engine as well as in HSP (Miscibility) mode.

### References

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), 'Pure and Pseudo-pure Fluid Thermophysical Property Evaluation and the Open-Source Thermophysical Property Library CoolProp', *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London, United Kingdom.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), 'Prediction of the best cosolvents to solubilise fatty acids in supercritical CO2 using the Hansen solubility theory', *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), 'The Selective Supercritical Extraction of High-value Fatty Acids from Tetraselmis suecica using the Hansen Solubility Theory', *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), 'The Hansen theory to choose the best cosolvent for supercritical CO2 extraction of beta-carotene from *Dunaliella salina*', *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
