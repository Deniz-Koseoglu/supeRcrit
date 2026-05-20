## Solute Characterization

This module characterizes target solutes using **Group Contribution Methods (GCMs)** to estimate thermophysical properties needed for supercritical extraction process design. The estimated properties are used in the Miscibility Optimization and Comparison modules to evaluate co-solvent suitability via **Hansen Solubility Parameters (HSPs)**.

### Workflow

Given the SMILES string and molecular geometry (MOL file) of a solute, the module estimates the boiling point, critical temperature, and Hansen Solubility Parameters via GCMs. The influence of temperature on solute HSPs is calculated from reduced temperatures. The HSP distance *Rₐ* between solvent and solute is then used to evaluate miscibility.

### Estimated Properties

**Normal boiling point (Tᵦ)** is estimated by Joback-Reid, Stein-Brown, Nannoolal (2004), or Hukkerikar methods. It serves as input for critical parameter estimation. **Critical temperature (Tꞓ), pressure (Pꞓ), and volume (Vꞓ)** are estimated by Joback-Reid, Nannoolal (2007), or Hukkerikar methods. These are needed for temperature-dependent HSP corrections via reduced temperatures. **Hansen Solubility Parameters (δD, δP, δH)** represent the dispersion, polarity, and hydrogen bonding components of molecular interactions. Estimated by Stefanis-Panayiotou (2008, 2012) or Hukkerikar methods. **Van der Waals volume** is estimated by Zhao (2003), Bondi (1964), or Slonimskii (1970) methods.

### Fragmentation Simplicity

Controls how the molecular structure is decomposed into GCM groups. **Auto** selects the best setting for each method. **Simple** allows overlapping substructure groups. **Normal** prevents overlaps, accepting the first valid fragmentation. **Complex** evaluates all possible fragmentation patterns (slower but more thorough).

### References

**Bell, Ian H., Wronski, Jorrit, Quoilin, Sylvain, Lemort, Vincent** (2014), 'Pure and Pseudo-pure Fluid Thermophysical Property Evaluation and the Open-Source Thermophysical Property Library CoolProp', *Industrial & Engineering Chemistry Research* **53** (6), pp. 2498-2508. [DOI](https://doi.org/10.1021/ie4033999)

**Hansen, Charles M.** (2007), *Hansen Solubility Parameters: A User's Handbook (2nd edition)*, CRC Press, London, United Kingdom.

**Tirado, Diego F., Tenorio, Maria Jose, Cabanas, Albertina, Calvo, Lourdes** (2018), 'Prediction of the best cosolvents to solubilise fatty acids in supercritical CO2 using the Hansen solubility theory', *Chemical Engineering Science* **190**, pp. 14-20. [DOI](https://www.doi.org/10.1016/j.ces.2018.06.017)

**Tirado, Diego F., Rousset, Amandine, Calvo, Lourdes** (2019), 'The Selective Supercritical Extraction of High-value Fatty Acids from Tetraselmis suecica using the Hansen Solubility Theory', *Chemical Engineering Transactions* **75**, pp. 133-138. [DOI](https://www.doi.org/10.3303/CET1975023)

**Tirado, Diego F., Calvo, Lourdes** (2019), 'The Hansen theory to choose the best cosolvent for supercritical CO2 extraction of beta-carotene from *Dunaliella salina*', *The Journal of Supercritical Fluids* **145**, pp. 211-218. [DOI](https://www.doi.org/10.1016/j.supflu.2018.12.013)
