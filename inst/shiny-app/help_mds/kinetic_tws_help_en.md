## Two-Site Kinetic Desorption (TWS) Model

The TWS model is an empirical two-site first-order desorption model widely used to describe subcritical water extraction (SWE) kinetics. Originally developed for PAH extraction from soil (**Hawthorne et al., 2002**), it has been adopted for plant extract studies by **Kim et al. (2020)**, **Abidin et al. (2024)**, and many other authors.

### Model Equation

The fractional yield (ratio of extract at time *t* to the maximum extractable amount) is:

$$e(t) = 1 - F \cdot e^{-k_1 t} - (1 - F) \cdot e^{-k_2 t}$$

The model describes two parallel desorption mechanisms. The **fast site** releases fraction *F* of the total extractable material with rate constant *k*₁, while the **slow site** releases fraction (1 − *F*) with rate constant *k*₂. Typically *k*₁ ≫ *k*₂.

### Parameters

**Fitted by the model:** *k*₁ is the fast desorption rate constant (min⁻¹) describing rapid release of easily accessible compounds. *k*₂ is the slow desorption rate constant (min⁻¹) describing the gradual release of matrix-bound compounds. *F* is the fraction of easily desorbed solute (0–1), optionally fitted or set as a fixed value.

**User-specified:** *c*₀ is the maximum possible yield in your response units. *m*ᵢₙ is the mass of raw material loaded (g). *P* is the extraction pressure (bar). *T* is the extraction temperature (°C). Flow rate and units define solvent delivery.

### Optimization Methods

**Global** (recommended) searches broadly for the best fit using multiple random starting points. **Robust** is slower but handles noisy data or outliers better.

### Output

The model produces fitted values of *k*₁, *k*₂, and optionally *F*, along with R², AARD, and RMSE statistics, observed vs. predicted comparison tables, and kinetic curve plots in both time and solvent-to-feed ratio (*q*) domains.

### References

**Abidin, Z.Z., Samadi, M., Biak, D.R.A., Yunus, R.** (2024), 'Mathematical Modelling Of Extraction Of Oil From *Aquilaria malacenssis* Wood Employing Subcritical Conditions', *Journal of Applied Science and Engineering* **27** (12), pp. 3725-3738. [DOI](http://dx.doi.org/10.6180/jase.202412_27(12).0012)

**Anepankul, T., Goto, M., Sasaki, M., Pavasant, P., Shotipruk, A.** (2007), 'Extraction of anti-cancer damnacanthal from roots of *Morinda citrifolia* by subcritical water', *Separation and Purification Technology* **55**, pp. 343-349. [DOI](http://dx.doi.org/10.1016/j.seppur.2007.01.004)

**Hawthorne, S.B., Grabanski, C.B., Martin, E., Miller, D.J.** (2002), 'Comparisons of Soxhlet extraction, pressurized liquid extraction, supercritical fluid extraction and subcritical water extraction for environmental solids: recovery, selectivity and effects on sample matrix', *Journal of Chromatography A* **892**, pp. 421-433. [DOI](https://doi.org/10.1021/es010771i)

**Jamaludin, R., Kim, D.-S., Salleh, L.M., Lim, S.-B.** (2021), 'Kinetic Study of Subcritical Water Extraction of Scopoletin, Alizarin, and Rutin from *Morinda citrifolia*', *Foods* **2021** (10), article 2260. [DOI](https://doi.org/10.3390/foods10102260)

**Pereira, D.T.V., Tarone, A.G., Cazarin, C.B.B., Barbero, G.F., Martinez, J.** (2019), 'Pressurized liquid extraction of bioactive compounds from grape marc', *Journal of Food Engineering* **240**, pp. 105-113. [DOI](https://doi.org/10.1016/j.jfoodeng.2018.07.019)
