# Solubility & Separator Cascade

Estimate supercritical-CO2 solubility curves and simulate how a depressurisation **separator
cascade** fractionates a multi-solute extract.

## Solubility models

The **Solubility Model** selector chooses how each solute's scCO₂ solubility is computed. Only
solutes carrying the data a model needs are selectable for it:

- **Experimental** — density-based **Chrastil** $k$/$A$/$B$ coefficients *measured* for the
  compound (most reliable). Pure CO₂.
- **CSP-2021** — a predictive COSMO-SAC-Phi equation of state for compounds in its database
  (quantum σ-profiles + parameters). Supports a co-solvent.
- **openCOSMO-RS-Phi** — the newer open COSMO-SAC-Phi EoS (Markgraf et al., 2026). Per-compound
  parameters are fitted to vapour pressure + liquid molar volume (DIPPR), then solid solubility is
  predicted from the fugacity ratio plus a fusion term (melting point + ΔH_fus). Pure CO₂, for
  compounds in its bundled database. Predictive/semi-quantitative — good for screening and
  temperature/pressure trends; a systematic bias correction is applied where available.
- **Makarov (ML)** — a machine-learning model predicting from SMILES. Supports a co-solvent.

A co-solvent (entrainer) modifies only the Makarov and CSP-2021 models; the Chrastil/experimental
and openCOSMO-RS-Phi models represent pure CO₂ and ignore it.

## The Chrastil model

$$S = \rho^{k}\,e^{A/T + B}$$

where $S$ is the solute solubility, $\rho$ the CO2 density (from the Span-Wagner reference
equation of state by default), $T$ the temperature (K), and $k$, $A$, $B$ are solute-specific
coefficients. Coefficients are stored on a **mol/mol** (mole-fraction) basis; the **g/L**
concentration is obtained with $S_{g/L} = y \cdot (MW/44.01) \cdot \rho$.

## Coefficient sources

1. **Solute Selection** — import solutes from *Solute Characterization*. Only calculations for
   which the **Estimate Chrastil solubility parameters** option was enabled appear here
   (the list mirrors the *Miscibility Comparison* module). Group-contribution estimates are
   approximate (reported AARD ≈ 51 %), so use them for screening.
2. **Database** — search the bundled database of 613 compounds with fitted coefficients
   (consolidated from *Chrastil revisited*, J. Supercrit. Fluids 2021). These are the most
   reliable values.
3. **Manual entry** — type in your own $k$, $A$, $B$ (and molar mass).

The working set can be exported/imported as CSV (columns `name, k, A, B, mw, basis`).

## Solubility curves

Choose one or more temperatures and a range of pressures; each solute's solubility is plotted
against pressure (one line per temperature). Switch between mole-fraction and g/L on the plot.

## Separator cascade

The extract leaves the extractor saturated at the **extraction conditions**, then passes through
the listed **separators** in order. At each stage the CO2 density — and hence each solute's
solubility — drops, so whatever exceeds the new saturation limit precipitates there. Because
solutes have different solubility curves, they deposit preferentially in different stages: this
is the basis for fractionation.

- **CO2 flow rate** scales the absolute mass flows; recoveries are independent of it.
- The model is an ideal equilibrium-stage cascade in the dilute limit (no solute–solute
  interaction, full saturation leaving each stage).
- A molar mass is required for the cascade (to convert to mass); solutes without one are shown
  in the mole-fraction solubility curves but excluded from the cascade.

Outputs: deposited mass and recovery fraction per stage, and each stage's deposit composition
(useful for judging product purity). Everything can be exported as a single CSV.
