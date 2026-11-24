## Liquid–Liquid Extraction

This tool designs and ranks solvents for **extracting a target solute out of a feed (diluent) phase** — for example, pulling a flavour, drug, or natural-product molecule out of an aqueous stream. Everything is computed from molecular structure with the native **COSMO-SAC-dsp** activity-coefficient model (no experimental partition data required), and applies to **any molecule you can draw** thanks to the built-in σ-profile predictor.

It offers **two analysis modes**: a fast **quick screen** that ranks solvents by distribution coefficient, and a rigorous **two-phase flash** that solves the actual liquid–liquid equilibrium — splitting the feed into two real phases, distributing every solute between them, reporting the mutual solubility of the two solvents, and optimizing over temperature (an AMS-style "Optimize LLE").

### The physics

A solute distributes between two contacting liquid phases toward the phase in which its **activity coefficient** is lower. At the dilute limit this is the infinite-dilution activity coefficient, γ∞. For each candidate solvent the tool reports:

- **D — distribution coefficient**: `D = γ∞(feed) / γ∞(solvent)` — how strongly the solvent pulls the solute out of the feed. **Higher is better.**
- **Capacity** = `1 / γ∞(solvent)` — how much solute the solvent phase can hold.
- **γ∞ (solvent)** — the underlying infinite-dilution activity coefficient.
- **Selectivity** (optional) = `D(solute) / D(co-extract)` — how cleanly the solvent prefers your target over a named impurity. `S > 1` favours the target.

Because D is a *ratio* of activity coefficients, it is independent of the solute's melting point: any solid-fusion term cancels between the two phases. The screening therefore needs only σ-profiles, not Tm.

### The engine

Activity coefficients come from **COSMO-SAC-dsp** (Hsieh, Lin & Vrabec 2014) — the dispersion-corrected COSMO-SAC model with the 2010 hydrogen-bonding split (non-hydrogen-bonding / hydroxyl / other). This native implementation is validated bit-for-bit against the NIST reference implementation (Bell et al. 2020).

### Two analysis modes

- **Quick screen** *(default)* — ranks the whole solvent library in one pass from infinite-dilution activity coefficients (γ∞). Fast, ideal for a first look across dozens of solvents. This is the mode described in "The physics" above.
- **Two-phase flash** — solves the *actual* liquid–liquid equilibrium for a real feed (diluent + solvent + solutes). It answers what a γ∞ ranking cannot: does a genuine two-phase split even form? What is the true, finite-concentration distribution of each solute? How much of each solvent dissolves in the other phase? And at what temperature is the separation best?
- **Optimize blend** — searches the *composition* of a chosen 2–4-solvent mixture (multistart from a simplex grid + local refinement, optionally warm-started across temperature) for the recipe that maximizes the separation selectivity or the target's distribution. Because the objective comes from the full flash, blends that would phase-separate are automatically rejected. A tuned mixture frequently out-performs any single solvent (this is the mixture-design analog of the AMS "Optimize LLE" MINLP).

### Two-phase flash (rigorous LLE)

In flash mode the tool builds a feed of the diluent + each candidate solvent (at the chosen mole ratio) plus your solutes (kept dilute), and for every solvent and every temperature in a scanned range it:

1. runs a **tangent-plane stability test** (Michelsen) to decide whether the mixture splits into two liquid phases at all — solvents fully miscible with the diluent are flagged *single phase* (no extraction possible);
2. solves the **isoactivity flash** (Rachford–Rice + successive substitution, `K_i = γ_i(raffinate) / γ_i(extract)`) for the composition of **both** equilibrium phases;
3. reports each solute's **distribution** `D = x(extract) / x(raffinate)`, the pairwise **selectivity** `S = D₁ / D₂` of the target over the first additional solute, and the **mutual solubility** of the two solvents (solvent-in-raffinate and diluent-in-extract).

Results are ranked best-first — by selectivity when two or more solutes are given, otherwise by D. The **Two-Phase Optimize** tab shows the ranked (solvent × temperature) table plus the full mole-fraction composition of both phases at the best point; the **Distribution vs T** tab plots how D or selectivity varies with temperature, so you can read off the optimal extraction temperature. To separate **two or more** solutes, add them under *Additional solute(s) to separate*.

### Compound databases

Each compound's **σ-profile** (the distribution of screening-charge density over its molecular surface) is the key input. Two bundled, self-consistent databases are available:

- **Curated (UD)** — 2,259 common solvents and solutes with **familiar names** (DMol3 quantum chemistry). Best for routine screening with everyday solvents.
- **CHAOS** — 53,088 molecules (Gond et al. 2026, ωB97X-D/def2-TZVP), selected **by SMILES**. Vastly broader coverage, including designer, heavy, and specialty compounds.

> The two databases use different quantum-chemical methods, so their activity coefficients are on slightly different scales and **must not be compared across databases** — pick one per ranking. Within each database the ranking is fully consistent.

### FastSigma — any molecule, even if it is not in a database

In the **CHAOS** database you can **type a brand-new SMILES** that is not in the catalogue. The built-in **FastSigma** model then estimates its σ-profile directly from molecular structure (a machine-learning model trained on all 53k CHAOS profiles; held-out profile accuracy ≈ 0.98–0.99 cosine similarity). This means designer or derivatised targets that exist in no database can still be screened. The dispersion energy and hydrogen-bond class are computed deterministically from the structure.

### The "Solute σ-Profile" tab

This tab shows the COSMO-SAC σ-profile of the selected solute:

- For a **new SMILES**, the FastSigma estimate is drawn (so you can inspect the predicted profile).
- For a compound **already in the CHAOS database**, the FastSigma estimate is **overlaid on the true profile** in a second colour — a direct visual check of how accurate the prediction is.

The hydrogen-bond donor/acceptor cutoff lines are shown for reference.

### How to use

1. **Compound database** — Curated (named) or CHAOS (by SMILES).
2. **Target solute** — pick from the list, or (CHAOS) type any SMILES.
3. **Feed / diluent phase** — where the solute starts (default **water**).
4. **Temperature** — extraction temperature (°C).
5. **Co-extracted solute** *(optional)* — an impurity to leave behind; enables the selectivity column.
6. **Solvent screening** — a built-in library of ~66 common, readily-available extraction solvents (from the GSK solvent sustainability guide) is screened **automatically** (no manual selection needed). Optionally restrict to solvent classes (alkane, aromatic, ether, alcohol, …).
7. **Product / application context** — choose *General*, *Pharmaceutical*, *Food / feed*, or *Cosmetics*. The screen is then restricted to solvents acceptable for that context (e.g. food-grade only, or ICH-Q3C Class-1 solvents excluded for pharma), with a warning shown at the input stage.
8. **Run Screening.** Results appear ranked by **D**, with value-box highlights, a sortable table, a bar chart, and the σ-profile of the solute. Use **Export Results** for the full CSV.

### Sustainability & safety (risk assessment)

Because the highest-D solvent is not always a *sensible* one, every result carries a **GSK sustainability band** — Recommended (green), Some issues (amber), Hazardous (orange), Highly Hazardous (red) — plus its **ICH Q3C residual-solvent class and permitted concentration limit in ppm** (e.g. "ICH 2 (≤720 ppm)" for THF, "ICH 3 (≤5,000 ppm)" for low-toxicity solvents such as ethanol or ethyl acetate) and a short **hazard note** (e.g. "suspected carcinogen", "peroxide-forming", "reprotoxic — restricted"). These ppm limits are the maximum residual amounts allowed in the final product, so they tell you how thoroughly the solvent must be removed. If the top-ranked solvent carries a flag, the tool points you to the greenest high-ranking alternative (for example, 2-methyltetrahydrofuran in place of THF).

**Nothing is excluded by default — you decide.** All solvents are always screened and shown. The application context only *flags* unsuitable choices (e.g. "not food-grade", "ICH Class 1") in the results; it never removes them. If you want to hide the four highly hazardous solvents (benzene, 1,2-dichloroethane, carbon tetrachloride, carbon disulfide), tick the optional **Exclude highly hazardous solvents** box. This keeps the screen a combined **performance + safety** view without over-restricting your options.

### Notes & limitations

- COSMO-SAC is a *predictive* model: it captures trends and screens candidates well, but absolute γ∞ for strongly associating / aqueous systems can be over-predicted. Treat the **ranking** as the primary output rather than the absolute numbers.
- The **quick screen** models only dilute-limit (γ∞) behaviour; for a genuine two-phase split and finite-concentration distribution use the **two-phase flash** mode. The flash treats the solutes as dilute in the feed (lower the *solute mole fraction* to approach the infinite-dilution limit).
- FastSigma profiles are on the CHAOS scale, so the σ-profile overlay/comparison is meaningful in **CHAOS** mode; in Curated (UD) mode the tab shows only the database profile.

### References

- **Gond, D.; Arweiler, J.; Specht, T.; Hasse, H.; Jirasek, F. (2026)**, *CHAOS — A Large-scale Database for σ-Profiles and Other Molecular Descriptors*, arXiv:2511.19002; Zenodo 10.5281/zenodo.17691924.
- **Bell, I. H. et al. (2020)**, *A benchmark open-source implementation of COSMO-SAC*, J. Chem. Theory Comput. 16, 2635–2646.
- **Hsieh, C.-M.; Lin, S.-T.; Vrabec, J. (2014)**, *Considering the dispersive interactions in the COSMO-SAC model…*, Fluid Phase Equilibria 367, 109–116.
- **Hsieh, C.-M.; Sandler, S. I.; Lin, S.-T. (2010)**, *Improvements of COSMO-SAC for VLE and LLE predictions*, Fluid Phase Equilibria 297, 90–97.
- **Alder, C. M. et al. (2016)**, *Updating and further expanding GSK's solvent sustainability guide*, Green Chem. 18, 3879. (Solvent sustainability bands.) ICH Q3C(R8) *Residual Solvents*.
