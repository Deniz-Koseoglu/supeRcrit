## Fractionation Column

Simulate a **counter-current supercritical-CO₂ fractionation column** with the GC(A)-EOS — the process-design tool for deterpenation, deacidification, squalene recovery and similar high-pressure separations. The model is a Naphtali–Sandholm equilibrium-stage solver (the rigorous simultaneous-correction method, robust at the very large solvent ratios of SFE).

**Feed solutes**

Tick **1–3** solutes from the session list (the oil to be fractionated); CO₂ is the implicit solvent. **Load Examples** / **Load from Disk** / **Remove** manage the session components. All solutes must share one EOS model.

**Feed composition (oil)**

The mole (or **mass**, via the checkbox) fractions of the oil solutes, on a CO₂-free basis; they must sum to **1**. The badge turns red and blocks the run otherwise.

**Column parameters**

- **Stages** — number of equilibrium stages (stage 1 = top).
- **Feed stage** — where the liquid oil feed enters; stages below it form the rectifying section.
- **Reflux ratio** — external reflux R/E (0 = no reflux). Higher reflux sharpens the split.
- **S/F (mol)** — solvent-to-feed molar ratio (CO₂ flow ÷ oil feed flow); the main extraction knob.
- **Temperature** (°C) and **Pressure** (bar). Tick **temperature gradient** to impose a linear top→bottom profile (which creates internal reflux).

**Results**

- **Products** — convergence status, the solvent-free **extract** (top) and **raffinate** (bottom) compositions, each product's purity and flow, and the per-solute **recovery** (fraction leaving in the extract).
- **Stage Profiles** — the solvent-free liquid mole fraction of each solute along the column (interactive; exportable).
- **Sizing** — from a feed rate (kg/h), the **column diameter** and **packed height** via the packed-SCF flooding model (Diaz et al. 2005), using GC-EOS or Budich–Brunner phase densities.

**Notes**

- Counter-current convention: oil flows down, CO₂ flows up; the **extract** leaves the top, the **raffinate** the bottom.
- With the general (shipped) parameters a given separation needs a higher S/F than the literature value tied to a specific fitted parameter set — increase S/F and/or reflux for a sharper split.
