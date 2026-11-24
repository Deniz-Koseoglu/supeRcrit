## Components & Parameters

Build a component for the GC-EOS / GCA-EOS phase-equilibria tools from a molecular structure. Every successful analysis is added to the session automatically, ready for the flash, fractionation-column and phase-diagram tools.

**Identifying the molecule**

- **EOS Model** — **GC-EOS** for hydrocarbons and terpenes (deterpenation, squalene), or **GCA-EOS**, which adds hydrogen-bonding *association* for fatty acids, water and alcohols.
- **Load Example** — opens a dialog to pick a built-in molecule; its data fills the fields, which stay editable.
- **CAS Number** — the primary identifier; the structure and SMILES are retrieved automatically. A CAS number is required when *T*c or *T*b is estimated by GCM.
- **Display Name** and **SMILES** — optional, enabled with their toggles. Provide a SMILES directly to work offline when both *T*c and *T*b are entered manually.

**Critical temperature and boiling point**

Each of *T*c and *T*b can be **entered manually** or **estimated by a Group Contribution Method** (Joback-Reid, Nannoolal, Stein-Brown, Hukkerikar, …). The hard-sphere critical diameter **dc** is regressed so the EOS reproduces the normal boiling point (*P*sat = 1 atm at *T*b). When a GCM is selected, a **Group Order** control sets the maximum group order and a **Show Visualization** button displays the fragments used.

**Vapour pressure**

Enter one or more temperatures (up to 10) as tags; the pure-component vapour pressure is reported at each, with a plot when more than two are given.

**Results & visualisation**

The result panel shows the molecule split into EOS groups (left), the group decomposition, the fitted **dc**, *T*c, *T*b, the vapour pressures, and — for GCA-EOS — the **associating groups**.

**Saving**

The component is added to the session automatically. **Save to Disk** opens a dialog to choose a filename and folder and writes the component as JSON.

**Notes**

- Range badges turn **red** and block the run when a value is outside its valid range.
- GCM estimation and CAS lookup require an internet connection (PubChem); a SMILES with manual *T*c / *T*b works offline.
