## Phase Behaviour

Compute **CO2 + solute** phase behaviour with the GC(A)-EOS. CO2 is always the light component; the **solutes** are the components you built in the **Components & Parameters** tool.

**Solutes**

Tick **1 or 2** solutes from the session list. **Load Examples** adds the built-in example components; **Remove Selected from Session** deletes the ticked ones. All chosen solutes must share the same EOS model (shown below the list) — mixing a GC-EOS component with a GCA-EOS component is blocked.

**Conditions**

- **Temperature** (°C) and **Pressure** (bar) — for the flash and the P-x,y diagram. The range badges turn **red** and block the run when a value is out of range.
- **Feed composition** — the overall **mole** (or **mass**, via the checkbox) fractions of CO2 and the solute(s), used by the flash; normalised automatically.
- **Critical-locus temperature range** — the temperature sweep for the critical-locus tab.

**Results (tabs)**

The three calculations share the conditions and are shown as tabs, each computed on first view:

- **Flash** — splits the CO2 + solute(s) feed into equilibrium phases (1, 2 or 3) and reports the distribution coefficients *K = y/x*. With two solutes the relative volatility *K*₂/*K*₃ is shown. A stability test escalates to a three-phase vapour–liquid–liquid split where one exists.
- **P–x,y Diagram** *(binary only)* — at fixed temperature, the bubble (liquid *x*) and dew (vapour *y*) curves versus pressure, with the mixture **critical point** marked.
- **Critical Locus** *(binary only)* — the mixture critical pressure as a function of temperature.

The P–x,y and critical-locus tabs require a single solute (a binary CO2 + solute system); with two solutes they show a note.

**Notes**

- Temperatures are in °C and pressures in bar; the plots are interactive (hover for the exact point).
- A “did not converge” result usually means the conditions lie outside the two-phase region (e.g. above the critical locus).
