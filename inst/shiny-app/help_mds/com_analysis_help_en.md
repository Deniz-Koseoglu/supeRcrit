## Cost of Manufacturing (COM) Analysis

This module calculates the **Cost of Manufacturing (COM)** for supercritical fluid extraction (SFE), counter-current supercritical extraction (CC-SFE), or subcritical water extraction (SWE) processes, following the methodology of **Turton et al. (1998)**.

### Process Types

**SFE** — Batch supercritical CO₂ extraction from solid raw material. Material is loaded into a vessel, CO₂ flows through the packed bed, and discrete extraction cycles are counted.

**CC-SFE** — Counter-current supercritical CO₂ extraction from liquid feed. Both the feed and CO₂ flow continuously in opposite directions. There are no discrete cycles; monthly throughput is calculated from continuous operating hours.

**SWE** — Subcritical water extraction from solid raw material. Similar to SFE but uses pressurized water as the main solvent.

### COM Formula

When the Turton et al. coefficients are enabled:

$$COM = 0.304 \times FMC + 2.73 \times COL + 1.23 \times (CRM + CUT)$$

Otherwise, COM is the weighted sum of all cost components with user-defined coefficients.

### Cost of Raw Materials (CRM)

For batch modes, monthly extraction cycles are:

$$N_{ex} = \frac{W_{sh} \times W_{hr}}{(T_{ex} + T_{aux})/60} \times W_{days}$$

For CC-SFE, monthly feed throughput is: *M*<sub>feed</sub> = *F*<sub>feed</sub> × *T*<sub>monthly</sub> / 1000 (kg)

### Cost of Utilities (CUT)

Main power plus auxiliary power for drying, comminution, and evaporation.

### Cost of Labor (COL)

$$COL = W_{sh} \times W_{pers} \times W_{pay}$$

### Economic Indicators

$$SC = COM / Y_{month} \qquad GPr = (Y_{month} \times SP_{kg}) - COM$$

$$NPr = GPr \times (1 - taxrate) \qquad PBK = CAPEX / (NPr \times 12)$$

### References

**Turton, R., Bailie, R.C., Whiting, W.B., Shaeiwitz, J.A.** (1998), *Analysis, Synthesis and Design of Chemical Process, PTR*, Prentice Hall, Upper Saddle River, NJ, USA.
