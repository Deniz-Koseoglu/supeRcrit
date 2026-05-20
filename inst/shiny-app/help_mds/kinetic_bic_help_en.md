## Broken-and-Intact Cells (BIC) Kinetic Model

The BIC model (**Sovova, 2005**; **2012**; **2017**) describes supercritical fluid extraction kinetics by distinguishing two regions within raw material particles: **broken cells** where solute is easily accessible on the surface, and **intact cells** where solute must diffuse through the microporous matrix. The model was previously utilized by **Rizza (2014)** and many other authors.

### Extraction Regions

The Overall Extraction Curve (OEC) is divided into three sequential periods:

**1. Constant Extraction Rate (CER):** Solute is washed from broken cells by convective film diffusion. The rate is limited only by solubility — not by mass transfer. Typically 50–90% of lipophilic constituents are extracted here. The slope of a clearly linear CER region estimates apparent solubility *yₛ* and extraction rate, while the total yield fraction represents the extract portion in broken cells.

**2. Falling Extraction Rate (FER):** Film diffusion from broken cells progressively decreases while intraparticle diffusion from intact cells increases. Characterized by an exponential decrease in extraction rate.

**3. Diffusion-Controlled (DC):** Rate is exclusively limited by intraparticle diffusion from intact cells. The total extract yield is estimated from the asymptote.

### Model Types

**Simplified** models only the CER region based on apparent solubility. **Complete** models CER + DC (2 regions) or CER + FER + DC (3 regions) using the full **Sovova (2005)** formulation with mass transfer coefficients. **Characteristic Times** uses an alternative formulation (**Sovova, 2012**) based on dimensionless time parameters.

### Parameters

**Fitted by the model:** *r* is the grinding efficiency (fraction of broken cells, 0–1). *kₛaₛ* is the product of solid-phase mass transfer coefficient and specific interfacial area (s⁻¹). *kf* is the fluid-phase film mass transfer coefficient (s⁻¹). *θₑ* is the dimensionless external mass transfer parameter. *qₘ* is the relative solvent consumed at the CER endpoint (kg/kg). *tᵢ* is the FER period duration (min).

**User-specified:** *cᵤ* is the maximum extractable fraction (0–1). *P* and *T* are extraction pressure and temperature. Material mass, moisture content, vessel dimensions (D, L), real density, and particle diameter define the extraction bed geometry. Flow rate and co-solvent fraction define solvent delivery.

**Calculated automatically:** Bed porosity *ε*, specific surface area *a₀* (1/m), CO₂-to-solid ratio *γ* (kg/kg), dry mass *N* (g), insoluble mass *Nₘ* (g), solute-to-solid ratio *xᵤ* (kg/kg), and apparent solubility *yₛ* (g/g).

### References

**Rizza, C.S.** (2014), *Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae*, MSc thesis, Universita Degli Studi Di Padova, Dipartimento Di Ingegneria Industriale.

**Sovova, H.** (2005), 'Mathematical model for supercritical fluid extraction of natural products and extraction curve evaluation', *The Journal of Supercritical Fluids* **33** (1), pp. 35-52. [DOI](https://doi.org/10.1016/j.supflu.2004.03.005)

**Sovova, H.** (2012), 'Steps of supercritical fluid extraction of natural products and their characteristic times', *The Journal of Supercritical Fluids* **66**, pp. 73-79. [DOI](https://doi.org/10.1016/j.supflu.2011.11.004)

**Sovova, H.** (2017), 'Broken-and-intact cell model for supercritical fluid extraction: Its origin and limits', *The Journal of Supercritical Fluids* **129**, pp. 3-8. [DOI](https://doi.org/10.1016/j.supflu.2017.02.014)
