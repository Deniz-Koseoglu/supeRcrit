# Seed (example) components for the Process-Simulation (GC/GCA-EOS) tools.
# These are pre-built component objects (groups + Tc + dc + MW) so example
# molecules are immediately available to the flash / column / phase-diagram tools
# via "Load Examples", without a runtime decomposition + dc fit. The dc values
# were regressed (gceos_fit_dc, dc_range = c(2,12)) from the listed Tc / Tb, or
# taken from the validation / parameter sets (squalene & triolein gceos-hmw;
# the Diaz pair from the diaz2005 published table).
#
# Same-model mixable pairs (resolve_model blocks mixing different EOS models):
#   * Limonene + Linalool  (gceos)      -> citrus-oil deterpenation (generic demo)
#   * Limonene + Linalool  (diaz2005)   -> EXACT Diaz 2005 deterpenation parameters
#   * Squalene + Triolein  (gceos-hmw)  -> squalene recovery from a triglyceride oil
eos_seed_components <- function() {
  mp <- function(m) switch(m, gcaeos = "GCA-EOS", `gceos-fae` = "GC-EOS (esters)",
    `gceos-hmw` = "GC-EOS (high-MW)", diaz2005 = "GC-EOS (Diaz)", ganan2011 = "GC-EOS (Ganan)", "GC-EOS")
  mk <- function(name, model, smiles, cas, groups, Tc, Tb, dc, MW) {
    list(
      display_name  = sprintf("%s | %s | dc=%.3f", name, mp(model), dc),
      timestamp     = "seed",
      source_module = "EOS Components (example)",
      is_seed       = TRUE,
      component = list(name = name, model = model, smiles = smiles, cas = cas,
        groups = groups, Tc = Tc, Tb = Tb, dc = dc, MW = MW)
    )
  }
  list(
    # --- general GC-EOS terpenes (generic demo decomposition) ---
    mk("Limonene", "gceos", "CC1=CCC(CC1)C(=C)C", "5989-27-5",
       list("CH2=C" = 1, "CH=C" = 1, "cyCH2" = 3, "cyCH" = 1, "CH3" = 2),
       Tc = 660, Tb = 449.65, dc = 5.6130, MW = 136.24),
    mk("Linalool", "gceos", "CC(C)=CCCC(C)(O)C=C", "78-70-6",
       list("COH" = 1, "CH2=CH" = 1, "CH=C" = 1, "CH3(B)" = 1, "CH2(B)" = 1, "CH3" = 2, "CH2" = 1),
       Tc = 630.5, Tb = 471.15, dc = 6.2177, MW = 154.25),
    # --- Diaz 2005 terpenes (published aromatic-ring-approximation decomposition) ---
    mk("Limonene", "diaz2005", "CC1=CCC(CC1)C(=C)C", "5989-27-5",
       list("CH3(B)" = 1, "CYCH2" = 3, "CYCH" = 1, "ACH" = 1, "C=CH2" = 1, "ACCH3" = 1),
       Tc = 660, Tb = 449.65, dc = 5.6027, MW = 136.20),
    mk("Linalool", "diaz2005", "CC(C)=CCCC(C)(O)C=C", "78-70-6",
       list("CH2" = 2, "C=CH" = 1, "CH=CH2" = 1, "CH3(B)" = 3, "COH" = 1),
       Tc = 630.5, Tb = 471.15, dc = 6.1712, MW = 154.40),
    # --- high-MW pair for squalene recovery from a triglyceride oil ---
    mk("Squalene", "gceos-hmw",
       "CC(=CCCC(=CCCC(=CCCC=C(C)CCC=C(C)CCC=C(C)CCC=C(C)C)C)C)C", "111-02-4",
       list("CH=C" = 7, "CH3" = 9, "CH2" = 12),
       Tc = 795, Tb = 694.55, dc = 11.0439, MW = 410.72),
    mk("Triolein", "gceos-hmw",
       "CCCCCCCC/C=C\\CCCCCCCC(=O)OCC(OC(=O)CCCCCCC/C=C\\CCCCCCCC)COC(=O)CCCCCCC/C=C\\CCCCCCCC", "122-32-7",
       list("TG" = 1, "CH=CH" = 3, "CH3" = 3, "CH2" = 42),
       Tc = 1043.3, Tb = NA, dc = 11.839, MW = 885.43)
  )
}
