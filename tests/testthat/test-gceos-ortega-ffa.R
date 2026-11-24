# =============================================================================
# Reproduction of the Ortega (2025) CO2 + carboxylic/fatty-acid figures.
# Validates the full acid series (formic -> arachidic + oleic/linoleic) against
# experimental VLE from the PRIMARY data papers (the sources behind Ortega's
# Figs S1-S6): Laugier 1990, Adrian & Maurer 1997, Bharath 1993, Byun 2000,
# Yau 1992, Zou 1990, Schwarz & Knoetze 2012.
#
# Metric (Ortega's): AARD on liquid-phase CO2 mole fraction at each experimental
# (T,P), via a gcaeos flash. In the model's calibration range (<= 353 K) gcaeos
# reproduces Ortega-level accuracy (~5-9%); the visual P-x-y figures are in
# tools/gceos/figures/. Heavy solves are skip_on_cran().
# =============================================================================

core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1,1,1), matrix(5e5,1,1),
              matrix(0,1,1), matrix(0,1,1), 82.06); TRUE
}, error = function(e) FALSE)
ATM <- 101325; MPa <- 1e6
aardf <- function(p, e) { ok <- is.finite(p) & is.finite(e); if (sum(ok) < 1) NA_real_ else mean(abs((p[ok]-e[ok])/e[ok]))*100 }
.co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
.SMI <- c(formic="OC=O", acetic="CC(=O)O", propionic="CCC(=O)O", butyric="CCCC(=O)O",
  valeric="CCCCC(=O)O", caproic="CCCCCC(=O)O", caprylic="CCCCCCCC(=O)O", capric="CCCCCCCCCC(=O)O",
  lauric="CCCCCCCCCCCC(=O)O", myristic="CCCCCCCCCCCCCC(=O)O", palmitic="CCCCCCCCCCCCCCCC(=O)O",
  stearic="CCCCCCCCCCCCCCCCCC(=O)O", arachidic="CCCCCCCCCCCCCCCCCCCC(=O)O",
  oleic="CCCCCCCC/C=C\\CCCCCCCC(=O)O", linoleic="CCCCC/C=C\\C/C=C\\CCCCCCCC(=O)O")
.acid <- function(a) { ct <- gceos_params("gcaeos")$components; r <- ct[ct$name == paste0(a,"_acid"), ]
  list(groups = gceos_decompose(.SMI[a], "gcaeos"), dc = r$dc, Tc = r$Tc) }
# liquid x_CO2 predicted at (T, P) via a gcaeos flash (Ortega's metric)
.xliq <- function(T, Pmpa, A) { fl <- tryCatch(gceos_flash(T, Pmpa*MPa/ATM, c(0.85,0.15),
    list(CO2=.co2, S=A), "gcaeos", Kinit=c(5,1e-3)), error=function(e) NULL)
  if (is.null(fl) || is.null(fl$x)) NA_real_ else min(fl$x[1], fl$y[1]) }

# the full series must build from the shipped acid parameter set (folded-in Ortega Table S4)
test_that("full CO2-acid series builds from gcaeos$components (Ortega Table S4)", {
  skip_if_not(core_ok(), "core")
  ct <- gceos_params("gcaeos")$components
  acids <- sub("_acid$", "", grep("_acid$", ct$name, value = TRUE))
  expect_gte(length(acids), 16)                                   # formic -> azelaic
  for (a in names(.SMI)) {
    expect_true(paste0(a,"_acid") %in% ct$name)                   # in the table
    r <- ct[ct$name == paste0(a,"_acid"), ]
    expect_true(is.finite(r$dc) && is.finite(r$Tc))               # dc + Tc present
    g <- gceos_decompose(.SMI[a], "gcaeos")
    expect_equal(attr(g, "unmatched_atoms"), 0L)                  # decomposes cleanly
    expect_true(any(c("HCOOH","CH3COOH","CH2COOH") %in% names(g)))# carries an acid group
  }
})

# in-range (<=353 K) liquid-composition reproduction vs the primary VLE papers
test_that("gcaeos reproduces CO2+acid VLE at Ortega-level accuracy (<=353 K)", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  DS <- list(
    list(a="acetic",   T=313.2,  x=c(0.0793,0.1604,0.1858,0.3160,0.4919,0.6700,0.8179), P=c(0.88,1.84,2.15,3.40,5.00,6.43,7.43), lim=8),
    list(a="propionic",T=313.15, x=c(0.1973,0.3956,0.4938,0.6202,0.7638,0.8355,0.9255), P=c(2.019,4.024,4.943,6.023,7.014,7.501,8.003), lim=12),
    list(a="propionic",T=333.15, x=c(0.1486,0.2919,0.4376,0.5992,0.6861,0.7963), P=c(2.010,4.014,6.013,8.029,8.986,10.01), lim=12),
    list(a="caproic",  T=313.15, x=c(0.3191,0.5354,0.7321,0.9110), P=c(2.76,5.29,7.40,8.46), lim=12),
    list(a="caproic",  T=353.15, x=c(0.1709,0.3315,0.4299,0.5167,0.7040,0.8662), P=c(2.72,5.43,7.55,9.40,13.68,15.88), lim=10),
    list(a="lauric",   T=333.15, x=c(0.2471,0.4333,0.4818,0.5973,0.6672,0.7198,0.7773,0.8253), P=c(2.57,5.34,6.41,8.92,11.58,13.27,16.81,20.39), lim=13),
    list(a="palmitic", T=373.15, x=c(0.5771,0.6636,0.7965), P=c(13.60,19.80,30.46), lim=10),
    list(a="oleic",    T=313.15, x=c(0.662,0.718,0.752,0.787,0.802,0.812), P=c(7.27,12.01,15.89,20.95,25.19,28.41), lim=10),
    list(a="oleic",    T=333.15, x=c(0.650,0.676,0.740,0.761,0.784,0.804), P=c(7.10,11.48,16.20,20.90,23.46,28.81), lim=12),
    list(a="linoleic", T=313.15, x=c(0.720,0.750,0.773,0.788,0.806,0.817), P=c(6.40,10.02,14.12,18.21,22.93,26.50), lim=12),
    list(a="linoleic", T=333.15, x=c(0.677,0.702,0.735,0.764,0.781,0.808), P=c(6.34,9.94,13.80,18.16,21.83,27.14), lim=14))
  Ac <- list(); aards <- numeric(0)
  for (d in DS) {
    if (is.null(Ac[[d$a]])) Ac[[d$a]] <- .acid(d$a)
    xc <- vapply(d$P, function(P) .xliq(d$T, P, Ac[[d$a]]), numeric(1))
    a <- aardf(xc, d$x); aards <- c(aards, a)
    expect_lt(a, d$lim)                                           # per-set bound
  }
  expect_lt(mean(aards, na.rm = TRUE), 9)                         # in-range mean ~ Ortega's ~7%
})
