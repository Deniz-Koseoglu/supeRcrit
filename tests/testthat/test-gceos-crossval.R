# =============================================================================
# Cross-validation matrix: every literature dataset x every applicable model,
# bounded by the validated AAD (predictive, zero fitted binary params unless the
# molecule-specific Diaz/Ganan sets are used). Folds tools/gceos/validate_all.R
# into regression tests. Heavy solves are skip_on_cran().
#
# Headline conclusions encoded here:
#   * gceos-hmw is the best GENERAL model (terpene alcohols, esters, squalene)
#   * diaz2005 is best for limonene/linalool specifically; ganan2011 reproduces limonene
#   * gcaeos is the ONLY viable FFA model -- association is essential (~5x)
#   * a-tocopherol is the predictive accuracy boundary for all models
# =============================================================================

core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1,1,1), matrix(5e5,1,1),
              matrix(0,1,1), matrix(0,1,1), 82.06); TRUE
}, error = function(e) FALSE)
ATM <- 101325; mpa2atm <- function(p) p*1e6/ATM; atm2mpa <- function(p) p*ATM/1e6
aad <- function(p, e) { ok <- is.finite(p); if (sum(ok) < 1) NA_real_ else mean(abs((p[ok]-e[ok])/e[ok]))*100 }
co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)

.cv <- new.env()
.cv$g_lim <- NULL
cvsetup <- function() {
  if (!is.null(.cv$g_lim)) return(invisible())
  .cv$g_lim <- gceos_decompose("CC1=CCC(CC1)C(=C)C")
  .cv$g_lin <- gceos_decompose("CC(C)=CCCC(C)(O)C=C")
  .cv$g_mo  <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OC")
  .cv$g_oa  <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)O", "gcaeos")
  .cv$diaz  <- gceos_params("diaz2005")
  .cv$ganan <- gceos_params("ganan2011")
  fit <- function(g, Tc, Tb, m, rng) suppressWarnings(gceos_fit_dc(g, Tc, Tb, 1, m, rng))
  .cv$dc_lim   <- fit(.cv$g_lim, 660, 449.65, "gceos", c(2,12))
  .cv$dc_lim_h <- fit(.cv$g_lim, 660, 449.65, "gceos-hmw", c(2,12))
  .cv$dc_lim_g <- fit(c(`C=CH2`=1,`C=CH`=1,CYCH2=3,CYCH=1,CH3=2), 660, 449.65, .cv$ganan, c(2,12))
  .cv$dc_lin   <- fit(.cv$g_lin, 630.5, 471.15, "gceos", c(2,12))
  .cv$dc_lin_h <- fit(.cv$g_lin, 630.5, 471.15, "gceos-hmw", c(2,12))
  invisible()
}
dzc <- function(nm) { d <- gceos_params("diaz2005"); g <- d$decomp[nm,]; g <- g[g>0]
  list(groups = g, dc = d$components[nm,"dc"], Tc = d$components[nm,"Tc"]) }
# bubble pressure (MPa) for solute mol under model
bubM <- function(T, x, mol, model) {
  cvsetup()
  C <- switch(paste(mol, model),
    "limonene gceos"       = list(groups=.cv$g_lim, dc=.cv$dc_lim,   Tc=660),
    "limonene gceos-hmw"   = list(groups=.cv$g_lim, dc=.cv$dc_lim_h, Tc=660),
    "limonene diaz"        = dzc("limonene"),
    "limonene ganan"       = list(groups=c(`C=CH2`=1,`C=CH`=1,CYCH2=3,CYCH=1,CH3=2), dc=.cv$dc_lim_g, Tc=660),
    "linalool gceos"       = list(groups=.cv$g_lin, dc=.cv$dc_lin,   Tc=630.5),
    "linalool gceos-hmw"   = list(groups=.cv$g_lin, dc=.cv$dc_lin_h, Tc=630.5),
    "linalool diaz"        = dzc("linalool"),
    "methyl_oleate gceos-hmw" = list(groups=.cv$g_mo, dc=gceos_dc_ester(.cv$g_mo, w=1), Tc=770.9))
  mdl <- if (model=="diaz") .cv$diaz else if (model=="ganan") .cv$ganan else model
  cc  <- if (model=="diaz") dzc("CO2") else co2
  s <- tryCatch(gceos_saturation(T, c(x,1-x), list(CO2=cc, S=C), model=mdl, type="bubble", Prange=c(1,400)),
                error=function(e) NULL)
  if (is.null(s)) NA_real_ else atm2mpa(s$P)
}

# ---- datasets ----
IWAI <- data.frame(T=rep(c(313.2,323.2,333.2),each=5),
  x=c(0.3847,0.5805,0.7004,0.8025,0.8871,0.2873,0.4541,0.6650,0.7744,0.8338,0.3401,0.4812,0.6415,0.7109,0.7696),
  P=c(3.94,5.97,6.93,7.53,7.87,3.94,6.02,7.97,8.94,9.27,4.99,6.97,8.82,9.73,10.26))
RAE <- rbind(
  data.frame(T=313.15,x=c(0.9501,0.9302,0.8011,0.7014,0.6096,0.4003,0.2009),P=c(8.278,8.203,7.747,7.191,6.402,4.123,2.025)),
  data.frame(T=323.15,x=c(0.9501,0.9302,0.8011,0.7014,0.6096,0.4003,0.2009),P=c(10.043,9.959,9.237,8.426,7.392,4.634,2.240)),
  data.frame(T=333.15,x=c(0.9302,0.8011,0.7014,0.6096,0.4003,0.2009),P=c(11.874,10.807,9.706,8.373,5.144,2.450)))

test_that("CO2+limonene bubble P (Iwai 1996): all models within validated AAD", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  P <- function(m) aad(mapply(function(T,x) bubM(T,x,"limonene",m), IWAI$T, IWAI$x), IWAI$P)
  expect_lt(P("gceos"), 13); expect_lt(P("gceos-hmw"), 10)
  expect_lt(P("diaz"), 9);   expect_lt(P("ganan"), 13)        # ganan2011 reproduces limonene (~10%)
})

test_that("CO2+linalool bubble P (Raeissi 2001): diaz best, hmw best general", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  P <- function(m) aad(mapply(function(T,x) bubM(T,x,"linalool",m), RAE$T, RAE$x), RAE$P)
  a_gceos <- P("gceos"); a_hmw <- P("gceos-hmw"); a_diaz <- P("diaz")
  expect_lt(a_diaz, 8)                 # molecule-specific best
  expect_lt(a_hmw, 14)                 # best general model
  expect_lt(a_gceos, 24)
  expect_lt(a_hmw, a_gceos)            # gceos-hmw beats plain gceos for the alcohol
})

test_that("CO2+methyl oleate (Fang 2004): gceos-hmw is the ester model", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  d <- data.frame(T=c(313.15,313.15,313.15,333.15,333.15,333.15),
                  x=c(0.75857,0.79513,0.85923,0.57829,0.66402,0.78158), P=c(6.20,6.69,8.09,5.21,6.90,9.55))
  expect_lt(aad(mapply(function(T,x) bubM(T,x,"methyl_oleate","gceos-hmw"), d$T, d$x), d$P), 7)
})

test_that("Squalene solubility (Catchpole 1998): gceos-hmw works, plain gceos cannot split", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  sq <- list(groups=c(CH3=8,CH2=10,`CH=C`=6), dc=9.244, Tc=838.1)
  sol <- function(P, model) { fl <- tryCatch(gceos_flash(323, mpa2atm(P), c(0.97,0.03),
           list(CO2=co2,S=sq), model=model, Kinit=c(3,5e-4)), error=function(e) NULL)
    if (is.null(fl)||is.null(fl$y)) return(NA_real_); y <- if (fl$y[1]>=fl$x[1]) fl$y else fl$x
    1000*y[2]*410.7/(y[1]*44.01) }
  P <- c(15,17.5,20,22.5); S <- c(10.63,17.38,23.84,30.12)
  expect_lt(aad(vapply(P, function(p) sol(p,"gceos-hmw"), 0), S), 10)   # ~6% at 323 K
  expect_true(all(is.na(vapply(P, function(p) sol(p,"gceos"), 0))))      # gceos can't form the split
})

test_that("CO2+oleic acid (Nilsson 1991): gcaeos + association is essential", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  cvsetup(); gca <- gceos_params("gcaeos")
  nil <- rbind(data.frame(T=323.15,P=c(13.7,15.1,17.2,20.6),w=c(0.39,0.58,0.98,1.49)),
               data.frame(T=333.15,P=c(13.1,13.7,15.1,19.9),w=c(0.17,0.31,0.54,0.96)))
  oaW <- function(T, P, dc, assoc) {
    C <- list(groups=.cv$g_oa, dc=dc, Tc=781); xi <- .gceos_inputs(T, list(CO2=co2,S=C), gca)
    numu <- if (assoc) xi$numu else NULL; D0 <- if (assoc) xi$Delta0 else NULL
    fr <- tryCatch(gceos_flash_cpp(T, mpa2atm(P), c(0.99,0.01), xi$d,xi$q,xi$nu,xi$g,xi$dg,xi$alpha,
            82.06, c(5,1e-3), 300L, 1e-5, numu, D0), error=function(e) NULL)
    if (is.null(fr) || fr$status != 1) return(NA_real_)
    y <- if (fr$y[1]>=fr$x[1]) fr$y else fr$x; 100*y[2]*282.46/(y[2]*282.46+y[1]*44.01) }
  aON  <- aad(mapply(function(T,P) oaW(T,P,7.3712,TRUE),  nil$T, nil$P), nil$w)
  aOFF <- aad(mapply(function(T,P) oaW(T,P,7.3712,FALSE), nil$T, nil$P), nil$w)
  expect_lt(aON, 60)                   # ~49%, better than Ortega's own predictive vapor AARD (167%)
  expect_lt(aON, aOFF/3)               # association cuts the error >3x (machinery verified correct)
})

test_that("a-tocopherol solubility (Pereira 1993): the predictive boundary", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  toc <- list(groups=c(CH3=4,CH2=9,CH=2,cyCH2=3,AC=1,ACCH3=3,ACOCH3=1,ACOH=1), dc=8.858, Tc=857.4)
  yt <- function(P) { fl <- tryCatch(gceos_flash(323.1, mpa2atm(P), c(0.995,0.005),
          list(CO2=co2,S=toc), model="gceos", Kinit=c(2,1e-4)), error=function(e) NULL)
    if (is.null(fl)) NA_real_ else { y <- if (fl$y[1]>=fl$x[1]) fl$y else fl$x; y[2] } }
  P <- c(15.10,18.30,20.60,22.90,26.05); y <- c(0.00041,0.00077,0.00085,0.00133,0.00138)
  a <- aad(vapply(P, yt, 0), y)
  expect_gt(a, 30); expect_lt(a, 65)   # ~49% -- documents the boundary (polar chromanol)
})

test_that("CO2+linalool mixture criticals (Raeissi 2001): gceos", {
  skip_if_not(core_ok(), "core"); skip_on_cran(); cvsetup()
  crit <- data.frame(Tc=c(311.41,318.11,320.96,326.43,343.27), Pc=c(8.218,9.186,9.660,10.676,13.689),
                     xc=c(0.9928,0.9801,0.9701,0.9501,0.9302))
  lin <- list(groups=.cv$g_lin, dc=.cv$dc_lin, Tc=630.5)
  pcp <- xcp <- rep(NA_real_, nrow(crit))
  for (i in seq_len(nrow(crit))) { cc <- tryCatch(gceos_critical(crit$Tc[i], list(CO2=co2,S=lin), "gceos",
      xrange=c(0.80,0.998), Prange=c(20,400)), error=function(e) NULL)
    if (!is.null(cc)) { pcp[i] <- atm2mpa(cc$P_c); xcp[i] <- cc$x_c } }
  expect_lt(aad(pcp, crit$Pc), 9); expect_lt(aad(xcp, crit$xc), 2)
})
