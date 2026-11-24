# =============================================================================
# Squalene fractionation / concentration with SC-CO2.
#  - Ternary CO2 + methyl oleate + squalene VLE  vs Ruivo, Paiva & Simoes (2004)
#    A, J. Supercrit. Fluids 29:77, Table 1 (the selectivity data behind Vasquez
#    2007 Fig 4).  Model = gceos-hmw (Fornari HMW olefin set; Vasquez 2007 d_c).
#  - Counter-current column reproduces Ruivo (2001): >95 wt% squalene raffinate.
# Metric = Vasquez/Ruivo's (solvent-free composition at measured T,P), NOT
# bubble pressure (ill-conditioned at the ~80% CO2 near-critical liquid).
# =============================================================================

core_ok <- function() tryCatch({
  gceos_ar_rt(300, 1e7, 1, 3.3, 1.261, matrix(1,1,1), matrix(5e5,1,1),
              matrix(0,1,1), matrix(0,1,1), 82.06); TRUE
}, error = function(e) FALSE)
ATM <- 101325; mpa2atm <- function(p) p*1e6/ATM
aardf <- function(p, e) { ok <- is.finite(p) & is.finite(e); if (sum(ok) < 3) NA_real_ else mean(abs((p[ok]-e[ok])/e[ok]))*100 }
.co2 <- list(groups = c(CO2 = 1), dc = 3.1289, Tc = 304.2)
.sq  <- list(groups = c(CH3 = 8, CH2 = 10, `CH=C` = 6), dc = 9.336, Tc = 838.1)  # Vasquez 2007 Table 2
.mo  <- function() list(groups = gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OC"), dc = 7.982, Tc = 767.0)
MWmo <- 296.5; MWsq <- 410.7

# Ruivo 2004A Table 1: P(MPa), T(K), liquid x1/x2/x3, vapor y1/y2/y3  (1=MO,2=SQ,3=CO2)
.RUIVO <- read.table(text="
11.15 313.2 0.1253 0.0200 0.8547 0.0043 0.0002 0.9956
13.14 323.2 0.1302 0.0221 0.8477 0.0036 0.0001 0.9963
15.13 323.2 0.1039 0.0256 0.8705 0.0074 0.0006 0.9919
11.12 313.2 0.1072 0.0769 0.8158 0.0022 0.0002 0.9976
13.07 313.2 0.0949 0.0807 0.8244 0.0036 0.0006 0.9957
14.98 313.2 0.0899 0.0826 0.8275 0.0046 0.0012 0.9942
16.91 313.2 0.0713 0.0815 0.8472 0.0057 0.0019 0.9924
13.09 323.2 0.1083 0.0791 0.8126 0.0024 0.0003 0.9973
16.86 323.2 0.0788 0.0812 0.8400 0.0046 0.0013 0.9941
11.16 313.2 0.0597 0.1440 0.7963 0.0013 0.0005 0.9982
13.06 313.2 0.0512 0.1410 0.8078 0.0013 0.0006 0.9981
14.96 313.2 0.0427 0.1423 0.8150 0.0017 0.0012 0.9971
20.52 313.2 0.0328 0.1254 0.8418 0.0027 0.0028 0.9945
13.16 323.2 0.0637 0.1397 0.7967 0.0011 0.0004 0.9985
15.00 323.2 0.0516 0.1363 0.8121 0.0013 0.0006 0.9980
18.00 323.2 0.0443 0.1306 0.8251 0.0019 0.0013 0.9968
20.83 323.2 0.0370 0.1229 0.8401 0.0024 0.0023 0.9953
18.03 343.2 0.0604 0.1323 0.8073 0.0007 0.0003 0.9990
20.96 343.2 0.0499 0.1263 0.8238 0.0012 0.0008 0.9980
15.23 313.2 0.0198 0.1791 0.8012 0.0010 0.0017 0.9973
15.18 323.2 0.0232 0.1912 0.7856 0.0005 0.0009 0.9985
", col.names = c("P","T","x1","x2","x3","y1","y2","y3"))

test_that("squalene + methyl oleate build & decompose under gceos-hmw", {
  skip_if_not(core_ok(), "core")
  g <- gceos_decompose("CCCCCCCC/C=C\\CCCCCCCC(=O)OC")            # methyl oleate
  expect_equal(unname(g["CH2COO"]), 1L); expect_equal(attr(g, "unmatched_atoms"), 0L)
  expect_equal(unname(.sq$groups[c("CH3","CH2","CH=C")]), c(8L,10L,6L))
  x <- .gceos_inputs(313, list(MO=.mo(), SQ=.sq, CO2=.co2), "gceos-hmw")  # builds w/o error
  expect_true(is.finite(x$d[2]))
})

test_that("gceos-hmw reproduces the CO2+MO+squalene ternary VLE (Ruivo 2004A)", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  comps <- list(MO = .mo(), SQ = .sq, CO2 = .co2)
  xsf_e <- ysf_e <- xsf_c <- a_e <- a_c <- rep(NA_real_, nrow(.RUIVO))
  for (i in seq_len(nrow(.RUIVO))) { d <- .RUIVO[i, ]
    z <- 0.5*(c(d$x1,d$x2,d$x3) + c(d$y1,d$y2,d$y3)); z <- z/sum(z)
    fl <- tryCatch(gceos_flash(d$T, mpa2atm(d$P), z, comps, "gceos-hmw", Kinit=c(0.03,0.005,1.2)), error=function(e) NULL)
    xsf_e[i] <- d$x2/(d$x1+d$x2); a_e[i] <- (d$y1/d$x1)/(d$y2/d$x2)
    if (is.null(fl) || is.null(fl$x)) next
    liq <- if (fl$x[3] <= fl$y[3]) fl$x else fl$y; vap <- if (fl$x[3] <= fl$y[3]) fl$y else fl$x
    xsf_c[i] <- liq[2]/(liq[1]+liq[2]); a_c[i] <- (vap[1]/liq[1])/(vap[2]/liq[2])
  }
  expect_lt(aardf(xsf_c, xsf_e), 3)        # solvent-free squalene in liquid: ~0.6% (essentially exact)
  expect_lt(aardf(a_c, a_e), 40)           # MO/SQ selectivity: ~27% (= Ruivo's own SRK-MKP ~30%)
  expect_gt(sum(is.finite(xsf_c)), 15)     # nearly all points converge
})

test_that("squalene fractionation column concentrates the raffinate (vs Ruivo 2001 >95 wt%)", {
  skip_if_not(core_ok(), "core"); skip_on_cran()
  S <- list(MO = .mo(), SQ = .sq, CO2 = .co2)
  nmo <- 0.60/MWmo; nsq <- 0.40/MWsq; zL <- c(nmo,nsq,0)/(nmo+nsq)        # 40 wt% squalene feed
  SF <- 20 * (zL[1]*MWmo+zL[2]*MWsq)/44.01                                 # G/L = 20 (mass) -> molar
  # NS solver collapses on this heavy near-critical pair; the VLLE phase-stability solver handles it
  r <- gceos_column_vlle(N=8, T=313, P=mpa2atm(11.5), comps=S, FL=1, zL=zL, FV=SF, zV=c(0,0,1),
                         feedL=1, reflux=0, solvent="CO2", model="gceos-hmw", max_iter=80, force=TRUE, n_homotopy=4)
  sq_raff_wt <- 100 * r$raffinate$x[2]*MWsq / (r$raffinate$x[1]*MWmo + r$raffinate$x[2]*MWsq)
  expect_gt(sq_raff_wt, 90)               # Ruivo 2001: >95 wt% squalene at G/L>20 (we get ~99.8)
  expect_true(any(r$nphase == 2))         # genuine two-phase fractionation (not single-phase collapse)
})
