# Precompile the expensive CoSolvent_Choice vignette.
#
# WHY: that vignette runs the ML-solubility + COSMO-SAC + cascade workflow end to end
# (~21 live chunks, ~4 min). `R CMD build` evaluates vignette chunks, so every build/check
# paid that cost. Here we knit the live source ONCE into a *static* `.Rmd` whose chunks are
# already "cooked" (code echoed + outputs + figures baked in as plain markdown), so the
# normal vignette engine renders it at build time with NO R evaluation (seconds, not minutes).
# End-user load time is unchanged — installed vignettes were always pre-rendered HTML.
#
# SOURCE OF TRUTH: vignettes/CoSolvent_Choice.Rmd.orig   (the live, runnable vignette)
# GENERATED (ships): vignettes/CoSolvent_Choice.Rmd + vignettes/cosolvent-figure/*
#
# RE-RUN whenever the vignette's prose/code or the underlying supeRcrit API changes:
#   R CMD INSTALL .            # install the current package first
#   Rscript vignettes/precompile.R
# Only CoSolvent_Choice is precompiled; the other four vignettes stay live (they are cheap).

if (!requireNamespace("knitr", quietly = TRUE)) stop("knitr is required to precompile.")

local({
  old <- setwd("vignettes")
  on.exit(setwd(old), add = TRUE)
  # Figures land in vignettes/cosolvent-figure/ and are embedded (self-contained) at build.
  knitr::opts_chunk$set(fig.path = "cosolvent-figure/")
  knitr::knit("CoSolvent_Choice.Rmd.orig", output = "CoSolvent_Choice.Rmd")
  message("Precompiled -> vignettes/CoSolvent_Choice.Rmd")
})
