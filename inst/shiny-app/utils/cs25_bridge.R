# =====================================================================================================
# CS25 (JCOSMO) bridge  --  a real, local CS25 activity-coefficient engine for the LLE module.
#
# WHAT THIS IS. CS25 is the 2025 reparametrisation of COSMO-SAC shipped in JCOSMO 3.1.4 (atom-pair
# dispersion, a 13-interaction-type hydrogen-bonding scheme, NWChem cavities). Rather than reimplement
# its 3,150-line engine, this bridge drives the actual jcosmo.jar headlessly through a persistent
# `jshell` process (the JVM starts once; R then streams compositions in and reads ln-gamma out). So the
# numbers here are *exactly* JCOSMO's -- no approximation. It is the reference oracle the native C++
# port (src/cs25_core.cpp, in progress) is validated against.
#
# LOCAL ONLY. It needs a Java 11+ `jshell` on PATH and a JCOSMO 3.1.4 install (the folder that contains
# lib/jcosmo.jar, pars/CS25 and profiles/). Where either is absent `cs25_available()` returns FALSE and
# the app simply hides the CS25 engine choice -- nothing else changes, and the deployed host (no Java)
# is unaffected. Point it at a non-default install with options(supercrit.jcosmo_dir=) or the
# SUPERCRIT_JCOSMO_DIR environment variable.
# =====================================================================================================

# ---- install discovery ------------------------------------------------------------------------------

# Candidate JCOSMO install directories, best first. A directory qualifies if it has lib/jcosmo.jar.
.cs25_candidate_dirs <- function() {
  c(getOption("supercrit.jcosmo_dir", ""),
    Sys.getenv("SUPERCRIT_JCOSMO_DIR", ""),
    # bundled-adjacent dev install (this repo)
    file.path(getwd(), "scratch_tq", "jcosmo3"),
    "E:/Downloads/Potential Industries/1) Altraflora Documents/Internal Company Docs/Software/My R Packages/supeRcrit/scratch_tq/jcosmo3")
}

cs25_jcosmo_dir <- function() {
  for (d in .cs25_candidate_dirs()) {
    if (nzchar(d) && file.exists(file.path(d, "lib", "jcosmo.jar"))) return(normalizePath(d, winslash = "/"))
  }
  NA_character_
}

# TRUE when CS25 can actually run here (install present + jshell on PATH + processx available).
cs25_available <- function() {
  ok <- tryCatch({
    !is.na(cs25_jcosmo_dir()) &&
      nzchar(Sys.which("jshell")) &&
      requireNamespace("processx", quietly = TRUE)
  }, error = function(e) FALSE)
  isTRUE(ok)
}

# ---- persistent jshell gateway ----------------------------------------------------------------------
# A session is an environment holding the live process plus the currently-loaded compound set (so we
# only pay setCompounds -- which re-reads sigma profiles -- when the component list actually changes).

.CS25_MARK_READY <- "CS25RDYOK"   # readiness sentinel (content ignored)
.CS25_MARK_BEG   <- "CS25BEG:"    # opens the gamma payload
.CS25_MARK_END   <- "CS25ENDX"    # closes it -- we wait for THIS (it always rides with the data)

# jshell (even with --feedback silent) echoes each submitted statement as "-> <source>", AND it flushes
# System.out per print() call -- so (a) a marker literal in the source would also appear in the echo,
# and (b) a bare leading marker arrives in its own pipe chunk before the data. We defeat both: markers
# are emitted split across two string literals ("CS25" + "RDYOK") so the contiguous token exists only in
# OUTPUT, and every payload is built into ONE string printed by a SINGLE println bracketed by BEG/END,
# so matching END guarantees the numbers are already buffered. .cs25_emit() returns the Java expression.
.cs25_emit <- function(tok) {
  k <- max(1L, floor(nchar(tok) / 2))
  sprintf('"%s" + "%s"', substr(tok, 1, k), substr(tok, k + 1, nchar(tok)))
}

# Accumulate stdout (discarding stderr so its pipe can't fill and block jshell) until `marker` appears
# in the buffer, then return the WHOLE accumulated buffer (so the caller can extract a payload that may
# have been split across reads) and reset it. Errors if the process dies or we time out.
# Per-call timeout: a legitimate compound-load or gamma evaluation completes in a few seconds, so a long
# wait means JCOSMO could not resolve a compound (e.g. an abbreviation/SMILES it does not know) and is
# hung. 20 s fails those fast instead of burning the old 90 s per bad compound. (cs25_open overrides this.)
.cs25_wait <- function(sess, marker, timeout = 20) {
  p <- sess$proc; t0 <- Sys.time()
  repeat {
    p$poll_io(400)
    try(p$read_error(), silent = TRUE)                              # drain & drop netlib/log4j noise
    out <- p$read_output()
    if (nzchar(out)) sess$buf <- c(sess$buf, strsplit(out, "\n", fixed = TRUE)[[1]])
    if (any(grepl(marker, sess$buf, fixed = TRUE))) { res <- sess$buf; sess$buf <- character(0); return(res) }
    if (!p$is_alive()) stop("CS25 jshell process exited unexpectedly.", call. = FALSE)
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout)
      stop(sprintf("CS25 jshell timed out waiting for '%s'.", marker), call. = FALSE)
  }
}

.cs25_send <- function(sess, line) sess$proc$write_input(paste0(line, "\n"))

# Open a CS25 session. Returns an environment; close it with cs25_close(). model defaults to "CS25"
# (pass "CS25-SVP" etc. to use another JCOSMO model name).
cs25_open <- function(model = "CS25", timeout = 120) {
  dir <- cs25_jcosmo_dir()
  if (is.na(dir)) stop("No JCOSMO install found (set options(supercrit.jcosmo_dir=).", call. = FALSE)
  # --feedback silent: no prompts/value echo. NO --execution local (that breaks --class-path -> the
  # jcosmo classes won't load). Run from `dir` so pars/ and profiles/ resolve relatively.
  p <- processx::process$new(
    "jshell",
    c("--feedback", "silent", "--no-startup", "--class-path", "lib/jcosmo.jar"),
    wd = dir, stdin = "|", stdout = "|", stderr = "|")
  sess <- new.env(parent = emptyenv())
  sess$proc <- p; sess$buf <- character(0); sess$comps <- NULL; sess$model <- model
  # Build the model once, then signal readiness (split-marker so the echo can't false-trigger).
  .cs25_send(sess, 'var __jc = new lvpp.JCOSMOPy();')
  .cs25_send(sess, sprintf('var __m = __jc.newModel("%s");', model))
  .cs25_send(sess, sprintf('System.out.println(%s);', .cs25_emit(.CS25_MARK_READY)))
  tryCatch(.cs25_wait(sess, .CS25_MARK_READY, timeout = timeout),
           error = function(e) { cs25_close(sess); stop(e) })
  reg.finalizer(sess, function(s) try(cs25_close(s), silent = TRUE), onexit = TRUE)
  sess
}

cs25_close <- function(sess) {
  if (is.null(sess) || is.null(sess$proc)) return(invisible())
  try(.cs25_send(sess, "/exit"), silent = TRUE)
  try(sess$proc$kill(), silent = TRUE)
  invisible()
}

# Load a component set into the session (JCOSMO resolves names from its own DB; names are upper-cased
# and spaces -> underscores to match JCOSMO conventions). Cached: a no-op if already loaded.
.cs25_jname <- function(x) toupper(gsub("[[:space:]]+", "_", trimws(x)))

cs25_set_compounds <- function(sess, comps) {
  jn <- .cs25_jname(comps)
  if (!is.null(sess$comps) && identical(sess$comps, jn)) return(invisible())
  arr <- paste0('"', jn, '"', collapse = ",")
  .cs25_send(sess, sprintf('__m.setCompounds(new String[]{%s});', arr))
  .cs25_send(sess, sprintf('System.out.println(%s);', .cs25_emit(.CS25_MARK_READY)))
  .cs25_wait(sess, .CS25_MARK_READY)
  sess$comps <- jn
  invisible()
}

# ln(gamma) for composition x (mole fractions, auto-normalised) at temperature T (K). Returns a numeric
# vector aligned with `comps`; NA for any component JCOSMO could not evaluate.
cs25_lng <- function(sess, comps, x, T = 298.15) {
  stopifnot(length(comps) == length(x))
  cs25_set_compounds(sess, comps)
  x <- as.numeric(x); x <- x / sum(x)
  xs <- paste0(vapply(x, function(v) formatC(v, format = "e", digits = 12), character(1)), collapse = ",")
  .cs25_send(sess, sprintf('__m.setTemperature(%s);', formatC(T[1], format = "f", digits = 6)))
  .cs25_send(sess, sprintf('__m.setComposition(new double[]{%s});', xs))
  # Build the whole payload (BEG + comma list + END) into one String and print with ONE println, so
  # it is flushed/read as a unit and matching END implies the numbers are present.
  .cs25_send(sess, sprintf(
    '{ double[] __g = __m.activityCoefficientLn(); StringBuilder __b = new StringBuilder(%s); for (double __v : __g) __b.append(__v).append(","); __b.append(%s); System.out.println(__b.toString()); }',
    .cs25_emit(.CS25_MARK_BEG), .cs25_emit(.CS25_MARK_END)))
  joined <- paste(.cs25_wait(sess, .CS25_MARK_END), collapse = "")
  inner <- sub(paste0(".*", .CS25_MARK_BEG), "", joined)            # drop echo + everything up to BEG
  inner <- sub(paste0(.CS25_MARK_END, ".*"), "", inner)            # drop END + trailing CR/echo
  as.numeric(strsplit(inner, ",", fixed = TRUE)[[1]])
}

# ---- LLE flash on top of the gateway ----------------------------------------------------------------
# Two-liquid isoactivity flash by successive substitution with a two-trial-phase seed and a
# trivial-solution guard. Matches csac_lle_flash()'s return shape so it drops into the LLE module.

# Rachford-Rice: solve sum_i z_i (K_i-1)/(1+beta(K_i-1)) = 0 for beta in the feasible window. K_i = x_i^II/x_i^I.
.cs25_rr_beta <- function(z, K) {
  if (all(abs(K - 1) < 1e-10)) return(NA_real_)
  lo <- 1 / (1 - max(K)) + 1e-9; hi <- 1 / (1 - min(K)) - 1e-9   # poles of the RR function
  lo <- max(lo, 0); hi <- min(hi, 1)
  if (!(hi > lo)) return(NA_real_)
  f <- function(b) sum(z * (K - 1) / (1 + b * (K - 1)))
  flo <- f(lo); fhi <- f(hi)
  if (is.na(flo) || is.na(fhi) || flo * fhi > 0) return(NA_real_)
  stats::uniroot(f, lower = lo, upper = hi, tol = 1e-12)$root
}

#' CS25 two-phase (liquid-liquid) flash via the JCOSMO gateway.
#' components: character names; z: feed mole fractions (default equimolar); T in K.
#' Returns list(split, beta, phase=data.frame(component,z,x1,x2,gamma1,gamma2,D)).
cs25_lle_flash <- function(components, z = NULL, T = 298.15, sess = NULL,
                           max_iter = 400, tol = 1e-9) {
  own <- is.null(sess); if (own) { sess <- cs25_open(); on.exit(cs25_close(sess), add = TRUE) }
  n <- length(components)
  if (is.null(z)) z <- rep(1 / n, n)
  z <- as.numeric(z); z <- z / sum(z); Tk <- T[1]

  lng <- function(x) cs25_lng(sess, components, x, Tk)

  # Seed: a trial phase I rich in component 1 (feed/diluent) and phase II rich in component 2 (solvent).
  # If there is no distinct 2nd component, perturb around the feed.
  e <- 1e-3
  xI0 <- z; xII0 <- z
  if (n >= 2) {
    xI0  <- z + e; xI0[1]  <- xI0[1]  + 1;      xI0  <- xI0  / sum(xI0)
    xII0 <- z + e; xII0[2] <- xII0[2] + 1;      xII0 <- xII0 / sum(xII0)
  }
  gI <- lng(xI0); gII <- lng(xII0)
  if (anyNA(gI) || anyNA(gII)) return(list(split = NA, beta = NA_real_,
      phase = data.frame(component = components, z = z, x1 = NA_real_, x2 = NA_real_,
                         gamma1 = NA_real_, gamma2 = NA_real_, D = NA_real_, stringsAsFactors = FALSE)))
  K <- exp(gI - gII)                                    # K_i = gamma_i^I / gamma_i^II

  beta <- 0.5; x1 <- z; x2 <- z; split <- FALSE
  for (it in seq_len(max_iter)) {
    b <- .cs25_rr_beta(z, K)
    if (is.na(b)) break
    x1 <- z / (1 + b * (K - 1)); x1 <- x1 / sum(x1)      # phase I
    x2 <- K * z / (1 + b * (K - 1)); x2 <- x2 / sum(x2)  # phase II
    g1 <- lng(x1); g2 <- lng(x2)
    if (anyNA(g1) || anyNA(g2)) break
    Knew <- exp(g1 - g2)
    if (max(abs(log(Knew) - log(K))) < tol) { K <- Knew; beta <- b; split <- TRUE; break }
    K <- Knew; beta <- b
  }
  # Trivial solution (both phases identical) => single phase, no split.
  if (!split || max(abs(x1 - x2)) < 1e-4 || is.na(beta) || beta <= 1e-6 || beta >= 1 - 1e-6) {
    g <- lng(z)
    return(list(split = FALSE, beta = NA_real_,
                phase = data.frame(component = components, z = z, x1 = z, x2 = NA_real_,
                                   gamma1 = exp(g), gamma2 = NA_real_, D = NA_real_, stringsAsFactors = FALSE)))
  }
  g1 <- lng(x1); g2 <- lng(x2)
  list(split = TRUE, beta = beta,
       phase = data.frame(component = components, z = z, x1 = x1, x2 = x2,
                          gamma1 = exp(g1), gamma2 = exp(g2), D = x1 / x2, stringsAsFactors = FALSE))
}

#' CS25 analogue of csac_lle_extract(): screen candidate solvents (x temperature) for extracting the
#' solutes out of `feed`. Same column schema so the LLE flash UI consumes it unchanged.
cs25_lle_extract <- function(solutes, solvents, feed = "WATER", T = 298.15,
                             solute_frac = 0.01, solvent_ratio = 1, sess = NULL,
                             progress = NULL) {
  solutes <- solutes[nzchar(solutes)]; solvents <- solvents[nzchar(solvents)]
  if (!length(solutes)) stop("Provide at least one solute.", call. = FALSE)
  if (!length(solvents)) stop("Provide at least one candidate solvent.", call. = FALSE)
  own <- is.null(sess); if (own) { sess <- cs25_open(); on.exit(cs25_close(sess), add = TRUE) }
  ns <- length(solutes)
  grid <- expand.grid(solvent = solvents, T_K = as.numeric(T), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(grid)), function(g) {
    if (is.function(progress)) progress(g / nrow(grid))
    sv <- grid$solvent[g]; Tk <- grid$T_K[g]
    comps <- c(feed, sv, solutes)
    z <- c(1, solvent_ratio, rep(solute_frac, ns)); z <- z / sum(z)
    fl <- tryCatch(cs25_lle_flash(comps, z = z, T = Tk, sess = sess), error = function(e) NULL)
    base <- data.frame(solvent = sv, T_K = Tk, stringsAsFactors = FALSE)
    Dcols <- stats::setNames(as.list(rep(NA_real_, ns)), paste0("D_", solutes))
    if (is.null(fl) || !isTRUE(fl$split)) {
      return(cbind(base, split = FALSE, as.data.frame(Dcols), selectivity = NA_real_,
                   solvent_in_raffinate = NA_real_, feed_in_extract = NA_real_, beta = NA_real_))
    }
    ph <- fl$phase; svi <- 2L
    if (ph$x1[svi] >= ph$x2[svi]) { xe <- ph$x1; xr <- ph$x2 } else { xe <- ph$x2; xr <- ph$x1 }
    D <- xe[2 + seq_len(ns)] / xr[2 + seq_len(ns)]
    for (k in seq_len(ns)) Dcols[[k]] <- D[k]
    cbind(base, split = TRUE, as.data.frame(Dcols),
          selectivity = if (ns >= 2 && isTRUE(is.finite(D[2])) && D[2] > 0) D[1] / D[2] else NA_real_,
          solvent_in_raffinate = xr[svi], feed_in_extract = xe[1], beta = fl$beta)
  })
  res <- do.call(rbind, rows)
  dcn <- grep("^D_", names(res), value = TRUE)
  key <- if (ns >= 2) res$selectivity else res[[dcn[1]]]
  res[order(-ifelse(is.na(key), -Inf, key)), , drop = FALSE]
}
