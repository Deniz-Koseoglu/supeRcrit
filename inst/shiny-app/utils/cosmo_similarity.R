# cosmo_similarity.R — COSMO scCO2 solubility engine (CSP-2021 / openCOSMO-RS-Phi) database-membership
# checks and structural-similarity ("nearest available compound") suggestions for Solute Characterization.
#
# Membership is a fast name/SMILES/CAS lookup. Similarity uses rcdk 'extended' fingerprints + Tanimoto over
# a per-engine index whose SMILES are parsed & fingerprinted ONCE and cached process-wide (survives across
# Shiny sessions in the same R process), so a query is just one parse+fingerprint plus cheap bit-ops.
#
# CSP-2021 carries no SMILES of its own (name-indexed), so its similarity index resolves each CSP name to a
# SMILES via a bundled map (openCOSMO-RS-Phi metadata UNION the experimental-property DB) — ~80% coverage;
# the unresolved rest simply never appear as suggestions.

# Process-wide fingerprint-index cache.
.cosmo_fp_cache <- new.env(parent = emptyenv())

.cosmo_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Is the loaded solute present in a given COSMO engine's database?
#   engine = "csp"     -> CSP-2021, resolved by NAME and required to actually carry CSP-2021 parameters
#                         (db_engine_eligible is has_csp21-aware after the exp_db_helpers.R fix).
#   engine = "ocrsphi" -> openCOSMO-RS-Phi, matched by name OR SMILES OR CAS against its metadata.
cosmo_in_db <- function(name = NA, cas = NA, smiles = NA, engine = c("csp", "ocrsphi")) {
  engine <- match.arg(engine)
  # NULL/vector-safe scalar coercion: NULL or NA -> "", otherwise the first element trimmed.
  s1 <- function(x) { x <- x[1]; if (is.null(x) || is.na(x)) "" else trimws(as.character(x)) }
  nm <- s1(name); cs <- s1(cas); sm <- s1(smiles)
  if (engine == "csp") {
    if (!nzchar(nm)) return(FALSE)                        # CSP is name-indexed only
    isTRUE(tryCatch(db_engine_eligible(nm, "csp"), error = function(e) FALSE))
  } else {
    # Fitted-only: a "pending-fit" bundle entry has a profile but placeholder params, so it is not usable.
    m <- tryCatch(ocrsphi_fitted_meta(), error = function(e) NULL); if (is.null(m)) return(FALSE)
    hit <- FALSE
    if (nzchar(nm)) hit <- hit || (.cosmo_norm(nm) %in% .cosmo_norm(m$compound))
    if (nzchar(sm)) hit <- hit || (tolower(sm) %in% tolower(m$smiles))
    # CAS cells can be pipe-delimited lists (e.g. "80-56-8|7785-26-4"); match any component, like exp_db_match.
    if (nzchar(cs)) hit <- hit || any(!is.na(m$cas) & grepl(paste0("(^|\\|)", cs, "(\\||$)"), as.character(m$cas), fixed = FALSE))
    isTRUE(hit)
  }
}

# Bundled offline name -> SMILES map for CSP compounds: openCOSMO-RS-Phi meta UNION the experimental-property
# DB, keyed by normalised name (ocrsphi wins on collision).
.cosmo_csp_name_smiles <- function() {
  bmap <- function(nm, smi) {
    ok <- !is.na(smi) & nzchar(smi) & !is.na(nm) & nzchar(nm)
    if (!any(ok)) return(character(0))
    tapply(as.character(smi[ok]), .cosmo_norm(nm[ok]), function(v) v[[1]])
  }
  m   <- tryCatch(ocrsphi_data()$meta, error = function(e) NULL)
  edb <- tryCatch(exp_prop_db(),       error = function(e) NULL)
  a <- if (!is.null(m))   bmap(m$compound, m$smiles) else character(0)
  b <- if (!is.null(edb)) bmap(edb$name,   edb$smiles) else character(0)
  c(a, b[setdiff(names(b), names(a))])
}

# Per-engine structural index: data.frame(name, cas, smiles, fp), SMILES parsed+fingerprinted ONCE & cached.
cosmo_build_fingerprints <- function(engine = c("csp", "ocrsphi"), fp_type = "extended") {
  engine <- match.arg(engine)
  ck <- paste0(engine, ":", fp_type)
  if (!is.null(.cosmo_fp_cache[[ck]])) return(.cosmo_fp_cache[[ck]])
  if (engine == "ocrsphi") {
    m <- ocrsphi_fitted_meta()   # only suggest FITTED compounds as usable ocrsphi neighbours
    df <- data.frame(name = as.character(m$compound), cas = as.character(m$cas),
                     smiles = as.character(m$smiles), stringsAsFactors = FALSE)
  } else {
    cc <- csp_compounds()
    cc <- cc[!is.na(cc$has_csp21) & cc$has_csp21, , drop = FALSE]   # only CSP-2021-parameterised compounds
    nm2s <- .cosmo_csp_name_smiles()
    smi <- vapply(seq_len(nrow(cc)), function(i) {
      q <- .cosmo_norm(c(cc$jcosmo[i], cc$s2[i])); h <- nm2s[q]; h <- h[!is.na(h)]
      if (length(h)) as.character(h[[1]]) else NA_character_
    }, character(1))
    df <- data.frame(name = as.character(cc$s2), cas = NA_character_, smiles = smi, stringsAsFactors = FALSE)
  }
  df <- df[!is.na(df$smiles) & nzchar(df$smiles), , drop = FALSE]
  mols <- suppressWarnings(rcdk::parse.smiles(df$smiles))
  keep <- !vapply(mols, is.null, logical(1))
  df <- df[keep, , drop = FALSE]
  df$fp <- lapply(mols[keep], function(mm) tryCatch(rcdk::get.fingerprint(mm, type = fp_type), error = function(e) NULL))
  df <- df[!vapply(df$fp, is.null, logical(1)), , drop = FALSE]
  .cosmo_fp_cache[[ck]] <- df
  df
}

# Nearest structural neighbours (Tanimoto over 'extended' fingerprints) of a query SMILES that ARE in the
# engine's database. Returns data.frame(name, cas, smiles, tanimoto) sorted desc, filtered >= threshold, top n.
# Empty when the query SMILES is missing/unparseable or nothing clears the threshold.
cosmo_find_similar <- function(query_smiles, engine = c("csp", "ocrsphi"),
                               n = 5L, threshold = 0.5, fp_type = "extended") {
  engine <- match.arg(engine)
  empty <- data.frame(name = character(0), cas = character(0), smiles = character(0), tanimoto = numeric(0),
                      stringsAsFactors = FALSE)
  if (is.null(query_smiles) || is.na(query_smiles) || !nzchar(as.character(query_smiles))) return(empty)
  qm <- tryCatch(suppressWarnings(rcdk::parse.smiles(as.character(query_smiles))[[1]]), error = function(e) NULL)
  if (is.null(qm)) return(empty)
  qfp <- tryCatch(rcdk::get.fingerprint(qm, type = fp_type), error = function(e) NULL)
  if (is.null(qfp)) return(empty)
  db <- tryCatch(cosmo_build_fingerprints(engine, fp_type), error = function(e) NULL)
  if (is.null(db) || !nrow(db)) return(empty)
  tani <- vapply(db$fp, function(f) tryCatch(fingerprint::distance(qfp, f, method = "tanimoto"),
                                             error = function(e) NA_real_), numeric(1))
  ord <- order(tani, decreasing = TRUE)
  res <- data.frame(name = db$name[ord], cas = db$cas[ord], smiles = db$smiles[ord],
                    tanimoto = round(tani[ord], 3), stringsAsFactors = FALSE)
  res <- res[is.finite(res$tanimoto) & res$tanimoto >= threshold, , drop = FALSE]
  utils::head(res, n)
}
