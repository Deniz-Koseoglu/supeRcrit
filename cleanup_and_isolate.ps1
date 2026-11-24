# ============================================================================
# cleanup_and_isolate.ps1  -  supeRcrit deployment cleanup (Windows PowerShell)
#
# Supersedes data-raw/CLEANUP_SCRATCH.sh. Reclaims disk space + makes the
# package build lean, by (a) DELETING unambiguous junk and (b) ISOLATING
# valuable-but-not-needed-to-run files into  Desktop\supeRcrit_archive\ .
#
# NOTHING HAPPENS BY DEFAULT - this is a DRY RUN that only prints what it would
# do. Review the output, then run for real:
#
#     powershell -ExecutionPolicy Bypass -File .\cleanup_and_isolate.ps1 -Go
#
# The ~13 GB ORCA sigma-profile intermediates are GATED behind a second switch
# (the .orcacosmo profiles themselves are KEPT):
#
#     ... -Go -IncludeOrca        # ONLY after the SFE-giants profiling is done!
#
# Safe to re-run; already-moved/deleted items are simply skipped. ASCII-only so
# Windows PowerShell 5.1 parses it regardless of file encoding.
#
# Approx sizes (from the audit):  DELETE ~1.2 GB package + 0.3 GB HELPER;
#   ISOLATE ~1.4 GB (data-raw 85 MB, scratch_tq 1.3 GB, C: cosmo-src ~0.1 GB);
#   with -IncludeOrca a further ~12 GB of ORCA intermediates are deleted.
# ============================================================================
param([switch]$Go, [switch]$IncludeOrca)
$ErrorActionPreference = 'SilentlyContinue'
$dry = -not $Go

$REPO   = "E:\Downloads\Potential Industries\1) Altraflora Documents\Internal Company Docs\Software\My R Packages\supeRcrit"
$HELPER = "E:\Downloads\Potential Industries\1) Altraflora Documents\Internal Company Docs\Software\My R Packages\HELPER FILES\COSMO"
$UHOME  = "C:\Users\Denizcan"
$ARCH   = Join-Path ([Environment]::GetFolderPath('Desktop')) "supeRcrit_archive"

function Targets($pat) {   # a literal path -> the item ITSELF (so whole dirs move); a wildcard -> the matches
  if ($pat -match '[*?]') { Get-ChildItem -Path $pat -Force -ErrorAction SilentlyContinue }
  else { Get-Item -LiteralPath $pat -Force -ErrorAction SilentlyContinue }
}
function Del($pat) {
  foreach ($i in (Targets $pat)) {
    if ($dry) { Write-Host ("  [DRY] DELETE   " + $i.FullName) }
    else { Remove-Item -LiteralPath $i.FullName -Recurse -Force; Write-Host ("  DELETED  " + $i.FullName) } }
}
function Iso($pat, $sub) {
  $dest = Join-Path $ARCH $sub
  foreach ($i in (Targets $pat)) {
    if ($dry) { Write-Host ("  [DRY] MOVE     " + $i.FullName + "   ->   " + $sub + "\"); continue }
    New-Item -ItemType Directory -Force -Path $dest | Out-Null
    if ($i.PSIsContainer) {   # robocopy /MOVE is robust for OneDrive / cross-drive / many-file dir moves
      $d2 = Join-Path $dest $i.Name
      robocopy "$($i.FullName)" "$d2" /E /MOVE /NFL /NDL /NJH /NJS /R:1 /W:1 | Out-Null
      $ok = ($LASTEXITCODE -lt 8)
      Write-Host ("  " + $(if ($ok) { "MOVED   " } else { "FAILED(rc=$LASTEXITCODE) " }) + $i.FullName + "   ->   " + $sub + "\")
    } else {
      Move-Item -LiteralPath $i.FullName -Destination $dest -Force
      Write-Host ("  MOVED    " + $i.FullName + "   ->   " + $sub + "\")
    }
  }
}

Write-Host ("=" * 78)
Write-Host ("  supeRcrit cleanup  -  " + $(if ($dry) { "DRY RUN (nothing changed; add -Go to apply)" } else { "APPLYING CHANGES" }))
Write-Host ("  archive target: " + $ARCH)
Write-Host ("=" * 78)

Write-Host "`n[DELETE] package junk ------------------------------------------------------"
Del "$REPO\R\sysdata.rda.bak*"      # ~800 MB of stale sysdata backups (the real sysdata.rda is KEPT)
Del "$REPO\catboost_info"           # ML training logs (the trained model is bundled in inst/extdata)
Del "$REPO\..Rcheck"                # R CMD check output
Del "$REPO\.agents"                 # empty
Del "$REPO\the"                     # 0-byte stray
Del "$REPO\bash.exe.stackdump"      # crash dump
Del "$REPO\.Rhistory"               # R console history

Write-Host "`n[DELETE] HELPER FILES\COSMO scratch ----------------------------------------"
Del "$HELPER\cod_work"              # 342 MB COD density-pipeline scratch
Del "$HELPER\cosmo_out"             # regenerable .cosmo/.gout exports
Del "$HELPER\*.bak"                 # comps.xml.bak, CSPhi_subgroups.csv.bak
Del "$HELPER\gout_names.txt"
Del "$HELPER\tier1_names.txt"
Del "$HELPER\CSP_2472_vs_COD.csv"

Write-Host "`n[ISOLATE] package provenance / scratch   ->   Desktop\supeRcrit_archive -----"
# data-raw = the data-GENERATION scripts + provenance. VERIFIED: nothing at runtime/build reads it
# (the app loads everything from inst/extdata/*.rds + R/sysdata.rda). Kept for reproducibility/re-fitting.
Iso "$REPO\data-raw"          "data-raw"      # 85 MB  (NOTE: moving also removes it from the git repo - see notes)
Iso "$REPO\scratch_tq"        "scratch"       # 1.3 GB COSMO dev scratch
Iso "$REPO\scratch_markgraf"  "scratch"       # reference-paper text extracts

Write-Host "`n[ISOLATE] C:\ COSMO dev dirs + loose scripts   ->   archive -----------------"
Iso "$UHOME\openCOSMO-RS-Phi_cpp"              "cosmo-src"   # the openCOSMO-RS-Phi engine source (git repo)
Iso "$UHOME\openCOSMO-RS_conformer_pipeline"   "cosmo-src"
Iso "$UHOME\openCOSMO-RS_py"                    "cosmo-src"
Iso "$UHOME\csp_setup"                          "cosmo-src"
Iso "$UHOME\ocrsphi_port"                       "cosmo-src"
Iso "$UHOME\orca_mpi_test"                      "cosmo-src"
# the C:-home loose dev scripts from the CSP / exp-DB sessions (moved, not deleted, so you can review):
foreach ($n in @('audit_chrastil_sfe','build_master_fitlist','bundle_struct','check_benzoic_psat','check_install',
  'check_mw','check_salicylate','debug_benzoic','finalize_43','fix_salicylate_name','fix_two','hc_postfix',
  'inspect_benzoic2','inspect_other','list_unresolved','map_fullnames','probe_bundle','probe_bundle_crash',
  'probe_export','probe_lnphi','profiled_sfe','quantify_industrial','read_paper','refine_sfe','resolve_pubchem',
  'scope_analysis','test_4param','test_bundle_predict','test_predictive','test_sfe_ocrsphi','time_fit',
  'update_resolved_csv','verify_install','xlsx_lookup','csp_true','test_elig2','csp_elig_diag','test_dbfilter',
  'test_dbfilter2')) {
  Iso "$UHOME\$n.R" "c-home-scripts"; Iso "$UHOME\${n}_out.txt" "c-home-scripts"
}
Iso "$UHOME\*_out.txt"             "c-home-scripts"   # one-off stdout captures
Iso "$UHOME\main_*.log"            "c-home-scripts"
Iso "$UHOME\ocrsphi_lib4_build*.log" "c-home-scripts"

if ($IncludeOrca) {
  Write-Host "`n[GATED] ORCA sigma-profile intermediates (csp_profiles_orca, ~13 GB) --------"
  Write-Host "  !! Confirm the SFE-giants profiling backlog is FINISHED first - these regenerate"
  Write-Host "     per run; the valuable .orcacosmo profiles + .dat are KEPT.`n"
  $orca = "$UHOME\csp_profiles_orca"
  foreach ($e in @('*.tmp','*.gbw','*.densities','*.ges','*.cpcm','*.bas1','*.bas2','*.bas3','calculation_files.zip')) {
    $f = Get-ChildItem -Path $orca -Recurse -Filter $e -File -Force -ErrorAction SilentlyContinue
    if ($f) { $mb = ($f | Measure-Object -Sum Length).Sum / 1MB
      if ($dry) { Write-Host ("  [DRY] DELETE  {0,6} files  {1,10:N0} MB   {2}" -f $f.Count, $mb, $e) }
      else { $f | Remove-Item -Force; Write-Host ("  DELETED {0,6} files  {1,10:N0} MB   {2}" -f $f.Count, $mb, $e) } }
  }
} else {
  Write-Host "`n[SKIPPED] ORCA intermediates (~12 GB) - re-run with -IncludeOrca when the giants are done."
  Write-Host "  (keeps: $UHOME\csp_profiles_orca\*.orcacosmo = the 133 MB of actual sigma profiles)"
}

Write-Host "`n[KEEP - untouched] ---------------------------------------------------------"
Write-Host "  R\*.R + R\sysdata.rda, src\, inst\, man\, data\, DESCRIPTION/NAMESPACE, README, DEPLOYMENT.md"
Write-Host "  HELPER..\COSMO\exports (CSP TSVs), fusion_db\, jcosmo3\, export tooling (.bat/.ps1/.md)"
Write-Host "  HELPER..\COSMO\DBs (CHAOS_v2.zip 4.1 GB, LVPP, VT, Yaws, ...) - source DBs, LEFT in place on E:"
Write-Host "  csp_profiles_orca\*.orcacosmo - the extracted sigma profiles"

Write-Host ("`n" + "-" * 78)
Write-Host ("  " + $(if ($dry) { "DRY RUN - review the list above, then re-run with -Go to apply." } else { "DONE." }))
Write-Host ("-" * 78)
