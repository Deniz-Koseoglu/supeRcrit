# Deploying the supeRcrit Shiny app

The app can be (a) installed from GitHub and run locally with `supeRcrit::run_app()`, or
(b) deployed to **shinyapps.io**. Both run the full feature set. The only platform-specific
hurdle is the **Java toolchain** (`rJava` → `rcdk`, `depict`), which a few features need.

---

## 1. What needs Java

The analysis pipeline — EOS phase/column/sizing/**optimization**, cost (COM), DOE,
kinetics — is pure Rcpp/R and needs **no Java**. Java (via `rJava`/`rcdk`/`depict`) is used
only for **live molecule work**:

- decomposing a *new* SMILES string into GC-EOS groups (EOS Components module);
- molecular depiction / highlighted-substructure images (HSP / solute characterization).

Pre-built components (`eos_seed_components`, saved components) and manual group entry do
**not** need Java, so the whole analysis app is usable even where Java is unavailable.

---

## 2. Local install from GitHub (full functionality)

Requires a working JDK + `rJava` on the machine.

```r
# 1. JDK must be installed and configured for R:  R CMD javareconf   (run in a shell)
# 2. install the GitHub dependency, then the package (its Remotes: handles depict)
remotes::install_github("CDK-R/depict")
remotes::install_github("<your-org>/supeRcrit")

# 3. launch
library(supeRcrit)
run_app("com_analysis")   # or any module; the app is one dashboard
```

If `rJava` fails to load, install/repair a JDK and run `R CMD javareconf`, then reinstall
`rJava` from source.

---

## 3. Deploy to shinyapps.io (full functionality)

shinyapps.io servers **do** have a JRE, but compiling `rJava` from source on the build
host often fails (`/usr/bin/ld: cannot find -lpcre2-8`). The fix is to use a prebuilt
binary via **Package Cache**.

### One-time setup

1. **Install the GitHub packages locally first** so `rsconnect` captures their sources:
   ```r
   remotes::install_github("CDK-R/depict")
   remotes::install_github("<your-org>/supeRcrit")
   ```
2. In your shinyapps.io account → **Settings → enable "Package Cache"**. This uses the
   prebuilt `rJava` binary instead of source compilation.
3. Authenticate `rsconnect` (`rsconnect::setAccountInfo(...)`) once.

### Deploy

```r
rsconnect::deployApp(
  appDir   = system.file("shiny-app", package = "supeRcrit"),  # or "inst/shiny-app" from source
  appName  = "supercrit"
)
```

`rsconnect` bundles supeRcrit + depict (from GitHub, via the recorded remotes) and the CRAN
dependencies. `app.R` does `library(supeRcrit)`, so it is recorded as a dependency and its
bare `gceos_*` calls resolve on the server.

### Instance size

The optimization spawns a **second R process** (`callr`) that also loads the Java
namespace. Use a **≥ 2 GB instance**; 1 GB can OOM when both sessions hold the namespace.

### If `rJava` still won't build

Fall back to a small **CDK/RDKit microservice** on a host you control (Docker), exposing
`/decompose` (SMILES → groups) and `/depict` (SMILES + SMARTS → PNG). The Shiny app calls
it over HTTPS — identical UX, all features, Java off the shiny host. (Not yet implemented;
ask if you want it scaffolded.)

---

## 4. Settings & saved items: how persistence works

| Action | Mechanism | Works on shinyapps.io? | Durable? |
|---|---|---|---|
| **Export settings to `.json`** | browser **download** (`downloadHandler`) | ✅ yes | ✅ user keeps the file |
| **Import settings from `.json`** | browser **upload** (`fileInput`) | ✅ yes | ✅ from the user's file |
| Save component / GCM calc / DOE analysis "to disk" | writes to `supercrit_user_dir()` (`tools::R_user_dir`) | ✅ writes succeed | ⚠️ per-session only |

All server-side writes now go to a **writable per-user directory** (`supercrit_user_dir()`),
not the read-only install dir. On a desktop install this persists across sessions. On
**shinyapps.io the container filesystem is ephemeral**, so server-side saved items live only
for the running instance's lifetime.

➡ **For users to keep settings across sessions on shinyapps.io, use Export → `.json`
(download) and Import (upload).** That round-trip is fully supported and machine-local.

For automatic durable cloud persistence (no manual download), an external store would be
needed (e.g. `pins` → S3 / Google Drive, or a database) — a separate enhancement.

---

## 5. Pre-deploy checklist

- [ ] JDK present locally; `remotes::install_github("CDK-R/depict")` and supeRcrit succeed.
- [ ] `library(supeRcrit); run_app()` works locally (smoke test).
- [ ] shinyapps.io **Package Cache enabled**.
- [ ] Instance memory **≥ 2 GB**.
- [ ] After deploy: load a prebuilt component, run a column solve + optimization, and
      test Export/Import settings `.json`. If molecule-from-SMILES decomposition errors,
      `rJava` didn't build — enable Package Cache or use the microservice fallback (§3).
