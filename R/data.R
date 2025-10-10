#' Example data for estimation of solubility and critical parameters
#'
#' @description A data frame with 33 rows and 7 columns containing example data about chemically diverse compounds to be used for
#' Hansen solubility parameter (HSP), boiling point, and critical parameter estimation (pressure, temperature, volume). Data includes the following columns:
#' \describe{
#'   \item{Name}{Name of the compound.}
#'   \item{CAS}{The CAS number.}
#'   \item{MVol}{The molar volume of the compound under standard temperature and pressure.}
#'   \item{D}{The deltaD (dispersion, van der Waals forces) Hansen solubility parameter.}
#'   \item{P}{The deltaP (dipole moment) Hansen solubility parameter.}
#'   \item{HB}{The deltaH (hydrogen bonding) Hansen solubility parameter.}
#'   \item{SMILES}{The \strong{canonical} SMILES notation.}
#' }
#' @references
#' Hansen Solubility Parameters, available at: \url{https://www.hansen-solubility.com} (accessed 02.06.2024).
#'
#' National Library of Medicine, available at: \url{https://pubchem.ncbi.nlm.nih.gov} (accessed 02.06.2024).
#'
"hspex"

#' Example data with DoE results
#'
#' @description A named nested \code{list} containing Design of Experiments (DOE) data from scientific literature to be used with \pkg{supercRit}.
#' Each element is itself a \code{list} including the following elements:
#' \enumerate{
#' \item \code{$data}: A \code{data.frame} containing the run order, coded and uncoded factor levels, and corresponding response values of the design.
#' \item \code{$opt_pars}: A \code{data.frame} of optimized parameters as stated in the source publication(s).
#' \item \code{$design}: A brief description of the DOE type and factors.
#' \item \code{$desc}: A brief description of the study response variable(s).
#' \item \code{$citation}: A reference to the source publication(s).
#' }
#'
#' Currently, the following designs and results are included encompassing Supercritical Fluid Extraction (SFE) and steam distillation processes:
#' \enumerate{
#'   \item \code{$ccd1}: Central Composite Inscribed Design (CCID) for ergosterol SFE from mushrooms (Almeida et al., 2024).
#'   \item \code{$ccd2}: Central Composite Design (CCD) for SFE of black pepper (Bagheri et al., 2014).
#'   \item \code{$ccd3}: Central Composite Face-centered Design (CCFD) for SFE of spearming flavonoids (Bimakr et al., 2011).
#'   \item \code{$ccd4}: CCD for SFE of lavender essential oil (Danh et al., 2012).
#'   \item \code{$ccd5}: CCFD for steam distillation of \emph{Myrtus communis} essential oil (Kaya et al., 2020).
#'   \item \code{$ccd6}: CCID for elimination of TiN peeling during CVD deposition (Buckner et al., 1997).
#'   \item \code{$bbd1}: Box-Behnken Design (BBD) for SFE of rapeseed oil (Cvjetko et al., 2012).
#'   \item \code{$bbd2}: BBD for steam distillation of \emph{Eucalyptus tereticornis} essential oil (Galadima et al., 2012).
#'   \item \code{$bbd3}: BBD for SFE of lavender flowers (Jerkovic et al., 2017).
#'   \item \code{$bbd4}: BBD for SFE of Dalmatian sage leaves (Jokic et al., 2018).
#'   \item \code{$bbd5}: BBD for SFE of antioxidants from Dalmatian sage (Pavic et al., 2019).
#'   \item \code{$bbd6}: BBD for SFE of wheat germ oil (Satyannarayana et al., 2018).
#'   \item \code{$ffd1}: Full Factorial Design (FFD, 2^5) for determining the effect of machining factors on ceramic strength (NIST/SEMATECH, 2024).
#'   \item \code{$frfd1}: Fractional Factorial Design (FrFD, 2^(5-1)) for determining significant factors affecting the distance (response) a ball
#'   is thrown by a makeshift catapult (NIST/SEMATECH, 2024).
#' }
#'
#' @references
#' Almeida, C.F., Manrique, Y.A., Lopes, J.C.B., Martins, F.G., Dias, M.M. (2024), 'Recovery of ergosterol from Agaricus bisporus mushrooms
#' via supercritical fluid extraction: A response surface methodology optimisation', \emph{HELIYON} \strong{10} (2), article e21943,
#' DOI: \url{https://doi.org/10.1016/j.heliyon.2023.e21943}.
#'
#' Bagheri, H., Manap, M.Y.B.A., Solati, Z. (2014), 'Response surface methodology applied to supercritical
#' carbon dioxide extraction of \emph{Piper nigrum} L. essential oil', \emph{LWT - Food Science and Technology} \strong{57},
#' pp. 149-155, DOI: \url{http://dx.doi.org/10.1016/j.lwt.2014.01.015}.
#'
#' Bimakr, M., Rahman, R.A., Ganjloo, A., Taip, F.S., Salleh, L.M., Sarker, M.Z.I. (2012),
#' 'Optimization of Supercritical Carbon Dioxide Extraction of Bioactive Flavonoid Compounds from Spearmint
#' (\emph{Mentha spicata} L.) Leaves by Using Response Surface Methodology', \emph{Food Bioprocessing Technology} \strong{5},
#' pp. 912-920, DOI: \url{https://www.doi.org/10.1007/s11947-010-0504-4}.
#'
#' Buckner, J., Cammenga, D.J., Weber, A. (1997), 'Elimination of TiN Peeling During Exposure to CVD Tungsten Deposition Process Using Designed Experiments',
#' In: \emph{Statistical Case Studies for Industrial Process Development} (Czitrom, V., Spagon, P.D., 1997, eds.), pp. 199-211.
#'
#' Cvjetko, M., Jokic, S., Lepojevic, Z., Vidovic, S., Maric, B., Redovnikovic, I.R. (2012),
#' 'Optimization of the Supercritical CO2 Extraction of Oil from Rapeseed Using Response Surface Methodology',
#' \emph{Food Technology and Biotechnology} \strong{50} (2), pp. 208-215.
#'
#' Danh, L.T., Triet, N.D.A., Han, L.T.N., Zhao, J., Mammucari, R., Foster, N. (2012),
#' 'Antioxidant activity, yield and chemical composition of lavender essential oil extracted by supercritical CO2',
#' \emph{Journal of Supercritical Fluids} \strong{70}, pp. 27-34, DOI: \url{https://dx.doi.org/10.1016/j.supflu.2012.06.008}.
#'
#' Galadima, M.S., Ahmed, A.S., Olawale, A.S., Bugaje, I.M. (2012),
#' 'Optimization of Steam Distillation of Essential Oil of \emph{Eucalyptus tereticornis} by Response Surface Methodology',
#' \emph{Nigerian Journal of Basic and Applied Science} \strong{20} (4), pp. 368-372.
#'
#' Jerkovic, I., Molnar, M., Vidovic, S., Vladic, J., Jokic, S. (2017), 'Supercritical CO2 Extraction of \emph{Lavandula angustifolia} Mill. Flowers:
#' Optimisation of Oxygenated Monoterpenes, Coumarin and Herniarin Content', \emph{Phytochemical Analysis} \strong{28}, pp. 558-566, DOI: \url{https://doi.org/10.1002/pca.2705}.
#'
#' Jokic, S., Molnar, M., Jakovlevic, M., Aladic, K., Jerkovic, I. (2018),
#' 'Optimization of supercritical CO2 extraction of \emph{Salvia officinalis} L. leaves targeted on Oxygenated monoterpenes, alpha-humulene, viridiflorol and manool',
#' \emph{Journal of Supercritical Fluids} \strong{133}, pp. 253-262, DOI: \url{https://doi.org/10.1016/j.supflu.2017.10.022}.
#'
#' Kaya, D.A., Ghica, M.V., Danila, E., Ozturk, S., Turkmen, M., Kaya, M.G.A., Dinu-Pirvu, C.-E. (2020),
#' 'Selection of Optimal Operating Conditions for Extraction of \emph{Myrtus communis} L. Essential Oil by the Steam Distillation Method',
#' \emph{Molecules} \strong{25}, article 2399, DOI: \url{http://dx.doi.org/10.3390/molecules25102399}.
#'
#' NIST/SEMATECH e-Handbook of Statistical Methods, available at: \url{http://www.itl.nist.gov/div898/handbook} (accessed 01.06.2024).
#'
#' Pavic, V., Jakovlevic, M., Molnar, M., Jokic, S. (2019),
#' 'Extraction of Carnosic Acid and Carnosol from Sage (\emph{Salvia officinalis} L.) Leaves by Supercritical Fluid
#' Extraction and Their Antioxidant and Antibacterial Activity', \emph{Plants} \strong{8}, article 16, DOI: \url{http://dx.doi.org/10.3390/plants8010016}.
#'
#' Satyannarayana, S., Anjaneyulu, B., Neeharika, T.S.V.R., Rani, K.N.P., Charkabarti, P.P. (2018),
#' 'Process optimization for the supercritical carbon dioxide (SC-CO2) extraction of wheat germ oil with respect to yield,
#' and phosphorous and tocol contents using a Box Behnken design', \emph{Grasas y Aceites} \strong{69} (3), article e259,
#' DOI: \url{https://doi.org/10.3989/gya.0102181}.
#'
"doex"

#' Example Supercritical Fluid Extraction (SFE) kinetic modeling data
#'
#' @description A named nested \code{list} containing extraction curve and Broken-and-Intact Cells (BIC) kinetic model data from scientific literature.
#' Each element is itself a \code{list} including the following elements:
#' \enumerate{
#' \item \code{$data}: A \code{data.frame} containing the OEC data (time, response and, optionally, amount of solvent expended).
#' \item \code{$add_pars}: Additional vector of \code{logical} parameters specifying whether the data is cumulative and whether the flow units are mass or volumetric.
#' \item \code{$units}: A named vector of units for the response (\code{["resp"]}) and flow (\code{["flow"]}).
#' \item \code{$ext_pars}: A vector of extraction parameters such as pressure and temperature.
#' \item \code{$input_pars}: Input model parameters (use \code{\link{show_pars}} for description and units of all parameters).
#' \item \code{$adj_pars}: Adjustable model parameters (use \code{\link{show_pars}} for description and units of all parameters).
#' \item \code{$proc_desc}: A short description of the modeled process.
#' \item \code{$citation}: A reference for the source publication.
#' }
#'
#' Currently, the following Overall Extraction Curves (OEC) and BIC model results are included for SFE of:
#' \enumerate{
#'   \item \code{$dimic}: Cherry seed oil (Dimic et al., 2021).
#'   \item \code{$rizza}: Lipids from microalgae (Rizza, 2014).
#' }
#'
#' @references
#' Dimic et al. (2021), 'Supercritical Fluid Extraction Kinetics of Cherry Seed Oil: Kinetics Modeling and ANN Optimization',
#' \emph{Foods} \strong{10}, article 1513, DOI: \url{https://doi.org/10.3390/foods10071513}.
#'
#' Rizza (2014), Experiments and Modeling of Supercritical CO2 Extraction of Lipids from Microalgae, Universita Degli Studi di Padova,
#' Padua, Italy, MSc thesis.
#'
#' @seealso \code{\link{show_pars}}
#'
"sfex"

#' Example Subcritical Water Extraction (SWE) kinetic modeling data
#'
#' @description A named nested \code{list} containing extraction curve and two-site kinetic desorption model data from scientific literature.
#' Each element is itself a \code{list} including the following elements:
#' \enumerate{
#' \item \code{$data}: A \code{data.frame} containing the OEC data (time and response).
#' \item \code{$resp_unit}: The response value units.
#' \item \code{$ext_pars}: A vector of extraction parameters such as pressure and temperature.
#' \item \code{$input_pars}: Input model parameters (use \code{\link{show_pars}} for description and units of all parameters).
#' \item \code{$output_pars}: Output model parameters (use \code{\link{show_pars}} for description and units of all parameters).
#' \item \code{$proc_desc}: A short description of the modeled process.
#' \item \code{$citation}: A reference for the source publication.
#' }
#'
#' Currently, the following Overall Extraction Curves (OEC) and kinetic desorption model results are included for SWE of:
#' \enumerate{
#'   \item \code{$abidin}: Cherry seed oil (Dimic et al., 2021).
#'   \item \code{$duba1}: Grape skins (Duba, 2015).
#'   \item \code{$duba2}: Grape seeds (Duba, 2015).
#'   \item \code{$jamaludin1}: Alizarin from \emph{Morinda citrifolia} (Jamaludin et al., 2021).
#'   \item \code{$jamaludin2}: Scopoletin from \emph{Morinda citrifolia} (Jamaludin et al., 2021).
#'   \item \code{$kim}: Citrus flavonoids (Kim et al., 2020).
#'   \item \code{$vasquez}: Anthraquinones from \emph{Heterophyllaea pustulata} (Barrera Vasquez et al., 2015).
#' }
#'
#' @references
#' Abidin, Z.Z., Samadi, M., Biak, D.R., Awang Yunus, R. (2024), 'Mathematical Modelling of Subcritical Water Extraction of
#' Essential Oil From Aquilaria Malacenssis Wood', \emph{Journal of Applied Science and Engineering} \strong{27} (12), pp. 3725-3738, DOI:
#' \emph{https://www.doi.org/10.6180/jase.202412_27(12).0012}.
#'
#' Barrera vazquez, M.F., Comini, L.R., Milanesio, J.M., Nunez Montoya, S.C., Cabrera, J.L., Bottini, S., Martini, R.E. (2015),
#' 'Pressurized hot water extraction of anthraquinones from \emph{Heterophyllaea pustulata} Hook f. (Rubiaceae)',
#' \emph{Journal of Supercritical Fluids} \strong{101}, pp. 170-175, DOI: \url{https://dx.doi.org/10.1016/j.supflu.2015.02.029}.
#'
#' Duba (2015), Supercritical Technologies for the Valorization of Wine Industry By Products, University of Trento, PhD thesis.
#'
#' Jamaludin, R., Kim, D.-S., Salleh, L.M., Lim, S.-B. (2021), 'Kinetic Study of SubcriticalWater Extraction of Scopoletin, Alizarin,
#' and Rutin from \emph{Morinda citrifolia}', \emph{Foods} \strong{10}, article 2260, DOI: \url{https://doi.org/10.3390/foods10102260}.
#'
#' Kim, D.-S. (2020), Subcritical Water Extraction and Hydrolysis of Citrus Flavonoids: Kinetics, Optimization, and Biological Activities,
#' Jeju National University, South Korea, PhD thesis.
#'
#' @seealso \code{\link{show_pars}}
#'
"swex"
