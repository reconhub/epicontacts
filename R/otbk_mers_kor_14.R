#'  2015 Middle East respiratory syndrome outbreak in South Korea
#'
#'  These datasets correspond to the initial information collected by the Epidemic Intelligence
#'  group at European Centre for Disease Prevention and Control (ECDC) during the first weeks of the
#'  outbreak of Middle East respiratory syndrome (MERS-CoV) outbreak (South Korea) in 2015.  The
#'  data were used to follow the daily evolution of this outbreak using public information
#'  available.  Information is compiled in two datasets:
#'
#'  - a line listing of MERS-CoV cases with a set of variables describing the patient's
#'  characteristics as follow: id (unique identifier), status (MERCoV status), age, age_class (age
#'  using 10-years groups), sex, place_infect (probable region of infection), reporting_ctry
#'  (country reporting case), loc_hosp (local hospital name where the case was hospitalized),
#'  dt_onset (date of onset of symptoms), dt_report (date of reporting), week_report (week number of
#'  date of reporting), dt_start_exp (date of first probable exposure to another MERCoV case),
#'  dt_end_exp (date of last probable exposure MERCoV case), dt_diag (date of MERCoV diagnosis) and
#'  dt_death (date of death).
#'
#'  Note that this dataset do not contain possible contact, therefore the network visualisation
#'  graphic will correspond to MERCoV transmission chains as the variable status corresponds to
#'  "case" only. (i.e. one node in the network represents one case).
#'
#'  - a database corresponding to the relationship between MERS-CoV cases as follow: from (unique
#'  identifier from the probable source patient), to (unique identifier from the secondary case),
#'  exposure (probable place of exposure) and diff_dt_onset (time in days between two successive
#'  cases). This second dataset is used to map the relationship between cases (i.e. edge in the
#'  transmission chain network).  This dataset is used for teaching purposes only and intend to be
#'  use with the package "contacts" in order to illustrate network visualisation of transmission
#'  chains in epidemiology. This example do not represent nor the final outbreak investigation
#'  results neither the consolidated and complete description of the transmission chain.
#'
#' @docType data
#'
#' @rdname otbk_mers_kor_14
#'
#' @aliases otbk_mers_kor_14
#'
#' @author Data collected by the European Centre for Disease Prevention and Control (Epidemic
#' Intelligence and Response section, contact: Bertrand Sudre (\email{
#' bertrand.sudre@ecdc.europa.eu}) and Kaja Kaasik Aaslav(\email{Kaja.KaasikAaslav@ecdc.europa.eu}).
#' Transfer to R and documentation by Bertrand Sudre (\email{ bertrand.sudre@ecdc.europa.eu}).
#'
#' @references More information on the intial stage of the outbreak in the following reference:
#' Penttinen PM, Kaasik-Aaslav K, Friaux A, Donachie A, Sudre B, Amato-Gauci AJ, Memish ZA,
#' Coulombier D. Taking stock of the first 133 MERS coronavirus cases globally--Is the epidemic
#' changing?  Euro Surveill. 2013 Sep 26;18(39). pii: 20596. PubMed PMID: 24094061.
#'
"otbk_mers_kor_14"


