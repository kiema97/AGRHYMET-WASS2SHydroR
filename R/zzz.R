#' @importFrom cli cli_h1 cli_rule cli_alert_info cli_alert_success
#' @importFrom utils packageVersion
.onLoad <- function(libname, pkgname) {
  # Enregistrement des methodes disponibles
  register_method("stat", function() method_id("stat"))
  register_method("hydro", function() method_id("hydro"))
  # Ajoutez dautres methodes au fur et a mesure de leur developpement
  # register_method("ml", function() method_id("ml"))
  # register_method("ai", function() method_id("ai"))

  # Message de chargement optionnel
  if (interactive()) {
    packageStartupMessage(
      "WASS2SHydroR ", packageVersion("WASS2SHydroR"),
      " loaded. Available methods: ",
      paste(ls(hf_registry), collapse = ", ")
    )
  }
}


.onAttach <- function(libname, pkgname) {
  # Évite d'imprimer le banner pendant R CMD check ou dans des scripts non interactifs
  if (!interactive() || nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) return(invisible())

  cli::cli_rule(center = "AGRHYMET - CCR-AOS") # ligne horizontale
  cli::cli_alert_info("West-African & Sahel S2S Hydrology (WASS2SHydroR)")
  #cli::cli_alert_success("AGRHYMET — Centre Régional")
  cli::cli_rule()
}





