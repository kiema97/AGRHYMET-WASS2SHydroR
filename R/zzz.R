#' @importFrom cli cli_rule cli_alert_info
# pas besoin d'importer packageVersion; on l’appelle qualifié
.onLoad <- function(libname, pkgname) {
  # Enregistrements uniquement
  register_method("stat",  function() method_id("stat"))
  #register_method("hydro", function() method_id("hydro"))
  register_method("ml",   function() method_id("ml"))
  # register_method("ai",   function() method_id("ai"))
}

.onAttach <- function(libname, pkgname) {
  # Évite d’imprimer pendant R CMD check
  if (!interactive()) return(invisible())

  cli::cli_rule(center = "AGRHYMET - CCR-AOS")
  cli::cli_alert_info("West-African & Sahel S2S Hydrology (WASS2SHydroR)")
  cli::cli_alert_info(paste0(
    "WASS2SHydroR ", utils::packageVersion("WASS2SHydroR"),
    " loaded. Available methods: ", paste(ls(hf_registry), collapse = ", ")
  ))
  cli::cli_rule()
}
