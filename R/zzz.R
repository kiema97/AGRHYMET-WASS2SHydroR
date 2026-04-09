#' @importFrom cli cli_rule cli_alert_info
# pas besoin d'importer packageVersion; on l’appelle qualifié
.onLoad <- function(libname, pkgname) {
  # Enregistrements uniquement
  register_method("stat",  function() method_id("stat"))
  register_method("hydro", function() method_id("hydro"))
  register_method("ml",   function() method_id("ml"))
  # register_method("ai",   function() method_id("ai"))
}


.onAttach <- function(libname, pkgname) {
  # Avoid printing during R CMD check
  if (!interactive()) return(invisible())

  pkg_version <- as.character(utils::packageVersion(pkgname))
  approaches <- c(
    "Process-based (HYDRO)",
    "Statistical (STAT)",
    "Machine Learning (ML)"
  )

  cli::cli_rule(center = "AGRHYMET - CCR-AOS")
  cli::cli_alert_info("WASS2SHydroR: West Africa and Sahel S2S Hydrology Toolkit")
  cli::cli_alert_success(paste0("Version ", pkg_version, " attached."))
  cli::cli_alert_info(
    paste0("Implemented approaches: ", paste(approaches, collapse = ", "))
  )
  cli::cli_rule()
}

onAttach__ <- function(libname, pkgname) {
  # Évite d’imprimer pendant R CMD check
  if (!interactive()) return(invisible())

  cli::cli_rule(center = "AGRHYMET - CCR-AOS")
  cli::cli_alert_info("West-African & Sahel S2S Hydrology (WASS2SHydroR)")
  cli::cli_alert_info(paste0(
    "WASS2SHydroR ", utils::packageVersion("WASS2SHydroR"),
    " loaded. Available approaches: ", paste(
      #ls(hf_registry),
      c("Process-based (HYDRO)","Statistical (STAT)","Machine Learning (ML) "),
      collapse = ", ")
  ))
  cli::cli_rule()
}
