#' Read YAML configuration
#' @param path Path to a YAML file.
#' @return A named list with configuration values.
#' @examples
#' \dontrun{
#' cfg <- read_cfg("config/config.dev.yaml")
#' cfg$data$products
#' }
#' @keywords internal
read_cfg <- function(path) {
  yaml::read_yaml(path)
}

# R/utils-global-vars.R
utils::globalVariables(c(
  ".config", "id", ".pred", "kge", "kge_mean",
  "YYYY", "w", "pred", "Q", "class_hat", "quality",
  "value", "prob","pred_final",".", "across"
))
