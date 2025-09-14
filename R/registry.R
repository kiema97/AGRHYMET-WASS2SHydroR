#' Registry of available forecasting methods
#'
#' Simple plugin registry so the core can discover and run methods
#' (e.g. "stat", "hydro", "ml") without hard dependencies.
#' 
#' @format An environment containing registered method constructors.
#' @export
hf_registry <- new.env(parent = emptyenv())

#' Register a method
#'
#' @param name Character ID, e.g. "stat", "hydro", "ml".
#' @param constructor Function returning a method id (see [method_id()]).
#' @return Invisibly returns the name of the registered method.
#' @keywords internal
register_method <- function(name, constructor) {
  # Input validation
  if (!is.character(name) || nchar(name) == 0) {
    stop("Method name must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.function(constructor)) {
    stop("Constructor must be a function", call. = FALSE)
  }
  
  # Check if method already exists
  if (exists(name, envir = hf_registry, inherits = FALSE)) {
    warning("Method '", name, "' is already registered. Overwriting.", call. = FALSE)
  }
  
  assign(name, constructor, envir = hf_registry)
  invisible(name)
}

#' Get a method by name
#'
#' @param name Character ID registered with [register_method()].
#' @return A `method` object produced by the registered constructor.
#' @keywords internal
get_method <- function(name) {
  if (!is.character(name) || nchar(name) == 0) {
    stop("Method name must be a non-empty character string", call. = FALSE)
  }
  
  ctor <- get0(name, envir = hf_registry, inherits = FALSE)
  if (is.null(ctor)) {
    stop("Method '", name, "' is not registered. Available methods: ", 
         paste(ls(hf_registry), collapse = ", "), call. = FALSE)
  }
  
  # Try to create the method object
  tryCatch({
    ctor()
  }, error = function(e) {
    stop("Failed to create method '", name, "': ", e$message, call. = FALSE)
  })
}

#' Build a method id (S3 tag)
#'
#' @param x Character tag, e.g. "stat".
#' @return An object with S3 class `c(paste0("method_", x), "method")`.
#' @export
#' @examples
#' method_id("stat")
#' method_id("hydro")
method_id <- function(x) {
  # Input validation
  if (!is.character(x) || nchar(x) == 0) {
    stop("Method ID must be a non-empty character string", call. = FALSE)
  }
  
  # Check if method is registered
  if (!exists(x, envir = hf_registry, inherits = FALSE)) {
    warning("Method '", x, "' is not registered. Available methods: ", 
            paste(ls(hf_registry), collapse = ", "), call. = FALSE)
  }
  
  structure(list(id = x), class = c(paste0("method_", x), "method"))
}

#' Generic runner for methods
#'
#' Each package (stat/hydro/ml) implements its own S3 method:
#' `run_method.method_stat()`, `run_method.method_hydro()`, etc.
#'
#' @param method An object created by [method_id()].
#' @param data_by_product Named list of data.frames for each climate product.
#' @param cfg Parsed YAML config (list).
#' @return Results from the specific method implementation.
#' @export
run_method <- function(method, data_by_product, cfg) {
  # Input validation
  if (!inherits(method, "method")) {
    stop("method must be an object created by method_id()", call. = FALSE)
  }
  
  if (!is.list(data_by_product) || length(data_by_product) == 0) {
    stop("data_by_product must be a non-empty named list", call. = FALSE)
  }
  
  if (!all(sapply(data_by_product, is.data.frame))) {
    stop("All elements of data_by_product must be data frames", call. = FALSE)
  }
  
  if (!is.list(cfg)) {
    stop("cfg must be a list (parsed YAML configuration)", call. = FALSE)
  }
  
  UseMethod("run_method", method)
}

#' Default method for unregistered method types
#'
#' @param method An object created by [method_id()].
#' @param data_by_product Named list of data.frames for each climate product.
#' @param cfg Parsed YAML config (list).
#' @export
run_method.default <- function(method, data_by_product, cfg) {
  stop("No run_method method implemented for class: ", 
       paste(class(method), collapse = ", "), call. = FALSE)
}

#' High-level runner for WASS2S
#'
#' @param method One of "stat", "hydro" (later "ai")
#' @param data_by_product Named list of data.frames
#' @param cfg List or path to YAML configuration file
#' @return Results from the selected method
#' @export
#' @examples
#' \dontrun{
#' # Example usage
#' result <- run_wass2s("hydro", data_by_product, "config.yaml")
#' }
run_wass2s <- function(method, data_by_product, cfg) {
  # Input validation
  if (!is.character(method) || length(method) != 1) {
    stop("method must be a single character string", call. = FALSE)
  }
  
  if (!is.list(data_by_product) && !is.character(data_by_product)) {
    stop("data_by_product must be a list or a file path", call. = FALSE)
  }
  
  # Load configuration if path is provided
  if (is.character(cfg) && file.exists(cfg)) {
    cfg <- tryCatch({
      yaml::read_yaml(cfg)
    }, error = function(e) {
      stop("Failed to read YAML configuration: ", e$message, call. = FALSE)
    })
  }
  
  if (!is.list(cfg)) {
    stop("cfg must be a list or a valid path to a YAML file", call. = FALSE)
  }
  
  # Create method object
  m <- tryCatch({
    method_id(method)
  }, error = function(e) {
    stop("Failed to create method ID: ", e$message, call. = FALSE)
  })
  
  # Run the method
  tryCatch({
    run_method(m, data_by_product, cfg)
  }, error = function(e) {
    stop("Error running method '", method, "': ", e$message, call. = FALSE)
  })
}

# Register available methods when the package is loaded
.onLoad <- function(libname, pkgname) {
  # Register statistical method
  register_method("stat", function() method_id("stat"))
  
  # Register hydro (ML) method
  register_method("hydro", function() method_id("hydro"))
  
  # Add more methods as they become available
  # register_method("ml", function() method_id("ml"))
  # register_method("ai", function() method_id("ai"))
}

# Optional: Provide a function to list available methods
#' List available forecasting methods
#'
#' @return Character vector of registered method names
#' @export
#' @examples
#' list_methods()
list_methods <- function() {
  ls(hf_registry)
}