# Setup the default options
.PKNCAEnv <- new.env(parent=emptyenv())
assign("options", NULL, envir=.PKNCAEnv)
assign("summary", list(), envir=.PKNCAEnv)
assign("interval.cols", list(), envir=.PKNCAEnv)
assign("interval.cols_sorted", NULL, envir = .PKNCAEnv)

#' Add columns for calculations within PKNCA intervals
#'
#' @param name The column name as a character string
#' @param FUN The function to run (as a character string) or `NA` if the
#'   parameter is automatically calculated when calculating another parameter.
#' @param values Valid values for the column
#' @param depends Character vector of columns that must be run before this
#'   column.
#' @param desc A human-readable description of the parameter (<=40 characters to
#'   comply with SDTM)
#' @param sparse Is the calculation for sparse PK?
#' @param unit_type The type of units to use for assigning and converting units.
#' @param pretty_name The name of the parameter to use for printing in summary
#'   tables with units.  (If an analysis does not include units, then the normal
#'   name is used.)
#' @param formalsmap A named list mapping parameter names in the function call
#'   to NCA parameter names.  See the details for information on use of
#'   `formalsmap`.
#' @param datatype The type of data used for the calculation
#' @returns NULL (Calling this function has a side effect of changing the
#'   available intervals for calculations)
#'
#' @details The `formalsmap` argument enables mapping some alternate formal
#' argument names to parameters.  It is used to generalize functions that may
#' use multiple similar arguments (such as the variants of mean residence time).
#' The names of the list should correspond to function formal parameter names
#' and the values should be one of the following:
#'
#' \itemize{
#'   \item{For the current interval:}
#'   \describe{
#'     \item{character strings of NCA parameter name}{The value of the parameter calculated for the current interval.}
#'     \item{"conc"}{Concentration measurements for the current interval.}
#'     \item{"time"}{Times associated with concentration measurements for the current interval (values start at 0 at the beginning of the current interval).}
#'     \item{"volume"}{Volume associated with concentration measurements for the current interval (typically applies for excretion parameters like urine).}
#'     \item{"duration.conc"}{Durations associated with concentration measurements for the current interval.}
#'     \item{"dose"}{Dose amounts assocuated with the current interval.}
#'     \item{"time.dose"}{Time of dose start associated with the current interval (values start at 0 at the beginning of the current interval).}
#'     \item{"duration.dose"}{Duration of dose (typically infusion duration) for doses in the current interval.}
#'     \item{"route"}{Route of dosing for the current interval.}
#'     \item{"start"}{Time of interval start.}
#'     \item{"end"}{Time of interval end.}
#'     \item{"options"}{PKNCA.options governing calculations.}
#'   }
#'   \item{For the current group:}
#'   \describe{
#'     \item{"conc.group"}{Concentration measurements for the current group.}
#'     \item{"time.group"}{Times associated with concentration measurements for the current group (values start at 0 at the beginning of the current interval).}
#'     \item{"volume.group"}{Volume associated with concentration measurements for the current interval (typically applies for excretion parameters like urine).}
#'     \item{"duration.conc.group"}{Durations assocuated with concentration measurements for the current group.}
#'     \item{"dose.group"}{Dose amounts assocuated with the current group.}
#'     \item{"time.dose.group"}{Time of dose start associated with the current group (values start at 0 at the beginning of the current interval).}
#'     \item{"duration.dose.group"}{Duration of dose (typically infusion duration) for doses in the current group.}
#'     \item{"route.group"}{Route of dosing for the current group.}
#'   }
#' }
#' @examples
#' \dontrun{
#' add.interval.col("cmax",
#'                  FUN="pk.calc.cmax",
#'                  values=c(FALSE, TRUE),
#'                  unit_type="conc",
#'                  pretty_name="Cmax",
#'                  desc="Maximum observed concentration")
#' add.interval.col("cmax.dn",
#'                  FUN="pk.calc.dn",
#'                  values=c(FALSE, TRUE),
#'                  unit_type="conc_dosenorm",
#'                  pretty_name="Cmax (dose-normalized)",
#'                  desc="Maximum observed concentration, dose normalized",
#'                  formalsmap=list(parameter="cmax"),
#'                  depends="cmax")
#' }
#' @family Interval specifications
#' @export
add.interval.col <- function(name,
                             FUN,
                             values=c(FALSE, TRUE),
                             unit_type,
                             pretty_name,
                             depends=NULL,
                             desc="",
                             sparse=FALSE,
                             formalsmap=list(),
                             datatype=c("interval",
                               "individual",
                               "population")) {
  # Check inputs
  if (!is.character(name)) {
    stop("name must be a character string")
  } else if (length(name) != 1) {
    stop("name must have length == 1")
  }
  if (length(FUN) != 1) {
    stop("FUN must have length == 1")
  } else if (!(is.character(FUN) | is.na(FUN))) {
    stop("FUN must be a character string or NA")
  }
  if (!is.null(depends)) {
    if (!is.character(depends)) {
      stop("'depends' must be NULL or a character vector")
    }
  }
  checkmate::assert_logical(sparse, any.missing=FALSE, len=1)
  unit_type <-
    match.arg(
      unit_type,
      choices=c(
        "unitless", "fraction", "%", "count",
        "time", "inverse_time",
        "amount", "amount_dose",
        "conc", "conc_dosenorm",
        "dose",
        "volume",
        "auc", "aumc",
        "auc_dosenorm", "aumc_dosenorm",
        "clearance", "renal_clearance", "renal_clearance_dosenorm"
      )
    )
  stopifnot("pretty_name must be a scalar"=length(pretty_name) == 1)
  stopifnot("pretty_name must be a character"=is.character(pretty_name))
  stopifnot("pretty_name must not be an empty string"=nchar(pretty_name) > 0)
  datatype <- match.arg(datatype)
  if (!(datatype %in% "interval")) {
    stop("Only the 'interval' datatype is currently supported.")
  }
  if (length(desc) != 1) {
    stop("desc must have length == 1")
  } else if (!is.character(desc)) {
    stop("desc must be a character string")
  }
  if (!is.list(formalsmap)) {
    stop("formalsmap must be a list")
  } else if (length(formalsmap) > 0 &
             is.null(names(formalsmap))) {
    stop("formalsmap must be a named list")
  } else if (length(formalsmap) > 0 &
             is.na(FUN)) {
    stop("formalsmap may not be given when FUN is NA.")
  } else if (!all(nchar(names(formalsmap)) > 0)) {
    stop("All formalsmap elements must be named")
  }
  # Ensure that the function exists
  if (!is.na(FUN) &&
      length(utils::getAnywhere(FUN)$objs) == 0) {
    stop("The function named '", FUN, "' is not defined.  Please define the function before calling add.interval.col.")
  }
  if (!is.na(FUN) &
      length(formalsmap) > 0) {
    # Ensure that the formalsmap parameters are all in the list of
    # formal arguments to the function.
    if (!all(names(formalsmap) %in% names(formals(utils::getAnywhere(FUN)$objs[[1]])))) {
      stop("All names for the formalsmap list must be arguments to the function.")
    }
  }
  current <- get("interval.cols", envir=.PKNCAEnv)
  current[[name]] <-
    list(
      FUN=FUN,
      values=values,
      unit_type=unit_type,
      pretty_name=pretty_name,
      desc=desc,
      sparse=sparse,
      formalsmap=formalsmap,
      depends=depends,
      datatype=datatype
    )
  assign("interval.cols", current, envir=.PKNCAEnv)
}

#' Perform topological sort of interval column specifications (Kahn's algorithm)
#'
#' Orders interval columns so that each parameter appears after
#' all parameters it depends on. Used internally by [sort.interval.cols()]
#' to ensure parameters are calculated in the correct dependency order.
#'
#' @param specs Named list of interval column specifications, where each
#'   element is a list containing at minimum a `depends` field (character
#'   vector of parameter names, or `NULL` for no dependencies).
#'
#' @return Character vector of sorted parameter names in topologically
#'   sorted order (dependencies appear before dependent parameters).
#'
#' @details
#' The function performs the following validations:
#' \itemize{
#'   \item Validates that `specs` is a properly structured named list
#'   \item Ensures all dependencies reference existing parameters
#'   \item Detects circular dependencies (returns error if found)
#'   \item Returns deterministic ordering based on insertion order
#'     when multiple valid orderings exist
#' }
#'
#' The algorithm uses Kahn's topological sorting algorithm with
#' insertion-order tie-breaking to ensure consistent results that
#' preserve the original parameter registration order.
#'
#' @section Errors:
#' The function will abort with an error if:
#' \itemize{
#'   \item Any parameter's `depends` field references a non-existent parameter
#'     (class: `pknca_error_missing_dependency`)
#'   \item A circular dependency is detected
#'     (class: `pknca_error_circular_dependency`)
#'   \item The structure of `specs` is invalid
#'     (class: `pknca_error_invalid_spec_structure`)
#' }
#'
#' @keywords internal
#' @noRd
topological_sort <- function(specs) {
  
  # ------------------------------------------------------------
  # 1. Structural validation
  # Ensure specs is a non-empty named list with unique names
  # and each element contains a valid 'depends' field
  # ------------------------------------------------------------
  checkmate::assert_list(
    specs,
    names = "named",
    min.len = 1,
    .var.name = "specs"
  )
  
  checkmate::assert_names(
    names(specs),
    type = "unique",
    .var.name = "names(specs)"
  )
  
  for (nm in names(specs)) {
    checkmate::assert_list(
      specs[[nm]],
      .var.name = paste0("specs[['", nm, "']]")
    )
    
    # Every spec element must have a 'depends' field (can be NULL)
    if (!"depends" %in% names(specs[[nm]])) {
      rlang::abort(
        sprintf("Column '%s' must contain a 'depends' field", nm),
        class = "pknca_error_invalid_spec_structure"
      )
    }
    
    # 'depends' must be a character vector if not NULL
    if (!is.null(specs[[nm]]$depends)) {
      checkmate::assert_character(
        specs[[nm]]$depends,
        any.missing = FALSE,
        .var.name = paste0("specs[['", nm, "']]$depends")
      )
    }
  }
  
  params <- names(specs)
  n <- length(params)
  
  # ------------------------------------------------------------
  # 2. Validate all dependencies reference existing parameters
  # Collect all missing deps across all specs in one pass
  # ------------------------------------------------------------
  missing <- unique(unlist(lapply(specs, function(spec) {
    if (is.null(spec$depends)) return(character(0))
    setdiff(spec$depends, params)
  }), use.names = FALSE))
  
  if (length(missing)) {
    rlang::abort(
      sprintf(
        "Missing interval column definitions for: %s",
        paste(dQuote(missing), collapse = ", ")
      ),
      class = "pknca_error_missing_dependency"
    )
  }
  
  # ------------------------------------------------------------
  # 3. Build dependency graph
  # in_deg: number of unresolved dependencies per parameter
  # adj: adjacency list — adj[[p]] lists params that depend on p
  # ------------------------------------------------------------
  in_deg <- setNames(integer(n), params)
  adj    <- setNames(vector("list", n), params)
  
  for (p in params) {
    deps <- specs[[p]]$depends
    if (is.null(deps)) next  # no dependencies, skip
    for (d in deps) {
      adj[[d]] <- c(adj[[d]], p)  # p depends on d, so d -> p edge
      in_deg[p] <- in_deg[p] + 1
    }
  }
  
  # ------------------------------------------------------------
  # 4. Kahn's algorithm — insertion order tie-breaking
  # Start with all params that have no dependencies (in_deg == 0)
  # Process one at a time, reducing in_deg of dependents
  # Insertion order is preserved (no sorting) for consistent output
  # ------------------------------------------------------------
  queue  <- params[in_deg == 0]  # initial queue in insertion order
  result <- character(n)
  idx    <- 1
  
  while (length(queue)) {
    node  <- queue[1]
    queue <- queue[-1]
    
    result[idx] <- node
    idx <- idx + 1
    
    # Reduce in_deg for all params that depend on this node
    # When in_deg reaches 0, the param is ready to be processed
    for (nbr in adj[[node]]) {
      in_deg[nbr] <- in_deg[nbr] - 1
      if (in_deg[nbr] == 0) {
        queue <- c(queue, nbr)  # append in insertion order
      }
    }
  }
  
  # ------------------------------------------------------------
  # 5. Cycle detection
  # If not all params were processed, a cycle exists
  # (nodes in a cycle never reach in_deg == 0)
  # ------------------------------------------------------------
  if (idx <= n) {
    rlang::abort(
      "Circular dependency detected in interval column specifications",
      class = "pknca_error_circular_dependency"
    )
  }
  
  result
}

#' Sort the interval columns by dependencies.
#'
#' Columns are always to the right of columns that they depend on.
sort.interval.cols <- function() {
  current <- get("interval.cols", envir=.PKNCAEnv)
  # Only sort if necessary
  sort_order <- get0("interval.cols_sorted", envir=.PKNCAEnv)
  
  if (identical(sort_order, names(current))) {
    # It is already sorted
    return(sort_order)
  }
  
  # ------------------------------------------------------------
  # Topological sort
  # ------------------------------------------------------------
  order <- topological_sort(current) # N
  
  # Update environment
  assign("interval.cols", current[order], envir = .PKNCAEnv)
  assign("interval.cols_sorted", order, envir = .PKNCAEnv)
  
  invisible(order)
}

#' Get the columns that can be used in an interval specification
#'
#' @returns A list with named elements for each parameter.  Each list element
#'   contains the parameter definition.
#' @seealso [check.interval.specification()] and the vignette "Selection of
#'   Calculation Intervals"
#' @examples
#' get.interval.cols()
#' @family Interval specifications
#' @export
get.interval.cols <- function() {
  sort.interval.cols()
  get("interval.cols", envir=.PKNCAEnv)
}

# Add the start and end interval columns
add.interval.col(
  "start",
  FUN = NA,
  values = as.numeric,
  unit_type="time",
  pretty_name="Interval Start",
  desc = "Starting time of the interval"
)
add.interval.col(
  "end",
  FUN = NA,
  values = as.numeric,
  unit_type="time",
  pretty_name="Interval End",
  desc = "Ending time of the interval (potentially infinity)"
)
