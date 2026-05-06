# =============================================================================
# PK Data Loader
# =============================================================================
# Loads, classifies, cleans, and standardises pharmacokinetic data from
# multiple file formats (XPT, XLSX, XLS, CSV, TXT, SAS7BDAT).
# =============================================================================

# =============================================================================
# 1.  Public API
# =============================================================================

#' Load and Process Pharmacokinetic Data
#'
#' Streamlines loading, cleaning, and standardisation of PK data from multiple
#' file formats (XPT, XLSX, XLS, CSV, TXT, SAS7BDAT).
#'
#' @param path         Character. Directory containing PK files. Default: \code{getwd()}.
#' @param file_types   Character vector. File extensions to search for. Default:
#'                     \code{c("xpt","xlsx","xls","csv","txt","sas7bdat")}.
#' @param patterns     Named list. Regex patterns for PK column roles.
#'                     See \code{\link{get_pk_patterns}}.
#' @param decimal_control Logical. Apply smart decimal formatting? Default \code{TRUE}.
#' @param blq_handling    Logical. Apply BLQ interpolation? Default \code{TRUE}.
#' @param bind_rows       Logical. Bind multiple files into one data frame? Default \code{TRUE}.
#' @param verbose         Logical. Print detailed progress? Default \code{TRUE}.
#'
#' @return A list of class \code{pk_data_list}:
#'   \item{conc}{Concentration data frame (if found)}
#'   \item{dose}{Dose data frame (if found)}
#'
#' @export
#' @examples
#' \dontrun{
#'   pk   <- load_pk_data(path = "path/to/data", verbose = TRUE)
#'   conc <- pk$conc
#'   dose <- pk$dose
#' }
load_pk_data <- function(path          = getwd(),
                         file_types    = NULL,
                         patterns      = get_pk_patterns(),
                         decimal_control = TRUE,
                         blq_handling  = TRUE,
                         bind_rows     = TRUE,
                         verbose       = TRUE) {
  
  # ---- argument validation --------------------------------------------------
  checkmate::assert_string(path, min.chars = 1)
  checkmate::assert_directory_exists(path)
  checkmate::assert_character(file_types, null.ok = TRUE, min.chars = 1)
  checkmate::assert_list(patterns, min.len = 1, names = "named")
  checkmate::assert_flag(decimal_control)
  checkmate::assert_flag(blq_handling)
  checkmate::assert_flag(bind_rows)
  checkmate::assert_flag(verbose)
  
  lapply(patterns, function(p) {
    if (!is.character(p))
      rlang::abort("All entries in `patterns` must be character vectors.")
  })
  
  # ---- 1. resolve files -----------------------------------------------------
  file_set <- resolve_pk_files(
    path      = path,
    file_types = file_types,
    patterns  = patterns,
    verbose   = verbose
  )
  
  # ---- 2. load & (optionally) bind ------------------------------------------
  result <- list()
  type_map <- c(conc = "concentration", dose = "dose")
  
  for (role in names(type_map)) {
    files <- file_set[[role]]
    if (length(files) == 0) next
    
    result[[role]] <- load_and_bind_pk_files(
      paths     = files,
      type      = type_map[[role]],
      patterns  = patterns,
      verbose   = verbose,
      bind_rows = bind_rows
    )
  }
  
  quick_validate(result)                 # lightweight safety net
  
  # ---- 3. post-processing ---------------------------------------------------
  process_map <- list(
    conc = function(x) process_conc_data(x, decimal_control, blq_handling, verbose),
    dose = function(x) process_dose_data(x, decimal_control, verbose)
  )
  
  for (role in names(process_map)) {
    if (!is.null(result[[role]]))
      result[[role]] <- process_map[[role]](result[[role]])
  }

  class(result) <- c("pk_data_list", class(result))
  result
}

#' Lightweight Result Validation
#'
#' @keywords internal
quick_validate <- function(result) {
  if (is.null(result$conc) && is.null(result$dose)){
    rlang::abort("No valid PK data loaded \u2014 neither conc nor dose was found.")
  }
    
  invisible(TRUE)
}

# =============================================================================
# 2.  File Resolution
# =============================================================================

#' Resolve PK Files in a Directory
#'
#' Scans \code{path} for supported files and classifies each as
#' "conc", "dose", "combined", or "unknown".
#'
#' @keywords internal
resolve_pk_files <- function(path,
                             file_types = NULL,
                             patterns,
                             verbose   = FALSE) {
  
  # path validity already guaranteed by load_pk_data(); skip redundant check
  if (is.null(file_types)){
    file_types <- c("xpt", "xlsx", "xls", "csv", "txt", "sas7bdat")
  }
    
  ext_pat <- paste0("\\.(", paste(file_types, collapse = "|"), ")$")
  
  if (verbose){message("Supported extensions: ", paste(file_types, collapse = ", "))}
  
  files <- list.files(path, pattern = ext_pat,
                      full.names = TRUE, ignore.case = TRUE)
  
  if (length(files) == 0){
    rlang::abort(sprintf("No supported files found in: %s", path))
  }
    
  # ---- role detection -------------------------------------------------------
  roles <- vapply(
    X         = files,
    FUN       = detect_role,
    FUN.VALUE = character(1),
    patterns  = patterns,
    verbose   = verbose
  )
  
  if (verbose) {
    message("File role detection:")
    for (i in seq_along(files))
      message(sprintf("  %s \u2192 %s", basename(files[i]), roles[i]))
  }
  
  combined_files <- files[roles == "combined"]
  conc_only      <- files[roles == "conc"]
  dose_only      <- files[roles == "dose"]
  
  # If any combined files exist, use them exclusively for both roles
  if (length(combined_files) > 0) {
    if (verbose) message("Combined conc+dose file(s) found \u2192 single-file mode")
    return(structure(
      list(conc = combined_files, dose = combined_files),
      class = "pk_file_set"
    ))
  }
  
  # Warn when one role is absent
  if (length(conc_only) == 0 && length(dose_only) == 0)
    rlang::abort(sprintf(
      "Neither concentration nor dose file detected.\nFiles found: %s\n%s",
      paste(basename(files), collapse = ", "),
      "Ensure files contain required columns (conc/concentration and/or dose/amt)."
    ))
  
  if (length(conc_only) == 0)
    rlang::warn(sprintf(
      "No concentration file detected. Dose file(s): %s",
      paste(basename(dose_only), collapse = ", ")
    ))
  
  if (length(dose_only) == 0)
    rlang::warn(sprintf(
      "No dose file detected. Concentration file(s): %s",
      paste(basename(conc_only), collapse = ", ")
    ))
  
  structure(list(conc = conc_only, dose = dose_only), class = "pk_file_set")
}


# =============================================================================
# 3.  File Loading & Binding
# =============================================================================

#' Load and Bind Multiple PK Files
#'
#' @keywords internal
load_and_bind_pk_files <- function(paths, type, patterns, verbose, bind_rows) {

  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files) > 0){
    rlang::abort(sprintf("File(s) do not exist:\n  %s", paste(missing_files, collapse = "\n  ")))
  }
  
  dfs <- lapply(paths, read_one_pk_file, patterns = patterns, verbose = verbose)
  
  if (length(dfs) == 1) return(dfs[[1]])
  
  if (!bind_rows) return(dfs)
  
  # -- check mapping consistency across files ---------------------------------
  mappings <- lapply(dfs, function(d) attr(d, "column_mapping"))
  reference_mapping <- mappings[[1]]
  
  inconsistent <- vapply(mappings[-1], function(m) {
    !identical(
      unlist(m[!is.na(unlist(m))]),
      unlist(reference_mapping[!is.na(unlist(reference_mapping))])
    )
  }, logical(1))
  
  if (any(inconsistent))
    rlang::warn(
      c("!" = "Column mappings differ across files being bound together.",
        "i" = "Using the mapping from the first file.",
        ">" = "Verify all files share the same column structure.")
    )
  
  # -- bind & restore attributes ----------------------------------------------
  bound <- dplyr::bind_rows(dfs)
  attr(bound, "column_mapping") <- reference_mapping
  class(bound) <- c("pk_data", class(bound))
  
  if (verbose){
    message(sprintf("  \u2022 Bound %d %s file(s) \u2192 %d rows", length(dfs), type, nrow(bound)))
  }
  
  bound
}


#' Read a Single PK File
#'
#' @keywords internal
read_one_pk_file <- function(filepath, patterns, verbose = TRUE) {
  
  if (verbose){rlang::inform(sprintf("Loading: %s", basename(filepath)))}
  
  df <- tryCatch(
    rio::import(file = filepath, which = 1),
    error = function(e)
      rlang::abort(sprintf("Failed to read '%s': %s", filepath, e$message))
  )
  
  if (nrow(df) == 0){rlang::abort(sprintf("Empty file: %s", basename(filepath)))}
  
  mapping <- create_column_mapping(names(df), patterns)
  attr(df, "column_mapping") <- mapping
  class(df) <- c("pk_data", class(df))
  df
}


# =============================================================================
# 4.  Role Detection
# =============================================================================

#' Detect the PK Role of a File
#'
#' @keywords internal
detect_role <- function(f, patterns, verbose = FALSE) {
  
  col_names <- read_column_names_only(f, verbose = verbose)
  if (is.null(col_names)) return("unknown")
  
  if (verbose){message(sprintf("  Columns in %s: %s", basename(f), paste(col_names, collapse = ", ")))}
  
  role_matches <- resolve_pk_column_roles(
    names_vec       = col_names,
    patterns        = patterns,
    mode            = "detect_all",
    stop_on_ambiguous = FALSE
  )
  
  has_conc <- isTRUE(role_matches["conc"])
  has_dose <- isTRUE(role_matches["dose"])
  
  if (has_conc && has_dose) return("combined")
  if (has_conc)             return("conc")
  if (has_dose)             return("dose")
  
  if (verbose){message(sprintf("  %s \u2192 no PK columns found", basename(f)))}
  
  "unknown"
}


#' Read Only Column Names from a File
#'
#' Attempts a zero-row or one-row read to obtain column names without loading
#' the full dataset. Handles formats that ignore \code{n_max}.
#'
#' @keywords internal
read_column_names_only <- function(f, verbose = FALSE) {
  
  ext <- tolower(tools::file_ext(f))
  
  col_names <- tryCatch({
    if (ext %in% c("xpt", "sas7bdat")) {
      # haven is more reliable for these formats
      if (requireNamespace("haven", quietly = TRUE)) {
        reader <- if (ext == "xpt") haven::read_xpt else haven::read_sas
        tmp <- reader(f, n_max = 1L)
        names(tmp)
      } else {
        tmp <- rio::import(file = f, which = 1)
        names(tmp)
      }
    } else {
      tmp <- rio::import(file = f, which = 1, n_max = 1L)
      names(tmp)
    }
  }, error = function(e) {
    if (verbose){message(sprintf("  Could not read columns from '%s': %s", basename(f), e$message))}
      
    NULL
  })
  
  col_names
}


# =============================================================================
# 5.  Column Mapping
# =============================================================================

#' Get Default PK Column Patterns
#'
#' Returns a named list of regex patterns used to identify concentration, dose,
#' subject, and time columns.
#'
#' @return Named list with patterns for \code{conc}, \code{dose},
#'   \code{subject}, and \code{time}.
#' @export
#' @examples
#' patterns <- get_pk_patterns()
#' # Add SDTM PCORRES support:
#' # patterns$conc <- c(patterns$conc, "^pcorres$", "^pcstresc$")
get_pk_patterns <- function() {
  list(
    # FIX #7: tightened greedy patterns with word boundaries / anchors
    conc = c(
      "^conc$", "^aval$", "^pcstresn$",  "^dv$",  "^concentration$",
      # "^pcorres$",   # opt-in: SDTM PCORRES (character result)
      # "^pcstresc$",  # opt-in: SDTM PCSTRESC (standardised char result)
      "^conc_",          # conc_ prefix (e.g. conc_plasma)
      "_conc$",          # _conc suffix
      "\\bconcentration\\b",
      "\\bng[_/]?ml\\b",
      "\\bmg[_/]?ml\\b",
      "\\bug[_/]?ml\\b"
    ),
    dose = c(
      "^dose$",  "^amount$", "^exdose$",  "^amt$",
      "^dose_",          # dose_ prefix
      "_dose$",          # _dose suffix
      "\\bmg$",
      "\\bug$"
    ),
    subject = c(
      "^usubjid$", "^id$", "^subject$", "^subjectid$", "^ptno$",
      "^subj$", "^subj_id$", "^subject_id$"
    ),
    time = c(
      "^time$", "^pctptnum$", "^atptn$", "^tad$", "^tafd$", "^hr$",
      "^hours$", "^time_h$", "^time_hr$",
      "\\btime\\s*\\(.*\\)"   # e.g. "Time (h)"
    )
  )
}


#' Create Column Mapping from Column Names
#'
#' @keywords internal
create_column_mapping <- function(original_names, patterns) {
  matches <- resolve_pk_column_roles(
    names_vec         = tolower(original_names),
    patterns          = patterns,
    mode              = "match",
    stop_on_ambiguous = FALSE
  )
  mapping <- vector("list", length(patterns))
  names(mapping) <- names(patterns)
  for (role in names(patterns)) {
    idx <- which(matches[[role]])
    mapping[[role]] <- if (length(idx) > 0) original_names[idx[1L]] else NA_character_
  }
  mapping
}


#' Get Mapped Column Name
#'
#' Returns the actual column name for a given PK role.
#'
#' @param data A data frame created by \code{load_pk_data()}.
#' @param role Character. One of \code{"subject"}, \code{"time"},
#'   \code{"conc"}, or \code{"dose"}.
#'
#' @return Character string - the matched column name.
#' @export
#' @examples
#' \dontrun{
#'   time_col <- get_mapped_column(pk$conc, "time")
#' }
get_mapped_column <- function(data, role) {
  
  mapping <- attr(data, "column_mapping")
  if (is.null(mapping)){
    rlang::abort(
      message = "Missing column mapping.",
      body = c(
        "i" = "This data frame was not produced by load_pk_data().",
        ">" = "Use load_pk_data() to load and map your data."
      )
    )
  }
    
  col <- mapping[[role]]
  if (is.na(col)) {
    available <- names(mapping)[!is.na(unlist(mapping))]
    rlang::abort(
      message = sprintf("Role '%s' not found in column mapping.", role),
      body = c(
        "i" = sprintf("Available roles: %s", paste(available, collapse = ", ")),
        ">" = "Check your data or adjust column patterns via get_pk_patterns()."
      )
    )
  }
  col
}


#' Resolve PK Column Roles
#'
#' Matches column names against PK role patterns.
#'
#' @param file            Character. Optional file path for error messages.
#' @param names_vec       Character vector (or data frame) of column names.
#' @param patterns        Named list of regex patterns.
#' @param mode            One of \code{"match"}, \code{"detect"}, \code{"detect_all"}.
#' @param stop_on_ambiguous Logical. Abort if multiple columns match one role?
#'
#' @keywords internal
resolve_pk_column_roles <- function(file             = NULL,
                                    names_vec,
                                    patterns,
                                    mode             = c("match", "detect", "detect_all"),
                                    stop_on_ambiguous = TRUE) {
  
  mode <- match.arg(mode)
  
  if (is.data.frame(names_vec)) {
    if (is.null(names_vec) || nrow(names_vec) == 0) {
      return(switch(mode,
                    detect     = FALSE,
                    detect_all = FALSE,
                    lapply(patterns, function(x) logical(0))))
    }
    names_vec <- names(names_vec)
  }
  
  lower_names <- tolower(names_vec)
  
  # Duplicate column check
  dupes <- lower_names[duplicated(lower_names)]
  if (length(dupes) > 0){
    rlang::abort(sprintf(
      "%sDuplicate column names (case-insensitive): %s",
      if (!is.null(file)) sprintf("File '%s': ", basename(file)) else "",
      paste(unique(dupes), collapse = ", ")
    ))
  }
  
  if (length(lower_names) == 0) {
    return(switch(mode,
                  detect     = FALSE,
                  detect_all = FALSE,
                  lapply(patterns, function(x) logical(0))))
  }
  
  # Match each role
  role_hits <- lapply(patterns, function(pats) {
    combined_pat <- paste0("(", paste(pats, collapse = "|"), ")")
    grepl(combined_pat, lower_names, ignore.case = TRUE, perl = TRUE)
  })
  
  # Ambiguity check
  if (stop_on_ambiguous) {
    ambiguous_roles <- names(role_hits)[vapply(role_hits, sum, integer(1)) > 1L]
    if (length(ambiguous_roles) > 0) {
      details <- vapply(ambiguous_roles, function(r) {
        hits <- which(role_hits[[r]])
        sprintf("%s \u2190 %s", r, paste(names_vec[hits], collapse = ", "))
      }, character(1))
      rlang::abort(sprintf(
        "%sAmbiguous column matches:\n  %s\n\nFix: Rename columns or supply custom patterns.",
        if (!is.null(file)) sprintf("File '%s': ", basename(file)) else "",
        paste(details, collapse = "\n  ")
      ))
    }
  }
  
  switch(mode,
         detect     = any(vapply(role_hits, any, logical(1))),
         detect_all = vapply(role_hits, any, logical(1)),
         role_hits   # "match" \u2014 return the full logical list
  )
}


# =============================================================================
# 6.  Post-Processing: Concentration & Dose
# =============================================================================

#' Process Concentration Data
#'
#' @keywords internal
process_conc_data <- function(df, decimal_control, blq_handling, verbose) {
  
  df <- remove_empty_data(df, verbose)
  
  mapping <- attr(df, "column_mapping")
  if (is.na(mapping$subject))
    df <- auto_create_subject_id(df, verbose = verbose)
  
  if (blq_handling)
    df <- clean_blq_values(df, verbose)
  
  if (decimal_control)
    df <- decimal_formatter(
      df          = df,
      col_max_map = list(time = 1L, conc = 3L),
      verbose     = verbose
    )
  
  df
}


#' Process Dose Data
#'
#' @keywords internal
process_dose_data <- function(df, decimal_control, verbose) {
  
  df <- remove_empty_data(df, verbose)
  
  mapping <- attr(df, "column_mapping")
  if (is.na(mapping$subject))
    df <- auto_create_subject_id(df, verbose = verbose)
  
  if (decimal_control)
    df <- decimal_formatter(
      df          = df,
      col_max_map = list(dose = 2L),
      verbose     = verbose
    )
  
  df
}


# =============================================================================
# 7.  Data Cleaning Utilities
# =============================================================================

#' Remove Empty Rows and Columns
#'
#' Thin wrapper around \code{janitor::remove_empty()} that preserves
#' custom attributes.
#'
#' @keywords internal
remove_empty_data <- function(df, verbose) {
  
  orig_rows <- nrow(df)
  orig_cols <- ncol(df)
  
  cleaned <- janitor::remove_empty(dat = df, which = c("rows", "cols"))
  
  removed_rows <- orig_rows - nrow(cleaned)
  removed_cols <- orig_cols - ncol(cleaned)
  
  if ((removed_rows > 0 || removed_cols > 0) && verbose){
    message(sprintf("  \u2022 Removed %d empty row(s), %d empty col(s)", removed_rows, removed_cols)) 
  }
  
  # Preserve attributes
  attr(cleaned, "column_mapping") <- attr(df, "column_mapping")
  class(cleaned) <- class(df)
  cleaned
}


#' Auto-Create Subject ID Column
#'
#' Creates a default subject ID (\code{"SUBJ001"}) when no subject column is
#' detected. Warns if duplicate time values suggest multiple subjects.
#'
#' @keywords internal
auto_create_subject_id <- function(df, verbose = FALSE) {
  
  mapping <- attr(df, "column_mapping")
  
  # Already mapped, nothing to do
  if (!is.null(mapping$subject) &&
      !is.na(mapping$subject) &&
      mapping$subject %in% names(df)) {
    return(df)
  }
  
  warned <- FALSE
  time_col <- mapping$time
  
  if (!is.na(time_col) && time_col %in% names(df)) {
    time_vals <- df[[time_col]]
    if (any(duplicated(stats::na.omit(time_vals)))) {
      warned <- TRUE
      rlang::warn(c(
        "!" = "No subject ID column detected.",
        "i" = "Duplicate time values found \u2014 possible multiple subjects.",
        ">" = "All data assigned to single subject ID = 'SUBJ001'.",
        ">" = "Add a subject column if multiple subjects are present."
      ))
    }
  }
  
  df$ID <- "SUBJ001"
  mapping$subject <- "ID"
  attr(df, "column_mapping") <- mapping
  
  if (verbose && !warned){rlang::inform("No subject ID detected \u2014 created default column 'ID' = 'SUBJ001'.")}
  
  df
}


# =============================================================================
# 8.  BLQ Handling
# =============================================================================

#' Handle BLQ (Below Limit of Quantification) Values
#'
#' Replaces BLQ strings with \code{NA} then interpolates using linear
#' interpolation pre-Cmax and log-linear interpolation post-Cmax.
#'
#' Negative-time rows are flagged with a warning before removal.
#'
#' @keywords internal
clean_blq_values <- function(data, verbose) {
  
  subj_col <- get_mapped_column(data, "subject")
  time_col <- get_mapped_column(data, "time")
  conc_col <- get_mapped_column(data, "conc")
  
  blq_strings <- c("blq", "bloq", "bql", "lloq", "na", "nr", "",
                   "nd", "<lloq", "< lloq")
  
  # warn about negative-time rows before dropping
  neg_time_rows <- which(
    suppressWarnings(as.numeric(as.character(data[[time_col]]))) < 0
  )
  if (length(neg_time_rows) > 0) {
    rlang::warn(c(
      "!" = sprintf("%d row(s) with negative time values will be removed.",
                    length(neg_time_rows)),
      "i" = "Negative time indicates pre-dose sampling \u2014 handle separately if needed.",
      ">" = "Rows removed from BLQ processing pipeline."
    ))
  }
  
  # ---- step 1: mutate -------------------------------------------------------

  processed <- data
  
  # keep original
  processed$conc_original <- processed[[conc_col]]
  
  # transform
  tmp <- trimws(tolower(as.character(processed[[conc_col]])))
  
  processed[[conc_col]] <- dplyr::case_when(
    tmp %in% blq_strings ~ NA_real_,
    tmp == "0"           ~ 0,
    TRUE                 ~ suppressWarnings(as.numeric(tmp))
  )
  
  # ---- step 2: filter -------------------------------------------------------
  processed <- dplyr::filter(
    processed,
    !is.na(.data[[time_col]]),
    is.finite(suppressWarnings(as.numeric(as.character(.data[[time_col]])))),
    suppressWarnings(as.numeric(as.character(.data[[time_col]]))) >= 0
  )
  
  # ---- step 3: arrange ------------------------------------------------------
  processed <- dplyr::arrange(
    processed,
    .data[[subj_col]],
    .data[[time_col]]
  )
  
  # ---- step 4: group-wise interpolation ------------------------------------
  split_data <- split(processed, processed[[subj_col]])
  
  interpolated_list <- lapply(split_data, function(sub_df) {
    interpolate_subject(
      sub_df   = sub_df,
      time_col = time_col,
      conc_col = conc_col,
      verbose  = verbose
    )
  })
  
  cleaned <- dplyr::bind_rows(interpolated_list)
  
  # ---- final arrange --------------------------------------------------------
  cleaned <- dplyr::arrange(
    cleaned,
    .data[[subj_col]],
    .data[[time_col]]
  )
  
  # ---- restore attributes ---------------------------------------------------
  attr(cleaned, "column_mapping") <- attr(data, "column_mapping")
  class(cleaned) <- class(data)
  cleaned
}


#' Interpolate BLQ Values for a Single Subject
#'
#' @keywords internal
interpolate_subject <- function(sub_df, time_col, conc_col, verbose) {
  
  if (nrow(sub_df) == 0) return(sub_df)
  
  sub_df    <- dplyr::arrange(sub_df, .data[[time_col]])
  t_vals    <- sub_df[[time_col]]
  conc_vals <- sub_df[[conc_col]]          # FIX #2
  method    <- rep("observed", length(t_vals))
  
  obs_idx <- which(!is.na(conc_vals))
  
  # All BLQ set everything to zero
  if (length(obs_idx) == 0) {
    sub_df[[conc_col]] <- 0
    sub_df$method      <- "all-blq"
    return(sub_df)
  }
  
  if (length(obs_idx) >= 2) {
    cmax_idx  <- obs_idx[which.max(conc_vals[obs_idx])]
    cmax_time <- t_vals[cmax_idx]
    
    first_obs  <- min(t_vals[obs_idx])
    last_obs   <- max(t_vals[obs_idx])
    middle_na  <- which(is.na(conc_vals) & t_vals > first_obs & t_vals < last_obs)
    
    if (length(middle_na) > 0) {
      conc_interp <- tryCatch(
        zoo::na.approx(conc_vals, x = t_vals, na.rm = FALSE),
        error = function(e) conc_vals
      )
      
      for (i in middle_na) {
        if (t_vals[i] <= cmax_time) {
          conc_vals[i] <- conc_interp[i]
          method[i]    <- "pre-cmax-linear"
        } else {
          before <- max(obs_idx[obs_idx < i])
          after  <- min(obs_idx[obs_idx > i])
          if (!is.na(before) && !is.na(after) &&
              conc_vals[before] > 0 && conc_vals[after] > 0) {
            lambda       <- log(conc_vals[before] / conc_vals[after]) /
              (t_vals[after] - t_vals[before])
            conc_vals[i] <- conc_vals[before] * exp(-lambda * (t_vals[i] - t_vals[before]))
            method[i]    <- "post-cmax-loglinear"
          } else {
            conc_vals[i] <- conc_interp[i]
            method[i]    <- "post-cmax-linear"
          }
        }
      }
    }
  }
  
  # Pre-dose zeros (before first observed value)
  first_obs_idx <- min(obs_idx)
  if (first_obs_idx > 1L) {
    conc_vals[seq_len(first_obs_idx - 1L)] <- 0
    method[seq_len(first_obs_idx - 1L)]    <- "pre-dose-zero"
  }
  
  sub_df[[conc_col]] <- conc_vals
  sub_df$method      <- method
  sub_df
}


# =============================================================================
# 9.  Decimal Formatting
# =============================================================================

#' Count Decimal Places in a Numeric Value
#'
#' @keywords internal
count_decimal_places <- function(x) {
  if (is.na(x) || !is.finite(x)) return(0L)
  x_str <- sub("0+$", "", sprintf("%.15f", x))
  if (grepl("\\.", x_str, fixed = FALSE))
    nchar(sub(".*\\.", "", x_str))
  else
    0L
}


#' Apply Consistent Decimal Formatting to PK Columns
#'
#' Decimal information is stored as an attribute on the *data frame*
#' (\code{"decimal_info"}) rather than on a copy of an individual column,
#' ensuring the metadata actually persists.
#'
#' @keywords internal
decimal_formatter <- function(df, col_max_map, verbose) {
  
  mapping <- attr(df, "column_mapping")
  if (is.null(mapping))
    rlang::abort("Internal error: column_mapping attribute is missing.")
  
  decimal_info <- attr(df, "decimal_info") %||% list()
  
  for (role in names(col_max_map)) {
    col <- mapping[[role]]
    if (is.null(col) || is.na(col) || !col %in% names(df)) next
    if (!is.numeric(df[[col]])) next
    
    places <- vapply(df[[col]], count_decimal_places, integer(1))
    optimal <- min(max(places, na.rm = TRUE), col_max_map[[role]])
    
    # store on the data frame, not on a discarded copy of the column
    decimal_info[[col]] <- optimal
    
    if (verbose){rlang::inform(sprintf("  \u2022 [%s] %s: using %d decimal place(s)", role, col, optimal))}
  }
  
  attr(df, "decimal_info") <- decimal_info
  df
}


# =============================================================================
# 10.  Usage Example  (wrapped in if (FALSE) so it never auto-runs)
# =============================================================================
if (FALSE) {
  
  data_dir <- "../data-raw/test"
  
  pk <- load_pk_data(
    path            = data_dir,
    bind_rows       = TRUE,
    decimal_control = TRUE,
    blq_handling    = TRUE,
    verbose         = TRUE
  )
  
  print(pk)
 
  conc <- pk$conc
  dose <- pk$dose
  
  # Retrieve mapped column names
  time_col <- get_mapped_column(conc, "time")
  conc_col <- get_mapped_column(conc, "conc")
  subj_col <- get_mapped_column(conc, "subject")
  
  # Ready for PKNCA
  conc_obj <- PKNCAconc(
    conc,
    formula = as.formula(
      sprintf("%s ~ %s | %s", conc_col, time_col, subj_col)
    )
  )
  dose_obj <- PKNCAdose(
    dose,
    formula = as.formula(
      sprintf("%s ~ %s | %s",
              get_mapped_column(dose, "dose"),
              get_mapped_column(dose, "time"),
              get_mapped_column(dose, "subject"))
    )
  )
  data_obj <- PKNCAdata(conc_obj, dose_obj)
  result   <- pk.nca(data_obj)
}