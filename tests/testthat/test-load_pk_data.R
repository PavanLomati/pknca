# =============================================================================
# HELPERS
# =============================================================================

make_conc_df <- function(n = 6) {
  data.frame(
    USUBJID = rep("SUBJ001", n),
    TIME    = seq(0, n - 1),
    CONC    = c(0, 10, 20, 15, 8, 3),
    stringsAsFactors = FALSE
  )
}

make_dose_df <- function() {
  data.frame(
    USUBJID = "SUBJ001",
    TIME    = 0,
    DOSE    = 100,
    stringsAsFactors = FALSE
  )
}

with_mapping <- function(df, mapping) {
  attr(df, "column_mapping") <- mapping
  class(df) <- c("pk_data", class(df))
  df
}

std_mapping <- function(subj = "USUBJID", time = "TIME",
                        conc = "CONC", dose = NA_character_) {
  list(subject = subj, time = time, conc = conc, dose = dose)
}


# =============================================================================
# 1. get_pk_patterns()
# =============================================================================

test_that("get_pk_patterns() returns a named list", {
  p <- get_pk_patterns()
  expect_type(p, "list")
  expect_named(p)
})

test_that("get_pk_patterns() contains required roles", {
  p <- get_pk_patterns()
  expect_true(all(c("conc", "dose", "subject", "time") %in% names(p)))
})

test_that("each pattern entry is a character vector", {
  p <- get_pk_patterns()
  for (role in names(p))
    expect_true(is.character(p[[role]]), info = paste("role:", role))
})

test_that("get_pk_patterns() conc patterns include common column names", {
  p <- get_pk_patterns()
  expect_true(any(grepl("conc", p$conc, ignore.case = TRUE)))
})

test_that("get_pk_patterns() subject patterns include USUBJID", {
  p <- get_pk_patterns()
  expect_true(any(grepl("usubjid", p$subject, ignore.case = TRUE)))
})


# =============================================================================
# 2. resolve_pk_column_roles()
# =============================================================================

test_that("detect mode returns TRUE when any PK column found", {
  result <- resolve_pk_column_roles(
    names_vec         = make_conc_df(),
    patterns          = get_pk_patterns(),
    mode              = "detect",
    stop_on_ambiguous = FALSE
  )
  expect_true(result)
})

test_that("detect_all mode returns named logical vector", {
  result <- resolve_pk_column_roles(
    names_vec         = make_conc_df(),
    patterns          = get_pk_patterns(),
    mode              = "detect_all",
    stop_on_ambiguous = FALSE
  )
  expect_type(result, "logical")
  expect_named(result)
})

test_that("match mode returns a named list of logical vectors", {
  result <- resolve_pk_column_roles(
    names_vec         = make_conc_df(),
    patterns          = get_pk_patterns(),
    mode              = "match",
    stop_on_ambiguous = FALSE
  )
  expect_type(result, "list")
  expect_named(result)
  for (role in names(result))
    expect_type(result[[role]], "logical")
})

test_that("CONC column is detected as 'conc' role", {
  result <- resolve_pk_column_roles(
    names_vec         = make_conc_df(),
    patterns          = get_pk_patterns(),
    mode              = "detect_all",
    stop_on_ambiguous = FALSE
  )
  expect_true(result["conc"])
})

test_that("TIME column is detected as 'time' role", {
  result <- resolve_pk_column_roles(
    names_vec         = make_conc_df(),
    patterns          = get_pk_patterns(),
    mode              = "detect_all",
    stop_on_ambiguous = FALSE
  )
  expect_true(result["time"])
})

test_that("empty names_vec returns logical(0) in match mode", {
  result <- resolve_pk_column_roles(
    names_vec         = character(0),
    patterns          = get_pk_patterns(),
    mode              = "match",
    stop_on_ambiguous = FALSE
  )
  expect_type(result, "list")
  for (role in names(result))
    expect_length(result[[role]], 0)
})

test_that("empty names_vec returns FALSE in detect mode", {
  result <- resolve_pk_column_roles(
    names_vec         = character(0),
    patterns          = get_pk_patterns(),
    mode              = "detect",
    stop_on_ambiguous = FALSE
  )
  expect_false(result)
})

test_that("duplicate column names abort with informative error", {
  expect_error(
    resolve_pk_column_roles(
      names_vec         = c("conc", "CONC"),
      patterns          = get_pk_patterns(),
      mode              = "match",
      stop_on_ambiguous = FALSE
    ),
    regexp = "(?i)duplicate"
  )
})

test_that("ambiguous columns abort when stop_on_ambiguous = TRUE", {
  expect_error(
    resolve_pk_column_roles(
      names_vec         = c("conc", "concentration", "TIME", "USUBJID"),
      patterns          = get_pk_patterns(),
      mode              = "match",
      stop_on_ambiguous = TRUE
    ),
    regexp = "(?i)ambiguous"
  )
})

test_that("ambiguous columns allowed when stop_on_ambiguous = FALSE", {
  expect_no_error(
    resolve_pk_column_roles(
      names_vec         = c("conc", "concentration", "TIME", "USUBJID"),
      patterns          = get_pk_patterns(),
      mode              = "match",
      stop_on_ambiguous = FALSE
    )
  )
})


# =============================================================================
# 3. create_column_mapping()
# =============================================================================

test_that("create_column_mapping() returns a named list", {
  mapping <- create_column_mapping(names(make_conc_df()), get_pk_patterns())
  expect_type(mapping, "list")
  expect_named(mapping)
})

test_that("conc role is mapped to CONC column", {
  mapping <- create_column_mapping(names(make_conc_df()), get_pk_patterns())
  expect_equal(mapping$conc, "CONC")
})

test_that("time role is mapped to TIME column", {
  mapping <- create_column_mapping(names(make_conc_df()), get_pk_patterns())
  expect_equal(mapping$time, "TIME")
})

test_that("subject role is mapped to USUBJID", {
  mapping <- create_column_mapping(names(make_conc_df()), get_pk_patterns())
  expect_equal(mapping$subject, "USUBJID")
})

test_that("unrecognised columns get NA mapping", {
  mapping <- create_column_mapping(c("alpha", "beta", "gamma"), get_pk_patterns())
  for (role in names(mapping))
    expect_true(is.na(mapping[[role]]), info = paste("role:", role))
})


# =============================================================================
# 4. get_mapped_column()
# =============================================================================

test_that("get_mapped_column() returns the correct column name", {
  df <- with_mapping(make_conc_df(), std_mapping())
  expect_equal(get_mapped_column(df, "time"), "TIME")
  expect_equal(get_mapped_column(df, "conc"), "CONC")
})

test_that("get_mapped_column() aborts when no mapping attribute", {
  expect_error(get_mapped_column(make_conc_df(), "time"), class = "error")
})

test_that("get_mapped_column() aborts for unmapped role", {
  df <- with_mapping(make_conc_df(), std_mapping(dose = NA_character_))
  expect_error(get_mapped_column(df, "dose"), regexp = "(?i)not found", perl = TRUE)
})

test_that("get_mapped_column() error lists available roles", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  err <- tryCatch(get_mapped_column(df, "dose"), error = function(e) e)
  expect_match(conditionMessage(err), "(?i)available", perl = TRUE)
})


# =============================================================================
# 5. quick_validate()
# =============================================================================

test_that("quick_validate() returns TRUE invisibly when conc present", {
  result <- list(conc = make_conc_df(), dose = NULL)
  expect_true(quick_validate(result))
})

test_that("quick_validate() returns TRUE invisibly when dose present", {
  result <- list(conc = NULL, dose = make_dose_df())
  expect_true(quick_validate(result))
})

test_that("quick_validate() aborts when both conc and dose are NULL", {
  expect_error(
    quick_validate(list(conc = NULL, dose = NULL)),
    regexp = "(?i)no valid PK data"
  )
})


# =============================================================================
# 6. remove_empty_data()
# =============================================================================

test_that("remove_empty_data() preserves column_mapping attribute", {
  m   <- std_mapping()
  df  <- with_mapping(make_conc_df(), m)
  out <- remove_empty_data(df, verbose = FALSE)
  expect_equal(attr(out, "column_mapping"), m)
})

test_that("remove_empty_data() removes all-NA rows", {
  df       <- make_conc_df()
  df[7, ]  <- NA
  df       <- with_mapping(df, std_mapping())
  out      <- remove_empty_data(df, verbose = FALSE)
  expect_lt(nrow(out), nrow(df))
})

test_that("remove_empty_data() removes all-NA columns", {
  df       <- make_conc_df()
  df$EMPTY <- NA
  df       <- with_mapping(df, std_mapping())
  out      <- remove_empty_data(df, verbose = FALSE)
  expect_false("EMPTY" %in% names(out))
})

test_that("remove_empty_data() preserves class", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- remove_empty_data(df, verbose = FALSE)
  expect_s3_class(out, "pk_data")
})


# =============================================================================
# 7. auto_create_subject_id()
# =============================================================================

test_that("auto_create_subject_id() adds ID column when subject is NA", {
  df        <- make_conc_df()
  df$USUBJID <- NULL
  df        <- with_mapping(df, std_mapping(subj = NA_character_))
  out       <- auto_create_subject_id(df)
  expect_true("ID" %in% names(out))
  expect_true(all(out$ID == "SUBJ001"))
})

test_that("auto_create_subject_id() does not add ID when subject already mapped", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- auto_create_subject_id(df)
  expect_false("ID" %in% names(out))
})

test_that("auto_create_subject_id() updates column_mapping$subject to 'ID'", {
  df        <- make_conc_df()
  df$USUBJID <- NULL
  df        <- with_mapping(df, std_mapping(subj = NA_character_))
  out       <- auto_create_subject_id(df)
  expect_equal(attr(out, "column_mapping")$subject, "ID")
})

test_that("auto_create_subject_id() warns about duplicate times when no subject", {
  df <- data.frame(TIME = c(0, 0, 1, 2), CONC = c(5, 10, 8, 3),
                   stringsAsFactors = FALSE)
  df <- with_mapping(df, std_mapping(subj = NA_character_))
  expect_warning(auto_create_subject_id(df), regexp = "(?i)duplicate", perl = TRUE)
})


# =============================================================================
# 8. count_decimal_places()
# =============================================================================

test_that("count_decimal_places() returns 0 for integer", {
  expect_equal(count_decimal_places(5), 0)
})

test_that("count_decimal_places() returns 2 for 1.23", {
  expect_equal(count_decimal_places(1.23), 2)
})

test_that("count_decimal_places() returns 3 for 0.001", {
  expect_equal(count_decimal_places(0.001), 3)
})

test_that("count_decimal_places() returns 0 for NA", {
  expect_equal(count_decimal_places(NA_real_), 0)
})

test_that("count_decimal_places() returns 0 for Inf", {
  expect_equal(count_decimal_places(Inf), 0)
})

test_that("count_decimal_places() returns 0 for NaN", {
  expect_equal(count_decimal_places(NaN), 0)
})


# =============================================================================
# 9. decimal_formatter()
# =============================================================================

test_that("decimal_formatter() returns data frame unchanged if role column missing", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- decimal_formatter(df, col_max_map = list(dose = 2), verbose = FALSE)
  expect_equal(nrow(out), nrow(df))
  expect_equal(ncol(out), ncol(df))
})

test_that("decimal_formatter() stores decimal_info attribute on data frame", {
  # FIX: decimal precision is stored on the df (decimal_info), not on the column
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- decimal_formatter(df, col_max_map = list(conc = 3), verbose = FALSE)
  expect_false(is.null(attr(out, "decimal_info")))
  expect_true("CONC" %in% names(attr(out, "decimal_info")))
})

test_that("decimal_formatter() respects col_max_map cap", {
  df       <- make_conc_df()
  df$CONC  <- c(0.12345, 1.23456, 2.34567, 3.45678, 4.56789, 5.67890)
  df       <- with_mapping(df, std_mapping())
  out      <- decimal_formatter(df, col_max_map = list(conc = 2), verbose = FALSE)
  dp       <- attr(out, "decimal_info")[["CONC"]]
  expect_lte(dp, 2)
})

test_that("decimal_formatter() aborts with missing column_mapping", {
  expect_error(
    decimal_formatter(make_conc_df(), col_max_map = list(conc = 3), verbose = FALSE),
    regexp = "(?i)mapping"
  )
})


# =============================================================================
# 10. interpolate_subject()
# =============================================================================

test_that("interpolate_subject() returns same number of rows", {
  sub <- make_conc_df()
  out <- interpolate_subject(sub, time_col = "TIME", conc_col = "CONC",
                             verbose = FALSE)
  expect_equal(nrow(out), nrow(sub))
})

test_that("interpolate_subject() adds method column", {
  out <- interpolate_subject(make_conc_df(), time_col = "TIME", conc_col = "CONC",
                             verbose = FALSE)
  expect_true("method" %in% names(out))
})

test_that("interpolate_subject() fills middle NA with interpolated values", {
  sub        <- make_conc_df()
  sub$CONC[3] <- NA
  out        <- interpolate_subject(sub, time_col = "TIME", conc_col = "CONC",
                                    verbose = FALSE)
  expect_false(any(is.na(out$CONC)))
})

test_that("interpolate_subject() sets pre-dose values to zero", {
  sub          <- make_conc_df()
  sub$CONC[1]  <- NA
  out          <- interpolate_subject(sub, time_col = "TIME", conc_col = "CONC",
                                      verbose = FALSE)
  expect_equal(out$CONC[1], 0)
})

test_that("interpolate_subject() handles all-NA concentration gracefully", {
  sub       <- make_conc_df()
  sub$CONC  <- NA_real_
  out       <- interpolate_subject(sub, time_col = "TIME", conc_col = "CONC",
                                   verbose = FALSE)
  expect_true(all(out$CONC == 0))
  expect_true(all(out$method == "all-blq"))
})

test_that("interpolate_subject() method is 'observed' for non-BLQ rows", {
  out <- interpolate_subject(make_conc_df(), time_col = "TIME", conc_col = "CONC",
                             verbose = FALSE)
  expect_true(any(out$method == "observed"))
})

test_that("interpolate_subject() returns empty df unchanged", {
  out <- interpolate_subject(make_conc_df()[0, ], time_col = "TIME",
                             conc_col = "CONC", verbose = FALSE)
  expect_equal(nrow(out), 0)
})


# =============================================================================
# 11. clean_blq_values()
# =============================================================================

test_that("clean_blq_values() converts BLQ strings to NA then interpolates", {
  df         <- make_conc_df()
  df$CONC    <- as.character(df$CONC)
  df$CONC[3] <- "BLQ"
  df         <- with_mapping(df, std_mapping())
  expect_no_error(clean_blq_values(df, verbose = FALSE))
})

test_that("clean_blq_values() preserves column_mapping", {
  m   <- std_mapping()
  df  <- with_mapping(make_conc_df(), m)
  out <- clean_blq_values(df, verbose = FALSE)
  expect_equal(attr(out, "column_mapping"), m)
})

test_that("clean_blq_values() adds method column", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- clean_blq_values(df, verbose = FALSE)
  expect_true("method" %in% names(out))
})

test_that("clean_blq_values() filters negative time rows", {
  df         <- make_conc_df()
  df$TIME[1] <- -1
  df         <- with_mapping(df, std_mapping())
  out        <- suppressWarnings(clean_blq_values(df, verbose = FALSE))
  expect_true(all(out$TIME >= 0))
})

test_that("clean_blq_values() preserves pk_data class", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- clean_blq_values(df, verbose = FALSE)
  expect_s3_class(out, "pk_data")
})


# =============================================================================
# 12. process_conc_data() & process_dose_data()
# =============================================================================

test_that("process_conc_data() returns a data frame", {
  df  <- with_mapping(make_conc_df(), std_mapping())
  out <- process_conc_data(df, decimal_control = TRUE,
                           blq_handling = TRUE, verbose = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("process_conc_data() preserves column_mapping", {
  m   <- std_mapping()
  df  <- with_mapping(make_conc_df(), m)
  out <- process_conc_data(df, decimal_control = FALSE,
                           blq_handling = FALSE, verbose = FALSE)
  expect_equal(attr(out, "column_mapping"), m)
})

test_that("process_dose_data() returns a data frame", {
  df  <- with_mapping(make_dose_df(),
                      list(subject = "USUBJID", time = "TIME",
                           conc = NA_character_, dose = "DOSE"))
  out <- process_dose_data(df, decimal_control = TRUE, verbose = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("process_dose_data() preserves column_mapping", {
  m   <- list(subject = "USUBJID", time = "TIME",
              conc = NA_character_, dose = "DOSE")
  df  <- with_mapping(make_dose_df(), m)
  out <- process_dose_data(df, decimal_control = FALSE, verbose = FALSE)
  expect_equal(attr(out, "column_mapping"), m)
})


# =============================================================================
# 13. quick_validate() edge cases
# =============================================================================

test_that("quick_validate() is invisible", {
  result <- list(conc = make_conc_df(), dose = NULL)
  expect_invisible(quick_validate(result))
})

test_that("quick_validate() passes when both conc and dose present", {
  result <- list(conc = make_conc_df(), dose = make_dose_df())
  expect_true(quick_validate(result))
})