# Save the original state
original_state <- get("interval.cols", envir=PKNCA:::.PKNCAEnv)

test_that("add.interval.col", {
  # Invalid inputs fail
  expect_error(
    add.interval.col(name=1),
    regexp="name must be a character string",
    info="interval column name must be a character string"
  )
  expect_error(
    add.interval.col(name=c("a", "b")),
    regexp="name must have length",
    info="interval column name must be a scalar character string"
  )
  expect_error(
    add.interval.col(name="a", FUN=c("a", "b")),
    regexp="FUN must have length == 1",
    info="interval column function must be a scalar character string or NA"
  )
  expect_error(
    add.interval.col(name="a", FUN=1),
    regexp="FUN must be a character string or NA",
    info="interval column function must be a character string or NA"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, datatype="interval", desc="test addition"),
    regexp='argument "unit_type" is missing, with no default'
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="foo", datatype="interval", desc="test addition"),
    regexp="should be one of .*inverse_time"
  )
  
  # pretty_name checks
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name=1:2, datatype="interval", desc=1),
    regexp="pretty_name must be a scalar"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name=1, datatype="interval", desc=1),
    regexp="pretty_name must be a character"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name="", datatype="interval", desc=1),
    regexp="pretty_name must not be an empty string"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name="a", datatype="individual"),
    regexp="Only the 'interval' datatype is currently supported.",
    info="interval column datatype must be 'interval'"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name="a", datatype="interval", desc=1:2),
    regexp="desc must have length == 1",
    info="interval column description must be a scalar"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name="a", datatype="interval", desc=1),
    regexp="desc must be a character string",
    info="interval column description must be a character scalar"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, depends=1, unit_type="conc", pretty_name="a", datatype="interval", desc=1),
    regexp="'depends' must be NULL or a character vector",
    info="depends column must be NULL or a character string"
  )
  expect_error(
    add.interval.col(name="a", FUN="this function does not exist", unit_type="conc", pretty_name="foo", datatype="interval", desc="test addition"),
    regexp="The function named '.*' is not defined.  Please define the function before calling add.interval.col.",
    info="interval column function must exist (or be NA)"
  )
  
  # formalsmap
  expect_error(
    add.interval.col(name="a", FUN="mean", unit_type="conc", pretty_name="foo", formalsmap=NA),
    regexp="formalsmap must be a list"
  )
  expect_error(
    add.interval.col(name="a", FUN="mean", unit_type="conc", pretty_name="foo", formalsmap=list(1)),
    regexp="formalsmap must be a named list"
  )
  expect_error(
    add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name="foo", formalsmap=list(A="b")),
    regexp="formalsmap may not be given when FUN is NA",
    info="formalsmap cannot be used with FUN=NA"
  )
  expect_error(
    add.interval.col(name="a", FUN="mean", unit_type="conc", pretty_name="foo", formalsmap=list(A="a", "b")),
    regexp="All formalsmap elements must be named"
  )
  expect_error(
    add.interval.col(name="a", FUN="mean", unit_type="conc", pretty_name="a", formalsmap=list(y="a")),
    regexp="All names for the formalsmap list must be arguments to the function",
    info="formalsmap arguments must map to function arguments"
  )
  
  # Correct storage - FUN=NA
  expect_equal(
    {
      add.interval.col(name="a", FUN=NA, unit_type="conc", pretty_name="a", datatype="interval", desc="test addition")
      get("interval.cols", PKNCA:::.PKNCAEnv)[["a"]]
    },
    list(
      FUN=NA,
      values=c(FALSE, TRUE),
      unit_type="conc",
      pretty_name="a",
      desc="test addition",
      sparse=FALSE,
      formalsmap=list(),
      depends=NULL,
      datatype="interval"
    ),
    info="interval column assignment works with FUN=NA"
  )
  
  # Correct storage - FUN=character
  expect_equal(
    {
      add.interval.col(name="a", FUN="mean", unit_type="conc", pretty_name="a", datatype="interval", desc="test addition")
      get("interval.cols", PKNCA:::.PKNCAEnv)[["a"]]
    },
    list(
      FUN="mean",
      values=c(FALSE, TRUE),
      unit_type="conc",
      pretty_name="a",
      desc="test addition",
      sparse=FALSE,
      formalsmap=list(),
      depends=NULL,
      datatype="interval"
    ),
    info="interval column assignment works with FUN=a character string"
  )
  
  # Correct storage - with formalsmap
  expect_equal(
    {
      add.interval.col(name="a", FUN="mean", unit_type="conc", pretty_name="a", formalsmap=list(x="values"), desc="test addition")
      get("interval.cols", PKNCA:::.PKNCAEnv)[["a"]]
    },
    list(
      FUN="mean",
      values=c(FALSE, TRUE),
      unit_type="conc",
      pretty_name="a",
      desc="test addition",
      sparse=FALSE,
      formalsmap=list(x="values"),
      depends=NULL,
      datatype="interval"
    ),
    info="interval column assignment works with formalsmap"
  )
})

# Reset the original state
assign("interval.cols", original_state, envir=PKNCA:::.PKNCAEnv)
assign("interval.cols_sorted", NULL, envir=PKNCA:::.PKNCAEnv)

# ---------------------------------------------------------------------------
# sort.interval.cols — missing dependency (topological_sort integration)
# ---------------------------------------------------------------------------

test_that("fake parameters - missing dependency error from topological_sort", {
  add.interval.col(
    name="fake_parameter",
    FUN="mean",
    unit_type="conc",
    pretty_name="a",
    formalsmap=list(x="values"),
    desc="test addition",
    depends="does_not_exist"
  )
  expect_error(
    sort.interval.cols(),
    regexp="Missing interval column definitions for:.*does_not_exist",
    class="pknca_error_missing_dependency"
  )
})

# Reset the original state
assign("interval.cols", original_state, envir=PKNCA:::.PKNCAEnv)
assign("interval.cols_sorted", NULL, envir=PKNCA:::.PKNCAEnv)

# ---------------------------------------------------------------------------
# sort.interval.cols — circular dependency (topological_sort integration)
# ---------------------------------------------------------------------------

test_that("circular dependency detected via sort.interval.cols", {
  add.interval.col(name="loop1", FUN="mean", unit_type="conc", pretty_name="loop1", desc="loop1")
  add.interval.col(name="loop2", FUN="mean", unit_type="conc", pretty_name="loop2", desc="loop2", depends="loop1")
  # Manually inject circular dependency
  current <- get("interval.cols", envir=PKNCA:::.PKNCAEnv)
  current[["loop1"]]$depends <- "loop2"
  assign("interval.cols", current, envir=PKNCA:::.PKNCAEnv)
  assign("interval.cols_sorted", NULL, envir=PKNCA:::.PKNCAEnv)
  
  expect_error(
    sort.interval.cols(),
    class="pknca_error_circular_dependency"
  )
})

# Reset the original state
assign("interval.cols", original_state, envir=PKNCA:::.PKNCAEnv)
assign("interval.cols_sorted", NULL, envir=PKNCA:::.PKNCAEnv)

# ---------------------------------------------------------------------------
# topological_sort — unit tests
# ---------------------------------------------------------------------------

make_spec <- function(...) {
  args <- list(...)
  lapply(args, function(dep) list(depends = dep))
}

# ---------------------------------------------------------------------------
# 1. Happy-path tests
# ---------------------------------------------------------------------------

test_that("single node with no dependencies returns that node", {
  specs <- make_spec(a = NULL)
  expect_equal(topological_sort(specs), "a")
})

test_that("two independent nodes are both returned", {
  specs <- make_spec(b = NULL, a = NULL)
  expect_setequal(topological_sort(specs), c("a", "b"))
})

test_that("simple linear chain: a -> b -> c", {
  specs <- list(
    a = list(depends = NULL),
    b = list(depends = "a"),
    c = list(depends = "b")
  )
  result <- topological_sort(specs)
  expect_equal(result, c("a", "b", "c"))
  expect_lt(which(result == "a"), which(result == "b"))
  expect_lt(which(result == "b"), which(result == "c"))
})

test_that("diamond dependency: a -> b, a -> c, b -> d, c -> d", {
  specs <- list(
    a = list(depends = NULL),
    b = list(depends = "a"),
    c = list(depends = "a"),
    d = list(depends = c("b", "c"))
  )
  result <- topological_sort(specs)
  expect_length(result, 4)
  expect_lt(which(result == "a"), which(result == "b"))
  expect_lt(which(result == "a"), which(result == "c"))
  expect_lt(which(result == "b"), which(result == "d"))
  expect_lt(which(result == "c"), which(result == "d"))
})

test_that("node with multiple dependencies lists all deps before it", {
  specs <- list(
    x = list(depends = NULL),
    y = list(depends = NULL),
    z = list(depends = c("x", "y"))
  )
  result <- topological_sort(specs)
  expect_length(result, 3)
  expect_lt(which(result == "x"), which(result == "z"))
  expect_lt(which(result == "y"), which(result == "z"))
})

test_that("all nodes independent - all returned", {
  specs <- make_spec(delta = NULL, alpha = NULL, gamma = NULL, beta = NULL)
  expect_setequal(topological_sort(specs), names(specs))
})

test_that("empty depends vector treated same as NULL", {
  specs <- list(
    a = list(depends = character(0)),
    b = list(depends = "a")
  )
  result <- topological_sort(specs)
  expect_lt(which(result == "a"), which(result == "b"))
})

test_that("result contains every parameter exactly once", {
  specs <- list(
    a = list(depends = NULL),
    b = list(depends = "a"),
    c = list(depends = "a"),
    d = list(depends = c("b", "c")),
    e = list(depends = NULL)
  )
  result <- topological_sort(specs)
  expect_setequal(result, names(specs))
  expect_length(result, length(specs))
})

test_that("deterministic: same result on repeated calls", {
  specs <- list(
    e = list(depends = NULL),
    d = list(depends = NULL),
    c = list(depends = c("d", "e")),
    b = list(depends = "c"),
    a = list(depends = NULL)
  )
  expect_equal(topological_sort(specs), topological_sort(specs))
})

# ---------------------------------------------------------------------------
# 2. Structural validation errors
# ---------------------------------------------------------------------------

test_that("error when specs is not a list", {
  expect_error(topological_sort(c(a = 1, b = 2)))
})

test_that("error when specs is an unnamed list", {
  expect_error(topological_sort(list(list(depends = NULL))))
})

test_that("error when specs is empty", {
  expect_error(topological_sort(list()))
})

test_that("error when specs has duplicate names", {
  specs <- list(a = list(depends = NULL), a = list(depends = NULL))
  expect_error(topological_sort(specs))
})

test_that("error when a spec element is missing 'depends' field", {
  specs <- list(
    a = list(foo = NULL),
    b = list(depends = "a")
  )
  expect_error(
    topological_sort(specs),
    class = "pknca_error_invalid_spec_structure"
  )
})

test_that("error message for missing 'depends' field mentions the column name", {
  specs <- list(broken = list(value = 42))
  expect_error(
    topological_sort(specs),
    regexp = "broken",
    class = "pknca_error_invalid_spec_structure"
  )
})

test_that("error when depends is not a character vector", {
  specs <- list(
    a = list(depends = NULL),
    b = list(depends = 1L)
  )
  expect_error(topological_sort(specs))
})

test_that("error when depends contains NA", {
  specs <- list(
    a = list(depends = NULL),
    b = list(depends = NA_character_)
  )
  expect_error(topological_sort(specs))
})

# ---------------------------------------------------------------------------
# 3. Missing dependency errors
# ---------------------------------------------------------------------------

test_that("error when dependency references non-existent parameter", {
  specs <- list(
    a = list(depends = NULL),
    b = list(depends = c("a", "ghost"))
  )
  expect_error(
    topological_sort(specs),
    class = "pknca_error_missing_dependency"
  )
})

test_that("error message for missing dependency names the missing param", {
  specs <- list(b = list(depends = "missing_param"))
  err <- tryCatch(topological_sort(specs), error = function(e) e)
  expect_true(inherits(err, "pknca_error_missing_dependency"))
  expect_match(conditionMessage(err), "missing_param")
})

test_that("all missing dependencies are listed in the error", {
  specs <- list(a = list(depends = c("x", "y", "z")))
  err <- tryCatch(topological_sort(specs), error = function(e) e)
  expect_true(inherits(err, "pknca_error_missing_dependency"))
  expect_match(conditionMessage(err), "x")
  expect_match(conditionMessage(err), "y")
  expect_match(conditionMessage(err), "z")
})

# ---------------------------------------------------------------------------
# 4. Circular dependency errors
# ---------------------------------------------------------------------------

test_that("direct self-loop triggers circular dependency error", {
  specs <- list(a = list(depends = "a"))
  expect_error(
    topological_sort(specs),
    class = "pknca_error_circular_dependency"
  )
})

test_that("two-node cycle triggers circular dependency error", {
  specs <- list(
    a = list(depends = "b"),
    b = list(depends = "a")
  )
  expect_error(
    topological_sort(specs),
    class = "pknca_error_circular_dependency"
  )
})

test_that("three-node cycle triggers circular dependency error", {
  specs <- list(
    a = list(depends = "c"),
    b = list(depends = "a"),
    c = list(depends = "b")
  )
  expect_error(
    topological_sort(specs),
    class = "pknca_error_circular_dependency"
  )
})

test_that("cycle embedded in larger graph still triggers error", {
  specs <- list(
    root  = list(depends = NULL),
    good  = list(depends = "root"),
    loop1 = list(depends = "loop2"),
    loop2 = list(depends = "loop1")
  )
  expect_error(
    topological_sort(specs),
    class = "pknca_error_circular_dependency"
  )
})

test_that("circular dependency error message is informative", {
  specs <- list(a = list(depends = "b"), b = list(depends = "a"))
  err <- tryCatch(topological_sort(specs), error = function(e) e)
  expect_true(inherits(err, "pknca_error_circular_dependency"))
  expect_match(conditionMessage(err), "(?i)circular")
})

# ---------------------------------------------------------------------------
# 5. Edge cases
# ---------------------------------------------------------------------------

test_that("single node that depends on itself = circular error", {
  specs <- list(only = list(depends = "only"))
  expect_error(
    topological_sort(specs),
    class = "pknca_error_circular_dependency"
  )
})

test_that("large linear chain is handled correctly", {
  n <- 50
  nms <- paste0("p", seq_len(n))
  specs <- setNames(
    lapply(seq_len(n), function(i) {
      list(depends = if (i == 1) NULL else nms[i - 1])
    }),
    nms
  )
  result <- topological_sort(specs)
  expect_setequal(result, nms)
  for (i in seq_len(n - 1)) {
    expect_lt(which(result == nms[i]), which(result == nms[i + 1]))
  }
})

test_that("wide graph (many independent nodes) all returned", {
  nms <- paste0("node", 1:20)
  specs <- setNames(lapply(nms, function(nm) list(depends = NULL)), nms)
  result <- topological_sort(specs)
  expect_setequal(result, nms)
  expect_length(result, length(nms))
})

test_that("extra fields in spec are allowed and ignored", {
  specs <- list(
    a = list(depends = NULL, extra = "ignored", value = 42),
    b = list(depends = "a", meta = list(1, 2, 3))
  )
  result <- topological_sort(specs)
  expect_lt(which(result == "a"), which(result == "b"))
})