# =============================================================================
# tests/verify_content.R — check the tutorial's content against the database
#
# Run from the project root:   Rscript tests/verify_content.R
# Exits non-zero if anything fails. Not shipped with the Shinylive bundle's
# runtime path; it is a development check.
#
# What it checks
#   1. Every R file parses.
#   2. Every read-only quest: the reference query and the shown solution both
#      run, the solution passes the grader, and a wrong answer fails it.
#   3. Every order-graded quest sorts by a TOTAL order (no ties the grader
#      could trip on): the reference result has no duplicate sort outcome.
#   4. Every writing task (Changing data, Tables): the task is not already
#      satisfied on a fresh copy, its solution satisfies it, and all tasks plus
#      all in-step demos can be applied to ONE copy, in either order, with
#      every task still passing (independence).
#   5. Every sql_code() block and sql_editor() starter in every tab runs.
#      Statements that are SUPPOSED to fail (they demonstrate a constraint)
#      are listed in EXPECTED_FAILURES and must fail.
#   6. The scratch copy keeps the real schema (PKs, NOT NULL, defaults, FKs,
#      indexes), which the writing tabs' prose depends on.
#   7. Every entry of KEYWORD_INDEX appears in the tab file it is listed under.
#   8. Every chip on the execution-order ribbon is lit on some tab, and every
#      routing card on the Start tab names a real nav panel.
#   9. The district material (R/district_helpers.R): every *_DQUEST runs on the
#      district database with the same checks as 2-3; every dsql_code() block
#      and every "dz" editor starter runs there; every *_DTASKS writing task is
#      unsatisfied before and satisfied after on a district copy, and all of them
#      coexist with the district demos on the writing tabs, in either order.
# =============================================================================

suppressMessages({ library(shiny); library(bslib); library(DBI); library(RSQLite) })

fails <- character()
fail  <- function(...) { msg <- paste0(...); fails <<- c(fails, msg); cat("  FAIL:", msg, "\n") }
ok    <- function(...) cat("  ok:  ", paste0(...), "\n")
section <- function(x) cat("\n==", x, "==\n")

r_files <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE), "data/build_db.R",
             "data/build_district.R", "data/generate_district.R", "tests/district_claims.R")

# ---- 1. parse ---------------------------------------------------------------
section("1. parse")
for (f in r_files) tryCatch(invisible(parse(f)), error = function(e) fail(f, ": ", conditionMessage(e)))
ok(length(r_files), " files parse")

# Load the app's definitions (shinyApp() at the end only builds an object).
sys.source("app.R", envir = globalenv())

ro <- institute_con_ro()
dro <- district_con_ro()
quiet <- function(expr) suppressWarnings(expr)

# ---- 2 + 3. read-only quests -------------------------------------------------
section("2-3. read-only quests")
ro_quests <- list(SELECT = SELECT_QUEST, WHERE = WHERE_QUEST, ORDER = ORDER_QUEST,
                  GROUP = GROUP_QUEST, FUNC = FUNC_QUEST, JOINS = JOINS_QUEST,
                  UNION = UNION_QUEST, SUBQ = SUBQ_QUEST, WIN = WIN_QUEST,
                  CAPSTONE = CAPSTONE_QUEST)
check_quests <- function(qs, con, nm, n_expected = NULL) {
  n <- 0
  if (!is.null(n_expected) && length(qs) != n_expected) fail(nm, ": has ", length(qs), " questions, not ", n_expected)
  for (i in seq_along(qs)) {
    q <- qs[[i]]; id <- sprintf("%s q%d", nm, i); n <- n + 1
    exp <- tryCatch(dbGetQuery(con, q$ref), error = function(e) { fail(id, " ref: ", conditionMessage(e)); NULL })
    if (is.null(exp)) next
    if (nrow(exp) == 0) fail(id, ": reference returns zero rows (nothing to grade)")
    sol <- run_query(con, q$solution)
    if (!isTRUE(sol$ok)) { fail(id, " solution: ", sol$error); next }
    v <- check_answer(sol$data, exp, ordered = isTRUE(q$ordered))
    if (!isTRUE(v$pass)) fail(id, ": shown solution does not pass the grader (", v$msg, ")")
    w <- check_answer(data.frame(x = 1), exp, ordered = isTRUE(q$ordered))
    if (isTRUE(w$pass)) fail(id, ": a wrong answer passes")
    if (isTRUE(q$ordered)) {
      # A total order leaves no two rows interchangeable: all rows distinct.
      if (anyDuplicated(exp) > 0 && length(unique(exp[[1]])) != nrow(exp) && ncol(exp) > 1)
        fail(id, ": order-graded but the reference has fully duplicated rows")
      # Re-run with the row order of the source perturbed: a second, reversed
      # tiebreak on rowid must give the same sequence if the order is total.
      ref2 <- sub(";\\s*$", "", q$ref)
      if (!grepl("\\bUNION\\b|\\bLIMIT\\b", ref2, ignore.case = TRUE)) {
        alt <- tryCatch(dbGetQuery(con, paste0(ref2, ", rowid DESC")), error = function(e) NULL)
        if (!is.null(alt) && !isTRUE(all.equal(alt, exp, check.attributes = FALSE)))
          fail(id, ": order-graded, but ties exist (a different tiebreak changes the row order)")
      } else if (grepl("\\bLIMIT\\b", ref2, ignore.case = TRUE)) {
        parts <- regmatches(ref2, regexec("^(.*?)(\\s+LIMIT\\b.*)$", ref2, ignore.case = TRUE))[[1]]
        alt <- tryCatch(dbGetQuery(con, paste0(parts[2], ", rowid DESC", parts[3])), error = function(e) NULL)
        if (!is.null(alt) && !isTRUE(all.equal(alt, exp, check.attributes = FALSE)))
          fail(id, ": order-graded with LIMIT, but ties exist at or inside the cut")
      }
    }
  }
  n
}
n_q <- 0
for (nm in names(ro_quests))
  n_q <- n_q + check_quests(ro_quests[[nm]], ro, nm, if (nm == "CAPSTONE") 20 else 10)
ok(n_q, " read-only questions checked")

# ---- 6. scratch copy keeps the schema ---------------------------------------
section("6. scratch copy schema")
sc <- institute_con_scratch()
chk <- function(desc, cond) if (isTRUE(cond)) ok(desc) else fail(desc)
chk("duplicate primary key is refused",
    inherits(try(dbExecute(sc, "INSERT INTO labs (lab_id, lab_name, founded_year) VALUES (1, 'x', 2000)"), silent = TRUE), "try-error"))
chk("NOT NULL is enforced",
    inherits(try(dbExecute(sc, "INSERT INTO labs (lab_id, founded_year) VALUES (50, 2000)"), silent = TRUE), "try-error"))
dbExecute(sc, "INSERT INTO enrollments (enrollment_id, study_id, participant_id) VALUES (900, 1, 1)")
chk("DEFAULT is applied (enrollments.dropped_out = 0)",
    dbGetQuery(sc, "SELECT dropped_out FROM enrollments WHERE enrollment_id = 900")[[1]] == 0)
chk("four indexes survive", dbGetQuery(sc, "SELECT COUNT(*) FROM sqlite_master WHERE type = 'index' AND name LIKE 'idx_%'")[[1]] == 4)
chk("six foreign keys are declared",
    sum(vapply(c("researchers","studies","enrollments","measurements"), function(t)
      nrow(dbGetQuery(sc, sprintf("SELECT * FROM pragma_foreign_key_list('%s')", t))), 1L)) == 6)
chk("foreign keys start OFF", dbGetQuery(sc, "PRAGMA foreign_keys")[[1]] == 0)
chk("participants_old exists on the copy only",
    dbExistsTable(sc, "participants_old") && !dbExistsTable(ro, "participants_old"))
chk("row counts match the shipped database", all(vapply(
  c("labs","researchers","studies","participants","measurements"), function(t)
    dbGetQuery(sc, paste("SELECT COUNT(*) FROM", t))[[1]] == dbGetQuery(ro, paste("SELECT COUNT(*) FROM", t))[[1]], TRUE)))
dbDisconnect(sc)

# ---- static extraction of SQL shown in the UI --------------------------------
# Walk each tab file's parse tree for sql_code("...") and sql_editor(starter=).
# A SQL argument is usually a string literal; a few are built by a helper call
# (att_sql("...") on the Attendance tab). Evaluate those to get the text.
sql_literal <- function(x) {
  if (is.character(x) || is.null(x)) return(x)
  if (is.call(x)) return(tryCatch(eval(x, globalenv()), error = function(e) NULL))
  NULL
}
extract_sql <- function(file) {
  out <- list()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- if (is.name(e[[1]])) as.character(e[[1]]) else ""
      if (fn %in% c("sql_code", "dsql_code") && length(e) >= 2 && is.character(sql_literal(e[[2]])))
        out[[length(out) + 1]] <<- list(kind = "code", sql = sql_literal(e[[2]]),
          db = if (fn == "dsql_code") "district" else "institute")
      if (fn %in% c("sql_editor", "dsql_editor")) {
        a <- as.list(e)[-1]
        nms <- names(a); if (is.null(nms)) nms <- rep("", length(a))
        pos <- a[nms == ""]
        id <- if (!is.null(a$id)) a$id else if (length(pos) >= 1) pos[[1]] else ""
        st <- if (!is.null(a$starter)) a$starter else if (fn == "dsql_editor" && length(pos) >= 2) pos[[2]] else NULL
        st <- sql_literal(st)
        if (!is.null(st) && is.character(st) && nzchar(st))
          out[[length(out) + 1]] <<- list(kind = "starter", sql = st,
            db = if (is.character(id) && startsWith(id, "dz")) "district" else "institute")
      }
      kids(e)
    } else if (is.pairlist(e) || is.expression(e)) kids(e)
  }
  # Empty arguments (the blank in x[, 1]) are the "missing" symbol; skip them.
  kids <- function(e) {
    l <- as.list(e)
    for (i in seq_along(l)) {
      if (is.symbol(l[[i]]) && identical(as.character(l[[i]]), "")) next
      walk(l[[i]])
    }
  }
  walk(parse(file))
  out
}
# R/district_sections.R holds one function per tab's district section. The
# writing tabs' sections (dsec_changing, dsec_tables) are demos on the district
# scratch copy; every other section is read-only. Extract each function's SQL
# separately so it is checked against the right database in the right mode.
extract_sql_by_function <- function(file) {
  ex <- parse(file); out <- list()
  for (e in as.list(ex)) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) &&
        identical(e[[3]][[1]], as.name("function"))) {
      nm <- as.character(e[[2]])
      tmp <- tempfile(fileext = ".R"); writeLines(deparse(e[[3]][[3]], width.cutoff = 500L), tmp)
      out[[nm]] <- tryCatch(extract_sql(tmp), error = function(err) list())
    }
  }
  out
}
DSEC <- extract_sql_by_function(file.path("R", "district_sections.R"))
DSEC_WRITING <- c("dsec_changing", "dsec_tables")

# Split a block into statements: drop -- comments, split on ';'.
split_statements <- function(sql) {
  sql <- gsub("--[^\n]*", "", sql)
  parts <- trimws(strsplit(sql, ";", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}

# Statements that exist to be refused. Matched as fixed substrings.
EXPECTED_FAILURES <- c(
  "INSERT INTO equipment (serial_no, cost) VALUES ('SG-002', 12.00)",          # NOT NULL
  "INSERT INTO equipment (item, cost) VALUES ('Thermometer, cricket-sized', -5)", # CHECK
  "DELETE FROM labs WHERE lab_id = 6"                                          # FK, enforcement ON
)

# ---- 5. SQL shown on the read-only tabs --------------------------------------
section("5a. SQL shown on the read-only tabs")
ro_tabs <- c("tab_start", "tab_select", "tab_where", "tab_order", "tab_group", "tab_functions",
             "tab_joins", "tab_union", "tab_subqueries", "tab_windows", "tab_capstone")
n_s <- 0
for (tb in ro_tabs) for (blk in extract_sql(file.path("R", paste0(tb, ".R")))) {
  if (grepl("^library\\(DBI\\)", blk$sql)) next            # the R snippet on the last tab
  if (blk$db == "district") next                              # checked in section 9
  for (st in split_statements(blk$sql)) {
    n_s <- n_s + 1
    r <- run_query(ro, st)
    if (!isTRUE(r$ok)) fail(tb, ": ", substr(gsub("\\s+", " ", st), 1, 70), " -> ", gsub("\n", " ", r$error))
  }
}
ok(n_s, " statements from read-only tabs run")

# ---- 4 + 5b. writing tabs ----------------------------------------------------
section("4-5b. writing tabs")
w_quests <- list(CRUD = CRUD_QUEST, TABLES = TABLES_QUEST)
for (nm in names(w_quests)) {
  qs <- w_quests[[nm]]
  if (length(qs) != 10) fail(nm, ": has ", length(qs), " tasks, not 10")
  for (i in seq_along(qs)) {
    q <- qs[[i]]; id <- sprintf("%s task %d", nm, i)
    c1 <- institute_con_scratch()
    before <- tryCatch(dbGetQuery(c1, q$verify), error = function(e) NULL)
    r <- run_query(c1, q$solution, allow_mutation = TRUE)
    if (!isTRUE(r$ok)) { fail(id, " solution: ", r$error); dbDisconnect(c1); next }
    if (!is.null(r$note)) fail(id, ": solution is more than one statement")
    after <- dbGetQuery(c1, q$verify)
    if (!is.null(before) && isTRUE(check_answer(before, after)$pass))
      fail(id, ": already satisfied on a fresh copy (the task checks nothing)")
    dbDisconnect(c1)
  }
}
ok("each task: unsatisfied before, satisfied after")

expected_for <- function(q) { c1 <- institute_con_scratch(); on.exit(dbDisconnect(c1))
  dbExecute(c1, q$solution); dbGetQuery(c1, q$verify) }
all_tasks <- c(CRUD_QUEST, TABLES_QUEST)
all_expected <- lapply(all_tasks, expected_for)

demo_statements <- unlist(lapply(c("tab_changing", "tab_tables"), function(tb)
  unlist(lapply(Filter(function(b) b$db == "institute", extract_sql(file.path("R", paste0(tb, ".R")))),
                function(b) split_statements(b$sql)))))
# The same statement is shown twice (code block, then editor starter), sometimes
# with different comments or spacing; a learner runs it once.
demo_statements <- demo_statements[!duplicated(gsub("\\s+", " ", demo_statements))]

run_everything <- function(task_order, label) {
  c1 <- institute_con_scratch(); on.exit(dbDisconnect(c1))
  for (st in demo_statements) {
    r <- run_query(c1, st, allow_mutation = TRUE)
    should_fail <- any(vapply(EXPECTED_FAILURES, function(x) grepl(x, st, fixed = TRUE), TRUE))
    if (should_fail && isTRUE(r$ok))  fail(label, ": expected a refusal but it ran: ", substr(st, 1, 60))
    if (!should_fail && !isTRUE(r$ok)) fail(label, ": demo failed: ", substr(gsub("\\s+", " ", st), 1, 60), " -> ", gsub("\n", " ", r$error))
  }
  if (dbGetQuery(c1, "PRAGMA foreign_keys")[[1]] != 0) fail(label, ": demos leave foreign keys ON")
  for (i in task_order) {
    r <- run_query(c1, all_tasks[[i]]$solution, allow_mutation = TRUE)
    if (!isTRUE(r$ok)) fail(label, ": task ", i, " failed after the others: ", gsub("\n", " ", r$error))
  }
  for (i in seq_along(all_tasks)) {
    got <- tryCatch(dbGetQuery(c1, all_tasks[[i]]$verify), error = function(e) NULL)
    if (!isTRUE(check_answer(got, all_expected[[i]])$pass))
      fail(label, ": task ", i, " no longer verifies once everything else has run")
  }
}
run_everything(seq_along(all_tasks), "demos then tasks in order")
run_everything(rev(seq_along(all_tasks)), "demos then tasks reversed")
ok(length(demo_statements), " demo statements and ", length(all_tasks), " tasks coexist on one copy, both orders")

# A multi-row INSERT that breaks a rule inserts nothing (claimed in the text).
c1 <- institute_con_scratch()
try(dbExecute(c1, "INSERT INTO labs (lab_id, lab_name, founded_year) VALUES (60, 'a', 2000), (1, 'dup', 2000)"), silent = TRUE)
chk("failed multi-row INSERT leaves no partial rows", dbGetQuery(c1, "SELECT COUNT(*) FROM labs WHERE lab_id = 60")[[1]] == 0)
# Two statements in one Run: first runs, a note is attached.
r <- run_query(c1, "SELECT 1; SELECT 2;", allow_mutation = TRUE)
chk("second statement in one Run is reported, not silently dropped", isTRUE(r$ok) && !is.null(r$note))
# The read-only guard.
chk("read-only tabs decline a DELETE hidden behind a comment",
    !isTRUE(run_query(ro, "-- tidy up\nDELETE FROM labs;")$ok))
dbDisconnect(c1)

# ---- 7. keyword index --------------------------------------------------------
section("7. keyword index")
tab_file <- c("Start here" = "tab_start", "SELECT" = "tab_select", "WHERE" = "tab_where",
              "ORDER BY" = "tab_order", "GROUP BY" = "tab_group", "Functions" = "tab_functions",
              "JOINs" = "tab_joins", "UNION" = "tab_union", "Subqueries" = "tab_subqueries",
              "Windows" = "tab_windows", "Changing data" = "tab_changing", "Tables" = "tab_tables",
              "Put it together" = "tab_capstone")
if (!setequal(names(KEYWORD_INDEX), names(tab_file))) fail("KEYWORD_INDEX tabs do not match the nav")
n_k <- 0
for (tab in names(KEYWORD_INDEX)) {
  src <- paste(readLines(file.path("R", paste0(tab_file[[tab]], ".R")), warn = FALSE), collapse = "\n")
  if (tab == "Start here") src <- paste(src, paste(readLines("R/sql_helpers.R", warn = FALSE), collapse = "\n"))
  for (kw in KEYWORD_INDEX[[tab]]) {
    n_k <- n_k + 1
    if (!grepl(kw, src, fixed = TRUE) && !grepl(tolower(kw), tolower(src), fixed = TRUE))
      fail("keyword '", kw, "' is indexed under ", tab, " but does not appear in ", tab_file[[tab]], ".R")
  }
}
ok(n_k, " indexed keywords found in their tabs")

# ---- 8. ribbon chips and routing ---------------------------------------------
section("8. ribbon and routing")
all_src <- paste(unlist(lapply(list.files("R", full.names = TRUE), readLines, warn = FALSE)), collapse = "\n")
lit <- unique(unlist(regmatches(all_src, gregexpr("active = c\\([^)]*\\)", all_src))))
lit_keys <- unique(unlist(regmatches(lit, gregexpr("\"[^\"]+\"", lit))))
lit_keys <- gsub("\"", "", lit_keys)
for (k in names(EXEC_ORDER)) if (!k %in% lit_keys) fail("ribbon chip '", k, "' is never lit on any tab")
for (k in lit_keys) if (!k %in% names(EXEC_ORDER)) fail("a tab lights unknown ribbon key '", k, "'")
ok(length(EXEC_ORDER), " ribbon chips, each lit somewhere")
chk("Start tab's \"SELECT runs sixth of ten\" matches the ribbon",
    length(EXEC_ORDER) == 10 && which(names(EXEC_ORDER) == "SELECT") == 6)
app_src <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
navs <- gsub("nav_panel\\(title = \"|\"", "", unlist(regmatches(app_src, gregexpr("nav_panel\\(title = \"[^\"]+\"", app_src))))
routes <- gsub("start_route\\(\"|\"", "", unlist(regmatches(all_src, gregexpr("start_route\\(\"[^\"]+\"", all_src))))
for (r in routes) if (!r %in% navs) fail("routing card '", r, "' has no nav panel")
for (n in setdiff(navs, "Start here")) if (!n %in% routes) fail("nav panel '", n, "' has no routing card")
ok(length(routes), " routing cards match the nav")

# ---- 9. district material ---------------------------------------------------
section("9. district")
dq_names <- ls(globalenv(), pattern = "_DQUEST$")
n_dq <- 0
for (nm in dq_names) n_dq <- n_dq + check_quests(get(nm), dro, nm)
ok(length(dq_names), " district quests, ", n_dq, " questions checked")

# Ids of graded district editors must not collide with demo editors.
n_ds <- 0
for (f in list.files("R", pattern = "^tab_.*\\.R$", full.names = TRUE)) {
  if (basename(f) %in% c("tab_changing.R", "tab_tables.R")) next
  for (blk in extract_sql(f)) {
    if (blk$db != "district") next
    for (st in split_statements(blk$sql)) {
      n_ds <- n_ds + 1
      r <- run_query(dro, st)
      if (!isTRUE(r$ok)) fail(basename(f), " (district): ", substr(gsub("\\s+", " ", st), 1, 70), " -> ", gsub("\n", " ", r$error))
    }
  }
}
for (fn in setdiff(names(DSEC), DSEC_WRITING)) for (blk in DSEC[[fn]]) {
  if (blk$db != "district") next
  for (st in split_statements(blk$sql)) {
    n_ds <- n_ds + 1
    r <- run_query(dro, st)
    if (!isTRUE(r$ok)) fail(fn, " (district): ", substr(gsub("\\s+", " ", st), 1, 70), " -> ", gsub("\n", " ", r$error))
  }
}
ok(n_ds, " district statements from read-only tabs run")

dt_names <- ls(globalenv(), pattern = "_DTASKS$")
d_tasks <- unlist(lapply(dt_names, get), recursive = FALSE)
for (i in seq_along(d_tasks)) {
  q <- d_tasks[[i]]; id <- sprintf("district task %d", i)
  c1 <- district_con_scratch()
  before <- tryCatch(dbGetQuery(c1, q$verify), error = function(e) NULL)
  r <- run_query(c1, q$solution, allow_mutation = TRUE)
  if (!isTRUE(r$ok)) { fail(id, " solution: ", r$error); dbDisconnect(c1); next }
  if (!is.null(r$note)) fail(id, ": solution is more than one statement")
  after <- dbGetQuery(c1, q$verify)
  if (!is.null(before) && isTRUE(check_answer(before, after)$pass))
    fail(id, ": already satisfied on a fresh copy (the task checks nothing)")
  dbDisconnect(c1)
}
ok(length(d_tasks), " district writing tasks: unsatisfied before, satisfied after")
if (length(d_tasks)) {
  d_expected <- lapply(d_tasks, function(q) { c1 <- district_con_scratch(); on.exit(dbDisconnect(c1))
    dbExecute(c1, q$solution); dbGetQuery(c1, q$verify) })
  d_demos <- unlist(lapply(DSEC_WRITING, function(fn)
    unlist(lapply(Filter(function(b) b$db == "district", DSEC[[fn]]),
                  function(b) split_statements(b$sql)))))
  d_demos <- d_demos[!duplicated(gsub("\\s+", " ", d_demos))]
  for (ord in list(seq_along(d_tasks), rev(seq_along(d_tasks)))) {
    c1 <- district_con_scratch()
    for (st in d_demos) {
      r <- run_query(c1, st, allow_mutation = TRUE)
      should_fail <- any(vapply(EXPECTED_FAILURES, function(x) grepl(x, st, fixed = TRUE), TRUE))
      if (should_fail && isTRUE(r$ok))  fail("district: expected a refusal but it ran: ", substr(st, 1, 60))
      if (!should_fail && !isTRUE(r$ok)) fail("district demo failed: ", substr(gsub("\\s+", " ", st), 1, 60), " -> ", gsub("\n", " ", r$error))
    }
    for (i in ord) {
      r <- run_query(c1, d_tasks[[i]]$solution, allow_mutation = TRUE)
      if (!isTRUE(r$ok)) fail("district task ", i, " failed after the others: ", gsub("\n", " ", r$error))
    }
    for (i in seq_along(d_tasks)) {
      got <- tryCatch(dbGetQuery(c1, d_tasks[[i]]$verify), error = function(e) NULL)
      if (!isTRUE(check_answer(got, d_expected[[i]])$pass))
        fail("district task ", i, " no longer verifies once everything else has run")
    }
    dbDisconnect(c1)
  }
  ok(length(d_demos), " district demos and ", length(d_tasks), " district tasks coexist, both orders")
}

dtab_file <- c("District data" = "tab_district_data", "Enrollment counts" = "tab_district_enrollment",
               "Attendance" = "tab_district_attendance", "Discipline and groups" = "tab_district_discipline",
               "Cohorts and outcomes" = "tab_district_cohorts", "Data quality" = "tab_district_quality",
               "District challenges" = "tab_district_challenges")
if (!setequal(names(DISTRICT_TOPIC_INDEX), names(dtab_file))) fail("DISTRICT_TOPIC_INDEX tabs do not match the district nav")
n_t <- 0
for (tab in names(DISTRICT_TOPIC_INDEX)) {
  src <- tolower(paste(readLines(file.path("R", paste0(dtab_file[[tab]], ".R")), warn = FALSE), collapse = "\n"))
  for (kw in DISTRICT_TOPIC_INDEX[[tab]]) {
    n_t <- n_t + 1
    if (!grepl(tolower(kw), src, fixed = TRUE)) fail("district topic '", kw, "' is indexed under ", tab, " but does not appear in ", dtab_file[[tab]], ".R")
  }
}
ok(n_t, " district topics found in their tabs")

dbDisconnect(dro)
dbDisconnect(ro)
cat("\n", if (length(fails)) sprintf("%d FAILURE(S)\n", length(fails)) else "ALL CHECKS PASSED\n", sep = "")
quit(status = if (length(fails)) 1 else 0)
