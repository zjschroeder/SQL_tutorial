# =============================================================================
# sql_helpers.R — shared machinery for the SQL walkthrough app
#
# This file holds everything that is NEW relative to the PSY 302 stats app:
#   - the database connection layer (read-only shared con; one private,
#     schema-preserving scratch copy per session for the two writing tabs)
#   - run_query(): execute any one statement, return a data frame, a rows-
#     affected count, or a friendly error
#   - the live SQL editor UI + server modules (read-only and writing)
#   - check_answer(): grade a learner's result set against an expected one
#   - execution_order_ribbon() / written_order_ribbon(): the clause-order widgets
#   - schema_explorer(): the clickable schema used on Tab 1 and in the sandbox
#   - term_table(): the small reference tables used where a step lists several
#     functions or keywords
#
# Everything visual here is built to match the stats app: same OI/PAL palette,
# same card idiom, JetBrains Mono for code to match the app's code_font.
# =============================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
})

# ---- Connection layer -------------------------------------------------------
# The shipped database lives in data/institute.sqlite. For the query tabs we
# open it READ-ONLY so a stray DROP/DELETE in the editor can never harm it.
# Under webR the file is in the virtual filesystem; read-only open is fine.

DB_PATH <- "data/institute.sqlite"

institute_con_ro <- function() {
  dbConnect(RSQLite::SQLite(), DB_PATH, flags = RSQLite::SQLITE_RO)
}

# For the writing tabs (Changing data, Tables): a private, disposable in-memory
# copy seeded from the file, so a session can INSERT/UPDATE/DELETE/CREATE freely
# and reset at will.
#
# The copy is rebuilt from the CREATE statements stored in sqlite_master, not
# with dbReadTable()/dbWriteTable(). The latter recreates each table from a data
# frame and so drops every PRIMARY KEY, NOT NULL, DEFAULT, and FOREIGN KEY, plus
# all four indexes, which would make half of what the writing tabs say about
# constraints untrue on the copy the learner is actually typing into.
#
# Foreign-key enforcement is set OFF explicitly. That is SQLite's documented
# default, but builds differ, and the Tables tab teaches switching it on, so the
# starting state has to be the same everywhere.
institute_con_scratch <- function() {
  src <- institute_con_ro()
  on.exit(dbDisconnect(src), add = TRUE)
  dst <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbExecute(dst, "PRAGMA foreign_keys = OFF;")
  schema <- dbGetQuery(src, paste(
    "SELECT type, name, sql FROM sqlite_master",
    "WHERE sql IS NOT NULL AND name NOT LIKE 'sqlite_%'"))
  tabs <- schema[schema$type == "table", , drop = FALSE]
  rest <- schema[schema$type != "table", , drop = FALSE]
  for (i in seq_len(nrow(tabs))) {
    dbExecute(dst, tabs$sql[i])
    df <- dbReadTable(src, tabs$name[i])
    if (nrow(df) > 0) dbAppendTable(dst, tabs$name[i], df)
  }
  for (i in seq_len(nrow(rest))) dbExecute(dst, rest$sql[i])   # indexes, views
  # One table the shipped database does not have: a stale partial backup, so the
  # Tables tab has something that can be DROPped without breaking anything.
  dbExecute(dst, paste(
    "CREATE TABLE participants_old AS",
    "SELECT participant_id, nickname, species FROM participants",
    "WHERE participant_id <= 12;"))
  dst
}

# One scratch connection per browser session, shared by every writing tab, so a
# table created on the Tables tab is still there on the Changing-data tab and a
# Reset on either tab resets both. Lives in session$userData.
scratch_holder <- function(session) {
  ud <- session$userData
  if (is.null(ud$scratch_env)) {
    env <- new.env(parent = emptyenv())
    env$con <- NULL
    ud$scratch_env <- env
    session$onSessionEnded(function() {
      if (!is.null(env$con) && dbIsValid(env$con)) dbDisconnect(env$con)
    })
  }
  env <- ud$scratch_env
  list(
    get = function() {
      if (is.null(env$con) || !dbIsValid(env$con)) env$con <- institute_con_scratch()
      env$con
    },
    reset = function() {
      if (!is.null(env$con) && dbIsValid(env$con)) dbDisconnect(env$con)
      env$con <- institute_con_scratch()
      invisible(env$con)
    }
  )
}

# ---- run_query: execute and never crash the app ----------------------------
# Returns list(ok = TRUE, data = <df>), list(ok = TRUE, data = NULL, verb = ...,
# affected = n), or list(ok = FALSE, error = <chr>). Any of them may carry a
# `note` (shown under the result), used when SQLite ran only the first of
# several statements.

# Strip leading whitespace and leading comments (both -- and /* */ styles) so
# the first real keyword can be read.
sql_strip_leading <- function(sql) {
  repeat {
    before <- sql
    sql <- sub("^\\s+", "", sql)
    sql <- sub("^--[^\n]*(\n|$)", "", sql)
    sql <- sub("^/\\*.*?\\*/", "", sql, perl = TRUE)
    if (identical(sql, before)) break
  }
  sql
}

sql_first_word <- function(sql) {
  tolower(sub("^([A-Za-z]+).*$", "\\1", sql_strip_leading(sql)))
}

# Statements the read-only query tabs decline up front, with a friendly message.
# (The connection is opened read-only regardless; this only improves the error.)
SQL_WRITE_VERBS <- c("insert", "update", "delete", "replace", "drop", "alter",
                     "create", "truncate", "begin", "commit", "end", "rollback",
                     "savepoint", "release", "vacuum", "reindex", "attach",
                     "detach")

sql_is_mutating <- function(sql) sql_first_word(sql) %in% SQL_WRITE_VERBS

run_query <- function(con, sql, allow_mutation = FALSE) {
  sql <- trimws(sql)
  if (!nzchar(sql_strip_leading(sql))) {
    return(list(ok = FALSE, error = "The editor is empty. Type a query, then press Run."))
  }
  if (!allow_mutation && sql_is_mutating(sql)) {
    return(list(ok = FALSE,
      error = paste0("This editor is read-only, so it runs SELECT queries and ",
                     "politely declines everything else. Statements that change ",
                     "things get their own tabs (", tags_b("Changing data"),
                     " and ", tags_b("Tables"),
                     "), with a private copy you are allowed to break.")))
  }
  verb <- sql_first_word(sql)
  note <- NULL
  out <- tryCatch(
    withCallingHandlers({
      # dbSendQuery() accepts any statement. Whether it produced columns tells
      # us if there are rows to fetch (SELECT, PRAGMA, EXPLAIN, ... RETURNING)
      # or only a count of rows changed (INSERT, UPDATE, DELETE).
      rs <- dbSendQuery(con, sql)
      on.exit(try(dbClearResult(rs), silent = TRUE), add = TRUE)
      if (nrow(dbColumnInfo(rs)) > 0) {
        list(ok = TRUE, data = dbFetch(rs))
      } else {
        n <- if (verb %in% c("insert", "update", "delete", "replace"))
          dbGetRowsAffected(rs) else NA_integer_
        list(ok = TRUE, data = NULL, verb = verb, affected = n)
      }
    },
    warning = function(w) {
      if (grepl("Ignoring remaining part of query", conditionMessage(w), fixed = TRUE)) {
        note <<- paste0("Only the first statement ran. This editor takes one ",
                        "statement per Run, so run the rest one at a time.")
      }
      invokeRestart("muffleWarning")
    }),
    error = function(e) list(ok = FALSE, error = friendly_sql_error(conditionMessage(e)))
  )
  out$note <- note
  out
}

tags_b <- function(x) as.character(shiny::tags$b(x))

# Translate the most common SQLite error messages into plain guidance.
friendly_sql_error <- function(msg) {
  m <- tolower(msg)
  hint <- if (grepl("no such column", m)) {
    "SQLite does not recognize one of your column names. Check the spelling, and check that the column lives in a table you actually named in FROM; the second is the more common mistake."
  } else if (grepl("no such table", m)) {
    "SQLite does not recognize one of your table names. The institute's six tables are labs, researchers, studies, participants, enrollments, and measurements, all plural. (On the writing tabs, a table you created is gone after a Reset.)"
  } else if (grepl("ambiguous column name", m)) {
    "Two of the joined tables share that column name, so SQLite cannot tell which one you mean. Qualify it: studies.lab_id rather than lab_id."
  } else if (grepl("misuse of aggregate", m)) {
    "An aggregate such as COUNT() or AVG() has turned up somewhere it cannot run, usually in WHERE. WHERE filters rows before any group exists, so a test on a group's total belongs in HAVING."
  } else if (grepl("misuse of (aliased )?window function", m)) {
    "A window function has turned up in WHERE, GROUP BY, or HAVING. Windows are computed after all three, so the value does not exist yet. Compute it in a subquery or CTE, then filter in the outer query."
  } else if (grepl("do not have the same number of result columns", m)) {
    "The two SELECTs either side of UNION / INTERSECT / EXCEPT return different numbers of columns. They have to match, column for column."
  } else if (grepl("unique constraint failed", m)) {
    "That value is already taken in a column that has to be unique (a PRIMARY KEY or UNIQUE column). Pick an unused value, or use ON CONFLICT to say what should happen instead."
  } else if (grepl("not null constraint failed", m)) {
    "That column is declared NOT NULL and the statement left it empty. Supply a value for it."
  } else if (grepl("check constraint failed", m)) {
    "The row broke a CHECK rule declared on the table, so SQLite refused it."
  } else if (grepl("foreign key constraint failed", m)) {
    "Foreign-key enforcement is on, and this change would leave a row pointing at a parent that does not exist (or delete a parent that still has children). Change the children first, or run PRAGMA foreign_keys = OFF; to stop enforcing."
  } else if (grepl("within a transaction", m)) {
    "A transaction is already open on your copy, and transactions do not nest. COMMIT or ROLLBACK the open one first."
  } else if (grepl("no transaction is active", m)) {
    "There is no open transaction to finish. COMMIT and ROLLBACK only mean something after a BEGIN."
  } else if (grepl("already exists", m)) {
    "Something with that name already exists in your copy. Drop it first, choose another name, or press Reset."
  } else if (grepl("readonly database", m)) {
    "This editor's connection is read-only. Statements that change things run on the Changing data and Tables tabs."
  } else if (grepl("right and full outer joins are not currently supported", m)) {
    "This build of SQLite predates version 3.39 (2022), which added RIGHT and FULL OUTER JOIN. Swap the table order and use LEFT JOIN."
  } else if (grepl("syntax error", m)) {
    "SQLite hit a syntax error. The likeliest causes, in order: a missing comma between columns, an unclosed quote, a stray comma before FROM, or clauses written out of order (they go SELECT, FROM, WHERE, GROUP BY, HAVING, ORDER BY, LIMIT)."
  } else {
    NULL
  }
  if (is.null(hint)) msg else paste0(hint, "\n\nSQLite said: ", msg)
}

# ---- check_answer: result-set grading --------------------------------------
# Compares a learner's result to the expected result. Order-insensitive by
# default (sorts both by all columns); set ordered = TRUE for ORDER BY items.
# Column names must match the expected set unless check_names = FALSE.
normalize_df <- function(df, ordered = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df[] <- lapply(df, function(col) if (is.numeric(col)) round(col, 6) else col)
  if (!ordered) {
    df <- df[do.call(order, c(lapply(df, as.character), list(na.last = TRUE))),
             , drop = FALSE]
    rownames(df) <- NULL
  }
  df
}

check_answer <- function(user_df, expected_df, ordered = FALSE,
                         check_names = TRUE) {
  if (is.null(expected_df)) {
    return(list(pass = FALSE, msg = "This question cannot be graded on this build of SQLite (the reference query would not run). Show a solution has the answer."))
  }
  if (is.null(user_df)) {
    return(list(pass = FALSE, msg = "That query did not return a table, so there is nothing to check yet."))
  }
  u <- normalize_df(user_df, ordered)
  e <- normalize_df(expected_df, ordered)
  if (check_names && !setequal(tolower(names(u)), tolower(names(e)))) {
    return(list(pass = FALSE,
      msg = sprintf("Right idea, wrong column names. The target has: %s.",
                    paste(names(e), collapse = ", "))))
  }
  # Align column order to expected for comparison
  if (check_names) u <- u[, match(tolower(names(e)), tolower(names(u))), drop = FALSE]
  same <- isTRUE(nrow(u) == nrow(e)) &&
          isTRUE(all.equal(as.data.frame(u), as.data.frame(e),
                           check.attributes = FALSE)) == TRUE
  if (identical(same, TRUE)) {
    list(pass = TRUE, msg = "Correct. The result set matches the target.")
  } else {
    msg <- if (nrow(u) != nrow(e)) {
      sprintf("Not yet. Your query returned %d row%s; the target has %d. Look for who is missing, or who snuck in.",
              nrow(u), if (nrow(u) == 1) "" else "s", nrow(e))
    } else {
      sprintf("Not yet. The row count is right (%d) and the columns are named correctly, but the values differ from the target. Reread what each column should contain%s.",
              nrow(e), if (ordered) ", and the sort order, which this question grades" else "")
    }
    list(pass = FALSE, msg = msg)
  }
}

# ---- Live SQL editor: UI -----------------------------------------------------
# A reusable block: a labelled prompt, a monospace textarea pre-filled with a
# starter query, a Run button, a results table, and (for graded items) a Check
# button + verdict. This is the live-query analog of the stats app's
# quest_question() <details> block.
#
# `id` namespaces every input/output. `graded = TRUE` adds the Check button.
sql_editor <- function(id, prompt = NULL, starter = "SELECT * FROM labs;",
                       graded = FALSE, height = "120px", number = NULL) {
  ns_id <- function(suffix) paste0(id, "_", suffix)
  div(
    class = "sql-editor mb-3",
    style = "background:#fff; border:1px solid #d8e3e8; border-radius:10px; padding:16px 18px;",
    if (!is.null(number))
      div(style = sprintf(
            "color:%s; font-weight:700; letter-spacing:0.05em; font-size:11px; text-transform:uppercase; margin-bottom:4px;",
            "#0072B2"),
          sprintf("Query %d", number)),
    if (!is.null(prompt))
      div(style = "line-height:1.55; margin-bottom:10px;", prompt),
    tags$textarea(
      id = ns_id("sql"), class = "sql-textarea form-control",
      style = sprintf("font-family:'JetBrains Mono',monospace; font-size:13.5px; min-height:%s; line-height:1.5; background:#0f2430; color:#e8f0f3; border:none; border-radius:8px; padding:12px 14px; resize:vertical;", height),
      spellcheck = "false", starter
    ),
    div(class = "mt-2 d-flex", style = "gap:8px; align-items:center;",
        actionButton(ns_id("run"), "Run", class = "btn-primary btn-sm"),
        if (graded)
          actionButton(ns_id("check"), "Check my answer",
                       class = "btn-outline-secondary btn-sm"),
        if (graded)
          actionButton(ns_id("reveal"), "Show a solution",
                       class = "btn-link btn-sm", style = "color:#7A4F00;"),
        span(style = "flex:1;"),
        uiOutput(ns_id("verdict"), inline = TRUE)
    ),
    uiOutput(ns_id("result"))
  )
}

# ---- Live SQL editor: server ------------------------------------------------
# Wires a sql_editor(). `con_fun` returns a fresh connection (so each run is
# isolated). For graded items pass `expected` (a data frame), `ordered`, and a
# `solution` string. Returns nothing; purely side-effecting on outputs.
sql_editor_server <- function(id, input, output, session, con_fun,
                              expected = NULL, ordered = FALSE,
                              solution = NULL, allow_mutation = FALSE) {
  ns_id <- function(suffix) paste0(id, "_", suffix)
  store <- reactiveValues(last = NULL, verdict = NULL)

  render_table <- function(res) render_result_block(res)

  observeEvent(input[[ns_id("run")]], {
    con <- con_fun(); on.exit(dbDisconnect(con), add = TRUE)
    res <- run_query(con, input[[ns_id("sql")]], allow_mutation = allow_mutation)
    store$last <- res
    store$verdict <- NULL
    output[[ns_id("result")]] <- renderUI(render_table(res))
  })

  observeEvent(input[[ns_id("check")]], {
    con <- con_fun(); on.exit(dbDisconnect(con), add = TRUE)
    res <- run_query(con, input[[ns_id("sql")]], allow_mutation = allow_mutation)
    store$last <- res
    output[[ns_id("result")]] <- renderUI(render_table(res))
    if (!res$ok) {
      store$verdict <- list(pass = FALSE, msg = "There is an error above to fix first; then check again.")
    } else {
      store$verdict <- check_answer(res$data, expected, ordered = ordered)
    }
    output[[ns_id("verdict")]] <- renderUI({
      v <- store$verdict
      col <- if (isTRUE(v$pass)) c("#E1F5EE", "#0F6E56") else c("#FFF1D6", "#7A4F00")
      mark <- if (isTRUE(v$pass)) "\u2713 " else ""
      span(style = sprintf("background:%s; color:%s; padding:4px 10px; border-radius:20px; font-size:12.5px; font-weight:600;", col[1], col[2]),
           paste0(mark, v$msg))
    })
  })

  if (!is.null(solution)) {
    observeEvent(input[[ns_id("reveal")]], {
      output[[ns_id("result")]] <- renderUI({
        tagList(
          div(class = "p-3 mt-2",
              style = "background:#0f2430; color:#e8f0f3; border-radius:8px; font-family:'JetBrains Mono',monospace; font-size:13.5px; white-space:pre-wrap; line-height:1.5;",
              solution),
          isolate(if (!is.null(store$last)) render_table(store$last))
        )
      })
    })
  }
}

# Minimal HTML table renderer (avoids a hard DT dependency in the WASM bundle;
# DT is available but base HTML keeps first paint fast).
render_html_table <- function(df) {
  if (nrow(df) == 0) {
    return(div(style = "padding:14px; color:#6b7c85; font-style:italic;",
               "No rows. The query ran; nothing matched."))
  }
  fmt <- function(x) {
    if (is.numeric(x)) x <- formatC(x, format = "g", digits = 6)
    x <- as.character(x)
    x[is.na(x)] <- "\u2014NULL\u2014"
    x
  }
  header <- tags$tr(lapply(names(df), function(nm)
    tags$th(style = "text-align:left; padding:7px 12px; background:#eef4f6; color:#1a3a4a; font-weight:600; font-size:12.5px; border-bottom:2px solid #d8e3e8; position:sticky; top:0;",
            nm)))
  rows <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(
      style = if (i %% 2 == 0) "background:#fafcfd;" else "",
      lapply(names(df), function(nm) {
        val <- fmt(df[[nm]][i])
        is_null <- identical(val, "\u2014NULL\u2014")
        tags$td(style = sprintf(
          "padding:6px 12px; font-size:13px; font-family:'JetBrains Mono',monospace; %s",
          if (is_null) "color:#b0a06b; font-style:italic;" else "color:#243b44;"),
          val)
      })
    )
  })
  tags$table(style = "width:100%; border-collapse:collapse;",
             tags$thead(header), tags$tbody(rows))
}

# ---- execution_order_ribbon: the recurring clause-order widget ---------------
# Renders the LOGICAL processing order of a SELECT as a horizontal ribbon. Pass
# `active` = one or more keys (the names of EXEC_ORDER) to spotlight them.
#
# Every chip on this ribbon is taught, and lit, on some tab:
#   FROM      SELECT (step 5), JOINs (step 5)     WINDOW    Windows (step 7)
#   WHERE     WHERE (step 5), GROUP BY (step 4)   SELECT    SELECT (step 5)
#   GROUP BY  GROUP BY (step 4)                   DISTINCT  SELECT (step 4)
#   HAVING    GROUP BY (step 4)                   SET       UNION (step 5)
#   ORDER BY, LIMIT   ORDER BY (step 5)
# If you add a chip, light it somewhere; if you stop teaching a clause, drop it.
#
# The order follows the SQL standard's logical model and SQLite's own account of
# SELECT processing (sqlite.org/lang_select.html): joins are part of FROM;
# window functions are evaluated with the select list, after HAVING; compound
# operators combine finished SELECTs; ORDER BY and LIMIT/OFFSET come last.
EXEC_ORDER <- c(
  "FROM"     = "FROM / JOIN",
  "WHERE"    = "WHERE",
  "GROUP BY" = "GROUP BY",
  "HAVING"   = "HAVING",
  "WINDOW"   = "window functions",
  "SELECT"   = "SELECT",
  "DISTINCT" = "DISTINCT",
  "SET"      = "UNION / INTERSECT / EXCEPT",
  "ORDER BY" = "ORDER BY",
  "LIMIT"    = "LIMIT / OFFSET"
)

# The order the clauses must be WRITTEN in, for the Start tab's side-by-side.
WRITTEN_ORDER <- c("SELECT", "DISTINCT", "FROM / JOIN", "WHERE", "GROUP BY",
                   "HAVING", "UNION / INTERSECT / EXCEPT", "ORDER BY",
                   "LIMIT / OFFSET")

.ribbon <- function(title, labels, lit, note = NULL) {
  chip <- function(label, on) {
    div(class = "exec-chip", style = sprintf(
      "padding:6px 12px; border-radius:6px; font-family:'JetBrains Mono',monospace; font-size:12.5px; font-weight:600; white-space:nowrap; %s",
      if (on) "background:#0072B2; color:#fff; box-shadow:0 2px 6px rgba(0,114,178,0.3);"
      else    "background:#eef4f6; color:#6b7c85;"),
      label)
  }
  arrow <- function() tags$span(style = "color:#b8c6cc; font-size:14px; flex-shrink:0;", HTML("&rarr;"))
  chips <- list(chip(labels[1], lit[1]))
  for (i in seq_along(labels)[-1]) chips <- c(chips, list(arrow(), chip(labels[i], lit[i])))
  div(class = "exec-ribbon my-3",
      div(style = "display:flex; align-items:center; gap:6px; flex-wrap:wrap; padding:12px 14px; background:#f6fafb; border:1px solid #e0e6ea; border-radius:10px;",
          tags$span(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-right:4px;",
                    title),
          chips
      ),
      if (!is.null(note))
        div(style = "font-size:12.5px; color:#6b7c85; margin-top:6px; padding-left:4px;", note)
  )
}

execution_order_ribbon <- function(active = NULL, note = NULL) {
  unknown <- setdiff(active, names(EXEC_ORDER))
  if (length(unknown)) stop("execution_order_ribbon(): unknown key(s): ",
                            paste(unknown, collapse = ", "))
  .ribbon("SQL runs in this order", unname(EXEC_ORDER),
          names(EXEC_ORDER) %in% active, note)
}

written_order_ribbon <- function(note = NULL) {
  .ribbon("You write it in this order", WRITTEN_ORDER,
          rep(FALSE, length(WRITTEN_ORDER)), note)
}

# ---- schema_explorer: clickable table cards ---------------------------------
# Static UI describing the six tables, their columns, and relationships. Used
# on Tab 1 and embeddable in the Tab 10 sandbox sidebar.
SCHEMA <- list(
  labs = list(
    blurb = "The institute's six labs, each a monument to a question nobody asked.",
    cols = c("lab_id (PK)", "lab_name", "building", "founded_year"),
    rel  = "One lab has many researchers and many studies."),
  researchers = list(
    blurb = "Who works here. lab_id and salary can be NULL (one visiting fellow, two undisclosed salaries).",
    cols = c("researcher_id (PK)", "full_name", "lab_id (FK\u2192labs)",
             "title", "salary", "start_year"),
    rel  = "Each researcher belongs to at most one lab; may lead studies."),
  studies = list(
    blurb = "The studies, ten so far, one abandoned.",
    cols = c("study_id (PK)", "title", "lab_id (FK\u2192labs)",
             "lead_researcher_id (FK\u2192researchers)", "year", "status", "sample_size"),
    rel  = "Belongs to one lab; led by one researcher; enrolls many participants."),
  participants = list(
    blurb = "Humans (eight named Dave), three seagulls, three crickets. age can be NULL, and is, for every seagull.",
    cols = c("participant_id (PK)", "nickname", "species", "age"),
    rel  = "Enrolls in many studies via enrollments."),
  enrollments = list(
    blurb = "Bridge table: which participant is in which study, and whether they stuck it out.",
    cols = c("enrollment_id (PK)", "study_id (FK\u2192studies)",
             "participant_id (FK\u2192participants)", "dropped_out"),
    rel  = "Connects studies and participants; has many measurements."),
  measurements = list(
    blurb = "Repeated trials (up to five) per enrollment. Built for the Windows tab.",
    cols = c("measurement_id (PK)", "enrollment_id (FK\u2192enrollments)",
             "trial", "value"),
    rel  = "Each row is one trial of one enrollment.")
)

schema_explorer <- function() {
  card <- function(name, info) {
    div(class = "col-md-6 col-lg-4 mb-3",
      div(style = "background:#fff; border:1px solid #d8e3e8; border-radius:10px; padding:14px 16px; height:100%;",
        div(style = "font-family:'JetBrains Mono',monospace; font-weight:700; color:#0072B2; font-size:15px; margin-bottom:4px;",
            name),
        div(style = "font-size:13px; color:#555; margin-bottom:8px; line-height:1.45;",
            info$blurb),
        tags$ul(style = "margin:0 0 8px 0; padding-left:18px; font-family:'JetBrains Mono',monospace; font-size:12px; color:#243b44; line-height:1.6;",
          lapply(info$cols, function(c) tags$li(c))),
        div(style = "font-size:12px; color:#6b7c85; font-style:italic; border-top:1px dashed #e0e6ea; padding-top:6px;",
            info$rel)
      ))
  }
  div(class = "row g-3",
      Map(card, names(SCHEMA), SCHEMA))
}

# ---- Walkthrough scaffolding shared by every query tab ----------------------
# A sticky sidebar table-of-contents for a gated walkthrough. `cur` is the
# current step (1-based), `titles` the step titles, `id_prefix` matches the step
# blocks' id_prefix so anchors line up (e.g. "where-step" -> #where-step-1).
walkthrough_toc <- function(cur, titles, id_prefix) {
  nT <- length(titles)
  pct <- if (nT > 1) round((cur - 1) / (nT - 1) * 100) else 100
  items <- lapply(seq_len(nT), function(i) {
    state <- if (i < cur) "completed" else if (i == cur) "current" else "locked"
    check <- if (state == "completed") tags$span(class = "toc-check", HTML("&#10003;")) else NULL
    href  <- if (state == "locked") NULL else sprintf("#%s-%d", id_prefix, i)
    tags$a(href = href, class = paste("toc-item", state),
           tags$span(class = "toc-num", i),
           tags$span(class = "toc-title", titles[i]), check)
  })
  div(class = "sidebar-toc",
      div(class = "sidebar-toc-title", "Walkthrough"),
      div(class = "toc-progress-wrap",
          div(class = "toc-progress-bar", style = sprintf("width:%d%%;", pct))),
      div(class = "toc-progress-label",
          sprintf("Step %d of %d · %d%%", cur, nT, pct)),
      items)
}

# Build the list of graded live-editor blocks for a quest section. `prefix`
# namespaces the editors (e.g. "wherq" -> wherq1..wherq10).
quest_editors_ui <- function(quest, prefix) {
  lapply(seq_along(quest), function(i) {
    q <- quest[[i]]
    sql_editor(paste0(prefix, i), prompt = q$prompt, starter = "",
               graded = TRUE, number = i)
  })
}

# Run a quest's reference query to get its expected result set. A reference
# that fails (say, on a SQLite build too old for the syntax) must not take the
# whole app down at startup, so it yields NULL and that one question reports
# that it cannot be graded here.
quest_expected <- function(ref) {
  con0 <- institute_con_ro()
  on.exit(dbDisconnect(con0), add = TRUE)
  tryCatch(dbGetQuery(con0, ref), error = function(e) NULL)
}

# Wire every graded editor in a quest: compute each expected result set by
# running its reference query against the read-only DB, then attach the server.
wire_quest_editors <- function(quest, prefix, input, output, session) {
  for (i in seq_along(quest)) {
    local({
      idx <- i
      q   <- quest[[idx]]
      expected <- quest_expected(q$ref)
      sql_editor_server(
        id = paste0(prefix, idx), input = input, output = output, session = session,
        con_fun = institute_con_ro, expected = expected,
        ordered = isTRUE(q$ordered), solution = q$solution)
    })
  }
}

# Wire the ungraded in-step "try it" editors (Run only, read-only). `prefix` +
# 1..n must match the sql_editor() ids used in the steps (e.g. "wsed", 5).
wire_demo_editors <- function(prefix, n, input, output, session) {
  for (i in seq_len(n)) {
    local({
      eid <- paste0(prefix, i)
      sql_editor_server(id = eid, input = input, output = output, session = session,
                        con_fun = institute_con_ro)
    })
  }
}

# Render a run_query() result (error panel / done message / table), plus the
# optional note. Shared by the read-only and the writing editors.
render_result_block <- function(res) {
  note <- if (!is.null(res$note))
    div(style = "color:#7A4F00; font-size:12.5px; margin-top:6px;", res$note)
  if (!res$ok) {
    return(div(class = "p-3 mt-2",
      style = "background:#FFF1D6; color:#7A4F00; border-radius:8px; white-space:pre-wrap; font-size:13.5px; line-height:1.5;",
      HTML(gsub("\n", "<br>", res$error))))
  }
  if (is.null(res$data)) {
    n <- res$affected
    msg <- if (is.null(n) || is.na(n)) {
      "Done. Nothing to show; this statement does not return rows."
    } else {
      sprintf("Done. %d row%s affected.", n, if (n == 1) "" else "s")
    }
    return(tagList(
      div(class = "p-3 mt-2",
          style = "background:#E1F5EE; color:#0F6E56; border-radius:8px;", msg),
      note))
  }
  df <- res$data; n <- nrow(df); cap <- 200; shown <- utils::head(df, cap)
  tagList(
    div(class = "mt-2", style = "overflow-x:auto; border:1px solid #e0e6ea; border-radius:8px;",
        render_html_table(shown)),
    div(style = "color:#6b7c85; font-size:12px; margin-top:4px;",
        sprintf("%d row%s%s", n, if (n == 1) "" else "s",
                if (n > cap) sprintf("  \u00b7  showing first %d", cap) else "")),
    note)
}

# ---- CRUD live editor: stateful, mutation-allowed ---------------------------
# Unlike the read-only editor (fresh connection per Run), this runs against a
# *persistent* per-session scratch connection so INSERT/UPDATE/DELETE
# accumulate. `con_holder()` returns the live DBI connection. "Run" applies the
# statement. "Check" (graded items) does NOT re-run the statement — it runs the
# `verify` query against the current DB state and compares to `expected`, so
# checking after Run never double-applies a mutation.
sql_editor_server_crud <- function(id, input, output, session, con_holder,
                                   verify = NULL, expected = NULL, solution = NULL) {
  ns_id <- function(s) paste0(id, "_", s)

  observeEvent(input[[ns_id("run")]], {
    con <- con_holder()
    res <- run_query(con, input[[ns_id("sql")]], allow_mutation = TRUE)
    output[[ns_id("result")]] <- renderUI(render_result_block(res))
  })

  if (!is.null(verify)) {
    observeEvent(input[[ns_id("check")]], {
      con <- con_holder()
      got <- tryCatch(dbGetQuery(con, verify), error = function(e) NULL)
      v <- check_answer(got, expected, ordered = FALSE)
      # check_answer()'s wording is about a query's result; here we inspected
      # the state of the copy, so say that instead.
      v$msg <- if (isTRUE(v$pass)) {
        "Correct. Your copy is in the target state."
      } else {
        "Not yet. Your copy is not in the target state. Check only inspects, so press Run first; if earlier experiments are in the way, Reset."
      }
      output[[ns_id("verdict")]] <- renderUI({
        col  <- if (isTRUE(v$pass)) c("#E1F5EE", "#0F6E56") else c("#FFF1D6", "#7A4F00")
        mark <- if (isTRUE(v$pass)) "✓ " else ""
        span(style = sprintf("background:%s; color:%s; padding:4px 10px; border-radius:20px; font-size:12.5px; font-weight:600;", col[1], col[2]),
             paste0(mark, v$msg))
      })
    })
  }

  if (!is.null(solution)) {
    observeEvent(input[[ns_id("reveal")]], {
      output[[ns_id("result")]] <- renderUI(
        div(class = "p-3 mt-2",
            style = "background:#0f2430; color:#e8f0f3; border-radius:8px; font-family:'JetBrains Mono',monospace; font-size:13.5px; white-space:pre-wrap; line-height:1.5;",
            solution))
    })
  }
}

# Wire the graded tasks of a writing tab. Each task's expected post-state is
# computed once, at startup, by applying its `solution` to a fresh scratch copy
# and running its `verify` query there.
wire_crud_quest <- function(quest, prefix, input, output, session, con_holder) {
  for (i in seq_along(quest)) {
    local({
      idx <- i
      q   <- quest[[idx]]
      con_tmp <- institute_con_scratch()
      expected <- tryCatch({
        dbExecute(con_tmp, q$solution)
        dbGetQuery(con_tmp, q$verify)
      }, error = function(e) NULL)
      dbDisconnect(con_tmp)
      sql_editor_server_crud(
        id = paste0(prefix, idx), input = input, output = output, session = session,
        con_holder = con_holder, verify = q$verify,
        expected = expected, solution = q$solution)
    })
  }
}

# The "this is your private copy" bar with its Reset button, used at the top of
# both writing tabs. `btn_id` / `msg_id` differ per tab; both reset the one
# shared copy.
scratch_reset_bar <- function(btn_id, msg_id) {
  div(class = "p-3 my-3 d-flex",
      style = "background:#E1F1FB; border-radius:10px; align-items:center; gap:14px; flex-wrap:wrap;",
      div(style = "flex:1; min-width:220px;",
          div(style = "font-weight:700; color:#003D6B;", "This is your private copy."),
          div(style = "font-size:13px; color:#244; line-height:1.5;",
              "Nothing you do here touches the real database or anyone else's ",
              "session. The Changing data and Tables tabs share this one copy, so a ",
              "table you create on one is there on the other. Break it with a clear ",
              "conscience, and reset whenever you want a clean slate.")),
      div(actionButton(btn_id, "\u21ba Reset my copy", class = "btn-outline-secondary btn-sm"),
          uiOutput(msg_id)))
}

# A small reference table for the steps that list several functions or
# keywords at once. `rows` is a list of c(term, meaning) pairs.
term_table <- function(rows, head = c("Write", "Get")) {
  tags$table(class = "my-3",
    style = "width:100%; border-collapse:collapse; background:#fff; border:1px solid #e0e6ea; border-radius:8px; overflow:hidden;",
    tags$thead(tags$tr(lapply(head, function(h)
      tags$th(style = "text-align:left; padding:7px 12px; background:#eef4f6; color:#1a3a4a; font-weight:600; font-size:12.5px; border-bottom:2px solid #d8e3e8;", h)))),
    tags$tbody(lapply(rows, function(r)
      tags$tr(
        tags$td(style = "padding:6px 12px; font-family:'JetBrains Mono',monospace; font-size:12.5px; color:#0072B2; white-space:nowrap; vertical-align:top; border-bottom:1px solid #f0f3f5;", r[1]),
        tags$td(style = "padding:6px 12px; font-size:13.5px; color:#243b44; line-height:1.5; border-bottom:1px solid #f0f3f5;", r[2])))))
}
