# =============================================================================
# sql_helpers.R — shared machinery for the SQL walkthrough app
#
# This file holds everything that is NEW relative to the PSY 302 stats app:
#   - the database connection layer (read-only shared con; per-session copies)
#   - run_query(): execute SQL, return a data frame OR a friendly error
#   - the live SQL editor UI + server module (the analog of quest_question())
#   - check_answer(): grade a learner's result set against an expected one
#   - execution_order_ribbon(): the recurring FROM→…→LIMIT clause widget
#   - schema_explorer(): the clickable schema used on Tab 1 and in the sandbox
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

# For the CRUD tab: a private, disposable in-memory copy seeded from the file,
# so a session can INSERT/UPDATE/DELETE freely and reset at will.
institute_con_scratch <- function() {
  src <- institute_con_ro()
  on.exit(dbDisconnect(src), add = TRUE)
  dst <- dbConnect(RSQLite::SQLite(), ":memory:")
  for (t in dbListTables(src)) {
    df <- dbReadTable(src, t)
    dbWriteTable(dst, t, df, overwrite = TRUE)
  }
  dst
}

# ---- run_query: execute and never crash the app ----------------------------
# Returns list(ok = TRUE, data = <df>) or list(ok = FALSE, error = <chr>).
# We classify the statement so the query tabs can refuse data-modifying SQL on
# the read-only connection with a friendly message instead of a raw SQLite error.
sql_is_mutating <- function(sql) {
  grepl("^\\s*(insert|update|delete|drop|alter|create|replace|truncate)\\b",
        sql, ignore.case = TRUE)
}

run_query <- function(con, sql, allow_mutation = FALSE) {
  sql <- trimws(sql)
  if (!nzchar(sql)) {
    return(list(ok = FALSE, error = "Type a query, then press Run."))
  }
  if (!allow_mutation && sql_is_mutating(sql)) {
    return(list(ok = FALSE,
      error = paste0("This editor is read-only, so it only runs SELECT ",
                     "queries. (Changing data comes later, on the ",
                     tags_b("Changing data"), " tab.)")))
  }
  out <- tryCatch(
    {
      if (allow_mutation && sql_is_mutating(sql)) {
        n <- dbExecute(con, sql)
        list(ok = TRUE, data = NULL, affected = n)
      } else {
        list(ok = TRUE, data = dbGetQuery(con, sql))
      }
    },
    error = function(e) list(ok = FALSE, error = friendly_sql_error(conditionMessage(e)))
  )
  out
}

tags_b <- function(x) as.character(shiny::tags$b(x))

# Translate the most common SQLite error messages into plain guidance.
friendly_sql_error <- function(msg) {
  m <- tolower(msg)
  hint <- if (grepl("no such column", m)) {
    "There's a column name SQLite doesn't recognize. Check spelling and that the column lives in a table you've named in FROM."
  } else if (grepl("no such table", m)) {
    "There's a table name SQLite doesn't recognize. The tables are labs, researchers, studies, participants, enrollments, measurements."
  } else if (grepl("syntax error", m)) {
    "SQLite hit a syntax error. A missing comma, an unclosed quote, or a keyword out of order is the usual cause."
  } else if (grepl("ambiguous column name", m)) {
    "Two joined tables share that column name, so you need to qualify it, e.g. studies.lab_id rather than lab_id."
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
  if (is.null(user_df)) {
    return(list(pass = FALSE, msg = "That query didn't return a table to check."))
  }
  u <- normalize_df(user_df, ordered)
  e <- normalize_df(expected_df, ordered)
  if (check_names && !setequal(tolower(names(u)), tolower(names(e)))) {
    return(list(pass = FALSE,
      msg = sprintf("The columns don't match yet. Expected: %s.",
                    paste(names(e), collapse = ", "))))
  }
  # Align column order to expected for comparison
  if (check_names) u <- u[, match(tolower(names(e)), tolower(names(u))), drop = FALSE]
  same <- isTRUE(nrow(u) == nrow(e)) &&
          isTRUE(all.equal(as.data.frame(u), as.data.frame(e),
                           check.attributes = FALSE)) == TRUE
  if (identical(same, TRUE)) {
    list(pass = TRUE, msg = "Correct \u2014 the result set matches.")
  } else {
    list(pass = FALSE, msg = sprintf(
      "Not quite. Your query returned %d row%s; the target has %d. Compare the two tables below.",
      nrow(u), if (nrow(u) == 1) "" else "s", nrow(e)))
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

  render_table <- function(res) {
    if (!res$ok) {
      return(div(class = "p-3 mt-2",
        style = "background:#FFF1D6; color:#7A4F00; border-radius:8px; white-space:pre-wrap; font-size:13.5px; line-height:1.5;",
        HTML(gsub("\n", "<br>", res$error))))
    }
    if (is.null(res$data)) {
      return(div(class = "p-3 mt-2",
        style = "background:#E1F5EE; color:#0F6E56; border-radius:8px;",
        sprintf("Done. %d row%s affected.", res$affected %||% 0,
                if ((res$affected %||% 0) == 1) "" else "s")))
    }
    df <- res$data
    n  <- nrow(df)
    cap <- 200
    shown <- utils::head(df, cap)
    tagList(
      div(class = "mt-2", style = "overflow-x:auto; border:1px solid #e0e6ea; border-radius:8px;",
          render_html_table(shown)),
      div(style = "color:#6b7c85; font-size:12px; margin-top:4px;",
          sprintf("%d row%s%s", n, if (n == 1) "" else "s",
                  if (n > cap) sprintf("  \u00b7  showing first %d", cap) else ""))
    )
  }

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
      store$verdict <- list(pass = FALSE, msg = "Fix the error above, then check again.")
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
               "No rows."))
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
# Renders the logical processing order FROM → WHERE → GROUP BY → HAVING →
# SELECT → DISTINCT → ORDER BY → LIMIT as a horizontal ribbon. Pass `active` =
# one of the clause keys to spotlight it; pass `dim` = clauses to grey out.
EXEC_ORDER <- c("FROM", "WHERE", "GROUP BY", "HAVING",
                "SELECT", "DISTINCT", "ORDER BY", "LIMIT")

execution_order_ribbon <- function(active = NULL, note = NULL) {
  chip <- function(label) {
    on <- !is.null(active) && label %in% active
    div(class = "exec-chip", style = sprintf(
      "padding:6px 12px; border-radius:6px; font-family:'JetBrains Mono',monospace; font-size:12.5px; font-weight:600; white-space:nowrap; %s",
      if (on) "background:#0072B2; color:#fff; box-shadow:0 2px 6px rgba(0,114,178,0.3);"
      else    "background:#eef4f6; color:#6b7c85;"),
      label)
  }
  arrow <- function() tags$span(style = "color:#b8c6cc; font-size:14px; flex-shrink:0;", HTML("&rarr;"))
  # Build [chip1, ->, chip2, ->, ...] as a flat tag list.
  chip_seq <- c(
    list(chip(EXEC_ORDER[1])),
    unlist(lapply(EXEC_ORDER[-1], function(l) list(arrow(), chip(l))),
           recursive = FALSE)
  )
  div(class = "exec-ribbon my-3",
      div(style = "display:flex; align-items:center; gap:6px; flex-wrap:wrap; padding:12px 14px; background:#f6fafb; border:1px solid #e0e6ea; border-radius:10px;",
          tags$span(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-right:4px;",
                    "SQL runs in this order"),
          chip_seq
      ),
      if (!is.null(note))
        div(style = "font-size:12.5px; color:#6b7c85; margin-top:6px; padding-left:4px;", note)
  )
}

# ---- schema_explorer: clickable table cards ---------------------------------
# Static UI describing the six tables, their columns, and relationships. Used
# on Tab 1 and embeddable in the Tab 10 sandbox sidebar.
SCHEMA <- list(
  labs = list(
    blurb = "The research labs in the institute.",
    cols = c("lab_id (PK)", "lab_name", "building", "founded_year"),
    rel  = "One lab has many researchers and many studies."),
  researchers = list(
    blurb = "Who works here. lab_id and salary can be NULL.",
    cols = c("researcher_id (PK)", "full_name", "lab_id (FK\u2192labs)",
             "title", "salary", "start_year"),
    rel  = "Each researcher belongs to at most one lab; may lead studies."),
  studies = list(
    blurb = "The (mostly absurd) studies.",
    cols = c("study_id (PK)", "title", "lab_id (FK\u2192labs)",
             "lead_researcher_id (FK\u2192researchers)", "year", "status", "sample_size"),
    rel  = "Belongs to one lab; led by one researcher; enrolls many participants."),
  participants = list(
    blurb = "Humans (many named Dave), seagulls, and crickets. age can be NULL.",
    cols = c("participant_id (PK)", "nickname", "species", "age"),
    rel  = "Enrolls in many studies via enrollments."),
  enrollments = list(
    blurb = "Bridge table: which participant is in which study.",
    cols = c("enrollment_id (PK)", "study_id (FK\u2192studies)",
             "participant_id (FK\u2192participants)", "dropped_out"),
    rel  = "Connects studies and participants; has many measurements."),
  measurements = list(
    blurb = "Repeated trials (1..k) per enrollment.",
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

# Wire every graded editor in a quest: compute each expected result set by
# running its reference query against the read-only DB, then attach the server.
wire_quest_editors <- function(quest, prefix, input, output, session) {
  for (i in seq_along(quest)) {
    local({
      idx <- i
      q   <- quest[[idx]]
      con0 <- institute_con_ro(); expected <- dbGetQuery(con0, q$ref); dbDisconnect(con0)
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

# Render a run_query() result (error panel / rows-affected / table). Used by the
# CRUD editor below; mirrors the read-only editor's inline renderer.
render_result_block <- function(res) {
  if (!res$ok) {
    return(div(class = "p-3 mt-2",
      style = "background:#FFF1D6; color:#7A4F00; border-radius:8px; white-space:pre-wrap; font-size:13.5px; line-height:1.5;",
      HTML(gsub("\n", "<br>", res$error))))
  }
  if (is.null(res$data)) {
    return(div(class = "p-3 mt-2",
      style = "background:#E1F5EE; color:#0F6E56; border-radius:8px;",
      sprintf("Done. %d row%s affected.", res$affected %||% 0,
              if ((res$affected %||% 0) == 1) "" else "s")))
  }
  df <- res$data; n <- nrow(df); cap <- 200; shown <- utils::head(df, cap)
  tagList(
    div(class = "mt-2", style = "overflow-x:auto; border:1px solid #e0e6ea; border-radius:8px;",
        render_html_table(shown)),
    div(style = "color:#6b7c85; font-size:12px; margin-top:4px;",
        sprintf("%d row%s%s", n, if (n == 1) "" else "s",
                if (n > cap) sprintf("  ·  showing first %d", cap) else "")))
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
