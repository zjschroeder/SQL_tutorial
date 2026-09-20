# =============================================================================
# tab_changing.R — "Changing data" walkthrough (DML + transactions)
#
# Unlike the query tabs, this one writes. Each session gets a PRIVATE in-memory
# copy of the database (scratch_holder() in sql_helpers.R), shared with the
# Tables tab, so INSERT/UPDATE/DELETE run freely and a Reset button re-seeds it.
# The copy keeps the real schema (primary keys, NOT NULL, defaults, indexes), so
# what the steps say about constraints is true of the copy being typed into.
#
# Six gated steps (the private copy; INSERT; UPDATE; DELETE; upsert and
# RETURNING; transactions). CREATE TABLE lives on the Tables tab.
#
# Graded items are checked by post-state verification: "Run" applies your
# statement; "Check" runs a verify query against the live copy and compares it
# to the expected post-state.
#
# The 10 tasks are independent of one another AND of the in-step demos: no demo
# touches a row that a task's verify query reads, except where the demo and the
# task are the same idempotent statement (deleting the dropouts).
# =============================================================================

CRUD_QUEST <- list(
  list(prompt = paste0("The institute has opened a seventh lab. Insert it: lab_id 7, lab_name ",
                       "'Quantum Napping Lab', building 'Wexler Hall', founded_year 2024."),
       verify = "SELECT lab_name, building, founded_year FROM labs WHERE lab_id = 7;",
       solution = "INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (7, 'Quantum Napping Lab', 'Wexler Hall', 2024);"),
  list(prompt = paste0("Add two participants with one statement: participant_id 25, nickname 'Dave', ",
                       "species 'human', age 40; and participant_id 26, nickname 'Gary', species ",
                       "'seagull', age unknown."),
       verify = "SELECT participant_id, nickname, species, age FROM participants WHERE participant_id IN (25, 26);",
       solution = "INSERT INTO participants (participant_id, nickname, species, age)\nVALUES (25, 'Dave', 'human', 40),\n       (26, 'Gary', 'seagull', NULL);"),
  list(prompt = paste0("Add a visiting researcher about whom little is known: researcher_id 13, full_name ",
                       "'Dr. Nil Void', lab_id NULL, title 'PI', salary NULL, start_year 2025."),
       verify = "SELECT full_name, lab_id, salary FROM researchers WHERE researcher_id = 13;",
       solution = "INSERT INTO researchers (researcher_id, full_name, lab_id, title, salary, start_year)\nVALUES (13, 'Dr. Nil Void', NULL, 'PI', NULL, 2025);"),
  list(prompt = "The cricket telepathy study was shelved prematurely, apparently. Update study_id 10 to status 'completed'.",
       verify = "SELECT status FROM studies WHERE study_id = 10;",
       solution = "UPDATE studies SET status = 'completed' WHERE study_id = 10;"),
  list(prompt = "Give researcher_id 2 (Wesley Tern, a grad student) a raise: set their salary to 36000.",
       verify = "SELECT salary FROM researchers WHERE researcher_id = 2;",
       solution = "UPDATE researchers SET salary = 36000 WHERE researcher_id = 2;"),
  list(prompt = "Every ongoing study wrapped up this week, conveniently. Set every study with status 'ongoing' to 'completed'.",
       verify = "SELECT COUNT(*) AS n FROM studies WHERE status = 'ongoing';",
       solution = "UPDATE studies SET status = 'completed' WHERE status = 'ongoing';"),
  list(prompt = "Clean up the dropouts: delete every enrollment where dropped_out = 1.",
       verify = "SELECT COUNT(*) AS n FROM enrollments WHERE dropped_out = 1;",
       solution = "DELETE FROM enrollments WHERE dropped_out = 1;"),
  list(prompt = "Pip the cricket has withdrawn from the institute. Delete the participant with participant_id 24.",
       verify = "SELECT COUNT(*) AS n FROM participants WHERE participant_id = 24;",
       solution = "DELETE FROM participants WHERE participant_id = 24;"),
  list(prompt = paste0("The Climate Affect Collective has found a home. Write an INSERT for lab_id 4, ",
                       "lab_name 'Climate Affect Collective', building 'Pickering Building', ",
                       "founded_year 2015 that, since lab 4 already exists, updates its building ",
                       "instead of failing. Use ON CONFLICT."),
       verify = "SELECT lab_name, building, founded_year FROM labs WHERE lab_id = 4;",
       solution = "INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (4, 'Climate Affect Collective', 'Pickering Building', 2015)\nON CONFLICT (lab_id) DO UPDATE SET building = excluded.building;"),
  list(prompt = "Study 4 recruited ten more people. Increase its sample_size by 10 (study_id 4), computing from the current value rather than typing the new number.",
       verify = "SELECT sample_size FROM studies WHERE study_id = 4;",
       solution = "UPDATE studies SET sample_size = sample_size + 10 WHERE study_id = 4;")
)

CRUD_STEP_TITLES <- c(
  "Your own private copy",
  "INSERT: add rows",
  "UPDATE: change rows",
  "DELETE: remove rows",
  "Insert-or-update, and RETURNING",
  "Transactions: BEGIN, COMMIT, ROLLBACK"
)

source_changing_server <- function(input, output, session) {

  # ---- The per-session scratch copy (shared with the Tables tab) ----------
  scratch <- scratch_holder(session)
  con_holder <- scratch$get

  observeEvent(input$crud_reset, {
    scratch$reset()
    output$crud_reset_msg <- renderUI(
      div(style = "color:#0F6E56; font-size:13px; margin-top:8px;",
          "\u2713 Fresh copy restored. Every table is back to its original rows, as if nothing happened."))
  })

  # ---- Step gating --------------------------------------------------------
  walk <- reactiveValues(step = 1)
  output$crud_step <- reactive(walk$step)
  outputOptions(output, "crud_step", suspendWhenHidden = FALSE)
  observeEvent(input$crud_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$crud_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$crud_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$crud_continue4, walk$step <- max(walk$step, 5))
  observeEvent(input$crud_continue5, walk$step <- max(walk$step, 6))

  output$crud_toc <- renderUI(
    walkthrough_toc(walk$step, CRUD_STEP_TITLES, "crud-step"))

  # ---- Wire the in-step demo editors against the scratch copy -------------
  for (.i in 1:6) {
    local({
      eid <- paste0("csed", .i)
      sql_editor_server_crud(eid, input, output, session, con_holder = con_holder)
    })
  }

  # ---- Wire the 10 graded tasks (post-state verification) -----------------
  wire_crud_quest(CRUD_QUEST, "crudq", input, output, session, con_holder)

  output$changing_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("crud_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Changing data — INSERT, UPDATE, DELETE, and taking it back",
            eyebrow = "Writing, safely",
            p(style = "margin-bottom:6px;",
              "Every other tab only ", tags$b("reads"), ". Here you ", tags$b("write"),
              ": add rows, change them, remove them. Since the fastest way to learn ",
              "what ", kbd("DELETE"), " without a ", kbd("WHERE"), " does is to do it once, ",
              "you get a ", tags$b("private, disposable copy"),
              " of the institute's database to do it on."),
            p(style = "margin-bottom:0;",
              "For an R user: these are the edits you would normally make by reassigning ",
              "a data frame (", kbd("df <- rbind(df, row)"), ", ", kbd("df$x[i] <- v"),
              ", ", kbd("df <- df[-i, ]"), "), except that they persist in the database ",
              "rather than in your session, which is why they deserve more caution.")
          ),

          scratch_reset_bar("crud_reset", "crud_reset_msg"),

          # ===== STEP 1 =====
          step_container(1, "Your own private copy",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "The query tabs connect read-only, so no statement can alter the ",
                "shipped data. This tab instead seeds a fresh in-memory database ",
                "from the same schema (keys, constraints, and indexes included) and ",
                "the same rows, scoped to your session: a disposable sandbox with ",
                "full write access."),
              plain = tagList(tags$b("In human words. "),
                "You are editing a throwaway clone, not the master. If you mangle it, ",
                "press ", tags$b("Reset"), " and it is pristine again. No one else ",
                "sees your changes, and no one else's changes reach you."),
              lab = tagList(tags$b("Lab database. "),
                "Same six tables, same rows you have been querying, now editable. ",
                "Run a harmless ", kbd("SELECT"), " to confirm the data is all here, ",
                "then start changing it.")
            ),
            sql_code("-- Your copy starts identical to the real thing:\nSELECT COUNT(*) AS labs FROM labs;"),
            sql_editor("csed1", starter = "SELECT COUNT(*) AS labs FROM labs;"),
            callout_warm(tags$b("Read-only elsewhere, writable here. "),
              "On the query tabs, an ", kbd("INSERT"), " or ", kbd("DELETE"),
              " is refused. On this tab it runs, against your copy only. The editors ",
              "here take one statement per Run; if you paste two, the first runs and a ",
              "note says the second did not."),
            continue_button("crud_continue1"),
            id_prefix = "crud-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.crud_step >= 2",
          step_container(2, "INSERT: add rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("INSERT INTO table (columns…) VALUES (values…)"),
                " appends a row. Naming the columns explicitly means the statement ",
                "still works if someone later adds a column, and lets you omit columns ",
                "that have defaults or accept ", tags$code("NULL"), "."),
              plain = tagList(tags$b("In human words. "),
                "List the columns, then the matching values in the same order. Use ",
                kbd("NULL"), " for an unknown, quotes for text, bare numbers for ",
                "numbers. Count the commas; the two lists must line up."),
              lab = tagList(tags$b("Lab database. "),
                kbd("INSERT INTO labs (lab_id, lab_name, building, founded_year) VALUES (8, 'Test Lab', 'Wexler Hall', 2025)"),
                " adds a lab. Then ", kbd("SELECT * FROM labs"), " to see it, ",
                "sitting there at the bottom, looking new.")
            ),
            sql_code("INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (8, 'Test Lab', 'Wexler Hall', 2025);"),
            sql_editor("csed2", starter = "INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (8, 'Test Lab', 'Wexler Hall', 2025);"),
            callout_warm(tags$b("Run it twice. "),
              "The second attempt fails with ", tags$em("UNIQUE constraint failed: labs.lab_id"),
              ". ", kbd("lab_id"), " is the table's primary key, and a primary key admits ",
              "no duplicates. The constraint did its job: the error is the database ",
              "declining to let lab 8 exist twice, which is what you would want it to do ",
              "at two in the morning. The Tables tab covers where such rules come from."),
            help_details("Several rows at once, and rows from a query",
              tagList(
                p(style = "margin-bottom:6px;", "One ", kbd("INSERT"), " can carry many rows: ",
                  kbd("VALUES (25, 'Dave', 'human', 40), (26, 'Gary', 'seagull', NULL)"),
                  ". Either every row goes in or, if any of them breaks a rule, none does."),
                p(style = "margin-bottom:0;", "The rows can also come from a query in place of ",
                  kbd("VALUES"), ": ", kbd("INSERT INTO archive SELECT * FROM studies WHERE status = 'abandoned'"),
                  " copies whatever the ", kbd("SELECT"), " returns, provided the columns ",
                  "line up. It is the usual way to move data between tables, and the ",
                  "Tables tab uses it."))),
            help_details("Leaving a column out",
              tagList("Omit a nullable column and it becomes ", kbd("NULL"),
                "; omit one with a default (like ", kbd("enrollments.dropped_out"),
                ") and it takes the default. Columns marked ", kbd("NOT NULL"),
                " with no default must be supplied, and the database will tell you ",
                "so, which is more than it does for most mistakes.")),
            continue_button("crud_continue2"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.crud_step >= 3",
          step_container(3, "UPDATE: change rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("UPDATE table SET col = expr WHERE …"),
                " modifies the rows matching the predicate. The ", tags$code("WHERE"),
                " is what scopes it; its expression may reference the column's ",
                "current value."),
              plain = tagList(tags$b("In human words. "),
                "Say what to change and, above all, ", tags$b("which rows"),
                ". You can compute from the old value, e.g. ",
                kbd("SET sample_size = sample_size + 10"), ", which is safer than ",
                "typing the new number, since it is right even if the old number ",
                "wasn't what you remembered."),
              lab = tagList(tags$b("Lab database. "),
                kbd("UPDATE researchers SET salary = 36000 WHERE researcher_id = 2"),
                " gives one grad student a raise; without the ", kbd("WHERE"),
                " it would set ", tags$em("everyone's"), " salary to 36000, ",
                "which the PIs would notice.")
            ),
            sql_code("UPDATE studies\nSET sample_size = sample_size + 10\nWHERE study_id = 3;"),
            sql_editor("csed3", starter = "UPDATE studies\nSET sample_size = sample_size + 10\nWHERE study_id = 3;"),
            help_details("Several columns at once; values from a subquery",
              tagList("Separate the assignments with commas: ",
                kbd("SET status = 'completed', sample_size = 61"),
                ". The new value can be any expression, a subquery included: ",
                kbd("UPDATE researchers SET salary = (SELECT AVG(salary) FROM researchers) WHERE researcher_id = 8"),
                " pays Bram Shudder the institute average. Everything on the right of the ",
                kbd("="), " signs is computed from the row as it was before the update, so ",
                kbd("SET a = b, b = a"), " swaps the two columns.")),
            callout_warm(tags$b("UPDATE without WHERE. "),
              "It changes every row in the table, and it does not ask first. The ",
              "habit that prevents it: write the ", kbd("WHERE"), " first, then go ",
              "back and fill in the ", kbd("SET"), "."),
            continue_button("crud_continue3"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.crud_step >= 4",
          step_container(4, "DELETE: remove rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("DELETE FROM table WHERE …"),
                " removes the matching rows. As with ", tags$code("UPDATE"),
                ", the ", tags$code("WHERE"), " defines the scope; omitting it ",
                "empties the table."),
              plain = tagList(tags$b("In human words. "),
                "Pick the rows to drop with ", kbd("WHERE"),
                ", and they are gone. Without a ", kbd("WHERE"), ", every row is gone. ",
                "There is no confirmation dialog; SQL was designed by people who ",
                "assumed you meant it."),
              lab = tagList(tags$b("Lab database. "),
                kbd("DELETE FROM enrollments WHERE dropped_out = 1"),
                " purges the dropouts; ", kbd("DELETE FROM participants WHERE participant_id = 24"),
                " removes just Pip, who was enrolled in nothing anyway.")
            ),
            sql_code("DELETE FROM enrollments\nWHERE dropped_out = 1;"),
            sql_editor("csed4", starter = "DELETE FROM enrollments\nWHERE dropped_out = 1;"),
            callout_warm(tags$b("Preview before you delete. "),
              "Run the same ", kbd("WHERE"), " as a ", kbd("SELECT"),
              " first: ", kbd("SELECT * FROM enrollments WHERE dropped_out = 1"),
              " shows exactly which rows are about to vanish. Then swap ", kbd("SELECT *"),
              " for ", kbd("DELETE"), ". This takes ten seconds and has saved a great ",
              "many careers."),
            help_details("DELETE, TRUNCATE, DROP",
              tagList("Three ways to lose data, in ascending order of thoroughness. ",
                kbd("DELETE FROM t WHERE …"), " removes rows. ", kbd("DELETE FROM t"),
                " removes all of them and leaves an empty table. ", kbd("TRUNCATE TABLE t"),
                ", in PostgreSQL, MySQL, and SQL Server, is a faster way of saying the ",
                "same thing; SQLite has no ", kbd("TRUNCATE"), " and does not need one. ",
                kbd("DROP TABLE t"), " (Tables tab) removes the rows, the columns, and the ",
                "table.")),
            continue_button("crud_continue4"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.crud_step >= 5",
          step_container(5, "Insert-or-update, and RETURNING",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "An ", tags$code("ON CONFLICT"), " clause on an ", tags$code("INSERT"),
                " specifies what to do when the new row would violate a uniqueness ",
                "constraint: ", tags$code("DO NOTHING"), ", or ", tags$code("DO UPDATE SET …"),
                ", within which ", tags$code("excluded"), " refers to the row that failed ",
                "to insert (an “upsert”). A ", tags$code("RETURNING"),
                " clause makes a data-changing statement return the rows it affected."),
              plain = tagList(tags$b("In human words. "),
                "“Add this row; if it is already there, update it.” Without an upsert ",
                "that is a ", kbd("SELECT"), " to look, then an ", kbd("INSERT"), " or an ",
                kbd("UPDATE"), " depending on what you found, and a chance for something ",
                "to change in between. ", kbd("RETURNING *"), " shows you the row as it ",
                "ended up, which saves running a ", kbd("SELECT"), " afterwards to check."),
              lab = tagList(tags$b("Lab database. "),
                "The statement below files a Test Lab under ", kbd("lab_id"),
                " 8. If step 2 already created lab 8, the insert conflicts, and the ",
                kbd("DO UPDATE"), " moves the existing lab to the Greenhouse Annex. Either ",
                "way there is one lab 8 at the end, and you can run it as often as you like.")
            ),
            sql_code("INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (8, 'Test Lab', 'Greenhouse Annex', 2025)\nON CONFLICT (lab_id) DO UPDATE SET building = excluded.building\nRETURNING *;"),
            sql_editor("csed5", starter = "INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (8, 'Test Lab', 'Greenhouse Annex', 2025)\nON CONFLICT (lab_id) DO UPDATE SET building = excluded.building\nRETURNING *;", height = "140px"),
            callout_warm(tags$b("This is the least standardized statement in the tutorial. "),
              "SQLite and PostgreSQL share the ", kbd("ON CONFLICT"), " syntax above. ",
              "MySQL writes ", kbd("ON DUPLICATE KEY UPDATE"), "; SQL Server and Oracle use ",
              "a separate ", kbd("MERGE"), " statement. ", kbd("RETURNING"),
              " is likewise SQLite (3.35 and later) and PostgreSQL; SQL Server's version is ",
              "called ", kbd("OUTPUT"), ". The need is universal, and you will have to ",
              "look up the local spelling."),
            help_details("INSERT OR REPLACE, and why it is not the same thing",
              tagList("SQLite also accepts ", kbd("INSERT OR REPLACE INTO …"), " (or, shorter still, ",
                kbd("REPLACE INTO …"), "), which looks like a more compact upsert. It works by ",
                "deleting the conflicting row and inserting the new one, so any column you ",
                "did not mention reverts to its default, and with foreign keys enforced ",
                "the delete can cascade. ", kbd("ON CONFLICT … DO UPDATE"),
                " changes only the columns you name. ", kbd("INSERT OR IGNORE"),
                " is the short form of ", kbd("DO NOTHING"), " and carries no such risk.")),
            continue_button("crud_continue5"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 6 =====
          conditionalPanel("output.crud_step >= 6",
          step_container(6, "Transactions: BEGIN, COMMIT, ROLLBACK",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A transaction groups statements into a unit that takes effect entirely ",
                "or not at all. ", tags$code("BEGIN"), " opens one; ", tags$code("COMMIT"),
                " makes its changes permanent; ", tags$code("ROLLBACK"),
                " discards them. Outside an explicit transaction each statement is its ",
                "own transaction and commits immediately (autocommit). The guarantees ",
                "are summarized as ACID: atomicity, consistency, isolation, durability."),
              plain = tagList(tags$b("In human words. "),
                "An undo, provided you asked for it in advance. Between ", kbd("BEGIN"),
                " and ", kbd("COMMIT"), " your changes are provisional: you can see them, ",
                "and ", kbd("ROLLBACK"), " makes them never have happened. The textbook ",
                "case is a bank transfer, a debit and a credit that must both succeed or ",
                "both fail. The everyday case is step 4's advice, upgraded: ",
                kbd("BEGIN"), ", run the ", kbd("DELETE"), ", look at the damage, and ",
                "only then decide."),
              lab = tagList(tags$b("Lab database. "),
                "Run the four statements below one at a time. After the second, all 289 ",
                "measurements are gone, and the count in the third confirms it. After the ",
                "fourth they are back, and nobody at the institute need ever know.")
            ),
            sql_code("BEGIN;\nDELETE FROM measurements;               -- no WHERE. On purpose, this once.\nSELECT COUNT(*) AS n FROM measurements;  -- 0\nROLLBACK;                                -- and run the SELECT again: 289"),
            p(style = "margin:8px 0 4px 0;", "One statement per Run. Start with ", kbd("BEGIN;"),
              " then replace the editor's contents with each of the others in turn:"),
            sql_editor("csed6", starter = "BEGIN;"),
            callout_warm(tags$b("Finish what you BEGIN. "),
              "An open transaction stays open, on this tab and in real life, until you ",
              kbd("COMMIT"), " or ", kbd("ROLLBACK"), ". Forget, and every later change ",
              "on this copy joins the same transaction, to be swept away by a ",
              kbd("ROLLBACK"), " you run half an hour from now. (On a shared database it ",
              "is worse: your uncommitted changes can hold locks that make everyone else ",
              "wait.) Once you ", kbd("COMMIT"), ", there is no undo. A real database ",
              "has no Reset button, and this is the nearest thing it has to one."),
            help_details("SAVEPOINT, and what the other databases call BEGIN",
              tagList("Transactions do not nest, but ", kbd("SAVEPOINT name"),
                " sets a marker inside one; ", kbd("ROLLBACK TO name"),
                " undoes back to the marker without abandoning the rest, and ",
                kbd("RELEASE name"), " removes it. Elsewhere, ", kbd("BEGIN"),
                " is also spelled ", kbd("BEGIN TRANSACTION"), " or ",
                kbd("START TRANSACTION"), " (MySQL). The permissions statements that ",
                "usually share a chapter with these, ", kbd("GRANT"), " and ",
                kbd("REVOKE"), ", do not exist in SQLite, whose only permission system ",
                "is whether you can open the file.")),
            callout_warm(tags$b("Once more, since it is the mistake that costs the most. "),
              "Scope every ", kbd("UPDATE"), " and ", kbd("DELETE"),
              " with a ", kbd("WHERE"), " unless you truly mean every row. Write the ",
              kbd("WHERE"), " before the ", kbd("SET"), " or the ", kbd("DELETE"),
              ". When the change matters, wrap it in a transaction and look before you ",
              kbd("COMMIT"), "."),
            continue_button("crud_done", "I've got DML \u2193"),
            id_prefix = "crud-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.crud_step >= 6",
          quest_section(
            "Ten changes to make writing automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "Write each statement, press ", tags$b("Run"), " to apply it to your ",
              "copy, then ", tags$b("Check my answer"), ", which inspects your copy's ",
              "new state and compares it to the target. The tasks are independent, ",
              "but if things get tangled, ", tags$b("Reset my copy"), " above puts ",
              "everything back."),
            quest_editors_ui(CRUD_QUEST, "crudq")
          ))
        )
      )
    )
  })
}
