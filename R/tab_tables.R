# =============================================================================
# tab_tables.R — "Tables" walkthrough (DDL: defining the database itself)
#
# Sits after Changing data. Every sequenced course consulted keeps changing rows
# (DML) apart from defining tables (DDL), and most teach DML first. Five gated
# steps (CREATE TABLE and types; constraints; foreign keys; ALTER and DROP;
# views, with CREATE TABLE AS and indexes in the collapsibles) then a
# 10-task quest.
#
# Runs on the same private scratch copy as the Changing-data tab
# (scratch_holder() in sql_helpers.R). Tasks are graded by post-state
# verification, mostly against SQLite's own catalogue (sqlite_master and the
# pragma_* table-valued functions).
#
# Independence: every task's verify query reads only objects that no other task
# and no in-step demo on either writing tab changes. In particular the view and
# CREATE TABLE AS tasks filter on studies.year and labs.founded_year, which no
# Changing-data task touches; the demo tables (lab_notes, equipment) have names
# no task uses; and the one table a task drops (participants_old) exists only
# on the scratch copy, where institute_con_scratch() puts it for the purpose.
# =============================================================================

TABLES_QUEST <- list(
  list(prompt = paste0("The institute is starting a seminar series. Create a table seminars with ",
                       "four columns: seminar_id (INTEGER, the primary key), topic (TEXT, required), ",
                       "lab_id (INTEGER), and attendance (INTEGER)."),
       verify = "SELECT name, UPPER(type) AS type, \"notnull\" AS required, pk FROM pragma_table_info('seminars');",
       solution = "CREATE TABLE seminars (\n  seminar_id INTEGER PRIMARY KEY,\n  topic      TEXT NOT NULL,\n  lab_id     INTEGER,\n  attendance INTEGER\n);"),
  list(prompt = paste0("Create a table buildings: building_id (INTEGER primary key), name (TEXT, ",
                       "required, and no two buildings may share one), and floors (INTEGER, ",
                       "defaulting to 1)."),
       verify = paste0("SELECT (SELECT COUNT(*) FROM pragma_table_info('buildings')) AS n_cols, ",
                       "(SELECT COUNT(*) FROM pragma_index_list('buildings') WHERE \"unique\" = 1 AND origin = 'u') AS n_unique, ",
                       "(SELECT \"notnull\" FROM pragma_table_info('buildings') WHERE name = 'name') AS name_required, ",
                       "(SELECT dflt_value IS NOT NULL FROM pragma_table_info('buildings') WHERE name = 'floors') AS floors_has_default;"),
       solution = "CREATE TABLE buildings (\n  building_id INTEGER PRIMARY KEY,\n  name        TEXT NOT NULL UNIQUE,\n  floors      INTEGER DEFAULT 1\n);"),
  list(prompt = paste0("Participants are occasionally paid. Create a table payments: payment_id ",
                       "(INTEGER primary key), participant_id (INTEGER, required), and amount (REAL), ",
                       "with a CHECK that the amount is greater than zero."),
       verify = paste0("SELECT (SELECT COUNT(*) FROM pragma_table_info('payments')) AS n_cols, ",
                       "(SELECT sql LIKE '%check%' FROM sqlite_master WHERE name = 'payments') AS has_check;"),
       solution = "CREATE TABLE payments (\n  payment_id     INTEGER PRIMARY KEY,\n  participant_id INTEGER NOT NULL,\n  amount         REAL CHECK (amount > 0)\n);"),
  list(prompt = paste0("Several labs keep a pet, for reasons connected with study 8. Create a table ",
                       "lab_pets: pet_id (INTEGER primary key), pet_name (TEXT, required), and lab_id ",
                       "(INTEGER), declared as a foreign key that REFERENCES labs(lab_id). Name the ",
                       "referenced column explicitly."),
       verify = "SELECT \"table\" AS parent, \"from\" AS child_column, \"to\" AS parent_column FROM pragma_foreign_key_list('lab_pets');",
       solution = "CREATE TABLE lab_pets (\n  pet_id   INTEGER PRIMARY KEY,\n  pet_name TEXT NOT NULL,\n  lab_id   INTEGER REFERENCES labs(lab_id)\n);"),
  list(prompt = "The institute would like to email its participants. Add a column email (TEXT) to the participants table.",
       verify = "SELECT name, UPPER(type) AS type FROM pragma_table_info('participants') WHERE name = 'email';",
       solution = "ALTER TABLE participants ADD COLUMN email TEXT;"),
  list(prompt = "In the measurements table, the column called value has always been a score. Rename it to score.",
       verify = "SELECT name FROM pragma_table_info('measurements');",
       solution = "ALTER TABLE measurements RENAME COLUMN value TO score;"),
  list(prompt = paste0("Your copy contains a table called participants_old, a partial backup that ",
                       "somebody made once and nobody has dared remove. Remove it."),
       verify = "SELECT COUNT(*) AS n FROM sqlite_master WHERE name = 'participants_old';",
       solution = "DROP TABLE participants_old;"),
  list(prompt = paste0("Create a view early_studies showing the title and year of every study run ",
                       "before 2018."),
       verify = "SELECT title, year FROM early_studies;",
       solution = "CREATE VIEW early_studies AS\nSELECT title, year\nFROM studies\nWHERE year < 2018;"),
  list(prompt = paste0("Create a table old_labs from a query: the lab_name and founded_year of every ",
                       "lab founded before 2000. Use CREATE TABLE … AS SELECT."),
       verify = "SELECT lab_name, founded_year FROM old_labs;",
       solution = "CREATE TABLE old_labs AS\nSELECT lab_name, founded_year\nFROM labs\nWHERE founded_year < 2000;"),
  list(prompt = paste0("Queries that filter studies by year currently scan the whole table. Create an ",
                       "index named idx_studies_year on the year column of studies."),
       verify = "SELECT name, tbl_name FROM sqlite_master WHERE type = 'index' AND name = 'idx_studies_year';",
       solution = "CREATE INDEX idx_studies_year ON studies(year);")
)

TABLES_STEP_TITLES <- c(
  "CREATE TABLE and data types",
  "Constraints: rules the table enforces",
  "Foreign keys",
  "ALTER TABLE and DROP TABLE",
  "Views"
)

source_tables_server <- function(input, output, session) {

  scratch <- scratch_holder(session)
  con_holder <- scratch$get

  observeEvent(input$tables_reset, {
    scratch$reset()
    output$tables_reset_msg <- renderUI(
      div(style = "color:#0F6E56; font-size:13px; margin-top:8px;",
          "✓ Fresh copy restored. Your tables, views, and indexes are gone, and the original six (plus participants_old) are back."))
  })

  walk <- reactiveValues(step = 1)
  output$tables_step <- reactive(walk$step)
  outputOptions(output, "tables_step", suspendWhenHidden = FALSE)
  observeEvent(input$tables_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$tables_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$tables_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$tables_continue4, walk$step <- max(walk$step, 5))

  output$tables_toc <- renderUI(
    walkthrough_toc(walk$step, TABLES_STEP_TITLES, "tables-step"))

  for (.i in 1:5) {
    local({
      eid <- paste0("tsed", .i)
      sql_editor_server_crud(eid, input, output, session, con_holder = con_holder)
    })
  }

  wire_crud_quest(TABLES_QUEST, "tabq", input, output, session, con_holder)
  # The "In a school district" section (R/district_sections.R) runs on a
  # separate private copy of the district database, shared by both writing tabs.
  dscratch <- district_scratch_holder(session)
  sql_editor_server_crud("dztab1", input, output, session, con_holder = dscratch$get)
  sql_editor_server_crud("dztab2", input, output, session, con_holder = dscratch$get)
  wire_crud_quest(TABLES_DTASKS, "dztabq", input, output, session, dscratch$get,
                  scratch_fun = district_con_scratch)

  output$tables_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("tables_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Tables — defining the database itself",
            eyebrow = "CREATE, ALTER, DROP",
            p(style = "margin-bottom:6px;",
              "Where did the six tables come from? Somebody wrote six ", kbd("CREATE TABLE"),
              " statements, and in doing so decided what the institute's data is allowed ",
              "to look like: that a study must have a title, that two labs cannot share an ",
              "id, that a salary may be left blank. Every NULL that has complicated the ",
              "earlier tabs is there because a table definition permitted it. This tab ",
              "is about writing those definitions, changing them, and removing them."),
            p(style = "margin-bottom:0;",
              "For an R user: a data frame's columns have types and nothing else; any ",
              "further rule lives in your head, or in a ", kbd("stopifnot()"),
              " you meant to write. A table carries its rules with it and enforces them ",
              "on everyone, including the person who wrote them, six months later.")
          ),

          scratch_reset_bar("tables_reset", "tables_reset_msg"),

          # ===== STEP 1 =====
          step_container(1, "CREATE TABLE and data types",
            explanation_triad(
              formal = tagList(
                tags$code("CREATE TABLE name (column type constraints, …)"),
                " defines a relation. SQLite stores every value as one of five storage ",
                "classes: ", tags$code("NULL"), ", ", tags$code("INTEGER"), ", ",
                tags$code("REAL"), ", ", tags$code("TEXT"), ", and ", tags$code("BLOB"),
                ". A column's declared type gives it an ", tags$em("affinity"),
                ", a preferred class to which incoming values are converted when the ",
                "conversion is lossless, and which is otherwise not enforced."),
              plain = tagList(
                "Name the table, then list each column with the kind of thing it holds: ",
                "whole numbers, decimals, or text. SQLite has no separate types for dates ",
                "or true/false; dates go in ", kbd("TEXT"), " (year first, as on the ",
                "Functions tab) and booleans go in ", kbd("INTEGER"), " as 0 and 1, which ",
                "is what ", kbd("enrollments.dropped_out"), " has been all along."),
              lab = tagList(
                "The statement below makes a table for lab notes. An ",
                kbd("INTEGER PRIMARY KEY"), " column numbers itself: leave ",
                kbd("note_id"), " out of an ", kbd("INSERT"), " and SQLite supplies the ",
                "next integer. (Your copy also holds one table the real database does ",
                "not, ", kbd("participants_old"), "; task 7 will deal with it.)")
            ),
            sql_code("CREATE TABLE lab_notes (\n  note_id  INTEGER PRIMARY KEY,\n  lab_id   INTEGER,\n  written  TEXT,      -- a date, stored as 'YYYY-MM-DD'\n  note     TEXT\n);"),
            sql_editor("tsed1", starter = "CREATE TABLE lab_notes (\n  note_id  INTEGER PRIMARY KEY,\n  lab_id   INTEGER,\n  written  TEXT,\n  note     TEXT\n);", height = "150px"),
            p(style = "margin:10px 0 4px 0;", "Then run these two, one at a time, in the same editor:"),
            sql_code("INSERT INTO lab_notes (lab_id, written, note)\nVALUES ('six', '2024-03-15', 'Crickets restless.');\n\nSELECT note_id, lab_id, typeof(lab_id) AS stored_as FROM lab_notes;"),
            callout_warm(tags$b("SQLite's types are suggestions. "),
              "That insert put the word “six” in an ", kbd("INTEGER"), " column, and SQLite ",
              "took it: ", kbd("typeof()"), " reports ", kbd("text"), ". Had you written ",
              kbd("'6'"), ", it would have been converted to the integer; “six” cannot be, ",
              "so it is stored as it came. This is SQLite's most unusual habit. PostgreSQL, ",
              "MySQL, and SQL Server would all have refused the row. If you want the ",
              "refusal, end the definition with ", kbd(") STRICT;"), " (SQLite 3.37 and ",
              "later), or add a ", kbd("CHECK"), ", as in the next step."),
            help_details("Types in other databases",
              tagList("Elsewhere the type list is longer and is enforced: ", kbd("INTEGER"),
                " / ", kbd("BIGINT"), ", ", kbd("NUMERIC(10,2)"), " for exact decimals such ",
                "as money, ", kbd("REAL"), " / ", kbd("DOUBLE PRECISION"), ", ",
                kbd("VARCHAR(50)"), " and ", kbd("TEXT"), ", ", kbd("BOOLEAN"), ", ",
                kbd("DATE"), ", ", kbd("TIMESTAMP"), ". SQLite accepts all of these ",
                "spellings in a ", kbd("CREATE TABLE"), " and maps each to one of its five ",
                "classes, which is why a schema written for PostgreSQL will usually load, ",
                "and why ", kbd("VARCHAR(50)"), " in SQLite will happily hold 500 characters.")),
            help_details("Running it twice; and reading the catalogue",
              tagList(
                p(style = "margin-bottom:6px;", "A second ", kbd("CREATE TABLE lab_notes"),
                  " fails with ", tags$em("table lab_notes already exists"), ". ",
                  kbd("CREATE TABLE IF NOT EXISTS lab_notes (…)"), " does nothing, quietly, ",
                  "when the table is already there, which is what a setup script that may ",
                  "be run more than once wants."),
                p(style = "margin-bottom:0;", "To see what your copy contains, query SQLite's own catalogue: ",
                  kbd("SELECT type, name FROM sqlite_master;"), " lists every table, index, ",
                  "and view, and its ", kbd("sql"), " column holds the statement that created ",
                  "each. ", kbd("PRAGMA table_info(labs);"), " describes one table's columns. ",
                  "(Other databases keep the same information in ",
                  kbd("information_schema.tables"), " and ", kbd("information_schema.columns"), ".)"))),
            continue_button("tables_continue1"),
            id_prefix = "tables-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.tables_step >= 2",
          step_container(2, "Constraints: rules the table enforces",
            explanation_triad(
              formal = tagList(
                "A constraint is a condition every row must satisfy; a statement that ",
                "would violate one fails, and changes nothing. ", tags$code("PRIMARY KEY"),
                " identifies the row (unique, and conventionally not NULL). ",
                tags$code("NOT NULL"), " forbids missing values. ", tags$code("UNIQUE"),
                " forbids duplicates. ", tags$code("CHECK (expr)"),
                " requires the expression not to be false. ", tags$code("DEFAULT value"),
                " supplies a value when an ", tags$code("INSERT"), " omits the column."),
              plain = tagList(
                "Rules you state once, in the table definition, and the database then ",
                "applies to every insert and update from every source, forever. A rule ",
                "in the table cannot be forgotten by a script written next year. If you ",
                "have ever found a participant aged −3 in a dataset, you have met a table ",
                "that lacked a ", kbd("CHECK"), "."),
              lab = tagList(
                "The institute's equipment register: an id, an item that must be named, ",
                "a cost that cannot be negative, a serial number that cannot repeat, and a ",
                "condition that is assumed to be ", kbd("'working'"), " unless someone says ",
                "otherwise, which at this institute is optimistic.")
            ),
            sql_code("CREATE TABLE equipment (\n  equipment_id INTEGER PRIMARY KEY,\n  item         TEXT NOT NULL,\n  serial_no    TEXT UNIQUE,\n  cost         REAL CHECK (cost >= 0),\n  condition    TEXT NOT NULL DEFAULT 'working'\n);"),
            sql_editor("tsed2", starter = "CREATE TABLE equipment (\n  equipment_id INTEGER PRIMARY KEY,\n  item         TEXT NOT NULL,\n  serial_no    TEXT UNIQUE,\n  cost         REAL CHECK (cost >= 0),\n  condition    TEXT NOT NULL DEFAULT 'working'\n);", height = "170px"),
            p(style = "margin:10px 0 4px 0;", "Now try to get bad data past it. The first ",
              "of these succeeds; run it twice and the second attempt trips ", kbd("UNIQUE"),
              ". The other two each break a different rule:"),
            sql_code("INSERT INTO equipment (item, serial_no, cost) VALUES ('Seagull decoy (stern)', 'SG-001', 45.50);\nINSERT INTO equipment (serial_no, cost) VALUES ('SG-002', 12.00);              -- no item\nINSERT INTO equipment (item, cost) VALUES ('Thermometer, cricket-sized', -5);  -- negative cost"),
            callout_warm(tags$b("A CHECK lets NULL through. "),
              kbd("CHECK (cost >= 0)"), " rejects −5 and accepts a missing cost, because ",
              kbd("NULL >= 0"), " is unknown, and a ", kbd("CHECK"), " fails only when its ",
              "expression is false. It is the mirror image of ", kbd("WHERE"),
              ", which keeps a row only when its test is true. If the cost must also be ",
              "present, say both: ", kbd("cost REAL NOT NULL CHECK (cost >= 0)"), "."),
            help_details("Keys made of two columns, and other table-level constraints",
              tagList("A constraint involving more than one column is written on its own ",
                "line after the columns. The institute's ", kbd("enrollments"),
                " table ought to have had ", kbd("UNIQUE (study_id, participant_id)"),
                ", so that nobody can enroll in the same study twice; nothing in the data ",
                "violates that, but nothing in the table prevents it either. A primary key ",
                "can be composite in the same way: ", kbd("PRIMARY KEY (study_id, participant_id)"),
                ". And a ", kbd("CHECK"), " may compare columns: ", kbd("CHECK (end_year >= start_year)"), ".")),
            help_details("Auto-numbering elsewhere, and two SQLite oddities",
              tagList(
                p(style = "margin-bottom:6px;", "SQLite's self-numbering ", kbd("INTEGER PRIMARY KEY"),
                  " is spelled ", kbd("SERIAL"), " or ", kbd("GENERATED ALWAYS AS IDENTITY"),
                  " in PostgreSQL, ", kbd("AUTO_INCREMENT"), " in MySQL, and ",
                  kbd("IDENTITY(1,1)"), " in SQL Server. (SQLite does have an ",
                  kbd("AUTOINCREMENT"), " keyword. It means something narrower, never reusing ",
                  "the id of a deleted row, and is rarely needed.)"),
                p(style = "margin-bottom:0;", "For historical reasons SQLite allows NULL in a ",
                  kbd("PRIMARY KEY"), " column unless the column is an ",
                  kbd("INTEGER PRIMARY KEY"), ", so add ", kbd("NOT NULL"), " to a text or ",
                  "composite key yourself. And ", kbd("UNIQUE"), " permits any number of ",
                  "NULLs, on the reasoning that two unknowns are not known to be equal. ",
                  "(PostgreSQL and MySQL agree; SQL Server allows only one.)"))),
            continue_button("tables_continue2"),
            id_prefix = "tables-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.tables_step >= 3",
          step_container(3, "Foreign keys",
            explanation_triad(
              formal = tagList(
                "A ", tags$code("FOREIGN KEY"), " constraint (", tags$code("REFERENCES parent(column)"),
                ") requires each non-NULL value in the child column to exist in the ",
                "referenced parent column: referential integrity. Optional ",
                tags$code("ON DELETE"), " and ", tags$code("ON UPDATE"),
                " actions state what happens to children when the parent row changes. ",
                "SQLite parses foreign keys always and enforces them only when ",
                tags$code("PRAGMA foreign_keys = ON"), " has been issued on the connection."),
              plain = tagList(
                "The Start tab called keys “the wiring.” A foreign key constraint is what ",
                "stops anyone from wiring a study to lab 99 when there is no lab 99, or ",
                "from deleting a lab that studies still point to. A NULL foreign key is ",
                "allowed (it means “points at nothing,” as with Dr. Roams), unless the ",
                "column is also ", kbd("NOT NULL"), "."),
              lab = tagList(
                "All six of the institute's foreign keys are declared in its table definitions, ",
                "and none has ever been enforced, because SQLite leaves enforcement off ",
                "unless asked. The four statements below ask, and then attempt to delete ",
                "the Bioacoustics & Thermometry Lab out from under its three researchers ",
                "and two studies.")
            ),
            sql_code("PRAGMA foreign_keys;               -- 0: not enforcing\nPRAGMA foreign_keys = ON;\nDELETE FROM labs WHERE lab_id = 6;  -- FOREIGN KEY constraint failed\nPRAGMA foreign_keys = OFF;          -- put it back, for the sake of the other tab"),
            p(style = "margin:8px 0 4px 0;", "One statement per Run, in order:"),
            sql_editor("tsed3", starter = "PRAGMA foreign_keys;"),
            callout_warm(tags$b("Off by default, and per connection. "),
              "SQLite added foreign-key enforcement in 2009 and left it off so as not to ",
              "break databases that already existed; it has been off by default ever ",
              "since. The setting belongs to the connection, not the file, so every ",
              "program that opens a SQLite database has to issue the ", kbd("PRAGMA"),
              " for itself. No other major database works this way; in PostgreSQL, MySQL, ",
              "and SQL Server a declared foreign key is always enforced. Do switch it off ",
              "again when you are done here: with it on, task 7 on the Changing data tab ",
              "(deleting the dropouts) fails, because their measurements still point at them."),
            help_details("ON DELETE CASCADE, SET NULL, RESTRICT",
              tagList("By default a parent with children cannot be deleted, as you saw. The ",
                "declaration can choose otherwise. ", kbd("REFERENCES labs(lab_id) ON DELETE CASCADE"),
                " deletes the children along with the parent (delete a lab and its ",
                "studies go too). ", kbd("ON DELETE SET NULL"), " keeps the children and ",
                "blanks their pointer, which is how a researcher comes to have no lab. ",
                kbd("CASCADE"), " is convenient and deserves some fear: one ",
                kbd("DELETE"), " on a parent table can remove rows from five others, ",
                "and the row count it reports mentions only the first.")),
            help_details("Two ways to write it",
              tagList("Inline, beside the column: ", kbd("lab_id INTEGER REFERENCES labs(lab_id)"),
                ". Or as a separate line after the columns, which is the only option when ",
                "the key spans two columns and is how the institute's tables were written: ",
                kbd("FOREIGN KEY (lab_id) REFERENCES labs(lab_id)"),
                ". The two are equivalent. The referenced column should be the parent's ",
                "primary key, or at least ", kbd("UNIQUE"), ".")),
            continue_button("tables_continue3"),
            id_prefix = "tables-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.tables_step >= 4",
          step_container(4, "ALTER TABLE and DROP TABLE",
            explanation_triad(
              formal = tagList(
                tags$code("ALTER TABLE"), " modifies an existing table's definition. SQLite ",
                "supports four forms: ", tags$code("ADD COLUMN"), ", ",
                tags$code("RENAME COLUMN … TO …"), ", ", tags$code("DROP COLUMN"),
                ", and ", tags$code("RENAME TO"), " (the table itself). ",
                tags$code("DROP TABLE name"), " removes a table together with its rows, ",
                "indexes, and triggers."),
              plain = tagList(
                "Tables change after they are born: a new column is needed, an old name ",
                "turns out to be misleading. ", kbd("ALTER TABLE"), " does that in place, ",
                "keeping the rows. A column added to a table that already has rows needs ",
                "a value for them, so it is either nullable or has a ", kbd("DEFAULT"), "."),
              lab = tagList(
                "The institute has heard of preregistration. The statement below adds a ",
                kbd("preregistered"), " flag to ", kbd("studies"),
                ". All ten existing studies receive the default, 0, which in their case is ",
                "also accurate.")
            ),
            sql_code("ALTER TABLE studies\nADD COLUMN preregistered INTEGER NOT NULL DEFAULT 0;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then ", kbd("SELECT title, preregistered FROM studies;"),
              " to see the new column filled in:"),
            sql_editor("tsed4", starter = "ALTER TABLE studies\nADD COLUMN preregistered INTEGER NOT NULL DEFAULT 0;"),
            term_table(list(
              c("ALTER TABLE t ADD COLUMN c TYPE", "A new column at the end. NOT NULL requires a DEFAULT."),
              c("ALTER TABLE t RENAME COLUMN a TO b", "Same data, new name. Views and indexes that mention the column are updated to match."),
              c("ALTER TABLE t DROP COLUMN c", "The column and its data go. Refused if c is a key, is indexed, or is used in a constraint or a view. (SQLite 3.35 and later.)"),
              c("ALTER TABLE t RENAME TO t2", "Renames the table."),
              c("DROP TABLE t", "Removes the table and everything in it. DROP TABLE IF EXISTS t does not complain if there is no t."))),
            callout_warm(tags$b("DROP TABLE has no WHERE, and no undo outside a transaction. "),
              kbd("DELETE"), " without a ", kbd("WHERE"), " leaves you an empty table to ",
              "refill. ", kbd("DROP TABLE"), " leaves you nothing, and it does not ask. In ",
              "SQLite and PostgreSQL a ", kbd("DROP"), " inside ", kbd("BEGIN … ROLLBACK"),
              " can be taken back; in MySQL and Oracle it cannot, because there any DDL ",
              "statement commits the transaction on the spot. The protection that works ",
              "everywhere is a backup taken before you start."),
            help_details("What ALTER TABLE cannot do in SQLite",
              tagList("It cannot change a column's type, add or remove a constraint, or ",
                "reorder columns. PostgreSQL and the others can do most of that with ",
                kbd("ALTER TABLE … ALTER COLUMN"), " and ", kbd("ADD CONSTRAINT"),
                ". In SQLite the procedure is to create a new table with the definition ",
                "you want, copy the rows across with ", kbd("INSERT INTO new SELECT … FROM old"),
                ", drop the old table, and rename the new one, all inside one transaction. ",
                "It is as tedious as it sounds, and it is a good argument for getting the ",
                "constraints right the first time.")),
            continue_button("tables_continue4"),
            id_prefix = "tables-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.tables_step >= 5",
          step_container(5, "Views",
            explanation_triad(
              formal = tagList(
                "A view is a named, stored query. ", tags$code("CREATE VIEW name AS SELECT …"),
                " records the definition; the view is then queried like a table, and its ",
                "rows are computed from the underlying tables each time. It stores no data ",
                "of its own. ", tags$code("DROP VIEW name"), " removes the definition and ",
                "affects no data."),
              plain = tagList(
                "A CTE that outlives the query. If you find yourself pasting the same ",
                "three-table join into the top of every analysis, give it a name once and ",
                "select from the name. Because the view is re-run each time, it is never ",
                "out of date, which a copied table would be by the following morning."),
              lab = tagList(
                kbd("study_overview"), " does the joins from the JOINs tab once: each study ",
                "with its lab's name and its lead researcher's name, in place of two id ",
                "numbers. Afterwards, “which ongoing studies does the Cinematic Dread Unit ",
                "have?” is a one-line query against the view.")
            ),
            sql_code("CREATE VIEW study_overview AS\nSELECT s.study_id, s.title, l.lab_name, r.full_name AS lead, s.year, s.status\nFROM studies s\nJOIN labs l             ON s.lab_id = l.lab_id\nLEFT JOIN researchers r ON s.lead_researcher_id = r.researcher_id;"),
            sql_editor("tsed5", starter = "CREATE VIEW study_overview AS\nSELECT s.study_id, s.title, l.lab_name, r.full_name AS lead, s.year, s.status\nFROM studies s\nJOIN labs l             ON s.lab_id = l.lab_id\nLEFT JOIN researchers r ON s.lead_researcher_id = r.researcher_id;", height = "150px"),
            p(style = "margin:10px 0 4px 0;", "Then query it like any table:"),
            sql_code("SELECT title, lead, status\nFROM study_overview\nWHERE lab_name = 'Cinematic Dread Unit';"),
            callout_warm(tags$b("A view holds no rows of its own. "),
              "Update a study's status on the Changing data tab and the view shows the ",
              "new status at once; there is nothing in the view to go stale. The ",
              "price is that the join is re-run on every query. Views are also a common ",
              "way to control access in multi-user databases: grant someone the view that ",
              "omits the ", kbd("salary"), " column, and not the table. In SQLite a view is ",
              "read-only; ", kbd("INSERT"), " into one and you get an error."),
            help_details("CREATE TABLE … AS SELECT: the copy, for when you want one",
              tagList(kbd("CREATE TABLE old_labs AS SELECT lab_name, founded_year FROM labs WHERE founded_year < 2000"),
                " runs the query once and stores the result as a real table. It is a ",
                "snapshot: later changes to ", kbd("labs"), " do not reach it. It also ",
                "copies only the column names and the data. The primary key, the ",
                kbd("NOT NULL"), "s, the defaults, and the indexes of the source do not ",
                "come along, which has surprised enough people to be worth saying twice: ",
                "a table made this way has no constraints at all. (PostgreSQL also ",
                "accepts this syntax; SQL Server writes ", kbd("SELECT … INTO new_table"), ".) ",
                kbd("CREATE TEMP TABLE"), " makes a table that disappears when the ",
                "connection closes, which is often what a scratch copy should be.")),
            help_details("Indexes are DDL too",
              tagList(kbd("CREATE INDEX idx_studies_year ON studies(year);"),
                " and ", kbd("DROP INDEX idx_studies_year;"), " belong to the same family ",
                "of statements. What an index is for, and how to tell whether the database ",
                "is using one, is the subject of the short section at the end of the last ",
                "tab. One thing fits here: ", kbd("PRIMARY KEY"), " and ", kbd("UNIQUE"),
                " constraints are enforced with an index the database creates for you, so ",
                "those columns are already fast to search. ", kbd("CREATE UNIQUE INDEX"),
                " is, in effect, another way of adding a ", kbd("UNIQUE"), " constraint.")),
            continue_button("tables_done", "I've got DDL ↓"),
            id_prefix = "tables-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.tables_step >= 5",
          quest_section(
            "Ten changes to make defining tables automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "Write each statement, press ", tags$b("Run"), " to apply it to your copy, ",
              "then ", tags$b("Check my answer"), ", which inspects the copy's catalogue ",
              "and compares it with the target. Use the table, column, and index names ",
              "exactly as given, since the names are what gets checked. If a task says a ",
              "table already exists, an earlier attempt is still there: ",
              tags$b("Reset my copy"), " clears it (along with everything else)."),
            quest_editors_ui(TABLES_QUEST, "tabq")
          ),
          dsec_tables())
        )
      )
    )
  })
}
