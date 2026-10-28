# =============================================================================
# tab_where.R — "WHERE ... " filtering walkthrough (Tab 3)
#
# Five gated steps (comparisons; AND/OR/NOT; BETWEEN/IN; LIKE; NULL & three-
# valued logic) then a 10-question live-editor quest graded by result-set
# comparison. NULL gets its own scaffolded step because the = NULL trap and the
# "filter silently drops NULL rows" surprise are top beginner failure points.
# =============================================================================

WHERE_QUEST <- list(
  list(prompt = "Return every column of the labs founded before the year 2000.",
       ref = "SELECT * FROM labs WHERE founded_year < 2000;", ordered = FALSE,
       solution = "SELECT * FROM labs WHERE founded_year < 2000;"),
  list(prompt = "Return the full_name of every researcher whose title is PI.",
       ref = "SELECT full_name FROM researchers WHERE title = 'PI';", ordered = FALSE,
       solution = "SELECT full_name FROM researchers WHERE title = 'PI';"),
  list(prompt = "Return the title of every study whose status is completed.",
       ref = "SELECT title FROM studies WHERE status = 'completed';", ordered = FALSE,
       solution = "SELECT title FROM studies WHERE status = 'completed';"),
  list(prompt = paste0("Return the nickname and age of every participant aged 30 or older. ",
                       "(Participants of unknown age should not appear, and won't, for reasons ",
                       "step 5 explains.)"),
       ref = "SELECT nickname, age FROM participants WHERE age >= 30;", ordered = FALSE,
       solution = "SELECT nickname, age FROM participants WHERE age >= 30;"),
  list(prompt = paste0("Return the full_name and salary of researchers earning between ",
                       "100000 and 120000 inclusive. Use BETWEEN."),
       ref = "SELECT full_name, salary FROM researchers WHERE salary BETWEEN 100000 AND 120000;",
       ordered = FALSE,
       solution = "SELECT full_name, salary FROM researchers WHERE salary BETWEEN 100000 AND 120000;"),
  list(prompt = "Return the title of every study run by lab 1, 3, or 5. Use IN.",
       ref = "SELECT title FROM studies WHERE lab_id IN (1, 3, 5);", ordered = FALSE,
       solution = "SELECT title FROM studies WHERE lab_id IN (1, 3, 5);"),
  list(prompt = paste0("Return the participant_id and nickname of every participant whose ",
                       "nickname starts with the letter D. Use LIKE."),
       ref = "SELECT participant_id, nickname FROM participants WHERE nickname LIKE 'D%';",
       ordered = FALSE,
       solution = "SELECT participant_id, nickname FROM participants WHERE nickname LIKE 'D%';"),
  list(prompt = "Return the full_name of researchers whose salary is undisclosed (NULL).",
       ref = "SELECT full_name FROM researchers WHERE salary IS NULL;", ordered = FALSE,
       solution = "SELECT full_name FROM researchers WHERE salary IS NULL;"),
  list(prompt = "Return the lab_name and building of labs that have a known building (not NULL).",
       ref = "SELECT lab_name, building FROM labs WHERE building IS NOT NULL;", ordered = FALSE,
       solution = "SELECT lab_name, building FROM labs WHERE building IS NOT NULL;"),
  list(prompt = "Return the title and status of every study that is NOT completed.",
       ref = "SELECT title, status FROM studies WHERE status <> 'completed';", ordered = FALSE,
       solution = "SELECT title, status FROM studies WHERE status <> 'completed';")
)

WHERE_STEP_TITLES <- c(
  "Filter rows with WHERE",
  "Combine conditions: AND, OR, NOT",
  "Ranges and sets: BETWEEN, IN",
  "Pattern matching with LIKE",
  "NULL and three-valued logic"
)

source_where_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$where_step <- reactive(walk$step)
  outputOptions(output, "where_step", suspendWhenHidden = FALSE)

  observeEvent(input$where_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$where_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$where_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$where_continue4, walk$step <- max(walk$step, 5))

  output$where_toc <- renderUI(
    walkthrough_toc(walk$step, WHERE_STEP_TITLES, "where-step"))

  wire_demo_editors("wsed", 5, input, output, session)
  wire_quest_editors(WHERE_QUEST, "wherq", input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzwhe", 1, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(WHERE_DQUEST, "dzwheq", input, output, session, con_fun = district_con_ro)

  output$where_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("where_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "WHERE — keeping only the rows you want",
            eyebrow = "Filtering",
            p(style = "margin-bottom:6px;",
              kbd("SELECT"), " chose ", tags$b("columns"), "; ", kbd("WHERE"),
              " chooses ", tags$b("rows"), ". You write a yes/no test, and SQLite ",
              "keeps each row for which the answer is yes. Simple enough, with one ",
              "exception large enough to get its own step: what happens when the ",
              "answer is neither yes nor no."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("WHERE age >= 30"), " is ",
              kbd("subset(participants, age >= 30)"), ", the same per-row test with ",
              "the same result. R's ", kbd("NA"), " even behaves much like SQL's ",
              kbd("NULL"), ", so if you have ever been surprised by ",
              kbd("mean()"), " returning ", kbd("NA"), ", you are already prepared for step 5.")
          ),

          # ===== STEP 1 =====
          step_container(1, "Filter rows with WHERE",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A ", tags$code("WHERE"), " clause supplies a predicate evaluated ",
                "per row against the source relation; rows for which it evaluates ",
                "to true are retained, the rest discarded before projection."),
              plain = tagList(tags$b("In human words. "),
                "Write a test each row either passes or fails. Passing rows stay. ",
                "The usual comparisons are ", kbd("="), ", ", kbd("<>"),
                " (not equal; ", kbd("!="), " is accepted too), ", kbd("<"), ", ",
                kbd(">"), ", ", kbd("<="), ", ", kbd(">="),
                ". One equals sign, not two; SQL predates that convention."),
              lab = tagList(tags$b("Lab database. "),
                kbd("WHERE founded_year < 2000"), " keeps the three older labs; ",
                kbd("WHERE title = 'PI'"), " keeps the seven principal investigators. ",
                "Text values go in ", tags$b("single quotes"), ".")
            ),
            sql_code("SELECT full_name, title\nFROM researchers\nWHERE title = 'PI';"),
            p(style = "margin:8px 0 4px 0;", "Run it, then try ", kbd("<>"),
              " to get everyone who is ", tags$em("not"), " a PI, or compare ",
              kbd("start_year"), " against a number:"),
            sql_editor("wsed1", starter = "SELECT full_name, title\nFROM researchers\nWHERE title = 'PI';"),
            callout_warm(tags$b("Quotes matter. "),
              "Single quotes are for text values: ", kbd("'PI'"), ". Numbers take ",
              "no quotes: ", kbd("founded_year < 2000"), ". Double quotes mean ",
              "something else in SQL: they mark an identifier (a column or table ",
              "name). SQLite muddies this. If a double-quoted word matches no column, ",
              "SQLite quietly treats it as text, so ", kbd("title = \"PI\""),
              " works here and is an error in PostgreSQL. And if the word does match a ",
              "column, as in ", kbd("title = \"title\""), ", you have compared the ",
              "column with itself and get all twelve researchers back. Use single ",
              "quotes for text and neither surprise can happen."),
            help_details("Capitals matter inside the quotes",
              tagList("Keywords and column names ignore case; text values do not. ",
                kbd("WHERE title = 'pi'"), " returns no rows, because the table says ",
                kbd("'PI'"), " and to ", kbd("="), " those are different strings. (",
                kbd("LIKE"), ", in step 4, is more relaxed about this.)")),
            continue_button("where_continue1"),
            id_prefix = "where-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.where_step >= 2",
          step_container(2, "Combine conditions: AND, OR, NOT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Predicates compose with the boolean operators ", tags$code("AND"),
                ", ", tags$code("OR"), ", ", tags$code("NOT"), ". ", tags$code("AND"),
                " binds tighter than ", tags$code("OR"), ", so parenthesize when ",
                "mixing them to make grouping explicit."),
              plain = tagList(tags$b("In human words. "),
                kbd("AND"), " means both must hold; ", kbd("OR"), " means either will do; ",
                kbd("NOT"), " flips a test. When you mix ", kbd("AND"), " and ",
                kbd("OR"), " in one clause, add parentheses. SQL will happily guess ",
                "what you meant, and it will guess the same way every time, which ",
                "is not the same as guessing correctly."),
              lab = tagList(tags$b("Lab database. "),
                kbd("WHERE title = 'PI' AND start_year < 2010"),
                " finds the senior PIs (four of them); ", kbd("WHERE title = 'PI' OR title = 'Postdoc'"),
                " widens the net to two ranks.")
            ),
            sql_code("SELECT full_name, title, start_year\nFROM researchers\nWHERE title = 'PI' AND start_year < 2010;"),
            sql_editor("wsed2", starter = "SELECT full_name, title, start_year\nFROM researchers\nWHERE title = 'PI' AND start_year < 2010;"),
            help_details("Why parentheses matter with AND/OR",
              tagList(kbd("WHERE a = 1 OR a = 2 AND b = 3"), " reads as ",
                kbd("a = 1 OR (a = 2 AND b = 3)"), " because ", kbd("AND"),
                " binds first. If you meant ", kbd("(a = 1 OR a = 2) AND b = 3"),
                ", you must write the parentheses yourself. An unnecessary pair costs ",
                "nothing.")),
            continue_button("where_continue2"),
            id_prefix = "where-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.where_step >= 3",
          step_container(3, "Ranges and sets: BETWEEN and IN",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("BETWEEN a AND b"), " is shorthand for ", tags$code(">= a AND <= b"),
                " (inclusive on both ends). ", tags$code("IN (…)"),
                " tests membership in an explicit set, equivalent to a chain of ",
                tags$code("OR"), " equalities."),
              plain = tagList(tags$b("In human words. "),
                kbd("BETWEEN"), " is a tidy way to say “inside this range, ",
                "endpoints included.” ", kbd("IN"), " is a tidy way to say ",
                "“any of these values.” Neither does anything ", kbd("AND"),
                " and ", kbd("OR"), " couldn't; they read closer to the question."),
              lab = tagList(tags$b("Lab database. "),
                kbd("salary BETWEEN 100000 AND 120000"), " catches the comfortably ",
                "paid; ", kbd("lab_id IN (1, 3, 5)"),
                " keeps studies from just those three labs.")
            ),
            sql_code("SELECT full_name, salary\nFROM researchers\nWHERE salary BETWEEN 100000 AND 120000;"),
            sql_editor("wsed3", starter = "SELECT full_name, salary\nFROM researchers\nWHERE salary BETWEEN 100000 AND 120000;"),
            help_details("NOT IN, NOT BETWEEN, NOT LIKE",
              tagList("Each of these operators takes a ", kbd("NOT"), " in front: ",
                kbd("lab_id NOT IN (1, 3, 5)"), ", ", kbd("salary NOT BETWEEN 100000 AND 120000"),
                ", ", kbd("nickname NOT LIKE 'D%'"), ". They read the way they sound. ",
                kbd("NOT IN"), " has one trap, involving NULL, that waits until the ",
                "Subqueries tab, where it does the most damage.")),
            callout_warm(tags$b("BETWEEN includes both endpoints. "),
              kbd("BETWEEN 100000 AND 120000"), " keeps a salary of exactly ",
              "100000 and exactly 120000. If you want to exclude an endpoint, ",
              "write the ", kbd(">"), " / ", kbd("<"), " version instead. Off-by-one ",
              "arguments about ranges are older than SQL and this will not settle them."),
            continue_button("where_continue3"),
            id_prefix = "where-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.where_step >= 4",
          step_container(4, "Pattern matching with LIKE",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("LIKE"), " matches a string against a pattern where ",
                tags$code("%"), " stands for any run of characters (including none) ",
                "and ", tags$code("_"), " for exactly one. SQLite's ", tags$code("LIKE"),
                " is case-insensitive for ASCII letters by default."),
              plain = tagList(tags$b("In human words. "),
                kbd("%"), " is a wildcard for “anything here”; ", kbd("_"),
                " is a wildcard for “one character here.” Between them you get ",
                "“starts with,” “ends with,” and “contains,” which covers most of ",
                "what people want before they reach for regular expressions."),
              lab = tagList(tags$b("Lab database. "),
                kbd("nickname LIKE 'D%'"), " finds every participant whose name ",
                "starts with D, which at this institute means eight people, all named ",
                "Dave. ", kbd("LIKE '%gull%'"), " finds Steven Seagull, a seagull.")
            ),
            sql_code("SELECT participant_id, nickname\nFROM participants\nWHERE nickname LIKE 'D%';"),
            sql_editor("wsed4", starter = "SELECT participant_id, nickname\nFROM participants\nWHERE nickname LIKE 'D%';"),
            help_details("% versus _",
              tagList(kbd("'D%'"), " matches Dave, and would match Dolores and D. if the ",
                "institute recruited any. ", kbd("'D_'"),
                " matches only two-character names beginning with D. Combine them: ",
                kbd("'_a%'"), " matches anything whose second letter is a, which here ",
                "means the Daves, Marf, and Tam.")),
            help_details("Case, literal percent signs, and GLOB",
              tagList(
                p(style = "margin-bottom:6px;", "In SQLite, ", kbd("LIKE 'd%'"), " finds the Daves too, because ",
                  kbd("LIKE"), " ignores case for the letters A to Z. This is a dialect ",
                  "matter: PostgreSQL's ", kbd("LIKE"), " is case-sensitive, and it offers ",
                  kbd("ILIKE"), " for the relaxed version."),
                p(style = "margin-bottom:6px;", "To match an actual percent sign or underscore, name an escape ",
                  "character: ", kbd("LIKE '50\\%' ESCAPE '\\'"), " matches the text “50%”."),
                p(style = "margin-bottom:0;", "SQLite also has ", kbd("GLOB"), ", which is case-sensitive and ",
                  "uses the file-system wildcards ", kbd("*"), " and ", kbd("?"),
                  " in place of ", kbd("%"), " and ", kbd("_"), ": ",
                  kbd("nickname GLOB 'D*'"), ". It is SQLite's own; don't expect it elsewhere. ",
                  "Regular expressions (", kbd("REGEXP"), ") are not available in this build."))),
            continue_button("where_continue4"),
            id_prefix = "where-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.where_step >= 5",
          step_container(5, "NULL and three-valued logic",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("NULL"), " denotes the absence of a value, not zero or ",
                "empty text. Comparisons with ", tags$code("NULL"), " yield ",
                tags$em("unknown"), " (a third truth value), and a ", tags$code("WHERE"),
                " keeps a row only when its predicate is ", tags$em("true"),
                ", so unknown rows are dropped. Test for it with ",
                tags$code("IS NULL"), " / ", tags$code("IS NOT NULL"), "."),
              plain = tagList(tags$b("In human words. "),
                tags$code("NULL"), " means “we don't know.” Asking ",
                kbd("salary = NULL"), " is asking “does an unknown equal an ",
                "unknown?”, and the only honest answer is “unknown,” which is not ",
                "“yes,” so the row is dropped. This is perhaps counterintuitive, ",
                "and it is also entirely consistent. Use ", kbd("IS NULL"),
                ", never ", kbd("= NULL"), "."),
              lab = tagList(tags$b("Lab database. "),
                "Two researchers have an undisclosed ", kbd("salary"),
                ", and the Climate Affect Collective has a ", kbd("NULL"),
                " building (it is between buildings). ",
                kbd("WHERE salary IS NULL"), " finds the discreet pair; ",
                kbd("= NULL"), " returns nothing at all, without complaint.")
            ),
            sql_code("SELECT full_name, salary\nFROM researchers\nWHERE salary IS NULL;"),
            sql_editor("wsed5", starter = "SELECT full_name, salary\nFROM researchers\nWHERE salary IS NULL;"),
            callout_warm(tags$b("A cautionary note is in order. "),
              "A filter like ", kbd("WHERE salary > 100000"), " also quietly drops ",
              "the NULL-salary rows. They are not greater, not less, just unknown, ",
              "and unknown fails every test. If you need those rows too, add ",
              kbd("OR salary IS NULL"), " explicitly. In my experience this is the ",
              "most common reason a query returns fewer rows than it should, and it ",
              "never produces an error message."),
            help_details("AND, OR, and NOT with an unknown in the mix",
              tagList("Three-valued logic has a short rulebook. ", kbd("NOT unknown"),
                " is unknown. ", kbd("unknown AND false"), " is false, since no value of the ",
                "unknown could rescue it; ", kbd("unknown AND true"), " stays unknown. ",
                kbd("unknown OR true"), " is true, for the mirror-image reason; ",
                kbd("unknown OR false"), " stays unknown. R users have seen all of this: ",
                kbd("NA & FALSE"), " is ", kbd("FALSE"), ", and ", kbd("NA | TRUE"),
                " is ", kbd("TRUE"), ". To swap a NULL for a stand-in value before ",
                "testing it, see ", kbd("COALESCE"), " on the Functions tab.")),
            execution_order_ribbon(
              active = c("FROM", "WHERE"),
              note = "WHERE runs right after FROM, before SELECT. By the standard, that means WHERE cannot use a column alias invented in SELECT, since the alias does not exist yet, and PostgreSQL, SQL Server, and Oracle stop with an error. SQLite lets it through by substituting the expression the alias stands for. Repeat the expression in WHERE and the query runs everywhere."),
            continue_button("where_done", "I've got WHERE ↓"),
            id_prefix = "where-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.where_step >= 5",
          quest_section(
            "Ten queries to make filtering automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "Each needs a ", kbd("WHERE"), " clause: comparisons, ",
              kbd("AND"), "/", kbd("OR"), ", ", kbd("BETWEEN"), ", ", kbd("IN"),
              ", ", kbd("LIKE"), ", or ", kbd("IS NULL"), ". Tables: ", kbd("labs"),
              ", ", kbd("researchers"), ", ", kbd("studies"), ", ", kbd("participants"), "."),
            quest_editors_ui(WHERE_QUEST, "wherq")
          ),
          dsec_where())
        )
      )
    )
  })
}
