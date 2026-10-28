# =============================================================================
# tab_order.R — "ORDER BY ... LIMIT" walkthrough (Tab 4)
#
# Five gated steps (sort asc/desc; multi-column sort; LIMIT; OFFSET / top-N;
# where NULLs sort) then a 10-question quest. These questions are graded with
# ordered = TRUE: row order is part of the answer, so the grader compares the
# sequence, not just the set. References are minimal canonical queries so a
# learner's identical query reproduces the same deterministic order. Every
# graded sort is a TOTAL order: where the main key has ties (two studies from
# 2022, two crickets aged 1) the prompt names a tiebreaker, because SQL leaves
# the order of tied rows undefined and an order-sensitive grader must not.
# =============================================================================

ORDER_QUEST <- list(
  list(prompt = "Return lab_name and founded_year for all labs, oldest first (founded_year ascending).",
       ref = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year;", ordered = TRUE,
       solution = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year;"),
  list(prompt = paste0("Return full_name and salary of researchers with a known salary, ",
                       "highest salary first."),
       ref = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC;",
       ordered = TRUE,
       solution = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC;"),
  list(prompt = paste0("Return nickname and age of every participant, youngest first, with the ",
                       "unknown ages at the end rather than the start. Break ties by nickname (A to Z)."),
       ref = "SELECT nickname, age FROM participants ORDER BY age NULLS LAST, nickname;", ordered = TRUE,
       solution = "SELECT nickname, age FROM participants ORDER BY age NULLS LAST, nickname;"),
  list(prompt = paste0("Return title and year of all studies, newest year first. Two studies share ",
                       "a year, so break ties by title (A to Z)."),
       ref = "SELECT title, year FROM studies ORDER BY year DESC, title;", ordered = TRUE,
       solution = "SELECT title, year FROM studies ORDER BY year DESC, title;"),
  list(prompt = paste0("Return title, status, and year of all studies, sorted by status ",
                       "(A to Z), then by year within each status."),
       ref = "SELECT title, status, year FROM studies ORDER BY status, year;", ordered = TRUE,
       solution = "SELECT title, status, year FROM studies ORDER BY status, year;"),
  list(prompt = "Return the lab_name and founded_year of the three oldest labs.",
       ref = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 3;", ordered = TRUE,
       solution = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 3;"),
  list(prompt = "Return the full_name and salary of the five highest-paid researchers.",
       ref = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC LIMIT 5;",
       ordered = TRUE,
       solution = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC LIMIT 5;"),
  list(prompt = "Return the title and sample_size of the single largest study by sample_size.",
       ref = "SELECT title, sample_size FROM studies ORDER BY sample_size DESC LIMIT 1;", ordered = TRUE,
       solution = "SELECT title, sample_size FROM studies ORDER BY sample_size DESC LIMIT 1;"),
  list(prompt = paste0("Return nickname and age of the three youngest participants whose age ",
                       "is known. (Ignore the unknown ages; step 5 explains why you must.) Two ",
                       "crickets are tied at age 1, so break ties by nickname."),
       ref = "SELECT nickname, age FROM participants WHERE age IS NOT NULL ORDER BY age, nickname LIMIT 3;",
       ordered = TRUE,
       solution = "SELECT nickname, age FROM participants WHERE age IS NOT NULL ORDER BY age, nickname LIMIT 3;"),
  list(prompt = paste0("Return the lab_name and founded_year of the second- and third-oldest labs ",
                       "(skip the oldest). Use LIMIT and OFFSET."),
       ref = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 2 OFFSET 1;", ordered = TRUE,
       solution = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 2 OFFSET 1;")
)

ORDER_STEP_TITLES <- c(
  "Sort with ORDER BY",
  "Sort by several columns",
  "Take the top few with LIMIT",
  "Skip rows with OFFSET",
  "Where NULLs sort"
)

source_order_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$order_step <- reactive(walk$step)
  outputOptions(output, "order_step", suspendWhenHidden = FALSE)

  observeEvent(input$order_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$order_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$order_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$order_continue4, walk$step <- max(walk$step, 5))

  output$order_toc <- renderUI(
    walkthrough_toc(walk$step, ORDER_STEP_TITLES, "order-step"))

  wire_demo_editors("osed", 5, input, output, session)
  wire_quest_editors(ORDER_QUEST, "ordq", input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzord", 1, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(ORDER_DQUEST, "dzordq", input, output, session, con_fun = district_con_ro)

  output$order_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("order_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "ORDER BY and LIMIT — sorting and taking the top N",
            eyebrow = "Putting rows in order",
            p(style = "margin-bottom:6px;",
              "In what order does a query return its rows? Whatever order the ",
              "database found convenient, which is a promise about nothing. ",
              kbd("ORDER BY"), " turns that into a real promise; ", kbd("LIMIT"),
              " keeps only the first few of the sorted rows. Together they answer ",
              "“top 5” and “most recent” questions, which is most of what sorting is for."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("ORDER BY x DESC"), " is ",
              kbd("arrange(desc(x))"), ", and ", kbd("LIMIT 5"), " is ", kbd("head(5)"),
              ". The one habit to unlearn is trusting that rows arrive in the order ",
              "you loaded them, which a data frame honours and a database does not.")
          ),

          # ===== STEP 1 =====
          step_container(1, "Sort with ORDER BY",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "An ", tags$code("ORDER BY"), " clause sorts the result by one or ",
                "more sort keys, each ascending (", tags$code("ASC"), ", the ",
                "default) or descending (", tags$code("DESC"), "). It is logically ",
                "the last step before ", tags$code("LIMIT"), "."),
              plain = tagList(tags$b("In human words. "),
                "Pick a column to sort by. Add ", kbd("DESC"), " for largest-first ",
                "or newest-first; leave it off for smallest-first (A→Z, low→high). ",
                "In practical terms, ", kbd("DESC"), " is the word you type when you ",
                "want the winners on top."),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY founded_year"), " lists labs oldest to newest, starting ",
                "with the Bioacoustics & Thermometry Lab (1979); ",
                kbd("ORDER BY salary DESC"), " puts Dr. Priya Eustachian on top at 121000.")
            ),
            sql_code("SELECT full_name, salary\nFROM researchers\nORDER BY salary DESC;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then flip to ", kbd("ASC"),
              " or sort by ", kbd("start_year"), " instead:"),
            sql_editor("osed1", starter = "SELECT full_name, salary\nFROM researchers\nORDER BY salary DESC;"),
            callout_warm(tags$b("Without ORDER BY, no order is promised. "),
              "Without ", kbd("ORDER BY"), " the database may return rows in any ",
              "order, and that order can change between runs for reasons it will not ",
              "share with you. If order matters to your answer, say so explicitly. ",
              "If it doesn't matter, you may still want to say so, to spare the next ",
              "reader from wondering."),
            continue_button("order_continue1"),
            id_prefix = "order-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.order_step >= 2",
          step_container(2, "Sort by several columns",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Listing multiple sort keys sorts by the first, breaking ties with ",
                "the second, and so on. Each key carries its own ", tags$code("ASC"),
                "/", tags$code("DESC"), "."),
              plain = tagList(tags$b("In human words. "),
                "Sort by A; when two rows tie on A, sort those by B. Sorting a ",
                "spreadsheet by one column and then a second as a tiebreaker is the ",
                "same idea, minus the dialog box."),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY status, year"), " groups studies by status (abandoned, ",
                "completed, ongoing, alphabetically) and, within each status, lists ",
                "them oldest to newest.")
            ),
            sql_code("SELECT title, status, year\nFROM studies\nORDER BY status, year;"),
            sql_editor("osed2", starter = "SELECT title, status, year\nFROM studies\nORDER BY status, year;"),
            help_details("Mixing ASC and DESC",
              tagList("Each key is independent: ", kbd("ORDER BY status, year DESC"),
                " sorts status A→Z but, within a status, newest year first. The ",
                kbd("DESC"), " applies only to the key it follows, not to the whole clause.")),
            callout_warm(tags$b("Tied rows come back in no promised order. "),
              "Two studies are from 2022. ", kbd("ORDER BY year DESC"), " puts them ",
              "next to each other and says nothing about which goes first; the database ",
              "may answer differently tomorrow, or after an index is added. When the ",
              "order of ties matters (and with ", kbd("LIMIT"), ", in the next step, it ",
              "decides who makes the cut), add keys until no two rows can tie. A primary ",
              "key as the last sort key always does it."),
            help_details("Sorting by an expression, an alias, or a column number",
              tagList(kbd("ORDER BY"), " takes any expression, not only a bare column: ",
                kbd("ORDER BY salary * 1.03 DESC"), ". Because it runs after ",
                kbd("SELECT"), ", it can also use a ", kbd("SELECT"), " alias, in every ",
                "database: ", kbd("SELECT salary * 1.03 AS after_raise … ORDER BY after_raise"),
                ". And ", kbd("ORDER BY 2"), " means “the second output column,” which is ",
                "quick to type and breaks the day someone reorders the select list. I'd ",
                "leave that one for throwaway queries.")),
            continue_button("order_continue2"),
            id_prefix = "order-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.order_step >= 3",
          step_container(3, "Take the top few with LIMIT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("LIMIT n"), " returns at most the first ", tags$code("n"),
                " rows of the (ordered) result. Paired with ", tags$code("ORDER BY"),
                " it expresses top-N queries."),
              plain = tagList(tags$b("In human words. "),
                "Sort first, then keep only the first few. ", kbd("ORDER BY x DESC LIMIT 5"),
                " is exactly “the top 5 by x.” Read it that way until you no longer ",
                "have to translate."),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY founded_year LIMIT 3"), " gives the three oldest labs; ",
                kbd("ORDER BY sample_size DESC LIMIT 1"), " gives the single biggest study, ",
                "the climate one, with 180 people. The seagull studies managed 40.")
            ),
            sql_code("SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 3;"),
            sql_editor("osed3", starter = "SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 3;"),
            callout_warm(tags$b("LIMIT without ORDER BY is a coin flip. "),
              kbd("LIMIT 5"), " on its own gives you ", tags$em("some"),
              " five rows, not a meaningful “top 5.” Sort first. The database will not ",
              "warn you, because as far as it is concerned it did exactly what you asked."),
            help_details("LIMIT elsewhere: TOP and FETCH FIRST",
              tagList(kbd("LIMIT"), " is how SQLite, PostgreSQL, and MySQL say it. SQL ",
                "Server writes ", kbd("SELECT TOP 3 …"), " at the front of the query ",
                "instead, and the SQL standard's own spelling, which PostgreSQL, Oracle, ",
                "and Db2 accept, is ", kbd("FETCH FIRST 3 ROWS ONLY"), " at the end. Same ",
                "idea in all three; it is one of the more annoying dialect differences, ",
                "since nearly every query someone hands you has one of them in it.")),
            continue_button("order_continue3"),
            id_prefix = "order-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.order_step >= 4",
          step_container(4, "Skip rows with OFFSET",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("OFFSET k"), " skips the first ", tags$code("k"),
                " rows before ", tags$code("LIMIT"), " takes over, which is the basis of ",
                "pagination (page 2 = ", tags$code("LIMIT 10 OFFSET 10"), ")."),
              plain = tagList(tags$b("In human words. "),
                "Jump past the first few rows, then take some. This is how you ask for ",
                "“the runner-up,” “the second page,” or “everyone but the winner.”"),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY founded_year LIMIT 2 OFFSET 1"),
                " skips the oldest lab and returns the next two: the Pediatric ",
                "Otolaryngology Annex (1985) and the Avian Intimidation Lab (1998).")
            ),
            sql_code("SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 2 OFFSET 1;"),
            sql_editor("osed4", starter = "SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 2 OFFSET 1;"),
            callout_warm(tags$b("Paging needs a total order. "),
              "If the sort has ties, page 1 and page 2 are each free to resolve them ",
              "differently, and a row can turn up on both pages or on neither. The ",
              "tiebreaker from step 2 stops being optional once ", kbd("OFFSET"),
              " is involved."),
            continue_button("order_continue4"),
            id_prefix = "order-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.order_step >= 5",
          step_container(5, "Where NULLs sort",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "In SQLite, ", tags$code("NULL"), " sorts as the lowest value: ",
                "first under ", tags$code("ASC"), ", last under ", tags$code("DESC"),
                ". A “smallest first” sort therefore surfaces the unknowns at the top. ",
                "The placement can be stated explicitly per sort key with ",
                tags$code("NULLS FIRST"), " or ", tags$code("NULLS LAST"), "."),
              plain = tagList(tags$b("In human words. "),
                "Unknown values clump at one end. Sort ages ascending and the ",
                "unknown ages come first, which is rarely what you meant by ",
                "“youngest.” The database has to put NULL somewhere, and SQLite ",
                "puts it first."),
              lab = tagList(tags$b("Lab database. "),
                "Six participants have a ", kbd("NULL"), " age, including all three ",
                "seagulls. ",
                kbd("ORDER BY age"), " lists those unknowns first; add ",
                kbd("WHERE age IS NOT NULL"), " when you mean “youngest known.”")
            ),
            sql_code("SELECT nickname, age\nFROM participants\nWHERE age IS NOT NULL\nORDER BY age\nLIMIT 3;"),
            sql_editor("osed5", starter = "SELECT nickname, age\nFROM participants\nORDER BY age;"),
            callout_warm(tags$b("“Top N” and NULLs. "),
              "A ", kbd("DESC"), " top-N is usually safe (NULLs sink to the bottom), ",
              "but an ", kbd("ASC"), " bottom-N will pull NULLs in unless you exclude ",
              "them first. Ask yourself, before every ", kbd("LIMIT"), ", where the ",
              "unknowns will land."),
            help_details("NULLS LAST, and what other databases do",
              tagList(kbd("ORDER BY age NULLS LAST"), " keeps the ascending sort and moves ",
                "the unknowns to the end, so the rows stay in the result but out of the ",
                "way. SQLite, PostgreSQL, and Oracle accept it; MySQL and SQL Server do ",
                "not. The default also varies: SQLite, MySQL, and SQL Server treat NULL ",
                "as the smallest value, while PostgreSQL and Oracle treat it as the ",
                "largest, so the same ", kbd("ORDER BY age"), " puts the seagulls first ",
                "here and last there. If the position of the NULLs matters, say where ",
                "you want them.")),
            execution_order_ribbon(
              active = c("ORDER BY", "LIMIT"),
              note = "ORDER BY and LIMIT run last, once every row and column of the result exists. That is why LIMIT without ORDER BY cuts an unsorted pile, and why ORDER BY can refer to a SELECT alias when WHERE, by the standard, cannot."),
            continue_button("order_done", "I've got ORDER BY ↓"),
            id_prefix = "order-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.order_step >= 5",
          quest_section(
            "Ten queries to make sorting automatic",
            intro = tagList(
              p(style = "line-height:1.6; margin-bottom:4px;",
                "These use ", kbd("ORDER BY"), ", ", kbd("NULLS LAST"), ", ",
                kbd("LIMIT"), ", and ", kbd("OFFSET"), ". Tables: ", kbd("labs"), ", ", kbd("researchers"),
                ", ", kbd("studies"), ", ", kbd("participants"), "."),
              p(style = "line-height:1.6; margin-bottom:0; color:#7A4F00; font-size:13px;",
                tags$b("Order counts here:"), " these are graded on the exact row ",
                "order, so get the sort direction right, and where a prompt names a ",
                "tiebreaker, use it. Elsewhere in the tutorial the grader is indifferent ",
                "to order; here it is the whole point.")),
            quest_editors_ui(ORDER_QUEST, "ordq")
          ),
          dsec_order())
        )
      )
    )
  })
}
