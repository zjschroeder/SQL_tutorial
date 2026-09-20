# =============================================================================
# tab_start.R — "Start here" landing tab
#
# Orients an R user who knows data frames but not databases: what a relational
# database is, the anatomy (tables/rows/columns/keys), what SQL is (declarative,
# which seeds order-of-execution later), the clickable schema of the running
# database, the written-order and execution-order ribbons, a vocabulary table
# (statement / clause / predicate ...; DQL / DML / DDL / TCL / DCL), and a
# routing grid.
# =============================================================================

source_start_server <- function(input, output, session) {

  # Clicking a "Where to go" routing card switches to that tab. The cards set
  # input$start_goto (see start_route()); we forward it to the navbar.
  observeEvent(input$start_goto, {
    nav_select(id = "main_nav", selected = input$start_goto, session = session)
  })

  output$start_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1100px; padding-top: 16px;",

      # ---- Welcome banner ----
      div(class = "p-4 mb-4",
          style = "background:linear-gradient(135deg,#e1f1fb 0%,#d3e9f6 100%); border-radius:14px; border-top:6px solid #0072B2;",
          div(style = "font-weight:700; letter-spacing:0.06em; text-transform:uppercase; font-size:12px; color:#0072B2; margin-bottom:6px;",
              "A hands-on tour of SQL"),
          h1("Learn SQL by interrogating a mildly disreputable research institute",
             style = "margin:0 0 10px 0; font-size:30px;"),
          p(style = "font-size:16px; line-height:1.6; color:#244; max-width:760px; margin-bottom:0;",
            "Recall the first time someone handed you a data frame and asked a ",
            "question of it. SQL is that same conversation with a database, and ",
            "the grammar is smaller than you'd guess. Each tab here teaches one ",
            "piece of that grammar, builds a query one clause at a time, then hands ",
            "you a live editor to run real queries against a real database, in your ",
            "browser, with nothing to install. The database belongs to ",
            tags$b("The Institute for Highly Questionable Research"),
            ", whose six labs have spent several decades and a surprising amount ",
            "of grant money finding out whether seagulls back off when stared at, ",
            "whether people named Dave know more Daves, and whether crickets are ",
            "telepathic (they are not; that study was abandoned).")
      ),

      # ---- For the R user ----
      div(class = "row g-3 mb-4",
        div(class = "col-md-4",
          div(style = "background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:18px; height:100%;",
            h4("If you know data frames…", style = "margin-top:0;"),
            p(style = "line-height:1.6; margin-bottom:0;",
              "…you already have most of the intuition, and I'd rather not pretend ",
              "otherwise. A ", tags$b("table"), " is a data frame. A ", tags$b("query"),
              " is what you'd do with ", kbd("subset()"), ", ", kbd("aggregate()"),
              ", or dplyr, with one difference in posture: you ",
              tags$em("describe the result you want"),
              " and the database works out how to produce it."))),
        div(class = "col-md-4",
          div(style = "background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:18px; height:100%;",
            h4("What's different", style = "margin-top:0;"),
            p(style = "line-height:1.6; margin-bottom:0;",
              "A database is ", tags$b("many tables linked by shared id columns"),
              " (keys) rather than one wide frame. A study stores its lab's id, not ",
              "the lab's name, building, and founding year over and over. That ",
              "linking is what ", kbd("JOIN"), " follows, and it is why nothing ",
              "gets typed twice across the whole institute."))),
        div(class = "col-md-4",
          div(style = "background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:18px; height:100%;",
            h4("How to use this", style = "margin-top:0;"),
            p(style = "line-height:1.6; margin-bottom:0;",
              "Work a tab top to bottom. Each step unlocks the next with a ",
              tags$b("Continue"), " button, and the sidebar keeps score. Every tab ",
              "ends with ten ", tags$b("Now you try"), " questions in a live editor. ",
              "Ten is roughly how many it takes before the clause stops feeling ",
              "like a foreign phrase.")))
      ),

      # ---- Anatomy ----
      h3("The anatomy of a database", style = "margin-bottom:10px;"),
      div(class = "row g-3 mb-4",
        div(class = "col-md-6",
          div(style = "line-height:1.7;",
            p(tags$b("Table"), " — a grid of data about one kind of thing ",
              "(a ", kbd("studies"), " table, a ", kbd("researchers"), " table). ",
              "The formal name is a ", tags$em("relation"), ", which is where ",
              "“relational database” comes from; nobody says it out loud."),
            p(tags$b("Row"), " — one record: a single study, a single ",
              "researcher. Formally a ", tags$em("tuple"), "."),
            p(tags$b("Column"), " — one field that every row has: a study's ",
              kbd("year"), ", a researcher's ", kbd("salary"), " (or, for two of ",
              "them, a discreet blank)."),
            p(tags$b("Primary key"), " — the column that uniquely names each ",
              "row (", kbd("study_id"), "). ", tags$b("Foreign key"),
              " — a column pointing at another table's primary key (",
              kbd("studies.lab_id"), " → ", kbd("labs.lab_id"), "). Keys are ",
              "the wiring; ", kbd("JOIN"), " is the act of following it."))),
        div(class = "col-md-6",
          div(style = "background:#0f2430; border-radius:10px; padding:16px; font-family:'JetBrains Mono',monospace; font-size:12.5px; color:#e8f0f3; line-height:1.7;",
            div(style="color:#8fd0bd;", "# studies (one row per study)"),
            div("study_id  title                          lab_id"),
            div(style="color:#b8c6cc;", "1         Do seagulls back off…         1"),
            div(style="color:#b8c6cc;", "2         Peas in babies' ears…         2"),
            div(style="margin-top:8px; color:#8fd0bd;", "# labs (lab_id is the link)"),
            div("lab_id    lab_name"),
            div(style="color:#b8c6cc;", "1         Avian Intimidation Lab"),
            div(style="color:#b8c6cc;", "2         Pediatric Otolaryngology Annex"),
            div(style="margin-top:8px; color:#f0c674;", "studies.lab_id → labs.lab_id")))
      ),

      # ---- Execution-order preview ----
      h3("The order the clauses actually run in", style = "margin-bottom:6px;"),
      p(style = "line-height:1.6; max-width:820px;",
        "You ", tags$em("write"), " a query starting with ", kbd("SELECT"),
        ", but the database does not run it in that order. It runs the clauses ",
        "in a fixed logical sequence, shown below. In my experience most “why ",
        "doesn't this work?” moments in SQL trace back to this sequence, so ",
        "the ribbon reappears on later tabs to light up whichever clause you are ",
        "learning at the time."),
      written_order_ribbon(
        note = "This order is fixed by the grammar. Put WHERE after GROUP BY and the reward is a syntax error."),
      execution_order_ribbon(
        active = NULL,
        note = "No need to memorize it yet. Notice only that SELECT, the clause you write first, runs sixth of ten, after the rows have been found, filtered, and grouped."),
      help_details("How literally should I take “runs in this order”?",
        tagList("This is the ", tags$em("logical"), " order: the order that explains ",
          "which clause can see what. The database's query planner is free to do the ",
          "physical work in any sequence that produces the same answer (it will often ",
          "apply a ", kbd("WHERE"), " filter while it is still reading the table, for ",
          "instance), and it does. One wrinkle is specific to SQLite and comes up on the ",
          "SELECT and WHERE tabs: SQLite lets ", kbd("WHERE"), " use a column alias ",
          "from ", kbd("SELECT"), ", which the logical order says should be impossible. ",
          "It manages this by quietly substituting the expression the alias stands for. ",
          "PostgreSQL, SQL Server, and Oracle refuse.")),

      # ---- Vocabulary ----
      h3("A few words you will keep meeting", style = "margin:22px 0 6px 0;"),
      p(style = "line-height:1.6; max-width:820px; margin-bottom:4px;",
        "Documentation and error messages use these terms without defining them, so ",
        "here they are once."),
      term_table(head = c("Term", "Meaning"), rows = list(
        c("statement", "One complete instruction, ending in a semicolon. “Query” usually means a statement that only reads, i.e., a SELECT."),
        c("clause", "A named part of a statement, introduced by a keyword: the FROM clause, the WHERE clause."),
        c("keyword", "A word SQL reserves for itself: SELECT, FROM, NULL. Keywords are not case-sensitive; they are written in capitals by convention, so they stand apart from the names."),
        c("identifier", "A name somebody chose: a table, a column, an alias. In SQLite these are not case-sensitive either."),
        c("literal", "A value typed out in the query: 'PI', 2019, NULL. Text literals are case-sensitive, so 'pi' is not 'PI'."),
        c("expression", "Anything that computes a value: salary * 1.03, UPPER(nickname), a literal on its own."),
        c("predicate", "An expression whose answer is true, false, or unknown: age >= 30. WHERE, HAVING, and ON each take one."),
        c("schema", "The design of the database: which tables exist, their columns, types, and keys. The six cards below are this database's schema."))),
      p(style = "line-height:1.6; max-width:820px;",
        "SQL's statements are conventionally sorted into families, and job postings use ",
        "the acronyms, so they are worth recognizing: ", tags$b("DQL"), " (querying: ",
        kbd("SELECT"), "), ", tags$b("DML"), " (changing rows: ", kbd("INSERT"), ", ",
        kbd("UPDATE"), ", ", kbd("DELETE"), "), ", tags$b("DDL"), " (defining tables: ",
        kbd("CREATE"), ", ", kbd("ALTER"), ", ", kbd("DROP"), "), ", tags$b("TCL"),
        " (transactions: ", kbd("BEGIN"), ", ", kbd("COMMIT"), ", ", kbd("ROLLBACK"),
        "), and ", tags$b("DCL"), " (permissions: ", kbd("GRANT"), ", ", kbd("REVOKE"),
        "). This tutorial covers the first four. SQLite has no DCL at all: the database ",
        "is a file, and whoever can open the file can do anything to it."),
      help_details("House rules of the grammar",
        tagList(
          p(style = "margin-bottom:6px;", "A statement ends with a semicolon. With a single statement in the ",
            "editor SQLite forgives its absence; write it anyway, since most other tools do not."),
          p(style = "margin-bottom:6px;", "Spaces and line breaks mean nothing to the database. Put each clause ",
            "on its own line for the benefit of whoever reads the query next, who is usually you."),
          p(style = "margin-bottom:6px;", "Comments come in two kinds: ", kbd("-- like this"),
            " (to the end of the line) and ", kbd("/* like this */"), " (between the marks, across lines if need be)."),
          p(style = "margin-bottom:0;", "Single quotes are for text values (", kbd("'PI'"),
            "); double quotes are for identifiers that need protecting, such as a column name with a space in it (",
            kbd("\"lab name\""), "). The WHERE tab has more to say about what happens when you mix them up."))),
      help_details("Which SQL is this?",
        tagList("SQL is a standard with dialects. PostgreSQL, MySQL, SQL Server, Oracle, ",
          "and SQLite each implement most of the standard and then add or omit things. ",
          "This tutorial runs SQLite, because SQLite runs in a browser. Nearly everything ",
          "here is portable; where SQLite does something the others don't (or the ",
          "reverse), the text says so, and the last tab collects the differences in one place.")),

      # ---- Schema explorer ----
      h3("The institute's database", style = "margin:18px 0 4px 0;"),
      p(style = "line-height:1.6; max-width:820px; margin-bottom:14px;",
        "Six tables, none large. This is the data every tab queries. Skim it now; ",
        "you will be back, probably within minutes."),
      schema_explorer(),

      # ---- Relationship map ----
      div(class = "p-3 my-4",
          style = "background:#f6fafb; border:1px solid #e0e6ea; border-radius:10px;",
          div(style="font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-bottom:8px;",
              "How the tables connect"),
          div(style = "font-family:'JetBrains Mono',monospace; font-size:13px; color:#243b44; line-height:1.9;",
            HTML("labs &nbsp;1───∞&nbsp; researchers<br>",
                 "labs &nbsp;1───∞&nbsp; studies<br>",
                 "researchers &nbsp;1───∞&nbsp; studies &nbsp;&nbsp;(as lead_researcher)<br>",
                 "studies &nbsp;∞───∞&nbsp; participants &nbsp;&nbsp;(via enrollments)<br>",
                 "enrollments &nbsp;1───∞&nbsp; measurements"))),

      # ---- Routing grid ----
      h3("Where to go", style = "margin:8px 0 12px 0;"),
      div(class = "row g-3 mb-4",
        start_route("SELECT",        "Pull columns out of a table. Every query starts here."),
        start_route("WHERE",         "Keep only the rows you want, and meet NULL, the idea that trips up everyone at least once."),
        start_route("ORDER BY",      "Sort, and take the top N."),
        start_route("GROUP BY",      "Summarize: counts, averages, one row per group."),
        start_route("Functions",     "Text, numbers, and dates; CASE for if-then logic; COALESCE for the NULLs."),
        start_route("JOINs",         "Combine tables by their keys. The reason a database is not a pile of spreadsheets."),
        start_route("UNION",         "Stack one query's rows on another's, or take the overlap, or the difference."),
        start_route("Subqueries",    "Queries inside queries, and CTEs so they stay readable."),
        start_route("Windows",       "Per-row calculations across a group: ranks, running totals, moving averages."),
        start_route("Changing data", "INSERT, UPDATE, DELETE, and transactions, on your own disposable copy."),
        start_route("Tables",        "CREATE TABLE, types, constraints, keys, ALTER, DROP, and views."),
        start_route("Put it together","20 mixed challenges, a free sandbox, a keyword index, and a short note on speed.")
      ),

      div(style = "text-align:center; color:#6b7c85; font-size:13px; padding:8px 0 24px 0;",
          "Start with ", tags$b("SELECT"), " in the top navigation, or tap a card above.")
    )
  })
}

# A routing card. Clicking it switches to that tab: the onclick sets a Shiny
# input which source_start_server() observes and feeds to nav_select(). `label`
# must match the nav_panel title exactly.
start_route <- function(label, blurb) {
  div(class = "col-md-4",
    div(class = "tab-grid-card", style = "cursor:pointer;",
      onclick = sprintf(
        "Shiny.setInputValue('start_goto', '%s', {priority:'event'});", label),
      div(style = "font-family:'JetBrains Mono',monospace; font-weight:700; color:#0072B2; font-size:15px; margin-bottom:6px;",
          label),
      div(style = "font-size:13.5px; color:#555; line-height:1.5;", blurb))
  )
}
