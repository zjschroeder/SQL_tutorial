# =============================================================================
# tab_start.R — "Start here" landing tab
#
# Orients an R user who knows data frames but not databases: what a relational
# database is, the anatomy (tables/rows/columns/keys), what SQL is (declarative,
# which seeds order-of-execution later), the clickable schema of the running
# database, a preview of the execution-order ribbon, and a routing grid.
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
          h1("Learn SQL by querying a slightly ridiculous research institute",
             style = "margin:0 0 10px 0; font-size:30px;"),
          p(style = "font-size:16px; line-height:1.6; color:#244; max-width:760px; margin-bottom:0;",
            "Every tab teaches one piece of SQL, builds a query one clause at a ",
            "time, then hands you a live editor to write real queries against a ",
            "real database \u2014 right here in your browser, nothing to install. ",
            "The data is ", tags$b("The Institute for Highly Questionable Research"),
            ", whose labs study whether seagulls back off when stared at, whether ",
            "people named Dave know more Daves, and other vital questions.")
      ),

      # ---- For the R user ----
      div(class = "row g-3 mb-4",
        div(class = "col-md-4",
          div(style = "background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:18px; height:100%;",
            h4("If you know data frames\u2026", style = "margin-top:0;"),
            p(style = "line-height:1.6; margin-bottom:0;",
              "\u2026you already have most of the intuition. A ", tags$b("table"),
              " is a data frame. A ", tags$b("query"), " is what you'd do with ",
              kbd("subset()"), ", ", kbd("aggregate()"), ", or dplyr \u2014 except ",
              "you ", tags$em("describe the result you want"),
              " and the database figures out how to get it."))),
        div(class = "col-md-4",
          div(style = "background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:18px; height:100%;",
            h4("What's different", style = "margin-top:0;"),
            p(style = "line-height:1.6; margin-bottom:0;",
              "A database is ", tags$b("many tables linked by shared id columns"),
              " (keys), not one flat frame. That linking is what makes ",
              kbd("JOIN"), " powerful, and it's why data isn't duplicated across ",
              "the whole institute."))),
        div(class = "col-md-4",
          div(style = "background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:18px; height:100%;",
            h4("How to use this", style = "margin-top:0;"),
            p(style = "line-height:1.6; margin-bottom:0;",
              "Work a tab top to bottom. Each step unlocks the next with a ",
              tags$b("Continue"), " button. The sidebar tracks your progress. ",
              "End each tab with its 10-question ", tags$b("Now you try"),
              " editor.")))
      ),

      # ---- Anatomy ----
      h3("The anatomy of a database", style = "margin-bottom:10px;"),
      div(class = "row g-3 mb-4",
        div(class = "col-md-6",
          div(style = "line-height:1.7;",
            p(tags$b("Table"), " \u2014 a grid of data about one kind of thing ",
              "(a ", kbd("studies"), " table, a ", kbd("researchers"), " table). ",
              "Also called a ", tags$em("relation"), "."),
            p(tags$b("Row"), " \u2014 one record: a single study, a single ",
              "researcher. Also called a ", tags$em("tuple"), "."),
            p(tags$b("Column"), " \u2014 one field that every row has: a study's ",
              kbd("year"), ", a researcher's ", kbd("salary"), "."),
            p(tags$b("Primary key"), " \u2014 the column that uniquely names each ",
              "row (", kbd("study_id"), "). ", tags$b("Foreign key"),
              " \u2014 a column pointing at another table's primary key (",
              kbd("studies.lab_id"), " \u2192 ", kbd("labs.lab_id"), "). Keys are ",
              "the wiring that ", kbd("JOIN"), " follows."))),
        div(class = "col-md-6",
          div(style = "background:#0f2430; border-radius:10px; padding:16px; font-family:'JetBrains Mono',monospace; font-size:12.5px; color:#e8f0f3; line-height:1.7;",
            div(style="color:#8fd0bd;", "# studies (one row per study)"),
            div("study_id  title                          lab_id"),
            div(style="color:#b8c6cc;", "1         Do seagulls back off\u2026         1"),
            div(style="color:#b8c6cc;", "2         Peas in babies' ears\u2026         2"),
            div(style="margin-top:8px; color:#8fd0bd;", "# labs (lab_id is the link)"),
            div("lab_id    lab_name"),
            div(style="color:#b8c6cc;", "1         Avian Intimidation Lab"),
            div(style="color:#b8c6cc;", "2         Pediatric Otolaryngology Annex"),
            div(style="margin-top:8px; color:#f0c674;", "studies.lab_id \u2192 labs.lab_id")))
      ),

      # ---- Execution-order preview ----
      h3("One idea that explains almost everything: order", style = "margin-bottom:6px;"),
      p(style = "line-height:1.6; max-width:820px;",
        "You ", tags$em("write"), " a query starting with ", kbd("SELECT"),
        ", but the database doesn't run it in that order. It runs the clauses ",
        "in a fixed logical order, shown below. Almost every \u201cwhy doesn't ",
        "this work?\u201d in SQL traces back to this sequence \u2014 so this ribbon ",
        "comes back on later tabs to light up whichever clause you're learning."),
      execution_order_ribbon(
        active = NULL,
        note = "Don't memorize it yet. Just notice that SELECT \u2014 the thing you write first \u2014 happens almost last."),

      # ---- Schema explorer ----
      h3("The institute's database", style = "margin:18px 0 4px 0;"),
      p(style = "line-height:1.6; max-width:820px; margin-bottom:14px;",
        "Six tables. This is the data every tab queries. Skim it now; you'll ",
        "come back to it constantly."),
      schema_explorer(),

      # ---- Relationship map ----
      div(class = "p-3 my-4",
          style = "background:#f6fafb; border:1px solid #e0e6ea; border-radius:10px;",
          div(style="font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-bottom:8px;",
              "How the tables connect"),
          div(style = "font-family:'JetBrains Mono',monospace; font-size:13px; color:#243b44; line-height:1.9;",
            HTML("labs &nbsp;1\u2500\u2500\u2500\u221e&nbsp; researchers<br>",
                 "labs &nbsp;1\u2500\u2500\u2500\u221e&nbsp; studies<br>",
                 "researchers &nbsp;1\u2500\u2500\u2500\u221e&nbsp; studies &nbsp;&nbsp;(as lead_researcher)<br>",
                 "studies &nbsp;\u221e\u2500\u2500\u2500\u221e&nbsp; participants &nbsp;&nbsp;(via enrollments)<br>",
                 "enrollments &nbsp;1\u2500\u2500\u2500\u221e&nbsp; measurements"))),

      # ---- Routing grid ----
      h3("Where to go", style = "margin:8px 0 12px 0;"),
      div(class = "row g-3 mb-4",
        start_route("SELECT",        "Pull columns out of a table. The foundation of every query."),
        start_route("WHERE",         "Keep only the rows you want \u2014 and meet NULL, SQL's trickiest idea."),
        start_route("ORDER BY",      "Sort and take the top N."),
        start_route("GROUP BY",      "Summarize: counts, averages, one row per group."),
        start_route("JOINs",         "Combine tables by their keys. The real power."),
        start_route("Subqueries",    "Queries inside queries; readable CTEs."),
        start_route("Windows",       "Per-row calculations across a group: ranks, running totals."),
        start_route("Changing data", "INSERT, UPDATE, DELETE \u2014 on your own private copy."),
        start_route("Put it together","20 mixed challenges, a free sandbox, and a note on speed.")
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
