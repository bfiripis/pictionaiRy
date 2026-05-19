# server.R  –  Pictionary AI Generator

function(input, output, session) {
  
  # Reactive states

  # Word bank returned by Claude (data.frame: theme, word, used, skipped)
  word_bank <- reactiveVal(NULL)
  
  # Game active flag
  game_active <- reactiveVal(FALSE)
  
  # Scores: named list team -> integer
  scores <- reactiveVal(list())
  
  # Team names vector (in order)
  team_names <- reactiveVal(character(0))
  
  # Current team index (1-based)
  current_team_idx <- reactiveVal(1)
  
  # Status message
  app_status <- reactiveVal("waiting")   # "waiting" | "ready" | "playing"
  
  # Skips remaining in current turn
  skips_remaining <- reactiveVal(0)
  
  # Active theme (selected by user for the current draw)
  active_theme <- reactiveVal(NULL)
  
  # Available custom themes accumulated by user
  extra_themes <- reactiveVal(character(0))
  
  # All available themes (default + user-added)
  all_themes <- reactive({
    c(DEFAULT_THEMES, extra_themes())
  })
  
  # Add custom theme
  observeEvent(input$add_theme, {
    req(input$custom_theme)
    ct <- str_trim(input$custom_theme)
    if (nchar(ct) > 0 && !ct %in% all_themes()) {
      extra_themes(c(extra_themes(), ct))
      # Update picker
      updatePickerInput(
        session,
        "themes",
        choices  = all_themes(),
        selected = c(input$themes, ct)
      )
      updateTextInput(session, "custom_theme", value = "")
    }
  })
  
  # Dynamic team name inputs
  output$team_name_inputs <- renderUI({
    n <- input$n_teams %||% 2
    inputs <- lapply(seq_len(n), function(i) {
      textInput(
        inputId = paste0("team_name_", i),
        label   = NULL,
        value   = paste("Team", i),
        placeholder = paste("Team", i, "name…")
      )
    })
    
    tagList(
      div(class = "sidebar-section-label mt-1", "Team Names"),
      inputs
    )
  })
  
  # Generate words via Claude
  observeEvent(input$generate_words, {
    req(input$themes)
    app_status("waiting")
    word_bank(NULL)
    
    n    <- input$n_per_category %||% 10L
    diff <- input$difficulty
    thm  <- input$themes
    
    showNotification("🤖 Asking Claude to generate words…", type = "message", duration = 4)
    
    tryCatch({
      prompt  <- build_claude_prompt(thm, n, diff)
      raw     <- call_claude(prompt)
      df      <- parse_claude_csv(raw)
      
      word_bank(df)
      app_status("ready")
      showNotification(
        glue("✅ {nrow(df)} words generated across {length(thm)} theme(s)!"),
        type     = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(paste("❌ Claude error:", e$message), type = "error", duration = 8)
    })
  })
  
  # Start game
  observeEvent(input$start_game, {
    req(word_bank())
    
    n <- input$n_teams %||% 2
    names_vec <- vapply(seq_len(n), function(i) {
      v <- input[[paste0("team_name_", i)]]
      if (is.null(v) || nchar(str_trim(v)) == 0) paste("Team", i) else str_trim(v)
    }, character(1))
    
    team_names(names_vec)
    sc <- setNames(as.list(rep(0L, n)), names_vec)
    scores(sc)
    current_team_idx(1)
    game_active(TRUE)
    app_status("playing")
    
    showNotification("🚀 Game started! Good luck 🎨", type = "message", duration = 4)
  })
  
  # Helpers: remaining words per theme
  remaining_for_theme <- function(theme_name) {
    wb <- word_bank()
    if (is.null(wb)) return(0L)
    wb |> filter(theme == theme_name, !used) |> nrow()
  }
  
  pick_random_word <- function(theme_name) {
    wb <- word_bank()
    if (is.null(wb)) return(NULL)
    pool <- wb |> filter(theme == theme_name, !used)
    if (nrow(pool) == 0) return(NULL)
    pool[sample(nrow(pool), 1), ]
  }
  
  mark_word_used <- function(word_val, theme_name, skipped = FALSE) {
    wb <- word_bank()
    if (is.null(wb)) return(invisible(NULL))
    idx <- which(wb$theme == theme_name & wb$word == word_val & !wb$used)
    if (length(idx) > 0) {
      wb$used[idx[1]]    <- TRUE
      wb$skipped[idx[1]] <- skipped
      word_bank(wb)
    }
  }
  
  add_score <- function(team, pts = 1L) {
    sc <- scores()
    if (!is.null(sc[[team]])) {
      sc[[team]] <- sc[[team]] + pts
      scores(sc)
    }
  }
  
  advance_team <- function() {
    n   <- length(team_names())
    idx <- current_team_idx()
    current_team_idx(if (idx >= n) 1L else idx + 1L)
  }
  
  current_team <- reactive({
    tn <- team_names()
    if (length(tn) == 0) return("—")
    idx <- current_team_idx()
    tn[min(idx, length(tn))]
  })
  
  # Status pill UI
  output$status_pill_ui <- renderUI({
    s <- app_status()
    switch(s,
           "waiting" = span(class = "badge text-bg-warning",  "⏳ Waiting for words"),
           "ready"   = span(class = "badge text-bg-success",  "✅ Words ready – start game"),
           "playing" = span(class = "badge text-bg-primary",  "🎮 Game in progress")
    )
  })
  
  # Word count summary
  output$word_count_summary <- renderUI({
    wb <- word_bank()
    if (is.null(wb)) return(NULL)
    total  <- nrow(wb)
    used   <- sum(wb$used)
    remain <- total - used
    tagList(
      span(style = "font-size:.85rem;",
           tags$b(remain, style = "color:var(--pict-green);"), " remaining / ",
           tags$b(total,  style = "color:var(--pict-purple);"), " total words"
      )
    )
  })
  
  # Current team badge
  output$current_team_badge <- renderUI({
    if (!game_active()) return(NULL)
    span(
      class = "badge text-bg-primary",
      paste0("🖊 ", current_team(), "'s turn")
    )
  })
  
  # Theme buttons (main game area)
  output$theme_buttons_ui <- renderUI({
    wb <- word_bank()
    
    if (is.null(wb)) return(waiting_ui())
    
    themes_in_bank <- unique(wb$theme)
    
    buttons <- lapply(themes_in_bank, function(thm) {
      rem <- remaining_for_theme(thm)
      disabled <- !game_active() || rem == 0
      
      btn_class <- paste0("pict-theme-btn", if (isTRUE(active_theme() == thm)) " pict-active" else "")
      
      tags$button(
        class    = btn_class,
        id       = paste0("theme_btn_", make.names(thm)),
        disabled = if (disabled) NA else NULL,
        onclick  = if (!disabled) {
          sprintf("Shiny.setInputValue('clicked_theme', '%s', {priority: 'event'})", thm)
        } else NULL,
        span(thm),
        span(
          class = "pict-badge-remaining",
          if (rem == 0) "✅ Done" else paste(rem, "left")
        )
      )
    })
    
    div(
      style = "display:grid; grid-template-columns: repeat(auto-fill, minmax(200px,1fr)); gap:8px;",
      buttons
    )
  })
  
  # SweetAlert turn flow
  observeEvent(input$clicked_theme, {
    req(game_active())
    thm  <- input$clicked_theme
    active_theme(thm)
    
    row  <- pick_random_word(thm)
    if (is.null(row)) {
      showNotification("No words left in this theme!", type = "warning")
      return()
    }
    
    word   <- row$word
    team   <- current_team()
    timer  <- input$timer_length %||% 60L
    skips  <- input$skips_allowed
    skips_remaining(skips)
    
    # Build SweetAlert HTML content
    popup_html <- build_popup_html(word, team, timer, skips)
    
    # Mark word as seen (used) immediately
    mark_word_used(word, thm, skipped = FALSE)
    
    sendSweetAlert(
      session = session,
      title   = NULL,
      html    = TRUE,
      text    = HTML(popup_html),
      type    = NULL,
      btn_labels = NA,    # we use custom buttons inside HTML
      closeOnClickOutside = FALSE,
      showCloseButton     = TRUE,
      width               = "45vw",
      customClass         = list(popup = "swal2-popup")
    )
    
    # Store current word for potential skip
    session$userData$current_word  <- word
    session$userData$current_theme <- thm
  })
  
  # Skip button handler
  observeEvent(input$skip_word, {
    sk <- skips_remaining()
    if (sk <= 0) {
      showNotification("No skips remaining!", type = "warning")
      return()
    }
    
    # Mark current word as skipped (already marked used above)
    mark_word_used(session$userData$current_word,
                   session$userData$current_theme,
                   skipped = TRUE)
    
    skips_remaining(sk - 1)
    
    # Pull next word from same theme
    thm <- session$userData$current_theme
    row <- pick_random_word(thm)
    if (is.null(row)) {
      closeSweetAlert(session)
      showNotification("No more words in this theme!", type = "warning")
      advance_team()
      return()
    }
    
    word  <- row$word
    timer <- input$timer_length
    team  <- current_team()
    mark_word_used(word, thm, skipped = FALSE)
    session$userData$current_word <- word
    
    popup_html <- build_popup_html(word, team, timer, skips_remaining())
    
    sendSweetAlert(
      session = session,
      title   = NULL,
      html    = TRUE,
      text    = HTML(popup_html),
      type    = NULL,
      btn_labels = NA,
      closeOnClickOutside = FALSE,
      showCloseButton     = TRUE,
      width               = "90vw"
    )
  })
  
  # Got it: Yes
  observeEvent(input$got_it_yes, {
    team <- current_team()
    add_score(team, 1L)
    closeSweetAlert(session)
    advance_team()
    showNotification(
      paste0("🎉 Point to ", team, "!"),
      type = "message", duration = 3
    )
  })
  
  # Got it: No
  observeEvent(input$got_it_no, {
    closeSweetAlert(session)
    advance_team()
    showNotification("😓 Better luck next time !", type = "warning", duration = 3)
  })
  
  # Scoreboard
  output$scoreboard_content <- renderUI({
    sc  <- scores()
    tn  <- team_names()
    idx <- current_team_idx()
    
    if (length(sc) == 0 || length(tn) == 0) {
      return(div(
        class = "text-center py-4",
        style = "opacity:.5;",
        tags$span("🏆", style = "font-size:2.5rem;display:block;margin-bottom:8px;"),
        tags$p("Start the game to see scores", class = "text-muted mb-0")
      ))
    }
    
    # Sort by score desc for display (keeping turn marker by current_team_idx)
    rows <- lapply(seq_along(tn), function(i) {
      nm        <- tn[i]
      pts       <- sc[[nm]] %||% 0
      is_active <- (i == idx)
      
      div(
        class = paste0("pict-score-row", if (is_active) " pict-score-row-active" else ""),
        div(
          class = "d-flex align-items-center gap-2",
          if (is_active) tags$span("🖊", class = "small") else tags$span(""),
          span(class = "fw-bold", nm)
        ),
        span(class = "pict-score-pts", pts)
      )
    })
    
    tagList(
      div(class = "mb-3", rows),
      if (game_active()) {
        div(
          class = "text-center mt-2",
          style = "font-size:.8rem; opacity:.55;",
          paste0("Current turn: ", tn[min(idx, length(tn))])
        )
      }
    )
  })
  
  # Main panel switcher
  output$main_panel <- renderUI({
    dashboard_ui()
  })
  
}