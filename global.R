# =============================================================================
# global.R  –  Pictionary AI Generator
# =============================================================================

library(shiny)
library(bslib)
library(shinyWidgets)
library(httr2)
library(jsonlite)
library(dplyr)
library(glue)
library(purrr)
library(stringr)

# -----------------------------------------------------------------------------
# App-wide theme  (pre-existing — do not modify)
# -----------------------------------------------------------------------------
app_theme <- function() {
  bs_theme(
    preset     = "lux",
    base_font  = font_google("Overpass"),
    font_scale = 1
  )
}

# -----------------------------------------------------------------------------
# HTML head extras  (pre-existing — do not modify)
# -----------------------------------------------------------------------------
app_html_head <- function() {
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/pictionaiRy.css"),
    tags$link(rel = "icon", type = "image/x-icon", href = "images/favicon/favicon.ico")
  )
}

# -----------------------------------------------------------------------------
# App header  (pre-existing — do not modify)
# -----------------------------------------------------------------------------
app_header <- function() {
  div(
    class = "app-header d-flex align-items-center gap-4",
    tags$img(
      src    = "images/Movember_Iconic_Mo_Black.png",
      height = "50px",
      alt    = "Logo Left"
    ),
    tags$img(
      src    = "images/pictionaiRy_logo.png",
      height = "140px",
      alt    = "Logo Right"
    )
  )
}

# -----------------------------------------------------------------------------
# Claude API helper
# -----------------------------------------------------------------------------
CLAUDE_MODEL   <- "claude-sonnet-4-6"
CLAUDE_MAX_TOK <- 2000

call_claude <- function(prompt_text, api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  message("DEBUG prompt: [", prompt_text, "] length:", length(prompt_text))  # ADD THIS
  
  if (is.null(api_key) || nchar(api_key) == 0) stop("ANTHROPIC_API_KEY not set.")
  
  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = CLAUDE_MODEL,
      max_tokens = CLAUDE_MAX_TOK,
      messages   = list(list(role = "user", content = prompt_text))
    )) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  
  parsed <- resp_body_json(resp)
  if (!is.null(parsed$error)) stop(parsed$error$message)
  parsed$content[[1]]$text
}

# -----------------------------------------------------------------------------
# Build the Claude prompt from setup options
# -----------------------------------------------------------------------------
build_claude_prompt <- function(themes, n_per_category, difficulty) {
  themes_str    <- paste(themes, collapse = ", ")
  difficulty_lc <- tolower(difficulty)
  
  glue(
    "You are helping run a Pictionary game. Generate a structured list of words/concepts \\
for players to draw.",
    "\n\n",
    "SETTINGS:\n",
    "- Themes: {themes_str}\n",
    "- Number of words per theme: {n_per_category}\n",
    "- Difficulty: {difficulty_lc}\n\n",
    "INSTRUCTIONS:\n",
    "Return ONLY a CSV block with exactly two columns: `theme` and `word`.\n",
    "Do not include any explanation, markdown fences, or extra text — just the raw CSV.\n",
    "The first row must be the header: theme,word\n",
    "Each subsequent row: one theme name (matching exactly one of the themes listed above), \\
a comma, then one word or short phrase (2-4 words max) appropriate for Pictionary at \\
{difficulty_lc} difficulty.\n",
    "Generate exactly {n_per_category} words for EACH theme listed.\n",
    "Example row: Animals,Elephant\n",
    "Only output the CSV. Nothing else."
  )
}

# -----------------------------------------------------------------------------
# Parse Claude CSV response → data.frame
# -----------------------------------------------------------------------------
parse_claude_csv <- function(raw_text) {
  # Strip any accidental markdown fences
  clean <- raw_text |>
    str_remove_all("```[a-z]*\n?") |>
    str_trim()
  
  df <- tryCatch(
    read.csv(text = clean, stringsAsFactors = FALSE, strip.white = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(df) || !all(c("theme", "word") %in% names(df))) {
    stop("Could not parse Claude's response as a valid CSV with columns 'theme' and 'word'.")
  }
  
  df |>
    mutate(
      theme  = str_trim(theme),
      word   = str_trim(word),
      used   = FALSE,
      skipped = FALSE
    )
}

# -----------------------------------------------------------------------------
# Difficulty choices
# -----------------------------------------------------------------------------
DIFFICULTY_CHOICES <- c("Easy", "Medium", "Hard", "Expert")

# Default themes (user can customise)
DEFAULT_THEMES <- c("Famous Moustaches", "Things That Are Hairy", "Things That Look Like Moustaches", "They Call It What?! (Aussie Slang)")

# -----------------------------------------------------------------------------
# Null-coalescing helper
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------------------------------------------------------
# Build SweetAlert popup HTML (word reveal + timer + controls)
# Defined here (global.R) so it is available before server.R is sourced,
# and so that server.R contains ONLY the bare server function expression.
# -----------------------------------------------------------------------------
# =============================================================================
# UI MODULE FUNCTIONS
# Defined here (global.R) so they are available to BOTH ui.R and server.R.
# server.R cannot see functions defined in ui.R; global.R is the shared scope.
# =============================================================================

# -----------------------------------------------------------------------------
# Waiting / pre-generation placeholder
# -----------------------------------------------------------------------------
waiting_ui <- function() {
  div(
    class = "text-center py-5 text-muted",
    tags$span("🎨", style = "font-size:3.5rem; display:block; margin-bottom:12px;"),
    tags$h5("No words generated yet"),
    tags$p(
      class = "small",
      "Use the sidebar to choose themes, configure options, and click Generate Words."
    )
  )
}

# -----------------------------------------------------------------------------
# Status bar
# -----------------------------------------------------------------------------
status_bar_ui <- function() {
  card(
    class = "mb-3",
    card_body(
      class = "py-2 px-3",
      div(
        class = "d-flex align-items-center justify-content-between flex-wrap gap-2",
        div(
          class = "d-flex align-items-center gap-2",
          tags$span("Status:", class = "text-muted small"),
          uiOutput("status_pill_ui")
        ),
        div(
          class = "d-flex align-items-center gap-3",
          uiOutput("word_count_summary")
        )
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Scoreboard card
# -----------------------------------------------------------------------------
scoreboard_ui <- function() {
  card(
    full_screen = FALSE,
    card_header(
      class = "text-center fw-semibold",
      "🏆 Scoreboard"
    ),
    card_body(
      uiOutput("scoreboard_content")
    )
  )
}

# -----------------------------------------------------------------------------
# Theme selection card (main game area)
# -----------------------------------------------------------------------------
theme_selection_ui <- function() {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("🎨 Select a Theme & Draw!"),
        uiOutput("current_team_badge")
      )
    ),
    card_body(
      uiOutput("theme_buttons_ui"),
      div(
        class = "mt-3 text-center text-muted small",
        "Select a theme, then click the button to reveal your word!"
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Main dashboard layout
# -----------------------------------------------------------------------------
dashboard_ui <- function() {
  tagList(
    status_bar_ui(),
    layout_columns(
      col_widths = c(8, 4),
      theme_selection_ui(),
      scoreboard_ui()
    )
  )
}

# -----------------------------------------------------------------------------
# Helper: stepper input  (- | value | +)
# id        : the numericInput inputId that holds the value
# label     : visible label text
# value     : starting value
# min / max : bounds enforced in JS
# step      : increment per button press
# -----------------------------------------------------------------------------
stepper_input <- function(id, label, value, min, max, step = 1) {
  tagList(
    tags$label(`for` = id, class = "form-label mb-1 small fw-semibold", label),
    div(
      class = "input-group input-group-sm mb-3",
      tags$button(
        class            = "btn btn-outline-secondary pict-stepper-btn",
        type             = "button",
        `data-target`    = id,
        `data-direction` = "down",
        `data-step`      = step,
        `data-min`       = min,
        `data-max`       = max,
        "−"
      ),
      div(
        style = "flex:1;",
        tags$input(
          id    = id,
          class = "form-control text-center shiny-bound-input",
          type  = "number",
          value = value,
          min   = min,
          max   = max,
          step  = step
        )
      ),
      tags$button(
        class            = "btn btn-outline-secondary pict-stepper-btn",
        type             = "button",
        `data-target`    = id,
        `data-direction` = "up",
        `data-step`      = step,
        `data-min`       = min,
        `data-max`       = max,
        "＋"
      )
    ),
    tags$script(HTML(sprintf("
  (function() {
    document.querySelectorAll('.pict-stepper-btn[data-target=\"%s\"]').forEach(function(btn) {
      btn.addEventListener('click', function() {
        var inp  = document.getElementById('%s');
        var dir  = btn.getAttribute('data-direction');
        var step = parseFloat(btn.getAttribute('data-step'));
        var mn   = parseFloat(btn.getAttribute('data-min'));
        var mx   = parseFloat(btn.getAttribute('data-max'));
        var cur  = parseFloat(inp.value) || 0;
        var nxt  = dir === 'up' ? Math.min(cur + step, mx) : Math.max(cur - step, mn);
        inp.value = nxt;
        inp.dispatchEvent(new Event('change'));
        Shiny.setInputValue('%s', nxt, {priority: 'event'});
      });
    });

    // Push initial value only after Shiny is fully connected
    $(document).one('shiny:connected', function() {
      Shiny.setInputValue('%s', %s);
    });
  })();
", id, id, id, id, value)))
  )
}

# -----------------------------------------------------------------------------
# Sidebar – Word-generation options
# -----------------------------------------------------------------------------
sidebar_word_gen_ui <- function() {
  tagList(
    # Collapsible section header — bold, no emoji
    tags$div(
      class = "pict-section-label",
      tags$a(
        href            = "#collapse-word-gen",
        `data-bs-toggle` = "collapse",
        `aria-expanded` = "true",
        `aria-controls` = "collapse-word-gen",
        class           = "pict-collapse-toggle",
        tags$strong("Word Generation"),
        tags$span(class = "pict-collapse-caret", "▾")
      )
    ),
    div(
      id    = "collapse-word-gen",
      class = "collapse show",
      pickerInput(
        inputId  = "themes",
        label    = "Themes",
        choices  = DEFAULT_THEMES,
        selected = DEFAULT_THEMES[1:3],
        multiple = TRUE,
        options  = pickerOptions(
          actionsBox         = TRUE,
          liveSearch         = TRUE,
          selectedTextFormat = "count > 2",
          countSelectedText  = "{0} themes selected",
          style              = "btn-outline-secondary btn-sm"
        )
      ),
      tags$label("Add custom theme", class = "form-label mb-1 small fw-semibold"),
      tags$style(HTML("
  .custom-theme-group .form-group { margin-bottom: 0; }
  .custom-theme-group input { border-radius: 4px 0 0 4px !important; }
")),
      div(
        class = "input-group input-group-sm mb-2 custom-theme-group",
        div(
          style = "flex: 1;",
          textInput(
            inputId     = "custom_theme",
            label       = NULL,
            value       = "",
            placeholder = "e.g. 80s Music…"
          )
        ),
        tags$button(
          id    = "add_theme",
          class = "btn btn-outline-secondary action-button",
          type  = "button",
          "＋"
        )
      ),
      stepper_input("n_per_category", "Words per theme", value = 10, min = 1, max = 30, step = 1),
      radioGroupButtons(
        inputId   = "difficulty",
        label     = "Difficulty",
        choices   = DIFFICULTY_CHOICES,
        selected  = "Medium",
        status    = "primary",
        size      = "sm",
        justified = TRUE
      ),
      br(),
      actionButton("generate_words", "🎲 Generate Words", class = "btn btn-primary w-100")
    )
  )
}

# -----------------------------------------------------------------------------
# Sidebar – Game options
# -----------------------------------------------------------------------------
sidebar_game_options_ui <- function() {
  tagList(
    # Collapsible section header — bold, no emoji
    tags$div(
      class = "pict-section-label mt-3",
      tags$a(
        href             = "#collapse-game-setup",
        `data-bs-toggle` = "collapse",
        `aria-expanded`  = "true",
        `aria-controls`  = "collapse-game-setup",
        class            = "pict-collapse-toggle",
        tags$strong("Game Setup"),
        tags$span(class = "pict-collapse-caret", "▾")
      )
    ),
    div(
      id    = "collapse-game-setup",
      class = "collapse show",
      stepper_input("timer_length", "Timer (seconds)", value = 60, min = 15, max = 360, step = 15),
      numericInput(
        inputId = "skips_allowed",
        label   = "Skips allowed per turn",
        value   = 2, min = 0, max = 10, step = 1
      ),
      numericInput(
        inputId = "n_teams",
        label   = "Number of teams",
        value   = 2, min = 2, max = 6, step = 1
      ),
      uiOutput("team_name_inputs"),
      br(),
      actionButton("start_game", "🚀 Start Game", class = "btn btn-success w-100")
    )
  )
}

# -----------------------------------------------------------------------------
# Full sidebar (both sections combined)
# -----------------------------------------------------------------------------
sidebar_panel_ui <- function() {
  sidebar(
    width = 320,
    sidebar_word_gen_ui(),
    sidebar_game_options_ui()
  )
}

# -----------------------------------------------------------------------------
# Root UI output (main panel placeholder)
# -----------------------------------------------------------------------------
root_ui <- function() {
  uiOutput("main_panel")
}

# =============================================================================
# SweetAlert popup builder
# =============================================================================

build_popup_html <- function(word, team, timer_secs, skips_left) {
  timer_id      <- "pict-timer"
  circumference <- round(2 * pi * 46, 1)
  skip_disabled <- if (skips_left <= 0) "disabled" else ""
  
  glue('
<div style="text-align:center; padding: 4px 0 16px;">

  <div class="text-muted small mb-2">
    Team drawing: <strong class="text-primary">{team}</strong>
    &nbsp;|&nbsp;
    Skips left: <strong class="text-warning" id="skips-display">{skips_left}</strong>
  </div>

  <div class="pict-timer-ring" id="{timer_id}-wrap" style="margin:14px auto;">
    <svg width="120" height="120" viewBox="0 0 100 100">
      <circle cx="50" cy="50" r="46"
        fill="none" stroke="#dee2e6" stroke-width="8"/>
      <circle id="{timer_id}-fg" cx="50" cy="50" r="46"
        fill="none" stroke="#0d6efd" stroke-width="8" stroke-linecap="round"
        stroke-dasharray="{circumference}" stroke-dashoffset="0"
        style="transition: stroke-dashoffset 1s linear; transform: rotate(-90deg); transform-origin: 50px 50px;"/>
    </svg>
    <div class="pict-timer-num" id="{timer_id}-num">{timer_secs}</div>
  </div>

  <div class="text-muted small mb-1 mt-2" id="pict-controls-label">Controls</div>

  <div id="pict-verdict" style="display:none; margin-top:4px; margin-bottom:4px;">
    <div class="text-muted small mb-2">Did they get it?</div>
    <div style="display:flex; gap:10px; justify-content:center;">
      <button onclick="Shiny.setInputValue(\'got_it_yes\', Date.now(), {{priority:\'event\'}})"
        class="btn btn-success px-4">Yes!</button>
      <button onclick="Shiny.setInputValue(\'got_it_no\', Date.now(), {{priority:\'event\'}})"
        class="btn btn-danger px-4">No</button>
    </div>
  </div>

  <div style="display:flex; gap:8px; justify-content:center; flex-wrap:wrap; margin-top:8px;" id="pict-actions">
    <button id="btn-start-timer"
      onclick="startPictTimer({timer_secs}, \'{timer_id}\')"
      class="btn btn-primary">Start Timer</button>
    <button id="btn-pause-timer"
      onclick="pauseResumePictTimer(\'{timer_id}\')"
      class="btn btn-outline-secondary"
      style="display:none;">Pause</button>
    <button id="btn-end-early"
      onclick="endPictTimerEarly(\'{timer_id}\')"
      class="btn btn-primary"
      style="display:none;">End Timer Early</button>
    <button id="btn-skip"
      onclick="Shiny.setInputValue(\'skip_word\', Date.now(), {{priority:\'event\'}})"
      {skip_disabled}
      class="btn btn-outline-warning">Skip ({skips_left})</button>
  </div>

  <hr style="margin: 16px 0 12px;"/>
  <div class="pict-word-display">{word}</div>

</div>

<script>
(function() {{
  // Guard: only define functions once per page lifetime
  if (window._pictTimerDefined) return;
  window._pictTimerDefined = true;

  // Internal timer state (one active timer at a time)
  window._pictTimer = {{
    interval:  null,
    paused:    false,
    remaining: 0,
    totalSecs: 0,
    wrapperId: null
  }};

  // ── Start ────────────────────────────────────────────────────────────────
  window.startPictTimer = function(totalSecs, wrapperId) {{
    var wrap      = document.getElementById(wrapperId + \'-wrap\');
    var fg        = document.getElementById(wrapperId + \'-fg\');
    var num       = document.getElementById(wrapperId + \'-num\');
    var btnStart  = document.getElementById(\'btn-start-timer\');
    var btnPause  = document.getElementById(\'btn-pause-timer\');
    var btnEnd    = document.getElementById(\'btn-end-early\');
    var acts      = document.getElementById(\'pict-actions\');
    var verdict   = document.getElementById(\'pict-verdict\');
    if (!wrap || !fg) return;

    // Show ring, swap buttons
    wrap.style.display = \'block\';
    if (btnStart) btnStart.style.display = \'none\';
    if (btnPause) btnPause.style.display = \'inline-block\';
    if (btnEnd)   btnEnd.style.display   = \'inline-block\';

    var circ = {circumference};
    fg.setAttribute(\'stroke-dasharray\', circ);
    fg.setAttribute(\'stroke-dashoffset\', \'0\');

    // Save state
    window._pictTimer.totalSecs = totalSecs;
    window._pictTimer.remaining = totalSecs;
    window._pictTimer.wrapperId = wrapperId;
    window._pictTimer.paused    = false;

    window._pictTimer.interval = setInterval(function() {{
      if (window._pictTimer.paused) return;   // skip ticks while paused

      window._pictTimer.remaining -= 1;
      var rem = window._pictTimer.remaining;

      if (num) num.textContent = rem;
      fg.setAttribute(\'stroke-dashoffset\', circ * (1 - rem / totalSecs));
      if (rem <= 5)  fg.setAttribute(\'stroke\', \'#dc3545\');
      if (rem <= 0) {{
        clearInterval(window._pictTimer.interval);
        window._pictTimer.interval = null;
        if (num)     num.textContent          = \'Done!\';
        if (acts)    acts.style.display       = \'none\';
        var lbl = document.getElementById(\'pict-controls-label\');
        if (lbl)     lbl.style.display        = \'none\';
        if (verdict) verdict.style.display    = \'block\';
      }}
    }}, 1000);
  }};

  // ── Pause / Resume ───────────────────────────────────────────────────────
  window.pauseResumePictTimer = function(wrapperId) {{
    var t       = window._pictTimer;
    var btnPause = document.getElementById(\'btn-pause-timer\');
    var fg       = document.getElementById(wrapperId + \'-fg\');

    if (!t.interval) return;   // timer not running

    t.paused = !t.paused;

    if (t.paused) {{
      // Paused state: freeze ring animation, update button label
      if (fg) fg.style.transition = \'none\';
      if (btnPause) btnPause.textContent = \'▶ Resume\';
      if (btnPause) btnPause.classList.replace(\'btn-outline-secondary\', \'btn-outline-success\');
    }} else {{
      // Resumed: re-enable animation, restore button
      if (fg) fg.style.transition = \'stroke-dashoffset 1s linear\';
      if (btnPause) btnPause.textContent = \'⏸ Pause\';
      if (btnPause) btnPause.classList.replace(\'btn-outline-success\', \'btn-outline-secondary\');
    }}
  }};

  // ── End Early ────────────────────────────────────────────────────────────
  window.endPictTimerEarly = function(wrapperId) {{
    var t = window._pictTimer;
    if (t.interval) {{
      clearInterval(t.interval);
      t.interval = null;
    }}
    var num     = document.getElementById(wrapperId + \'-num\');
    var fg      = document.getElementById(wrapperId + \'-fg\');
    var acts    = document.getElementById(\'pict-actions\');
    var verdict = document.getElementById(\'pict-verdict\');
    var lbl     = document.getElementById(\'pict-controls-label\');
    if (num) num.textContent = \'✓\';
    if (fg)  {{ fg.style.transition = \'none\'; fg.setAttribute(\'stroke\', \'#198754\'); }}
    if (acts)    acts.style.display    = \'none\';
    if (lbl)     lbl.style.display     = \'none\';
    if (verdict) verdict.style.display = \'block\';
  }};

}})();
</script>
')
}