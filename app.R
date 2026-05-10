library(shiny)
library(tidyverse)
library(data.table)
library(DT)

# -----------------------------
# Data loading and cleaning
# -----------------------------
load_brfss <- function() {
  files <- c(
    "NYSDOH_BRFSS_SurveyData_2023.csv",
    "data/NYSDOH_BRFSS_SurveyData_2023.csv"
  )
  file_path <- files[file.exists(files)][1]
  if (is.na(file_path)) {
    stop("Put NYSDOH_BRFSS_SurveyData_2023.csv in the same folder as app.R")
  }

  needed_cols <- c("_PAINDX3", "_RFHYPE6", "_AGEG5YR", "_SEX", "_INCOMG1", "_LLCPWT", "GENHLTH")
  dat <- fread(file_path, select = needed_cols, showProgress = FALSE, encoding = "Latin-1")

  dat %>%
    mutate(across(everything(), as.character)) %>%
    transmute(
      physical_activity = case_when(
        str_detect(`_PAINDX3`, regex("Did Not Meet Aerobic Recommendations", ignore_case = TRUE)) ~ "Does not meet recommendation",
        str_detect(`_PAINDX3`, regex("Meet Aerobic Recommendations", ignore_case = TRUE)) ~ "Meets recommendation",
        TRUE ~ NA_character_
      ),
      hypertension = case_when(
        `_RFHYPE6` == "Yes" ~ "Self-reported hypertension",
        `_RFHYPE6` == "No" ~ "No self-reported hypertension",
        TRUE ~ NA_character_
      ),
      age_group = case_when(
        str_detect(`_AGEG5YR`, "Age 18 to 24|Age 25 to 29") ~ "18-29",
        str_detect(`_AGEG5YR`, "Age 30 to 34|Age 35 to 39|Age 40 to 44") ~ "30-44",
        str_detect(`_AGEG5YR`, "Age 45 to 49|Age 50 to 54|Age 55 to 59") ~ "45-59",
        str_detect(`_AGEG5YR`, "Age 60 to 64|Age 65 to 69|Age 70 to 74|Age 75 to 79|Age 80 or older") ~ "60+",
        TRUE ~ NA_character_
      ),
      sex = case_when(
        `_SEX` == "Male" ~ "Male",
        `_SEX` == "Female" ~ "Female",
        TRUE ~ NA_character_
      ),
      income = `_INCOMG1`,
      general_health = if_else(GENHLTH %in% c("Excellent", "Very good", "Good", "Fair", "Poor"), GENHLTH, NA_character_),
      weight = suppressWarnings(as.numeric(`_LLCPWT`))
    ) %>%
    mutate(weight = if_else(is.na(weight) | weight <= 0, 1, weight)) %>%
    filter(!is.na(physical_activity), !is.na(hypertension), !is.na(age_group), !is.na(sex)) %>%
    mutate(
      physical_activity = factor(physical_activity, levels = c("Meets recommendation", "Does not meet recommendation")),
      hypertension = factor(hypertension, levels = c("No self-reported hypertension", "Self-reported hypertension")),
      age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60+")),
      sex = factor(sex, levels = c("Female", "Male")),
      general_health = factor(general_health, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
    )
}

brfss <- load_brfss()

weighted_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- as.numeric(x[ok])
  w <- as.numeric(w[ok])
  n <- length(x)
  if (n == 0 || sum(w) <= 0) {
    return(tibble(n = 0, weighted_n = 0, estimate = NA_real_, lower = NA_real_, upper = NA_real_, se = NA_real_, neff = NA_real_))
  }
  p <- sum(w * x) / sum(w)
  neff <- (sum(w)^2) / sum(w^2)
  se <- sqrt(pmax(p * (1 - p) / neff, 0))
  lower <- pmax(0, p - 1.96 * se)
  upper <- pmin(1, p + 1.96 * se)
  tibble(n = n, weighted_n = sum(w), estimate = p, lower = lower, upper = upper, se = se, neff = neff)
}

prevalence_by_pa <- function(dat) {
  dat %>%
    group_by(physical_activity) %>%
    summarise(
      weighted_ci(hypertension == "Self-reported hypertension", weight),
      .groups = "drop"
    ) %>%
    mutate(
      estimate_pct = estimate * 100,
      lower_pct = lower * 100,
      upper_pct = upper * 100,
      ci_label = paste0(round(estimate_pct, 1), "% (95% CI: ", round(lower_pct, 1), "–", round(upper_pct, 1), "%)")
    )
}

overall_prevalence <- function(dat) {
  weighted_ci(dat$hypertension == "Self-reported hypertension", dat$weight) %>%
    mutate(
      estimate_pct = estimate * 100,
      lower_pct = lower * 100,
      upper_pct = upper * 100,
      ci_label = paste0(round(estimate_pct, 1), "% (95% CI: ", round(lower_pct, 1), "–", round(upper_pct, 1), "%)")
    )
}

health_distribution_ci <- function(dat) {
  dat %>%
    filter(!is.na(general_health)) %>%
    group_by(physical_activity) %>%
    group_modify(~{
      total_w <- sum(.x$weight, na.rm = TRUE)
      total_n_eff <- (sum(.x$weight, na.rm = TRUE)^2) / sum(.x$weight^2, na.rm = TRUE)
      .x %>%
        group_by(general_health) %>%
        summarise(n = n(), weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
        complete(general_health = levels(brfss$general_health), fill = list(n = 0, weighted_n = 0)) %>%
        mutate(
          estimate = weighted_n / total_w,
          se = sqrt(pmax(estimate * (1 - estimate) / total_n_eff, 0)),
          lower = pmax(0, estimate - 1.96 * se),
          upper = pmin(1, estimate + 1.96 * se),
          estimate_pct = estimate * 100,
          lower_pct = lower * 100,
          upper_pct = upper * 100
        )
    }) %>%
    ungroup()
}


premium_plot_theme <- function() {
  theme_minimal(base_size = 14, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 19, color = "#10223e", margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#536174", size = 12.5, margin = margin(b = 16)),
      axis.title = element_text(color = "#34445a", face = "bold"),
      axis.text = element_text(color = "#536174"),
      axis.text.x = element_text(face = "bold", color = "#1c2b42"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#e9eff7", linewidth = .45),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", color = "#34445a"),
      legend.text = element_text(color = "#536174"),
      plot.caption = element_text(color = "#78869a", size = 10, hjust = 0),
      plot.margin = margin(16, 18, 14, 16)
    )
}

# -----------------------------
# User interface
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&display=swap", rel = "stylesheet"),
    tags$style(HTML("\n      :root{\n        --ink:#0b1220;--navy:#10223e;--slate:#536174;--muted:#78869a;\n        --bg1:#eef4ff;--bg2:#f8fbff;--card:#ffffff;--line:#e4ebf5;\n        --blue:#2f6df6;--cyan:#1cc7d0;--green:#21a67a;--amber:#f0b429;\n        --shadow:0 24px 70px rgba(16,34,62,.14);--shadow2:0 14px 36px rgba(16,34,62,.10);\n      }\n      html,body{min-height:100%;}\n      body{\n        color:var(--ink);font-family:'Inter',-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;\n        background:\n          radial-gradient(circle at 8% 4%, rgba(47,109,246,.18), transparent 28%),\n          radial-gradient(circle at 88% 6%, rgba(28,199,208,.16), transparent 26%),\n          linear-gradient(180deg,#f4f8ff 0%,#fbfdff 48%,#ffffff 100%);\n      }\n      .container-fluid{max-width:1440px;padding-left:30px;padding-right:30px;}\n      .app-shell{padding:22px 0 38px;}\n      .hero-premium{\n        position:relative;overflow:hidden;color:white;border-radius:30px;padding:34px 36px;margin:6px 0 24px;\n        background:linear-gradient(135deg,#0c1d35 0%,#173b72 47%,#2f6df6 100%);\n        box-shadow:var(--shadow);\n      }\n      .hero-premium:before{content:'';position:absolute;right:-90px;top:-100px;width:360px;height:360px;background:rgba(255,255,255,.14);border-radius:50%;filter:blur(1px);}\n      .hero-premium:after{content:'';position:absolute;left:42%;bottom:-120px;width:420px;height:220px;background:rgba(28,199,208,.18);border-radius:50%;filter:blur(12px);}\n      .hero-content{position:relative;z-index:2;}\n      .eyebrow{display:inline-flex;align-items:center;gap:8px;border:1px solid rgba(255,255,255,.24);background:rgba(255,255,255,.12);padding:7px 12px;border-radius:999px;font-size:12px;font-weight:800;letter-spacing:.10em;text-transform:uppercase;margin-bottom:12px;}\n      .hero-premium h1{font-size:38px;line-height:1.06;font-weight:900;letter-spacing:-.045em;margin:0 0 12px;}\n      .hero-premium p{font-size:16px;line-height:1.65;opacity:.92;max-width:930px;margin-bottom:20px;}\n      .hero-strip{display:flex;flex-wrap:wrap;gap:10px;}\n      .pill{background:rgba(255,255,255,.13);border:1px solid rgba(255,255,255,.20);border-radius:999px;padding:8px 12px;font-size:12px;font-weight:700;}\n      .layout-grid{display:grid;grid-template-columns:330px 1fr;gap:22px;align-items:start;}\n      .side-card,.glass-card,.metric,.interpretation-panel{background:rgba(255,255,255,.84);border:1px solid rgba(226,235,245,.94);box-shadow:var(--shadow2);backdrop-filter:blur(16px);}\n      .side-card{border-radius:26px;padding:22px;position:sticky;top:18px;}\n      .side-title{font-size:16px;font-weight:900;color:var(--navy);margin:0 0 16px;display:flex;align-items:center;gap:8px;}\n      .side-title:before{content:'';width:10px;height:10px;border-radius:50%;background:linear-gradient(135deg,var(--blue),var(--cyan));box-shadow:0 0 0 6px rgba(47,109,246,.12);}\n      .control-label{font-weight:800;color:#1c2b42;font-size:13px;margin-bottom:7px;}\n      .form-control,.selectize-input{border-radius:14px!important;border:1px solid #dbe5f2!important;box-shadow:none!important;min-height:42px;}\n      .checkbox label{font-weight:600;color:#34445a;}\n      .btn{border-radius:14px!important;font-weight:800;padding:10px 14px;border:0!important;}\n      .btn-primary{background:linear-gradient(135deg,#2f6df6,#1cc7d0)!important;box-shadow:0 12px 24px rgba(47,109,246,.22);} \n      .btn-default{background:#eef4ff!important;color:#17324d!important;}\n      .note{color:var(--slate);font-size:12.5px;line-height:1.62;background:#f7faff;border:1px solid #e5edf8;border-radius:18px;padding:14px;}\n      .main-card{border-radius:28px;padding:0;overflow:hidden;}\n      .nav-tabs{border:0;background:rgba(255,255,255,.72);padding:12px 14px 0;border-radius:26px 26px 0 0;}\n      .nav-tabs>li>a{border:0!important;border-radius:16px 16px 0 0!important;font-weight:850;color:#526176;padding:12px 16px;}\n      .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{background:#ffffff!important;color:#17324d!important;box-shadow:0 -4px 18px rgba(16,34,62,.07);}\n      .tab-content{background:rgba(255,255,255,.92);border-radius:0 0 28px 28px;padding:22px;}\n      .section-title{font-size:18px;font-weight:900;color:var(--navy);letter-spacing:-.02em;margin:10px 0 14px;}\n      .question-card{border-radius:22px;padding:20px 22px;background:linear-gradient(180deg,#ffffff,#f8fbff);border:1px solid var(--line);box-shadow:0 10px 26px rgba(16,34,62,.07);}\n      .question-card p{font-size:15px;line-height:1.65;margin:0 0 8px;color:#33445b;}\n      .metrics-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:16px;margin:18px 0 20px;}\n      .metric{position:relative;border-radius:24px;padding:20px;min-height:155px;overflow:hidden;}\n      .metric:after{content:'';position:absolute;right:-34px;top:-40px;width:110px;height:110px;border-radius:50%;background:linear-gradient(135deg,rgba(47,109,246,.13),rgba(28,199,208,.12));}\n      .metric-label{text-transform:uppercase;font-size:11px;letter-spacing:.11em;color:var(--muted);font-weight:900;margin-bottom:10px;position:relative;z-index:2;}\n      .metric-value{font-size:30px;font-weight:900;color:var(--navy);line-height:1.05;letter-spacing:-.04em;position:relative;z-index:2;}\n      .metric-sub{font-size:12.5px;color:var(--slate);margin-top:10px;line-height:1.45;position:relative;z-index:2;}\n      .viz-card{border-radius:24px;padding:20px;background:white;border:1px solid var(--line);box-shadow:0 14px 34px rgba(16,34,62,.08);}\n      .interpretation-panel{border-radius:24px;padding:22px 24px;line-height:1.78;font-size:15.5px;border-left:7px solid var(--blue);}\n      .interpretation-panel p{margin-bottom:12px;}\n      .about-copy{font-size:15px;line-height:1.7;color:#33445b;}\n      .about-copy li{margin-bottom:9px;}\n      table.dataTable{border-collapse:separate!important;border-spacing:0 6px!important;}\n      table.dataTable thead th{background:#f2f6fc!important;color:#1e314e!important;border-bottom:0!important;font-weight:900;}\n      table.dataTable tbody tr{background:#ffffff!important;box-shadow:0 4px 14px rgba(16,34,62,.05);}\n      table.dataTable tbody td{border-top:1px solid #eef2f7;border-bottom:1px solid #eef2f7;}\n      @media(max-width:1000px){.layout-grid{grid-template-columns:1fr}.side-card{position:relative;top:0}.metrics-grid{grid-template-columns:repeat(2,1fr)}.hero-premium h1{font-size:30px}}\n      @media(max-width:640px){.container-fluid{padding-left:16px;padding-right:16px}.metrics-grid{grid-template-columns:1fr}.hero-premium{padding:26px 22px;border-radius:24px}}\n    "))
  ),
  div(class = "app-shell",
    div(class = "hero-premium",
      div(class = "hero-content",
        div(class = "eyebrow", "Interactive public health analytics"),
        h1("NYS BRFSS 2023: Physical Activity and Hypertension"),
        p("A polished, decision-ready Shiny dashboard for exploring weighted hypertension prevalence, subgroup filters, uncertainty, and confidence intervals in the 2023 New York State BRFSS."),
        div(class = "hero-strip",
          span(class = "pill", "Weighted estimates"),
          span(class = "pill", "95% confidence intervals"),
          span(class = "pill", "Subgroup filters"),
          span(class = "pill", "BRFSS 2023")
        )
      )
    ),
    div(class = "layout-grid",
      div(class = "side-card",
        div(class = "side-title", "Dashboard controls"),
        selectInput("age", "Age group", choices = c("All", levels(brfss$age_group)), selected = "All"),
        checkboxGroupInput("sex", "Sex", choices = levels(brfss$sex), selected = levels(brfss$sex)),
        selectInput("plot_type", "Visualization", choices = c(
          "Hypertension prevalence with 95% CI",
          "General health distribution with 95% CI",
          "Sample size table"
        )),
        fluidRow(
          column(7, actionButton("update", "Update", class = "btn-primary", width = "100%")),
          column(5, actionButton("reset", "Reset", width = "100%"))
        ),
        br(),
        div(class = "note",
          strong("Uncertainty method: "),
          "95% confidence intervals use a weighted proportion with an effective sample size approximation. A formal publication should use the full BRFSS complex survey design."
        )
      ),
      div(class = "glass-card main-card",
        tabsetPanel(
          tabPanel("Dashboard",
            div(class = "section-title", "Research question"),
            div(class = "question-card",
              p(strong("Is meeting the aerobic physical activity recommendation associated with lower prevalence of self-reported hypertension among adults in the 2023 New York State BRFSS?")),
              p("Use the filters to compare weighted prevalence estimates, 95% confidence intervals, and descriptive subgroup patterns.")
            ),
            div(class = "metrics-grid",
              div(class = "metric", div(class = "metric-label", "Respondents"), div(class = "metric-value", textOutput("n_text")), div(class = "metric-sub", "Unweighted sample size after filters")),
              div(class = "metric", div(class = "metric-label", "Weighted prevalence"), div(class = "metric-value", textOutput("prev_text")), div(class = "metric-sub", textOutput("prev_ci_text"))),
              div(class = "metric", div(class = "metric-label", "Difference"), div(class = "metric-value", textOutput("gap_text")), div(class = "metric-sub", "Does not meet minus meets recommendation")),
              div(class = "metric", div(class = "metric-label", "Uncertainty"), div(class = "metric-value", textOutput("uncertainty_text")), div(class = "metric-sub", "Average 95% CI width across activity groups"))
            ),
            div(class = "section-title", "Main visual"),
            div(class = "viz-card", plotOutput("main_plot", height = "535px")),
            div(class = "section-title", "Interpretation"),
            div(class = "interpretation-panel", uiOutput("interpretation"))
          ),
          tabPanel("Summary statistics", br(), DTOutput("summary_table")),
          tabPanel("Data table", br(), DTOutput("data_table")),
          tabPanel("About variables",
            div(class = "about-copy",
              h3("Variables used"),
              p("The variable names in this tab match the names used in the data table."),
              tags$ul(
                tags$li(strong("_PAINDX3 — Aerobic physical activity status:"), " whether the respondent meets aerobic physical activity recommendations."),
                tags$li(strong("_RFHYPE6 — Hypertension status:"), " self-reported high blood pressure / hypertension status."),
                tags$li(strong("_AGEG5YR — Age group:"), " BRFSS age category recoded into broader age groups for app filters."),
                tags$li(strong("_SEX — Sex:"), " respondent sex."),
                tags$li(strong("GENHLTH — General health:"), " self-rated general health."),
                tags$li(strong("_INCOMG1 — Income group:"), " household income category."),
                tags$li(strong("_LLCPWT — Survey weight:"), " sampling weight. Unit: weighted persons represented by each respondent. It is not body weight in pounds or kilograms.")
              ),
              h3("How to read the confidence intervals"),
              p("A 95% confidence interval gives a plausible range around the estimated weighted prevalence. Wider intervals indicate more uncertainty, often because the filtered subgroup has fewer respondents or uneven survey weights."),
              p("This app is designed for exploration and communication. For formal inference, BRFSS analyses should use survey procedures that account for strata, clusters, and final weights.")
            )
          )
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSelectInput(session, "age", selected = "All")
    updateCheckboxGroupInput(session, "sex", selected = levels(brfss$sex))
    updateSelectInput(session, "plot_type", selected = "Hypertension prevalence with 95% CI")
  })

  filtered_data <- eventReactive(input$update, {
    dat <- brfss
    if (input$age != "All") dat <- filter(dat, age_group == input$age)
    if (length(input$sex) > 0) dat <- filter(dat, sex %in% input$sex) else dat <- dat[0, ]
    dat
  }, ignoreNULL = FALSE)

  prevalence_data <- reactive(prevalence_by_pa(filtered_data()))
  overall_data <- reactive(overall_prevalence(filtered_data()))

  output$n_text <- renderText(format(nrow(filtered_data()), big.mark = ","))

  output$prev_text <- renderText({
    ov <- overall_data()
    if (is.na(ov$estimate_pct)) return("N/A")
    paste0(round(ov$estimate_pct, 1), "%")
  })

  output$prev_ci_text <- renderText({
    ov <- overall_data()
    if (is.na(ov$estimate_pct)) return("95% CI unavailable")
    paste0("95% CI: ", round(ov$lower_pct, 1), "%–", round(ov$upper_pct, 1), "%")
  })

  output$gap_text <- renderText({
    pv <- prevalence_data()
    meet <- pv$estimate_pct[pv$physical_activity == "Meets recommendation"]
    not_meet <- pv$estimate_pct[pv$physical_activity == "Does not meet recommendation"]
    if (length(meet) == 0 || length(not_meet) == 0 || any(is.na(c(meet, not_meet)))) return("N/A")
    paste0(round(not_meet - meet, 1), " pp")
  })

  output$uncertainty_text <- renderText({
    pv <- prevalence_data()
    widths <- pv$upper_pct - pv$lower_pct
    widths <- widths[!is.na(widths)]
    if (length(widths) == 0) return("N/A")
    paste0(round(mean(widths), 1), " pp")
  })

  output$interpretation <- renderUI({
    pv <- prevalence_data()
    meet <- pv %>% filter(physical_activity == "Meets recommendation")
    not_meet <- pv %>% filter(physical_activity == "Does not meet recommendation")
    if (nrow(meet) == 0 || nrow(not_meet) == 0 || any(is.na(c(meet$estimate_pct, not_meet$estimate_pct)))) {
      return(p("The selected subgroup does not contain enough data for both physical activity groups. Try broadening the filters."))
    }
    gap <- not_meet$estimate_pct - meet$estimate_pct
    tagList(
      p("In the selected subgroup, the weighted prevalence of self-reported hypertension is ",
        strong(meet$ci_label),
        " among adults who meet the aerobic physical activity recommendation and ",
        strong(not_meet$ci_label),
        " among adults who do not meet the recommendation."),
      p("The estimated difference is ", strong(paste0(round(gap, 1), " percentage points")),
        ". The confidence intervals show statistical uncertainty around each estimate, so the comparison should be read as a descriptive pattern rather than a causal conclusion."),
      p("A mature next step would be to fit a survey-weighted regression model that adjusts for age, sex, income, and other confounders.")
    )
  })

  output$main_plot <- renderPlot({
    dat <- filtered_data()
    validate(need(nrow(dat) > 0, "No data available for the selected filters."))

    if (input$plot_type == "Hypertension prevalence with 95% CI") {
      ggplot(prevalence_data(), aes(x = physical_activity, y = estimate_pct, fill = physical_activity)) +
        geom_col(width = .58, alpha = .96, show.legend = FALSE) +
        geom_errorbar(aes(ymin = lower_pct, ymax = upper_pct), width = .16, linewidth = .9, color = "#10223e") +
        geom_text(aes(label = paste0(round(estimate_pct, 1), "%")), vjust = -0.85, fontface = "bold", size = 4.8, color = "#10223e") +
        scale_fill_manual(values = c("Meets recommendation" = "#2f6df6", "Does not meet recommendation" = "#1cc7d0")) +
        scale_y_continuous(limits = c(0, max(prevalence_data()$upper_pct, na.rm = TRUE) * 1.18), expand = expansion(mult = c(0, .03))) +
        labs(
          title = "Weighted hypertension prevalence by physical activity status",
          subtitle = "Bars show weighted prevalence; error bars show approximate 95% confidence intervals",
          x = NULL,
          y = "Weighted hypertension prevalence (%)",
          caption = "CI method: weighted proportion with effective sample size approximation."
        ) +
        premium_plot_theme()
    } else if (input$plot_type == "General health distribution with 95% CI") {
      hd <- health_distribution_ci(dat)
      ggplot(hd, aes(x = general_health, y = estimate_pct, fill = physical_activity)) +
        geom_col(position = position_dodge(width = .78), width = .66, alpha = .96) +
        geom_errorbar(aes(ymin = lower_pct, ymax = upper_pct), position = position_dodge(width = .78), width = .16, linewidth = .75, color = "#10223e") +
        scale_fill_manual(values = c("Meets recommendation" = "#2f6df6", "Does not meet recommendation" = "#1cc7d0")) +
        labs(
          title = "General health distribution by physical activity status",
          subtitle = "Percentages and approximate 95% confidence intervals are weighted",
          x = "Self-rated general health",
          y = "Weighted percent of selected respondents (%)",
          fill = "Physical activity status",
          caption = "CI method: weighted proportion with effective sample size approximation."
        ) +
        premium_plot_theme()
    } else {
      dat %>%
        count(age_group, sex, physical_activity) %>%
        ggplot(aes(x = age_group, y = n, fill = physical_activity)) +
        geom_col(position = "dodge", width = .72, alpha = .96) +
        scale_fill_manual(values = c("Meets recommendation" = "#2f6df6", "Does not meet recommendation" = "#1cc7d0")) +
        facet_wrap(~sex) +
        labs(
          title = "Unweighted sample size by age, sex, and physical activity status",
          subtitle = "Sample-size plots do not use confidence intervals because they are counts, not estimates",
          x = "Age group",
          y = "Number of respondents",
          fill = "Physical activity status"
        ) +
        premium_plot_theme()
    }
  })

  output$summary_table <- renderDT({
    prevalence_data() %>%
      transmute(
        `_PAINDX3 — Aerobic physical activity status` = physical_activity,
        `Unweighted n` = n,
        `_LLCPWT weighted n (weighted persons)` = round(weighted_n, 0),
        `_RFHYPE6 weighted hypertension prevalence (%)` = round(estimate_pct, 1),
        `95% CI lower (%)` = round(lower_pct, 1),
        `95% CI upper (%)` = round(upper_pct, 1),
        `Standard error (%)` = round(se * 100, 2),
        `Effective sample size` = round(neff, 1)
      ) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$data_table <- renderDT({
    filtered_data() %>%
      select(
        `_AGEG5YR — Age group` = age_group,
        `_SEX — Sex` = sex,
        `_PAINDX3 — Aerobic physical activity status` = physical_activity,
        `_RFHYPE6 — Hypertension status` = hypertension,
        `GENHLTH — General health` = general_health,
        `_INCOMG1 — Income group` = income,
        `_LLCPWT — Survey weight (weighted persons)` = weight
      ) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)
