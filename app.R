library(shiny)
library(shinydashboard)
library(shinyscreenshot)

# --- HELPER FUNCTION ---
calc_score <- function(inputs, weights, fte_weight) {
  clean_inputs <- sapply(inputs, function(x) ifelse(is.na(x) || x < 0, 0, x))
  sum(clean_inputs * weights) * fte_weight
}

# --- CSS STYLING ---
custom_css <- "
  .box-primary { border-top-color: #007bff !important; }
  .outreach-table { width: 100%; border-collapse: collapse; margin-bottom: 5px; font-family: 'Segoe UI', Arial, sans-serif; }
  .outreach-table th { background-color: #f2f2f2; border: 1px solid #cccccc; padding: 8px; font-size: 12px; font-weight: bold; text-align: center; }
  .outreach-table th.cat-header { text-align: left; padding-left: 10px; }
  .outreach-table th.qty-header { text-align: center; }
  .outreach-table td { border: 1px solid #cccccc; padding: 6px; text-align: center; font-size: 12px; vertical-align: middle; }
  .outreach-table .label-col { text-align: left; font-style: italic; width: 30%; font-weight: 500; background-color: #fafafa; padding-left: 10px; }
  .total-column { font-weight: bold; background-color: #f0f0f0; width: 100px; }
  .total-footer { background-color: #2c3e50; color: white; padding: 25px; font-weight: bold; font-size: 28px; text-align: center; border-radius: 8px; margin-top: 30px; border: 4px solid #1a252f; }
  .shiny-input-container { margin-bottom: 0px !important; }
  input[type='number'] { height: 28px; padding: 2px; text-align: center; width: 100%; }
  .header-box { background-color: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #bdc3c7; }
"

# --- UI SECTION ---
ui <- dashboardPage(
  dashboardHeader(title = "Warnell Outreach Score Sheet"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    fluidRow(
      column(width = 10, offset = 1,
             h1("Annual Outreach Evaluation Rubric", align = "center", style="margin-bottom: 30px;"),
             
             div(class = "header-box",
                 fluidRow(
                   column(6, textInput("fac_name", "Faculty Name:", placeholder = "Enter name...")),
                   column(6, numericInput("fac_fte", "Outreach Appointment FTE (e.g., 0.50):", value = 0.50, min = 0.01, max = 1.0, step = 0.05))
                 ),
                 p(style="margin-top:10px; color:#666; font-style:italic;", 
                   "Note: Point values are automatically scaled based on FTE. A 0.50 FTE is the standard baseline.")
             ),
             
             # I - PRESENTATIONS
             box(title = "I - Outreach Presentations", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Category"), 
                                tags$th(class="qty-header", "Base Qty"), 
                                tags$th(class="qty-header", "Prestige Qty"), 
                                tags$th("Base Pts"), tags$th("Addl Prestige Pts"), tags$th("Total")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Professional"), tags$td(numericInput("b_prof", NULL, 0, min=0)), tags$td(""), tags$td("2"), tags$td(""), tags$td(class="total-column", textOutput("t_prof_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Non-professional"), tags$td(numericInput("b_nprof", NULL, 0, min=0)), tags$td(""), tags$td("2"), tags$td(""), tags$td(class="total-column", textOutput("t_nprof_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Invited"), tags$td(numericInput("b_inv", NULL, 0, min=0)), tags$td(numericInput("p_inv", NULL, 0, min=0)), tags$td("3"), tags$td("+1"), tags$td(class="total-column", textOutput("t_inv_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Keynote"), tags$td(numericInput("b_key", NULL, 0, min=0)), tags$td(numericInput("p_key", NULL, 0, min=0)), tags$td("4.5"), tags$td("+2"), tags$td(class="total-column", textOutput("t_key_out", inline=T)))
                            )
                 )
             ),
             
             # II - PUBLICATIONS
             box(title = "II - Outreach Publications", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", rowspan=2, "Category"), 
                                tags$th(colspan=3, "Document Quantity"), 
                                tags$th(rowspan=2, "Base"), tags$th(rowspan=2, "Major Rev"), tags$th(rowspan=2, "Minor Rev"), tags$th(rowspan=2, "Prestige"), tags$th(rowspan=2, "Total")
                              ),
                              tags$tr(
                                tags$th(class="qty-header", "New"), 
                                tags$th(class="qty-header", "Major"), 
                                tags$th(class="qty-header", "Minor")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Books"), tags$td(numericInput("q_book", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("15"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_book_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Book Chapters"), tags$td(numericInput("q_chap", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("9"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_chap_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Peer-reviewed Articles"), tags$td(numericInput("q_jrnl", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("9"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_jrnl_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Proceedings"), tags$td(numericInput("q_proc", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("7"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_proc_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Outreach Document"), tags$td(numericInput("q_out", NULL, 0, min=0)), tags$td(numericInput("ma_out", NULL, 0, min=0)), tags$td(numericInput("mi_out", NULL, 0, min=0)), tags$td("5"), tags$td("1.5"), tags$td("2.5"), tags$td(""), tags$td(class="total-column", textOutput("t_out_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Technical Outreach Doc"), tags$td(numericInput("q_tech", NULL, 0, min=0)), tags$td(numericInput("ma_tech", NULL, 0, min=0)), tags$td(numericInput("mi_tech", NULL, 0, min=0)), tags$td("10"), tags$td("2.5"), tags$td("5"), tags$td(""), tags$td(class="total-column", textOutput("t_tech_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Gray Lit, Popular Press"), tags$td(numericInput("q_gray", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("4"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_gray_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Websites, Apps"), tags$td(numericInput("q_app", NULL, 0, min=0)), tags$td(numericInput("ma_app", NULL, 0, min=0)), tags$td(numericInput("mi_app", NULL, 0, min=0)), tags$td("10"), tags$td("2.5"), tags$td("5"), tags$td(""), tags$td(class="total-column", textOutput("t_app_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Podcasts"), tags$td(numericInput("q_pod", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("5"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_pod_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Social Media Posts"), tags$td(numericInput("q_smp", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("0.1"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_smp_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Social Media Products"), tags$td(numericInput("q_smprod", NULL, 0, min=0)), tags$td(""), tags$td(""), tags$td("1.5"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_smprod_out", inline=T)))
                            )
                 )
             ),
             
             # III - EVENTS
             box(title = "III - Outreach Events", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Category"), 
                                tags$th(class="qty-header", "New"), 
                                tags$th(class="qty-header", "Repeated"), 
                                tags$th(class="qty-header", "Substantial"), 
                                tags$th("New Pts"), tags$th("Rep Pts"), tags$th("Subst Pts"), tags$th("Total")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Events"), tags$td(numericInput("e_new", NULL, 0, min=0)), tags$td(numericInput("e_rep", NULL, 0, min=0)), tags$td(numericInput("e_sub", NULL, 0, min=0)), tags$td("9"), tags$td("2.25"), tags$td("13.5"), tags$td(class="total-column", textOutput("t_event_out", inline=T)))
                            )
                 )
             ),
             
             # IV - FUNDING
             box(title = "IV - Outreach Funding", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Funding Type"), 
                                tags$th(class="qty-header", "Small"), 
                                tags$th(class="qty-header", "Normal"), 
                                tags$th(class="qty-header", "Substantial"), 
                                tags$th("Small Pts"), tags$th("Normal Pts"), tags$th("Subst Pts"), tags$th("Total")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "New Awards"), tags$td(numericInput("fn_s", NULL, 0, min=0)), tags$td(numericInput("fn_n", NULL, 0, min=0)), tags$td(numericInput("fn_sub", NULL, 0, min=0)), tags$td("2.5"), tags$td("10"), tags$td("20"), tags$td(class="total-column", textOutput("t_fnew_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Ongoing Funding"), tags$td(numericInput("fo_s", NULL, 0, min=0)), tags$td(numericInput("fo_n", NULL, 0, min=0)), tags$td(numericInput("fo_sub", NULL, 0, min=0)), tags$td("1.5"), tags$td("5.5"), tags$td("11"), tags$td(class="total-column", textOutput("t_fong_out", inline=T)))
                            )
                 )
             ),
             
             # V - TECHNICAL ASSISTANCE
             box(title = "V - Technical Assistance", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Service Format"), 
                                tags$th(class="qty-header", "Quantity"), 
                                tags$th("Points per Unit"), tags$th("Total")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "On-site Visit"), tags$td(numericInput("ta_site", NULL, 0, min=0)), tags$td("0.8"), tags$td(class="total-column", textOutput("t_tasite_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Email/Phone Support"), tags$td(numericInput("ta_remote", NULL, 0, min=0)), tags$td("0.2"), tags$td(class="total-column", textOutput("t_taremote_out", inline=T)))
                            )
                 )
             ),
             
             # VI - AWARDS
             box(title = "VI - Outreach Awards", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Recognition"), 
                                tags$th(class="qty-header", "Base Qty"), 
                                tags$th(class="qty-header", "Prestige Qty"), 
                                tags$th("Base Pts"), tags$th("Prestige Pts"), tags$th("Total")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Award/Recognition"), tags$td(numericInput("aw_b", NULL, 0, min=0)), tags$td(numericInput("aw_p", NULL, 0, min=0)), tags$td("5"), tags$td("+7.5"), tags$td(class="total-column", textOutput("t_award_out", inline=T)))
                            )
                 )
             ),
             
             # VII - MEDIA CONTRIBUTIONS
             box(title = "VII - Media Contributions", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Media Type"), 
                                tags$th(class="qty-header", "Quantity"), 
                                tags$th("Points per Unit"), tags$th("Total")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Short Media Interview"), tags$td(numericInput("m_short", NULL, 0, min=0)), tags$td("0.5"), tags$td(class="total-column", textOutput("t_mshort_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Long Media Interview"), tags$td(numericInput("m_nat", NULL, 0, min=0)), tags$td("2"), tags$td(class="total-column", textOutput("t_mnat_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "National Coverage / Press Release"), tags$td(numericInput("m_feat", NULL, 0, min=0)), tags$td("5"), tags$td(class="total-column", textOutput("t_mfeat_out", inline=T)))
                            )
                 )
             ),
             
             # VIII - SPOTLIGHT
             box(title = "VIII - Spotlight Score", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(class="cat-header", "Item"), 
                                tags$th(class="qty-header", "Faculty Score (0-18)"), 
                                tags$th(class="qty-header", "ADO Score (0-18)"), 
                                tags$th("Average Score")
                              )
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Significant Effort"), tags$td(numericInput("s_fac", NULL, 0, min=0, max=18)), tags$td(numericInput("s_ado", NULL, 0, min=0, max=18)), tags$td(class="total-column", textOutput("t_spot_out", inline=T)))
                            )
                 )
             ),
             
             # EXPORT SECTION
             div(style = "margin-top: 20px; margin-bottom: 20px;",
                 fluidRow(
                   column(6, 
                          downloadButton("downloadData", "Export Data (CSV)", 
                                         style = "width: 100%; background-color: #34495e; color: white; border: none;")
                   ),
                   column(6, 
                          actionButton("screenshot", "Export Image (JPG)", 
                                       icon = icon("camera"),
                                       style = "width: 100%; background-color: #27ae60; color: white; border: none;")
                   )
                 )
             ),
             
             div(class = "total-footer",
                 "CUMULATIVE TOTAL OUTREACH SCORE: ",
                 textOutput("grand_total", inline = TRUE)
             )
      )
    )
  )
)

# --- SERVER SECTION ---
server <- function(input, output) {
  
  w <- reactive({
    fte <- ifelse(is.na(input$fac_fte) || input$fac_fte <= 0, 0.5, input$fac_fte)
    0.5 / fte
  })
  
  # Presentation Reactives
  t_prof   <- reactive({ calc_score(input$b_prof, 2, w()) })
  t_nprof  <- reactive({ calc_score(input$b_nprof, 2, w()) })
  t_inv    <- reactive({ calc_score(c(input$b_inv, input$p_inv), c(3, 1), w()) })
  t_key    <- reactive({ calc_score(c(input$b_key, input$p_key), c(4.5, 2), w()) })
  
  # Publication Reactives
  t_book   <- reactive({ calc_score(input$q_book, 16, w()) })
  t_chap   <- reactive({ calc_score(input$q_chap, 10, w()) })
  t_jrnl   <- reactive({ calc_score(input$q_jrnl, 10, w()) })
  t_proc   <- reactive({ calc_score(input$q_proc, 8, w()) })
  t_out    <- reactive({ calc_score(c(input$q_out, input$ma_out, input$mi_out), c(5, 1.5, 2.5), w()) })
  t_tech   <- reactive({ calc_score(c(input$q_tech, input$ma_tech, input$mi_tech), c(10, 2.5, 5), w()) })
  t_gray   <- reactive({ calc_score(input$q_gray, 4, w()) })
  t_app    <- reactive({ calc_score(c(input$q_app, input$ma_app, input$mi_app), c(10, 2.5, 5), w()) })
  t_pod    <- reactive({ calc_score(input$q_pod, 5, w()) })
  t_smp    <- reactive({ calc_score(input$q_smp, 0.1, w()) })
  t_smprod <- reactive({ calc_score(input$q_smprod, 1.5, w()) })
  
  # Other Reactives
  t_event  <- reactive({ calc_score(c(input$e_new, input$e_rep, input$e_sub), c(9, 2.25, 13.5), w()) })
  t_fnew   <- reactive({ calc_score(c(input$fn_s, input$fn_n, input$fn_sub), c(2.5, 10, 20), w()) })
  t_fong   <- reactive({ calc_score(c(input$fo_s, input$fo_n, input$fo_sub), c(1.5, 5.5, 11), w()) })
  t_tasite <- reactive({ calc_score(input$ta_site, 0.8, w()) })
  t_tarem  <- reactive({ calc_score(input$ta_remote, 0.2, w()) })
  t_award  <- reactive({ calc_score(c(input$aw_b, input$aw_p), c(5, 7.5), 1.0) })
  t_mshort <- reactive({ calc_score(input$m_short, 0.5, w()) })
  t_mnat   <- reactive({ calc_score(input$m_nat, 2, w()) })
  t_mfeat  <- reactive({ calc_score(input$m_feat, 5, w()) })
  
  t_spot   <- reactive({ 
    fac <- ifelse(is.na(input$s_fac) || input$s_fac < 0, 0, input$s_fac)
    ado <- ifelse(is.na(input$s_ado) || input$s_ado < 0, 0, input$s_ado)
    ((fac + ado) / 2) * w()
  })
  
  # Text Renderers
  output$t_prof_out <- renderText({ round(t_prof(), 2) })
  output$t_nprof_out <- renderText({ round(t_nprof(), 2) })
  output$t_inv_out  <- renderText({ round(t_inv(), 2) })
  output$t_key_out  <- renderText({ round(t_key(), 2) })
  output$t_book_out <- renderText({ round(t_book(), 2) })
  output$t_chap_out <- renderText({ round(t_chap(), 2) })
  output$t_jrnl_out <- renderText({ round(t_jrnl(), 2) })
  output$t_proc_out <- renderText({ round(t_proc(), 2) })
  output$t_out_out  <- renderText({ round(t_out(), 2) })
  output$t_tech_out <- renderText({ round(t_tech(), 2) })
  output$t_gray_out <- renderText({ round(t_gray(), 2) })
  output$t_app_out  <- renderText({ round(t_app(), 2) })
  output$t_pod_out  <- renderText({ round(t_pod(), 2) })
  output$t_smp_out  <- renderText({ round(t_smp(), 2) })
  output$t_smprod_out <- renderText({ round(t_smprod(), 2) })
  output$t_event_out <- renderText({ round(t_event(), 2) })
  output$t_fnew_out <- renderText({ round(t_fnew(), 2) })
  output$t_fong_out <- renderText({ round(t_fong(), 2) })
  output$t_tasite_out <- renderText({ round(t_tasite(), 2) })
  output$t_taremote_out <- renderText({ round(t_tarem(), 2) })
  output$t_award_out <- renderText({ round(t_award(), 2) })
  output$t_mshort_out <- renderText({ round(t_mshort(), 2) })
  output$t_mnat_out <- renderText({ round(t_mnat(), 2) })
  output$t_mfeat_out <- renderText({ round(t_mfeat(), 2) })
  output$t_spot_out <- renderText({ round(t_spot(), 2) })
  
  grand_total_val <- reactive({
    sum(t_prof(), t_nprof(), t_inv(), t_key(), t_book(), t_chap(), t_jrnl(), t_proc(), t_out(), 
        t_tech(), t_gray(), t_app(), t_pod(), t_smp(), t_smprod(), t_event(), 
        t_fnew(), t_fong(), t_tasite(), t_tarem(), t_award(), t_mshort(), t_mnat(), t_mfeat(), t_spot())
  })
  output$grand_total <- renderText({ round(grand_total_val(), 2) })
  
  # --- UPDATED CSV HANDLER (Optimized for Browser) ---
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Outreach_Breakdown_", gsub(" ", "_", input$fac_name), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      raw_sum <- function(...) {
        sum(sapply(list(...), function(x) ifelse(is.na(x) || x < 0, 0, x)))
      }
      
      export_df <- data.frame(
        "Category" = c("Presentations", "Publications", "Events", "Funding", "Technical", "Awards", "Media", "Spotlight", "TOTAL"),
        "Count" = c(
          raw_sum(input$b_prof, input$b_nprof, input$b_inv, input$p_inv, input$b_key, input$p_key),
          raw_sum(input$q_book, input$q_chap, input$q_jrnl, input$q_proc, input$q_out, input$ma_out, input$mi_out, input$q_tech, input$ma_tech, input$mi_tech, input$q_gray, input$q_app, input$ma_app, input$mi_app, input$q_pod, input$q_smp, input$q_smprod),
          raw_sum(input$e_new, input$e_rep, input$e_sub),
          raw_sum(input$fn_s, input$fn_n, input$fn_sub, input$fo_s, input$fo_n, input$fo_sub),
          raw_sum(input$ta_site, input$ta_remote),
          raw_sum(input$aw_b, input$aw_p),
          raw_sum(input$m_short, input$m_nat, input$m_feat),
          "N/A",
          ""
        ),
        "Score" = c(
          round(t_prof() + t_nprof() + t_inv() + t_key(), 2),
          round(t_book() + t_chap() + t_jrnl() + t_proc() + t_out() + t_tech() + t_gray() + t_app() + t_pod() + t_smp() + t_smprod(), 2),
          round(t_event(), 2),
          round(t_fnew() + t_fong(), 2),
          round(t_tasite() + t_tarem(), 2),
          round(t_award(), 2),
          round(t_mshort() + t_mnat() + t_mfeat(), 2),
          round(t_spot(), 2),
          round(grand_total_val(), 2)
        )
      )
      # In Shinylive, we must ensure the file is written to the provided path
      write.csv(export_df, file, row.names = FALSE)
    }
  )
  
  # --- IMAGE EXPORT ---
  observeEvent(input$screenshot, {
    safe_name <- gsub("[^[:alnum:]]", "_", input$fac_name)
    if(safe_name == "") safe_name <- "Warnell_Score"
    
    shinyscreenshot::screenshot(
      filename = paste0(safe_name, "_", Sys.Date()),
      downloadformat = "jpg",
      scale = 2
    )
  })
}

shinyApp(ui, server)
