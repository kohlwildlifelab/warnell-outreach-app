library(shiny)
library(shinydashboard)
library(shinyscreenshot)

# --- HELPER FUNCTION ---
calc_score <- function(inputs, weights, fte_weight) {
  clean_inputs <- sapply(inputs, function(x) {
    if (is.null(x) || is.na(x)) return(0)
    return(as.numeric(x))
  })
  sum(clean_inputs * weights) * fte_weight
}

# --- CSS STYLING ---
custom_css <- "
  .box-primary { border-top-color: #007bff !important; }
  .outreach-table { width: 100%; border-collapse: separate; border-spacing: 0; margin-bottom: 5px; font-family: 'Segoe UI', Arial, sans-serif; table-layout: fixed; }
  
  .outreach-table th, .outreach-table td { 
    border: 1px solid #cccccc; 
    padding: 4px 8px; 
    font-size: 11px; 
    text-align: center !important; 
    vertical-align: middle; 
    height: 38px !important; 
    box-sizing: border-box; 
  }

  .outreach-table td:has(input), .outreach-table td:has(.form-group) {
    display: table-cell;
    vertical-align: middle;
  }

  .outreach-table .form-group { 
    margin-bottom: 0px !important; 
    margin-top: 0px !important;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .outreach-table thead tr:first-child th:first-child, .outreach-table tbody td:first-child { text-align: left !important; padding-left: 12px !important; }
  .outreach-table th { background-color: #f2f2f2; font-weight: bold; }
  .label-col { font-style: italic; font-weight: 500; background-color: #fafafa; }
  .total-column { font-weight: bold; background-color: #f0f0f0 !important; }
  
  .summary-row td { 
    border-left: none; border-right: none; border-top: 1px solid #cccccc; border-bottom: 1px solid #cccccc; 
    background-color: #ffffff !important;
    white-space: nowrap !important; 
  }
  .summary-row td:first-child { border-left: 1px solid #cccccc; text-align: left !important; }
  
  .data-box { border: 1px solid #cccccc !important; font-weight: bold; background-color: #ffffff !important; }
  
  .subtotal-row td { background-color: #e3f2fd !important; font-weight: bold !important; border: 1px solid #cccccc !important; text-transform: uppercase; }
  .white-spacer td { border: none !important; background-color: #ffffff !important; height: 12px !important; }
  .bottom-header { background-color: #e9ecef !important; font-weight: bold; font-size: 9px; height: 25px !important; }

  .total-footer { background-color: #2c3e50; color: white; padding: 20px; font-weight: bold; font-size: 26px; text-align: center; border-radius: 8px; margin-top: 20px; border: 4px solid #1a252f; }
  
  input[type='number'] { height: 28px; text-align: center; width: 100%; font-size: 11px; border: 1px solid #ccc; border-radius: 4px; }
  .header-box { background-color: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #bdc3c7; }

  @media print {
    .main-header, .btn, .action-button, .header-box p, .main-sidebar { display: none !important; }
    .content-wrapper { background-color: white !important; }
    .total-footer { -webkit-print-color-adjust: exact; background-color: #2c3e50 !important; color: white !important; }
  }
"

ui <- dashboardPage(
  dashboardHeader(title = "Warnell Outreach Score Sheet"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    fluidRow(
      column(width = 10, offset = 1,
             h1("Annual Outreach Evaluation Rubric", align = "center", style="font-weight: bold;"),
             
             div(class = "header-box",
                 fluidRow(
                   column(6, textInput("fac_name", "Faculty Name:")),
                   column(6, numericInput("fac_fte", "Outreach Appointment FTE:", value = 0.50, min = 0.01, max = 1.0, step = 0.05))
                 )
             ),
             
             # I - PRESENTATIONS
             box(title = "I - Outreach Presentations", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Category"), tags$th(colspan=2, "Quantity"), tags$th(colspan=2, "Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("Base"), tags$th("Prestige"), tags$th("Base"), tags$th("Prestige"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Professional"), tags$td(numericInput("b_prof", NULL, 0)), tags$td(""), tags$td("2"), tags$td(""), tags$td(class="total-column", textOutput("t_prof_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Non-professional"), tags$td(numericInput("b_nprof", NULL, 0)), tags$td(""), tags$td("2"), tags$td(""), tags$td(class="total-column", textOutput("t_nprof_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Invited"), tags$td(numericInput("b_inv", NULL, 0)), tags$td(numericInput("p_inv", NULL, 0)), tags$td("3"), tags$td("+1"), tags$td(class="total-column", textOutput("t_inv_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Keynote"), tags$td(numericInput("b_key", NULL, 0)), tags$td(numericInput("p_key", NULL, 0)), tags$td("4.5"), tags$td("+2"), tags$td(class="total-column", textOutput("t_key_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=6, "")),
                              tags$tr(tags$td(colspan=5, style="border:none;"), tags$th(class="bottom-header", "Quantity")),
                              tags$tr(class="summary-row", tags$td(colspan=5, style="font-weight:bold;","Total Outreach Presentations (Calculated):"), tags$td(class="data-box", textOutput("q_total_1", inline=T))),
                              tags$tr(class="summary-row", tags$td(colspan=5, style="font-weight:bold;","Total Research Presentations (Manual Entry):"), tags$td(class="data-box", numericInput("res_pres_qty", NULL, 0))),
                              tags$tr(class="white-spacer", tags$td(colspan=6, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=5, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_1", inline=T)))
                            ))
             ),
             
             # II - PUBLICATIONS
             box(title = "II - Outreach Publications", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Category"), tags$th(colspan=4, "Quantity"), tags$th(colspan=4, "Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("New"), tags$th("Major Rev"), tags$th("Minor Rev"), tags$th("Prestige"), tags$th("Base"), tags$th("Major Rev"), tags$th("Minor Rev"), tags$th("Prestige"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Books"), tags$td(numericInput("q_book", NULL, 0)), tags$td(""), tags$td(""), tags$td(numericInput("p_book", NULL, 0)), tags$td("15"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_book_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Book Chapters"), tags$td(numericInput("q_chap", NULL, 0)), tags$td(""), tags$td(""), tags$td(numericInput("p_chap", NULL, 0)), tags$td("9"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_chap_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Peer-reviewed Articles"), tags$td(numericInput("q_jrnl", NULL, 0)), tags$td(""), tags$td(""), tags$td(numericInput("p_jrnl", NULL, 0)), tags$td("9"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_jrnl_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Proceedings"), tags$td(numericInput("q_proc", NULL, 0)), tags$td(""), tags$td(""), tags$td(numericInput("p_proc", NULL, 0)), tags$td("7"), tags$td(""), tags$td(""), tags$td("+1"), tags$td(class="total-column", textOutput("t_proc_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Outreach Document"), tags$td(numericInput("q_out", NULL, 0)), tags$td(numericInput("ma_out", NULL, 0)), tags$td(numericInput("mi_out", NULL, 0)), tags$td(""), tags$td("5"), tags$td("1.5"), tags$td("2.5"), tags$td(""), tags$td(class="total-column", textOutput("t_out_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Technical Outreach Doc"), tags$td(numericInput("q_tech", NULL, 0)), tags$td(numericInput("ma_tech", NULL, 0)), tags$td(numericInput("mi_tech", NULL, 0)), tags$td(""), tags$td("10"), tags$td("2.5"), tags$td("5"), tags$td(""), tags$td(class="total-column", textOutput("t_tech_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Gray Lit, Popular Press"), tags$td(numericInput("q_gray", NULL, 0)), tags$td(""), tags$td(""), tags$td(""), tags$td("4"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_gray_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Websites, Apps"), tags$td(numericInput("q_app", NULL, 0)), tags$td(numericInput("ma_app", NULL, 0)), tags$td(numericInput("mi_app", NULL, 0)), tags$td(""), tags$td("10"), tags$td("2.5"), tags$td("5"), tags$td(""), tags$td(class="total-column", textOutput("t_app_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Podcasts"), tags$td(numericInput("q_pod", NULL, 0)), tags$td(""), tags$td(""), tags$td(""), tags$td("5"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_pod_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Social Media Posts"), tags$td(numericInput("q_smp", NULL, 0)), tags$td(""), tags$td(""), tags$td(""), tags$td("0.1"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_smp_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Social Media Products"), tags$td(numericInput("q_smprod", NULL, 0)), tags$td(""), tags$td(""), tags$td(""), tags$td("1.5"), tags$td(""), tags$td(""), tags$td(""), tags$td(class="total-column", textOutput("t_smprod_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=10, "")),
                              tags$tr(tags$td(colspan=9, style="border:none;"), tags$th(class="bottom-header", "Quantity")),
                              tags$tr(class="summary-row", tags$td(colspan=9, style="font-weight:bold;","Total Outreach Publications (Calculated):"), tags$td(class="data-box", textOutput("q_total_2", inline=T))),
                              tags$tr(class="summary-row", tags$td(colspan=9, style="font-weight:bold;","Total Research Publications (Manual Entry):"), tags$td(class="data-box", numericInput("res_pub_qty", NULL, 0))),
                              tags$tr(class="white-spacer", tags$td(colspan=10, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=9, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_2", inline=T)))
                            ))
             ),
             
             # III - EVENTS
             box(title = "III - Outreach Events", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Category"), tags$th(colspan=3, "Quantity"), tags$th(colspan=3, "Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("New"), tags$th("Repeated"), tags$th("Substantial"), tags$th("New"), tags$th("Repeated"), tags$th("Substantial"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Events"), tags$td(numericInput("e_new", NULL, 0)), tags$td(numericInput("e_rep", NULL, 0)), tags$td(numericInput("e_sub", NULL, 0)), tags$td("9"), tags$td("2.25"), tags$td("13.5"), tags$td(class="total-column", textOutput("t_event_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=8, "")),
                              tags$tr(tags$td(colspan=7, style="border:none;"), tags$th(class="bottom-header", "Quantity")),
                              tags$tr(class="summary-row", tags$td(colspan=7, style="font-weight:bold;","Total Outreach Events:"), tags$td(class="data-box", textOutput("q_total_3", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=8, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=7, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_3", inline=T)))
                            ))
             ),
             
             # IV - FUNDING
             box(title = "IV - Outreach Funding", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(
                                tags$th(rowspan=2, "Funding Type", style="width: 25%"), 
                                tags$th(colspan=2, "Small"), 
                                tags$th(colspan=2, "Normal"), 
                                tags$th(colspan=2, "Substantial"), 
                                tags$th(rowspan=2, colspan=2, "Calculated Score", style="width: 22%;")
                              ),
                              tags$tr(
                                tags$th("Qty"), tags$th("Total $"),
                                tags$th("Qty"), tags$th("Total $"),
                                tags$th("Qty"), tags$th("Total $")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td(class="label-col", "New Funding"),
                                tags$td(numericInput("fn_s", NULL, 0)), tags$td(numericInput("fn_s_d", NULL, 0)),
                                tags$td(numericInput("fn_n", NULL, 0)), tags$td(numericInput("fn_n_d", NULL, 0)),
                                tags$td(numericInput("fn_sub", NULL, 0)), tags$td(numericInput("fn_sub_d", NULL, 0)),
                                tags$td(colspan=2, class="total-column", textOutput("t_fnew_out", inline=T))
                              ),
                              tags$tr(
                                tags$td(class="label-col", "Ongoing Funding"),
                                tags$td(numericInput("fo_s", NULL, 0)), tags$td(numericInput("fo_s_d", NULL, 0)),
                                tags$td(numericInput("fo_n", NULL, 0)), tags$td(numericInput("fo_n_d", NULL, 0)),
                                tags$td(numericInput("fo_sub", NULL, 0)), tags$td(numericInput("fo_sub_d", NULL, 0)),
                                tags$td(colspan=2, class="total-column", textOutput("t_fong_out", inline=T))
                              ),
                              tags$tr(class="white-spacer", tags$td(colspan=9, "")),
                              tags$tr(
                                tags$td(colspan=7, style="border:none; background-color:white;"),
                                tags$th(class="bottom-header", "Total Quantity"),
                                tags$th(class="bottom-header", "Total Amount")
                              ),
                              tags$tr(class="summary-row",
                                      tags$td(colspan=7, style="font-weight:bold;","Total Outreach Grants (Calculated):"),
                                      tags$td(class="data-box", textOutput("grand_q_out", inline=T)),
                                      tags$td(class="data-box", textOutput("grand_d_out", inline=T))
                              ),
                              tags$tr(class="summary-row",
                                      tags$td(colspan=7, style="font-weight:bold;","Total Research Grants (Manual Entry):"),
                                      tags$td(class="data-box", numericInput("res_total_q", NULL, 0)),
                                      tags$td(class="data-box", numericInput("res_total_d", NULL, 0))
                              ),
                              tags$tr(class="white-spacer", tags$td(colspan=9, "")),
                              tags$tr(class="subtotal-row",
                                      tags$td(colspan=7, "SUBTOTAL SCORE:"),
                                      tags$td(colspan=2, textOutput("s_total_4", inline=T))
                              )
                            ))
             ),
             
             # V - TECHNICAL ASSISTANCE
             box(title = "V - Technical Assistance", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Service Format"), tags$th("Quantity"), tags$th("Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("Unit Count"), tags$th("Per Unit"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "On-site Visit"), tags$td(numericInput("ta_site", NULL, 0)), tags$td("0.8"), tags$td(class="total-column", textOutput("t_tasite_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Email/Phone Support"), tags$td(numericInput("ta_remote", NULL, 0)), tags$td("0.2"), tags$td(class="total-column", textOutput("t_taremote_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=4, "")),
                              tags$tr(tags$td(colspan=3, style="border:none;"), tags$th(class="bottom-header", "Quantity")),
                              tags$tr(class="summary-row", tags$td(colspan=3, style="font-weight:bold;","Total Actions (Calculated):"), tags$td(class="data-box", textOutput("q_total_5", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=4, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=3, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_5", inline=T)))
                            ))
             ),
             
             # VI - AWARDS
             box(title = "VI - Outreach Awards", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Recognition"), tags$th(colspan=2, "Quantity"), tags$th(colspan=2, "Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("Base"), tags$th("Pres"), tags$th("Base"), tags$th("Pres"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Award/Recognition"), tags$td(numericInput("aw_b", NULL, 0)), tags$td(numericInput("aw_p", NULL, 0)), tags$td("5"), tags$td("+2.5"), tags$td(class="total-column", textOutput("t_award_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=6, "")),
                              tags$tr(tags$td(colspan=5, style="border:none;"), tags$th(class="bottom-header", "Quantity")),
                              tags$tr(class="summary-row", tags$td(colspan=5, style="font-weight:bold;","Total Awards (Calculated):"), tags$td(class="data-box", textOutput("q_total_6", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=6, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=5, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_6", inline=T)))
                            ))
             ),
             
             # VII - MEDIA CONTRIBUTIONS
             box(title = "VII - Media Contributions", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Media Type"), tags$th("Quantity"), tags$th("Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("Unit Count"), tags$th("Per Unit"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Short Interview"), tags$td(numericInput("m_short", NULL, 0)), tags$td("0.5"), tags$td(class="total-column", textOutput("t_mshort_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "Long Interview"), tags$td(numericInput("m_nat", NULL, 0)), tags$td("2"), tags$td(class="total-column", textOutput("t_mnat_out", inline=T))),
                              tags$tr(tags$td(class="label-col", "National/Press Release"), tags$td(numericInput("m_feat", NULL, 0)), tags$td("5"), tags$td(class="total-column", textOutput("t_mfeat_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=4, "")),
                              tags$tr(tags$td(colspan=3, style="border:none;"), tags$th(class="bottom-header", "Quantity")),
                              tags$tr(class="summary-row", tags$td(colspan=3, style="font-weight:bold;","Total Media Items (Calculated):"), tags$td(class="data-box", textOutput("q_total_7", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=4, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=3, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_7", inline=T)))
                            ))
             ),
             
             # VIII - SPOTLIGHT
             box(title = "VIII - Spotlight Score", width = 12, status = "primary", solidHeader = TRUE,
                 tags$table(class = "outreach-table",
                            tags$thead(
                              tags$tr(tags$th(rowspan=2, "Item"), tags$th(colspan=2, "Quantity (0-18)"), tags$th("Points"), tags$th(rowspan=2, "Total Score")),
                              tags$tr(tags$th("Faculty"), tags$th("ADO"), tags$th("Avg"))
                            ),
                            tags$tbody(
                              tags$tr(tags$td(class="label-col", "Significant Effort"), tags$td(numericInput("s_fac", NULL, value = 0, min = 0, max = 18, step = 1)), 
                                      tags$td(numericInput("s_ado", NULL, , value = 0, min = 0, max = 18, step = 1)), 
                                      tags$td(textOutput("spot_avg_display")), tags$td(class="total-column", textOutput("t_spot_out", inline=T))),
                              tags$tr(class="white-spacer", tags$td(colspan=5, "")),
                              tags$tr(class="subtotal-row", tags$td(colspan=4, "SUBTOTAL SCORE:"), tags$td(textOutput("s_total_8", inline=T)))
                            ))
             ),
             
             # --- EXPORT SECTION ---
             div(style = "margin-top: 20px; margin-bottom: 20px;",
                 fluidRow(
                   column(4, downloadButton("downloadData", "Export CSV", style = "width: 100%; background-color: #34495e; color: white; border: none;")),
                   column(4, actionButton("print_pdf", "Print to PDF", icon = icon("print"), onclick = "window.print();", style = "width: 100%; background-color: #2980b9; color: white; border: none;")),
                   column(4, actionButton("screenshot", "Export JPG", icon = icon("camera"), style = "width: 100%; background-color: #27ae60; color: white; border: none;"))
                 )
             ),
             
             div(class = "total-footer", "TOTAL OUTREACH SCORE: ", textOutput("grand_total", inline = TRUE))
      )
    )
  )
)

server <- function(input, output, session) {
  # Universal Weighting with requirement check
  w <- reactive({ 
    req(input$fac_fte)
    0.5 / max(0.01, as.numeric(input$fac_fte)) 
  })
  
  # I Logic
  t_prof <- reactive({ calc_score(input$b_prof, 2, w()) })
  t_nprof <- reactive({ calc_score(input$b_nprof, 2, w()) })
  t_inv <- reactive({ calc_score(c(input$b_inv, input$p_inv), c(3, 4), w()) })
  t_key <- reactive({ calc_score(c(input$b_key, input$p_key), c(4.5, 6.5), w()) })
  output$t_prof_out <- renderText({ round(t_prof(), 2) })
  output$t_nprof_out <- renderText({ round(t_nprof(), 2) })
  output$t_inv_out <- renderText({ round(t_inv(), 2) })
  output$t_key_out <- renderText({ round(t_key(), 2) })
  output$q_total_1 <- renderText({ sum(input$b_prof, input$b_nprof, input$b_inv, input$p_inv, input$b_key, input$p_key, na.rm=T) })
  output$s_total_1 <- renderText({ round(t_prof() + t_nprof() + t_inv() + t_key(), 2) })
  
  # II Logic
  t_pub_vals <- reactive({
    list(
      bk = calc_score(c(input$q_book, input$p_book), c(15, 16), w()),
      ch = calc_score(c(input$q_chap, input$p_chap), c(9, 10), w()),
      jr = calc_score(c(input$q_jrnl, input$p_jrnl), c(9, 10), w()),
      pr = calc_score(c(input$q_proc, input$p_proc), c(7, 8), w()),
      ou = calc_score(c(input$q_out, input$ma_out, input$mi_out), c(5, 1.5, 2.5), w()),
      te = calc_score(c(input$q_tech, input$ma_tech, input$mi_tech), c(10, 2.5, 5), w()),
      gr = calc_score(input$q_gray, 4, w()),
      ap = calc_score(c(input$q_app, input$ma_app, input$mi_app), c(10, 2.5, 5), w()),
      po = calc_score(input$q_pod, 5, w()),
      sp = calc_score(input$q_smp, 0.1, w()),
      sd = calc_score(input$q_smprod, 1.5, w())
    )
  })
  output$t_book_out <- renderText({ round(t_pub_vals()$bk, 2) })
  output$t_chap_out <- renderText({ round(t_pub_vals()$ch, 2) })
  output$t_jrnl_out <- renderText({ round(t_pub_vals()$jr, 2) })
  output$t_proc_out <- renderText({ round(t_pub_vals()$pr, 2) })
  output$t_out_out <- renderText({ round(t_pub_vals()$ou, 2) })
  output$t_tech_out <- renderText({ round(t_pub_vals()$te, 2) })
  output$t_gray_out <- renderText({ round(t_pub_vals()$gr, 2) })
  output$t_app_out <- renderText({ round(t_pub_vals()$ap, 2) })
  output$t_pod_out <- renderText({ round(t_pub_vals()$po, 2) })
  output$t_smp_out <- renderText({ round(t_pub_vals()$sp, 2) })
  output$t_smprod_out <- renderText({ round(t_pub_vals()$sd, 2) })
  output$q_total_2 <- renderText({ sum(input$q_book, input$p_book, input$q_chap, input$p_chap, input$q_jrnl, input$p_jrnl, input$q_proc, input$p_proc, input$q_out, input$ma_out, input$mi_out, input$q_tech, input$ma_tech, input$mi_tech, input$q_gray, input$q_app, input$ma_app, input$mi_app, input$q_pod, input$q_smp, input$q_smprod, na.rm=T) })
  output$s_total_2 <- renderText({ round(Reduce(`+`, t_pub_vals()), 2) })
  
  # III Logic
  t_event <- reactive({ calc_score(c(input$e_new, input$e_rep, input$e_sub), c(9, 2.25, 13.5), w()) })
  output$t_event_out <- renderText({ round(t_event(), 2) })
  output$q_total_3 <- renderText({ sum(input$e_new, input$e_rep, input$e_sub, na.rm=T) })
  output$s_total_3 <- renderText({ round(t_event(), 2) })
  
  # IV Logic (Funding)
  t_fnew <- reactive({ calc_score(c(input$fn_s, input$fn_n, input$fn_sub), c(2.5, 10, 20), w()) })
  t_fong <- reactive({ calc_score(c(input$fo_s, input$fo_n, input$fo_sub), c(1.5, 5.5, 11), w()) })
  output$t_fnew_out <- renderText({ round(t_fnew(), 2) })
  output$t_fong_out <- renderText({ round(t_fong(), 2) })
  output$grand_q_out <- renderText({ sum(input$fn_s, input$fn_n, input$fn_sub, input$fo_s, input$fo_n, input$fo_sub, na.rm=T) })
  output$grand_d_out <- renderText({
    total_d <- sum(input$fn_s_d, input$fn_n_d, input$fn_sub_d, input$fo_s_d, input$fo_n_d, input$fo_sub_d, na.rm=T)
    paste0("$", format(total_d, big.mark=","))
  })
  output$s_total_4 <- renderText({ round(t_fnew() + t_fong(), 2) })
  
  # V Logic
  t_ta_site <- reactive({ calc_score(input$ta_site, 0.8, w()) })
  t_ta_rem <- reactive({ calc_score(input$ta_remote, 0.2, w()) })
  output$t_tasite_out <- renderText({ round(t_ta_site(), 2) })
  output$t_taremote_out <- renderText({ round(t_ta_rem(), 2) })
  output$q_total_5 <- renderText({ sum(input$ta_site, input$ta_remote, na.rm=T) })
  output$s_total_5 <- renderText({ round(t_ta_site() + t_ta_rem(), 2) })
  
  # VI Logic
  t_awd <- reactive({ calc_score(c(input$aw_b, input$aw_p), c(5, 7.5), 1.0) }) # Awards fixed weighting
  output$t_award_out <- renderText({ round(t_awd(), 2) })
  output$q_total_6 <- renderText({ sum(input$aw_b, input$aw_p, na.rm=T) })
  output$s_total_6 <- renderText({ round(t_awd(), 2) })
  
  # VII Logic
  t_mshort <- reactive({ calc_score(input$m_short, 0.5, w()) })
  t_mnat <- reactive({ calc_score(input$m_nat, 2, w()) })
  t_mfeat <- reactive({ calc_score(input$m_feat, 5, w()) })
  output$t_mshort_out <- renderText({ round(t_mshort(), 2) })
  output$t_mnat_out <- renderText({ round(t_mnat(), 2) })
  output$t_mfeat_out <- renderText({ round(t_mfeat(), 2) })
  output$q_total_7 <- renderText({ sum(input$m_short, input$m_nat, input$m_feat, na.rm=T) })
  output$s_total_7 <- renderText({ round(t_mshort() + t_mnat() + t_mfeat(), 2) })
  
  # VIII Logic
  spot_avg <- reactive({ 
    # Helper function to keep value between 0 and 18
    clamp <- function(x) {
        val <- as.numeric(x %||% 0)
        max(0, min(18, val))
  }
    fac <- if(!is.null(input$s_fac)) as.numeric(input$s_fac) else 0
    ado <- if(!is.null(input$s_ado)) as.numeric(input$s_ado) else 0
    (fac + ado) / 2 
  })
  t_spot <- reactive({ spot_avg() * w() })
  output$spot_avg_display <- renderText({ round(spot_avg(), 2) })
  output$t_spot_out <- renderText({ round(t_spot(), 2) })
  output$s_total_8 <- renderText({ round(t_spot(), 2) })
  
  # Grand Total
  grand_total_val <- reactive({ 
    sum(t_prof(), t_nprof(), t_inv(), t_key(), Reduce(`+`, t_pub_vals()), t_event(), t_fnew(), t_fong(), t_ta_site(), t_ta_rem(), t_awd(), t_mshort(), t_mnat(), t_mfeat(), t_spot(), na.rm=T)
  })
  output$grand_total <- renderText({ round(grand_total_val(), 2) })
  
  # --- FILTERED DETAILED CSV EXPORT ---
  output$downloadData <- downloadHandler(
    filename = function() { 
      file_name <- if(input$fac_name == "") "Outreach_Score" else gsub(" ", "_", input$fac_name)
      paste0(file_name, "_Summary_", Sys.Date(), ".csv") 
    },
    content = function(file) {
      # Helper to safely grab numeric inputs
      get_val <- function(id) { if(is.null(input[[id]])) 0 else as.numeric(input[[id]]) }
      
      # 1. Create the full raw data frame
      raw_export <- data.frame(
        Category = c(
          "FACULTY INFO", "Outreach FTE",
          "I - PRESENTATIONS", "Professional", "Non-Professional", "Invited (Base)", "Invited (Prestige)", "Keynote (Base)", "Keynote (Prestige)", "Subtotal Section I",
          "II - PUBLICATIONS", "Books (New)", "Books (Prestige)", "Chapters (New)", "Chapters (Prestige)", "Journal Articles (New)", "Journal Articles (Prestige)", "Proceedings (New)", "Proceedings (Prestige)", "Outreach Doc (New)", "Outreach Doc (Major)", "Outreach Doc (Minor)", "Tech Doc (New)", "Tech Doc (Major)", "Tech Doc (Minor)", "Gray Lit", "Apps/Web (New)", "Apps/Web (Major)", "Apps/Web (Minor)", "Podcasts", "Social Media Posts", "Social Media Products", "Subtotal Section II",
          "III - EVENTS", "Events (New)", "Events (Repeated)", "Events (Substantial)", "Subtotal Section III",
          "IV - FUNDING", "New (Small)", "New (Normal)", "New (Substantial)", "Ongoing (Small)", "Ongoing (Normal)", "Ongoing (Substantial)", "Subtotal Section IV",
          "V - TECH ASSISTANCE", "On-Site Visits", "Remote Support", "Subtotal Section V",
          "VI - AWARDS", "Awards (Base)", "Awards (Prestige)", "Subtotal Section VI",
          "VII - MEDIA", "Short Interview", "Long Interview", "National/Press Release", "Subtotal Section VII",
          "VIII - SPOTLIGHT", "Faculty Score", "ADO Score", "Subtotal Section VIII",
          "GRAND TOTAL"
        ),
        Quantity = c(
          NA, get_val("fac_fte"),
          NA, get_val("b_prof"), get_val("b_nprof"), get_val("b_inv"), get_val("p_inv"), get_val("b_key"), get_val("p_key"), (t_prof() + t_nprof() + t_inv() + t_key()),
          NA, get_val("q_book"), get_val("p_book"), get_val("q_chap"), get_val("p_chap"), get_val("q_jrnl"), get_val("p_jrnl"), get_val("q_proc"), get_val("p_proc"), get_val("q_out"), get_val("ma_out"), get_val("mi_out"), get_val("q_tech"), get_val("ma_tech"), get_val("mi_tech"), get_val("q_gray"), get_val("q_app"), get_val("ma_app"), get_val("mi_app"), get_val("q_pod"), get_val("q_smp"), get_val("q_smprod"), round(Reduce(`+`, t_pub_vals()), 2),
          NA, get_val("e_new"), get_val("e_rep"), get_val("e_sub"), round(t_event(), 2),
          NA, get_val("fn_s"), get_val("fn_n"), get_val("fn_sub"), get_val("fo_s"), get_val("fo_n"), get_val("fo_sub"), round(t_fnew() + t_fong(), 2),
          NA, get_val("ta_site"), get_val("ta_remote"), round(t_ta_site() + t_ta_rem(), 2),
          NA, get_val("aw_b"), get_val("aw_p"), round(t_awd(), 2),
          NA, get_val("m_short"), get_val("m_nat"), get_val("m_feat"), round(t_mshort() + t_mnat() + t_mfeat(), 2),
          NA, get_val("s_fac"), get_val("s_ado"), round(t_spot(), 2),
          grand_total_val()
        ),
        Score = c(
          NA, NA,
          NA, NA, NA, NA, NA, NA, NA, round(t_prof() + t_nprof() + t_inv() + t_key(), 2),
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, round(Reduce(`+`, t_pub_vals()), 2),
          NA, NA, NA, NA, round(t_event(), 2),
          NA, NA, NA, NA, NA, NA, NA, round(t_fnew() + t_fong(), 2),
          NA, NA, NA, round(t_ta_site() + t_ta_rem(), 2),
          NA, NA, NA, round(t_awd(), 2),
          NA, NA, NA, NA, round(t_mshort() + t_mnat() + t_mfeat(), 2),
          NA, NA, NA, round(t_spot(), 2),
          round(grand_total_val(), 2)
        ),
        stringsAsFactors = FALSE
      )
      
      # 2. Define logic for filtering
      # We keep rows if:
      # - It's a Section Header (Quantity is NA and it's uppercase)
      # - Quantity is > 0
      # - It's the Grand Total
      
      filtered_export <- raw_export[
        is.na(raw_export$Quantity) | 
          raw_export$Quantity > 0 | 
          raw_export$Category == "GRAND TOTAL", 
      ]
      
      # 3. Final Clean-up: Remove headers that have no data underneath them
      # (Optional, but keeps the CSV very clean)
      header_indices <- which(is.na(filtered_export$Quantity))
      to_remove <- c()
      for(i in seq_along(header_indices)){
        idx <- header_indices[i]
        next_idx <- if(i < length(header_indices)) header_indices[i+1] else nrow(filtered_export) + 1
        # If the distance between this header and the next is only 1, the header is empty
        if(next_idx - idx == 1 && filtered_export$Category[idx] != "GRAND TOTAL"){
          to_remove <- c(to_remove, idx)
        }
      }
      
      if(length(to_remove) > 0) filtered_export <- filtered_export[-to_remove, ]
      
      write.csv(filtered_export, file, row.names = FALSE, na = "")
    }
  )
  
  observeEvent(input$screenshot, {
    shinyscreenshot::screenshot(selector="body", scale=2)
  })
}

shinyApp(ui, server)
