# app.R
library(shiny)
library(dplyr)
library(rintrojs)
library(ggplot2)
library(patchwork)

# ---- LOAD DATA ----
data_path <- "BMP_Scenario_Data.csv"
scenarios <- read.csv(data_path, stringsAsFactors = FALSE)

scenarios$till_date[scenarios$till_date %in% c("", "NA")] <- NA

scenarios <- scenarios %>%
  mutate(
    filter    = as.character(filter),
    till      = as.character(till),
    till_date = as.character(till_date),
    fert_comp = as.character(fert_comp),
    spr_fert  = as.character(spr_fert),
    fert_date = as.character(fert_date),
    sum_fert  = as.character(sum_fert),
    fert_app  = as.character(fert_app),
    cover     = as.character(cover)
  )
scenarios$row_id <- seq_len(nrow(scenarios))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Farming Management Scenario Comparison"),
  introjsUI(),
  actionButton("launch_tour", "Press for Guided Explanation",
               class = "btn bn-outline-secondary btn-sm"),
  
  tabsetPanel(
    tabPanel("Scenario Tool",
             
             fluidRow(
               column(
                 width = 12,
                 fluidRow(
                   # Scenario A
                   column(
                     width = 2,
                     wellPanel(id = "panel_A", style = "max-width: 350px;",
                               h4("Reference Scenario"),
                               selectInput("A_filter", "Filter strip (% of field area)",
                                           choices = sort(unique(scenarios$filter))),
                               selectInput("A_till", "Tillage",
                                           choices = sort(unique(scenarios$till))),
                               conditionalPanel(
                                 condition = "input.A_till == 'Till'",
                                 selectInput("A_till_date", "Tillage Date",
                                             choices = sort(unique(na.omit(scenarios$till_date))))
                               ),
                               selectInput("A_NP", "Fertilizer Composition",
                                           choices = sort(unique(scenarios$fert_comp))),
                               selectInput("A_spring_amt", "Spring Fertilizer Amount (lbs/ac)",
                                           choices = sort(as.numeric(unique(scenarios$spr_fert)))),
                               conditionalPanel(
                                 condition = "input.A_spring_amt != '0'",
                                 selectInput("A_spring_date", "Spring Fertilizer Date",
                                             choices = sort(unique(scenarios$fert_date)))
                               ),
                               selectInput("A_summer_amt", "Summer Fertilizer Amount (lbs/ac)",
                                           choices = sort(as.numeric(unique(scenarios$sum_fert)))),
                               conditionalPanel(
                                 condition = "input.A_spring_amt != '0' | input.A_summer_amt != '0'",
                                 selectInput("A_inject", "Fertilizer Application Method",
                                             choices = sort(unique(scenarios$fert_app)))
                               ),
                               selectInput("A_cover", "Cover Crop",
                                           choices = sort(unique(scenarios$cover)))
                     )
                   ),
                   
                   # Scenario B
                   column(
                     width = 2,
                     wellPanel(id = "panel_B", style = "max-width: 350px;",
                               h4("Comparison Scenario"),
                               selectInput("B_filter", "Filter strip (%)",
                                           choices = sort(unique(scenarios$filter))),
                               selectInput("B_till", "Tillage",
                                           choices = sort(unique(scenarios$till))),
                               conditionalPanel(
                                 condition = "input.B_till == 'Till'",
                                 selectInput("B_till_date", "Tillage Date",
                                             choices = sort(unique(na.omit(scenarios$till_date))))
                               ),
                               selectInput("B_NP", "Fertilizer Composition",
                                           choices = sort(unique(scenarios$fert_comp))),
                               selectInput("B_spring_amt", "Spring Fertilizer Amount (lbs/ac)",
                                           choices = sort(as.numeric(unique(scenarios$spr_fert)))),
                               conditionalPanel(
                                 condition = "input.B_spring_amt != '0'",
                                 selectInput("B_spring_date", "Spring Fertilizer Date",
                                             choices = sort(unique(scenarios$fert_date)))
                               ),
                               selectInput("B_summer_amt", "Summer Fertilizer Amount (lbs/ac)",
                                           choices = sort(as.numeric(unique(scenarios$sum_fert)))),
                               conditionalPanel(
                                 condition = "input.B_spring_amt != '0' | input.B_summer_amt != '0'",
                                 selectInput("B_inject", "Fertilizer Application Method",
                                             choices = sort(unique(scenarios$fert_app)))
                               ),
                               selectInput("B_cover", "Cover Crop",
                                           choices = sort(unique(scenarios$cover)))
                     )
                   ),
                   
                   # Plots
                   column(
                     width = 8,
                     div(id = "plot_panel",
                         # Top row: difference + yield side by side
                         fluidRow(
                           column(width = 7,
                                  h4("Nutrient Difference (Comparison - Reference)"),
                                  plotOutput("diff_plot", height = "300px")
                           ),
                           column(width = 5,
                                  h4("Crop Yield Percent Change (Comparison vs. Reference)"),
                                  plotOutput("yield_plot", height = "300px")
                           )
                         ),
                         hr(),
                         # Bottom row: absolute values
                         fluidRow(
                           column(width = 12,
                                  h4("Absolute Exports by Scenario"),
                                  plotOutput("abs_plot", height = "300px")
                           )
                         )
                     )
                   ),
                   
                   column(
                     width = 1,
                     div(id = "reset_button",
                         actionButton("reset", "Reset Scenarios",
                                      class = "btn btn-warning btn-sm"),
                         br(), br()
                     )
                   )
                 )
               )
             ),
             
             fluidRow(
               column(
                 width = 12,
                 hr(),
                 h5("Reference"),
                 textOutput("scenarioA_row"),
                 hr(),
                 h5("Comparison"),
                 textOutput("scenarioB_row")
               )
             )
    ),
    
    tabPanel("More Information",
             br(),
             h3("Additional Resources"),
             p("Get more information about these Best Management Practices"),
             tags$ul(
               tags$li(tags$a("American Farmland Trust - On Farm Conservation Practices",
                              href = "https://farmlandinfo.org/improve-on-farm-conservation",
                              target = "_blank"))
             )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  tour_steps <- data.frame(
    element = c(NA, "#panel_A", "#panel_B", "#plot_panel", "#reset_button"),
    intro = c(
      "Welcome! This tool compares two farming scenarios and shows how BMPs affect nutrient export and crop yield. Click Next to continue.",
      "This is your reference/baseline scenario. Set the farming practices you want to compare against.",
      "This is your comparison scenario. Change practices here to see how they differ from Scenario A.",
      "The top row shows the nutrient export difference (Comparison - Reference) and crop yield percent change (Comparison vs. Reference). The bottom row shows absolute exports for each scenario.",
      "Use this button to reset both scenarios."
    ),
    stringsAsFactors = FALSE
  )
  
  observe({
    introjs(session, options = list(steps = tour_steps, showProgress = TRUE,
                                    nextLabel = "Next →", prevLabel = "← Back",
                                    doneLabel = "Done ✓"))
  }) %>% bindEvent(session$clientData$url_hostname, once = TRUE)
  
  observeEvent(input$launch_tour, {
    introjs(session, options = list(steps = tour_steps, showProgress = TRUE,
                                    nextLabel = "Next →", prevLabel = "← Back",
                                    doneLabel = "Done ✓"))
  })
  
  observeEvent(input$A_spring_amt, {
    if (input$A_spring_amt == "0")
      updateSelectInput(session, "A_spring_date", selected = "1-May")
  })
  observeEvent(input$B_spring_amt, {
    if (input$B_spring_amt == "0")
      updateSelectInput(session, "B_spring_date", selected = "1-May")
  })
  observeEvent(c(input$A_spring_amt, input$A_summer_amt), {
    if (input$A_spring_amt == "0" && input$A_summer_amt == "0")
      updateSelectInput(session, "A_inject", selected = "Aerial Solid")
  })
  observeEvent(c(input$B_spring_amt, input$B_summer_amt), {
    if (input$B_spring_amt == "0" && input$B_summer_amt == "0")
      updateSelectInput(session, "B_inject", selected = "Aerial Solid")
  })
  
  scenario_A <- reactive({
    base <- scenarios %>%
      filter(
        filter    == input$A_filter,
        till      == input$A_till,
        fert_comp == input$A_NP,
        spr_fert  == input$A_spring_amt,
        fert_date == input$A_spring_date,
        sum_fert  == input$A_summer_amt,
        fert_app  == input$A_inject,
        cover     == input$A_cover
      )
    if (input$A_till != "Till") base %>% filter(is.na(till_date))
    else base %>% filter(till_date == input$A_till_date)
  })
  
  scenario_B <- reactive({
    base <- scenarios %>%
      filter(
        filter    == input$B_filter,
        till      == input$B_till,
        fert_comp == input$B_NP,
        spr_fert  == input$B_spring_amt,
        fert_date == input$B_spring_date,
        sum_fert  == input$B_summer_amt,
        fert_app  == input$B_inject,
        cover     == input$B_cover
      )
    if (input$B_till != "Till") base %>% filter(is.na(till_date))
    else base %>% filter(till_date == input$B_till_date)
  })
  
  # ---- Difference plot ----
  output$diff_plot <- renderPlot({
    A <- scenario_A()
    B <- scenario_B()
    req(nrow(A) == 1, nrow(B) == 1)
    
    same <- A$row_id == B$row_id
    
    diff_df <- data.frame(
      Metric  = c("Total Nitrogen", "Total Dissolved\nPhosphorus"),
      Diff    = c(B$TN_mean  - A$TN_mean,  B$TDP_mean  - A$TDP_mean),
      DiffMin = c(B$TN_min   - A$TN_max,   B$TDP_min   - A$TDP_max),
      DiffMax = c(B$TN_max   - A$TN_min,   B$TDP_max   - A$TDP_min)
    )
    diff_df$Label <- sprintf("%.2f", diff_df$Diff)
    
    # Place label above error bar max (or below min if negative)
    diff_df$y_text <- diff_df$Diff
    diff_df$vjust  <- ifelse(diff_df$Diff >= 0, -0.5, 1.5)
    
    p <- ggplot(diff_df, aes(x = Metric, y = Diff, fill = Diff > 0)) +
      geom_col(show.legend = FALSE, width = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      scale_fill_manual(values = c("TRUE" = "#C0392B", "FALSE" = "#27AE60")) +
      labs(y = "Difference (lbs/ac)", x = NULL) +
      theme_minimal(base_size = 16) +
      theme(panel.grid.minor   = element_blank(),
            axis.text          = element_text(size = 15),
            axis.title         = element_text(size = 16))
    
    if (!same) {
      p <- p +
        geom_errorbar(aes(ymin = DiffMin, ymax = DiffMax),
                      width = 0.2, linewidth = 0.8, color = "black") +
        geom_text(aes(y = y_text, label = Label, vjust = vjust),
                  hjust = 0, position = position_nudge(x=0.15), size = 8)
    } else {
      p <- p +
        geom_text(aes(y = y_text, label = Label, vjust = vjust), size = 8)
    }
    p
  })
  
  # ---- Yield plot ----
  output$yield_plot <- renderPlot({
    A <- scenario_A()
    B <- scenario_B()
    req(nrow(A) == 1, nrow(B) == 1)
    
    same <- A$row_id == B$row_id
    
    pct  <- (B$CY_mean - A$CY_mean) / A$CY_mean * 100
    pmin <- (B$CY_min  - A$CY_max)  / A$CY_max  * 100
    pmax <- (B$CY_max  - A$CY_min)  / A$CY_min  * 100
    
    yield_df <- data.frame(
      Metric  = "Crop Yield",
      Diff    = pct,
      DiffMin = pmin,
      DiffMax = pmax,
      Label   = sprintf("%.1f%%", pct)
    )
    yield_df$y_text <- pct
    yield_df$vjust  <- ifelse(pct >= 0, -0.5, 1.5)
    
    p <- ggplot(yield_df, aes(x = Metric, y = Diff, fill = Diff > 0)) +
      geom_col(show.legend = FALSE, width = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      scale_fill_manual(values = c("TRUE" = "#27AE60", "FALSE" = "#C0392B")) +
      labs(y = "Percent change (%)", x = NULL) +
      theme_minimal(base_size = 16) +
      theme(panel.grid.minor = element_blank(),
            axis.text        = element_text(size = 15),
            axis.title       = element_text(size = 16))
    
    if (!same) {
      p <- p +
        geom_errorbar(aes(ymin = DiffMin, ymax = DiffMax),
                      width = 0.15, linewidth = 0.8, color = "black") +
        geom_text(aes(y = y_text, label = Label, vjust = vjust),
                  hjust = 0, position = position_nudge(x=0.15), size = 8)
    } else {
      p <- p +
        geom_text(aes(y = y_text, label = Label, vjust = vjust), size = 8)
    }
    p
  })
  
  # ---- Absolute values plot ----
  output$abs_plot <- renderPlot({
    A <- scenario_A()
    B <- scenario_B()
    req(nrow(A) == 1, nrow(B) == 1)
    
    abs_df <- data.frame(
      Metric   = rep(c("Total Nitrogen", "Total Dissolved\nPhosphorus"), each = 2),
      Scenario = rep(c("A", "B"), times = 2),
      Mean     = c(A$TN_mean,  B$TN_mean,  A$TDP_mean,  B$TDP_mean),
      Min      = c(A$TN_min,   B$TN_min,   A$TDP_min,   B$TDP_min),
      Max      = c(A$TN_max,   B$TN_max,   A$TDP_max,   B$TDP_max)
    )
    abs_df$Label  <- sprintf("%.2f", abs_df$Mean)
    abs_df$y_text <- abs_df$Mean
    abs_df$vjust  <- -0.5
    
    ggplot(abs_df, aes(x = Scenario, y = Mean, fill = Scenario)) +
      geom_col(show.legend = TRUE, width = 0.5) +
      geom_errorbar(aes(ymin = Min, ymax = Max),
                    width = 0.2, linewidth = 0.8, color = "black") +
      geom_text(aes(y = y_text, label = Label, vjust = vjust), 
                hjust = 0, position = position_nudge(x=0.15), size = 8) +
      facet_wrap(~Metric, scales = "free_y") +
      scale_fill_manual(
        values = c("A" = "#298c8c", "B" = "#800074"),
        labels = c("A" = "Reference", "B" = "Comparison"),
        name   = NULL
      ) +
      labs(y = "Export (lbs/ac)", x = NULL) +
      theme_minimal(base_size = 16) +
      theme(
        panel.grid.minor  = element_blank(),
        axis.text         = element_text(size = 15),
        axis.title        = element_text(size = 16),
        strip.text        = element_text(size = 16, face = "bold"),
        legend.text       = element_text(size = 14),
        legend.position   = "bottom"
      )
  })
  
  output$scenarioA_row <- renderText({
    A <- scenario_A()
    req(nrow(A) == 1)
    inputs  <- c(A$filter, A$till, ifelse(is.na(A$till_date), "NA", A$till_date),
                 A$fert_comp, A$spr_fert, A$fert_date, A$sum_fert, A$fert_app, A$cover)
    outputs <- sprintf("NO3=%.2f, NO2=%.2f, NH3=%.2f, P=%.2f, Yield=%.2f",
                       A$no3, A$no2, A$nh3, A$phos, A$crop_yield)
    paste0("Row ", A$row_id, " | ", paste(inputs, collapse = " | "), " || ", outputs)
  })
  
  output$scenarioB_row <- renderText({
    B <- scenario_B()
    req(nrow(B) == 1)
    inputs  <- c(B$filter, B$till, ifelse(is.na(B$till_date), "NA", B$till_date),
                 B$fert_comp, B$spr_fert, B$fert_date, B$sum_fert, B$fert_app, B$cover)
    outputs <- sprintf("NO3=%.2f, NO2=%.2f, NH3=%.2f, P=%.2f, Yield=%.2f",
                       B$no3, B$no2, B$nh3, B$phos, B$crop_yield)
    paste0("Row ", B$row_id, " | ", paste(inputs, collapse = " | "), " || ", outputs)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "A_filter",      selected = sort(unique(scenarios$filter))[1])
    updateSelectInput(session, "A_till",        selected = sort(unique(scenarios$till))[1])
    updateSelectInput(session, "A_till_date",   selected = sort(unique(na.omit(scenarios$till_date)))[1])
    updateSelectInput(session, "A_NP",          selected = sort(unique(scenarios$fert_comp))[1])
    updateSelectInput(session, "A_spring_amt",  selected = sort(unique(scenarios$spr_fert))[1])
    updateSelectInput(session, "A_spring_date", selected = sort(unique(scenarios$fert_date))[1])
    updateSelectInput(session, "A_summer_amt",  selected = sort(unique(scenarios$sum_fert))[1])
    updateSelectInput(session, "A_inject",      selected = sort(unique(scenarios$fert_app))[1])
    updateSelectInput(session, "A_cover",       selected = sort(unique(scenarios$cover))[1])
    
    updateSelectInput(session, "B_filter",      selected = sort(unique(scenarios$filter))[1])
    updateSelectInput(session, "B_till",        selected = sort(unique(scenarios$till))[1])
    updateSelectInput(session, "B_till_date",   selected = sort(unique(na.omit(scenarios$till_date)))[1])
    updateSelectInput(session, "B_NP",          selected = sort(unique(scenarios$fert_comp))[1])
    updateSelectInput(session, "B_spring_amt",  selected = sort(unique(scenarios$spr_fert))[1])
    updateSelectInput(session, "B_spring_date", selected = sort(unique(scenarios$fert_date))[1])
    updateSelectInput(session, "B_summer_amt",  selected = sort(unique(scenarios$sum_fert))[1])
    updateSelectInput(session, "B_inject",      selected = sort(unique(scenarios$fert_app))[1])
    updateSelectInput(session, "B_cover",       selected = sort(unique(scenarios$cover))[1])
  })
}

shinyApp(ui, server)
