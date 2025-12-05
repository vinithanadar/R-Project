library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
library(scales)


dig <- read_csv("DIG.csv") %>% clean_names()

# Add age group column (used in multiple sections)

dig_mut1 <- dig %>%
  mutate(age_group = case_when(
    age < 50 ~ "Below 50",
    age >= 50 & age < 70 ~ "50-69",
    age >= 70 ~ "70+"
  ))

ui <- dashboardPage(
  dashboardHeader(title = "DIG Dashboard - All Analyses"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("A. Age vs Sex", 
               tabName = "age_sex", icon = icon("venus-mars")),
      menuItem("B. Age vs Diabetes",
               tabName = "age_diabetes", icon = icon("pie-chart")),
      menuItem("C. Age vs Hypertension",
               tabName = "age_hyper", icon = icon("heartbeat")),
      menuItem("D. Age vs Race",
               tabName = "age_race", icon = icon("users")),
      menuItem("E. BMI vs Race",
               tabName = "bmi_race", icon = icon("weight")),
      menuItem("F. ID vs NHOSP",
               tabName = "id_hosp", icon = icon("hospital")),
      menuItem("G. Heart Rate vs CREV",
               tabName = "hr_crev", icon = icon("chart-area")),
      menuItem("H. Treatment vs Death",
               tabName = "trt_death", icon = icon("skull-crossbones")),
      menuItem("I. BMI vs Diabetes",
               tabName = "bmi_diab", icon = icon("stethoscope")),
      menuItem("J. Treatment vs Outcomes",
               tabName = "trt_outcomes", icon = icon("heartbeat")),
      menuItem("K. Treatment Comparisons",
               tabName = "treat_compare", icon = icon("chart-bar")),
      menuItem("L. Treatment vs Continuous Variables",
               tabName = "treat_cont", icon = icon("sliders")),
      menuItem("M. WHF vs Clinical Parameters",
               tabName = "whf_compare", icon = icon("notes-medical"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # A--
      tabItem("age_sex",
              fluidRow(
                box(width = 6, title = "Age Distribution by Sex", plotOutput("age_sex_plot")),
                box(width = 6, title = "Summary Table", tableOutput("sex_age_summary"))
              )
      ),
      
      # B--
      tabItem("age_diabetes",
              fluidRow(
                box(width = 6, title = "Age Group vs Diabetes (Pie Chart)", plotOutput("age_diabetes_pie")),
                box(width = 6, title = "Summary Table", tableOutput("age_diabetes_table"))
              )
      ),
      
      # C--
      tabItem("age_hyper",
              fluidRow(
                box(width = 6, title = "Hypertension Cases by Age Group", plotOutput("pie_chart")),
                box(width = 6, title = "Summary Table", tableOutput("pie_summary"))
              )
      ),
      
      # D--
      tabItem("age_race",
              fluidRow(
                box(width = 6, title = "Age Distribution by Race", plotOutput("age_vs_race_plot")),
                box(width = 6, title = "Summary Table", tableOutput("summ_age_race"))
              )
      ),
      
      # E--
      tabItem("bmi_race",
              fluidRow(
                box(width = 6, title = "BMI Distribution by Race", plotOutput("bmi_race_box")),
                box(width = 6, title = "Summary Table", tableOutput("bmi_race_summary"))
              )
      ),
      
      # F--
      tabItem("id_hosp",
              fluidRow(
                box(width = 6, title = "NHOSP Histogram", plotOutput("hosp_hist")),
                box(width = 6, title = "Summary Table", tableOutput("hosp_summary"))
              )
      ),
      
      # G--
      tabItem("hr_crev",
              fluidRow(
                box(width = 6, title = "Heart Rate by CREV", plotOutput("hr_crev_plot")),
                box(width = 6, title = "Summary Table", tableOutput("hr_crev_summary"))
              )
      ),
      
      # H--
      tabItem("trt_death",
              fluidRow(
                box(width = 6, title = "Death Rate by Treatment", plotOutput("trt_death_bar")),
                box(width = 6, title = "Summary Table", tableOutput("trt_death_summary"))
              )
      ),
      
      #I--
      tabItem("bmi_diab",
              fluidRow(
                box(width = 6, title = "BMI Distribution by Diabetes", plotOutput("bmi_diab_plot")),
                box(width = 6, title = "Summary Table", tableOutput("bmi_diab_summary"))
              )
      ),
      
      # J--
      tabItem("trt_outcomes",
              fluidRow(
                box(width = 4, title = "Select Outcome", 
                    selectInput("outcome_var", "Outcome:", 
                                choices = c("Worsening HF" = "whf", "MI" = "mi", "Stroke" = "strk")
                    )),
                box(width = 8, title = "Outcome Plot", plotOutput("trt_outcome_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Summary Table", tableOutput("trt_outcome_summary"))
              )
      ),
      
      # K--
      tabItem("treat_compare",
              fluidRow(
                box(width = 4, title = "Age vs Treatment", plotOutput("age_treat_plot")),
                box(width = 4, title = "Sex vs Treatment", plotOutput("sex_treat_plot")),
                box(width = 4, title = "Race vs Treatment", plotOutput("race_treat_plot"))
              )
      ),
      
      # L----
      tabItem("treat_cont",
              fluidRow(
                box(width = 4, title = "Choose Variable",
                    selectInput("cont_var", "Variable:", 
                                choices = c("Ejection Fraction" = "ejf_per",
                                            "Systolic BP" = "sysbp",
                                            "Creatinine" = "creat"))
                ),
                box(width = 8, title = "Density Plot", plotOutput("trt_parameter_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Summary Table", tableOutput("trt_parameter_summary"))
              )
      ),
      
      #M---
      tabItem("whf_compare",
              fluidRow(
                box(width = 4, title = "WHF vs Heart Rate", plotOutput("whf_hr_plot")),
                box(width = 4, title = "WHF vs Diastolic BP", plotOutput("whf_diabp_plot")),
                box(width = 4, title = "WHF vs Systolic BP", plotOutput("whf_sysbp_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Summary Table", tableOutput("whf_summary"))
              )
      )
    )
  )
)


# SERVER CODE

server <- function(input, output, session) {
  
  # A--
  output$age_sex_plot <- renderPlot({
    ggplot(dig, aes(age, fill = factor(sex))) +
      geom_histogram(bins = 30, alpha = 0.6) +
      theme_minimal()
  })
  
  output$sex_age_summary <- renderTable({
    dig %>% group_by(sex) %>% summarise(
      count = n(),
      mean_age = mean(age),
      median_age = median(age),
      min_age = min(age),
      max_age = max(age)
    )
  })
  
  #B--
  output$age_diabetes_pie <- renderPlot({
    pie_data <- dig_mut1 %>% group_by(age_group, diabetes) %>% summarise(count = n())
    ggplot(pie_data, aes("", count, fill = interaction(age_group, diabetes))) +
      geom_col() + coord_polar("y") + theme_void()
  })
  
  output$age_diabetes_table <- renderTable({
    dig_mut1 %>% group_by(age_group, diabetes) %>% summarise(count = n())
  })
  
  # C--
  output$pie_chart <- renderPlot({
    pie_data <- dig_mut1 %>% 
      group_by(age_group) %>% 
      summarise(hyper_cases = sum(hyperten == 1))
    ggplot(pie_data, aes("", hyper_cases, fill = age_group)) +
      geom_col() + coord_polar("y") + theme_void()
  })
  
  output$pie_summary <- renderTable({
    dig_mut1 %>%
      group_by(age_group) %>%
      summarise(
        total = n(),
        hypertension_cases = sum(hyperten == 1),
        percent = hypertension_cases / total * 100
      )
  })
  
  # D--
  output$age_vs_race_plot <- renderPlot({
    ggplot(dig, aes(age, fill = factor(race))) +
      geom_histogram(bins = 30, alpha = 0.7)
  })
  
  output$summ_age_race <- renderTable({
    dig %>% group_by(race) %>% summarise(
      count = n(),
      mean_age = mean(age),
      median_age = median(age),
      min_age = min(age),
      max_age = max(age)
    )
  })
  
  # E--
  output$bmi_race_box <- renderPlot({
    ggplot(dig, aes(factor(race), bmi, fill = factor(race))) +
      geom_boxplot(alpha = 0.7)
  })
  
  output$bmi_race_summary <- renderTable({
    dig %>% group_by(race) %>% summarise(
      n = n(),
      mean = mean(bmi),
      median = median(bmi),
      min = min(bmi),
      max = max(bmi)
    )
  })
  
  # F--
  output$hosp_hist <- renderPlot({
    ggplot(dig, aes(nhosp)) +
      geom_histogram(bins = 20, fill = "red", alpha = 0.7)
  })
  
  output$hosp_summary <- renderTable({
    dig %>% summarise(
      n = n(),
      mean = mean(nhosp),
      median = median(nhosp),
      min = min(nhosp),
      max = max(nhosp)
    )
  })
  
  # G--
  output$hr_crev_plot <- renderPlot({
    ggplot(dig, aes(factor(crev), heartrte, fill = factor(crev))) +
      geom_boxplot(alpha = 0.7)
  })
  
  output$hr_crev_summary <- renderTable({
    dig %>% group_by(crev) %>% summarise(
      count = n(),
      mean_hr = mean(heartrte),
      median_hr = median(heartrte),
      min_hr = min(heartrte),
      max_hr = max(heartrte)
    )
  })
  
  # H----
  output$trt_death_bar <- renderPlot({
    df <- dig %>% 
      group_by(trtmt) %>% 
      summarise(
        n = n(),
        deaths = sum(death == 1),
        rate = deaths / n
      )
    ggplot(df, aes(factor(trtmt), rate, fill = factor(trtmt))) + geom_col()
  })
  
  output$trt_death_summary <- renderTable({
    dig %>% group_by(trtmt) %>% summarise(
      n = n(),
      deaths = sum(death == 1),
      percent = deaths / n * 100
    )
  })
  
  # I---
  output$bmi_diab_plot <- renderPlot({
    ggplot(dig, aes(factor(diabetes), bmi, fill = factor(diabetes))) +
      geom_violin() +
      geom_jitter(alpha = 0.2)
  })
  
  output$bmi_diab_summary <- renderTable({
    dig %>% group_by(diabetes) %>% summarise(
      count = n(),
      mean_bmi = mean(bmi),
      median_bmi = median(bmi),
      min_bmi = min(bmi),
      max_bmi = max(bmi)
    )
  })
  
  # J---
  summary_data <- reactive({
    out <- sym(input$outcome_var)
    dig %>% group_by(trtmt) %>% summarise(
      n = n(),
      events = sum(!!out == 1),
      rate = events / n
    )
  })
  
  output$trt_outcome_plot <- renderPlot({
    df <- summary_data()
    ggplot(df, aes(factor(trtmt), rate, fill = factor(trtmt))) +
      geom_col()
  })
  
  output$trt_outcome_summary <- renderTable({
    summary_data() %>% mutate(percent = rate * 100)
  })
  
  # K--
  output$age_treat_plot <- renderPlot({
    ggplot(dig, aes(factor(trtmt), age, fill = factor(trtmt))) +
      geom_boxplot()
  })
  
  output$sex_treat_plot <- renderPlot({
    dig %>% count(trtmt, sex) %>%
      ggplot(aes(factor(trtmt), n, fill = factor(sex))) +
      geom_bar(stat = "identity", position = "dodge")
  })
  
  output$race_treat_plot <- renderPlot({
    dig %>% count(trtmt, race) %>%
      ggplot(aes(factor(trtmt), n, fill = factor(race))) +
      geom_bar(stat = "identity", position = "dodge")
  })
  
  # L---
  cont_data <- reactive({
    dig %>% select(trtmt, value = !!sym(input$cont_var))
  })
  
  output$trt_parameter_plot <- renderPlot({
    ggplot(cont_data(), aes(value, fill = factor(trtmt))) +
      geom_density(alpha = 0.4) +
      facet_wrap(~trtmt)
  })
  
  output$trt_parameter_summary <- renderTable({
    cont_data() %>% group_by(trtmt) %>% summarise(
      n = n(),
      mean = mean(value),
      median = median(value),
      sd = sd(value),
      min = min(value),
      max = max(value)
    )
  })
  
  # M--
  output$whf_hr_plot <- renderPlot({
    ggplot(dig, aes(factor(whf), heartrte, fill = factor(whf))) +
      geom_boxplot(alpha = 0.8)
  })
  
  output$whf_diabp_plot <- renderPlot({
    ggplot(dig, aes(factor(whf), diabp, fill = factor(whf))) +
      geom_boxplot(alpha = 0.8)
  })
  
  output$whf_sysbp_plot <- renderPlot({
    ggplot(dig, aes(factor(whf), sysbp, fill = factor(whf))) +
      geom_boxplot(alpha = 0.8)
  })
  
  output$whf_summary <- renderTable({
    dig %>% group_by(whf) %>% summarise(
      count = n(),
      mean_heartrate = mean(heartrte),
      mean_diabp = mean(diabp),
      mean_sysbp = mean(sysbp),
      median_heartrate = median(heartrte),
      median_diabp = median(diabp),
      median_sysbp = median(sysbp),
      min_heartrate = min(heartrte),
      max_heartrate = max(heartrte),
      min_diabp = min(diabp),
      max_diabp = max(diabp),
      min_sysbp = min(sysbp),
      max_sysbp = max(sysbp)
    )
  })
}


shinyApp(ui, server)
