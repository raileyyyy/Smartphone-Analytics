# Load required libraries
library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(readxl)
library(stringr)
library(readr)
library(rpart)          
library(rpart.plot)     
library(e1071)          
library(caret)          

# LOAD & PREPROCESS DATA
data <- read_csv("C:/Users/agana/OneDrive/Desktop/Aidan/Projects/Data Analytics/group2_data_preprocess.csv")
data <- clean_names(data)

# Convert memory/storage to MB
convert_to_mb <- function(x) {
  if (is.na(x) || x == "") return(NA)
  num <- as.numeric(str_extract(x, "\\d+\\.?\\d*"))
  if (is.na(num)) return(NA)
  unit <- str_extract(tolower(x), "gb|mb")
  if (is.na(unit)) return(num * 1024)
  if (unit == "gb") return(num * 1024)
  if (unit == "mb") return(num)
  return(NA)
}

data$memory  <- sapply(data$memory,  convert_to_mb)
data$storage <- sapply(data$storage, convert_to_mb)

# Handle missing values (numeric -> median, categorical -> mode)
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data$memory[is.na(data$memory)]   <- median(data$memory,  na.rm = TRUE)
data$storage[is.na(data$storage)] <- median(data$storage, na.rm = TRUE)
data$rating[is.na(data$rating)]   <- median(data$rating,  na.rm = TRUE)

data$brands[is.na(data$brands)] <- get_mode(data$brands)
data$colors[is.na(data$colors)] <- get_mode(data$colors)
data$camera[is.na(data$camera)] <- get_mode(data$camera)
data$mobile[is.na(data$mobile)] <- get_mode(data$mobile)

# Text cleanup
data$brands <- str_to_upper(str_trim(data$brands))
data$colors <- str_to_title(str_trim(data$colors))
data$camera <- str_to_title(str_trim(data$camera))
data$mobile <- str_squish(data$mobile)

# Derived columns
data$os <- ifelse(
  str_detect(str_to_lower(data$brands), "apple|iphone"),
  "iOS", "Android"
)

data$discount_percentage <- round(
  ((data$original_price - data$selling_price) / data$original_price) * 100,
  2
)

data$price_category <- case_when(
  data$selling_price < 10000 ~ "Budget",
  data$selling_price < 25000 ~ "Mid-Range",
  TRUE                       ~ "Flagship"
)

# Remove duplicates
data <- distinct(data)

# Remove outliers on selling_price (IQR method)
Q1          <- quantile(data$selling_price, 0.25, na.rm = TRUE)
Q3          <- quantile(data$selling_price, 0.75, na.rm = TRUE)
IQR_value   <- IQR(data$selling_price, na.rm = TRUE)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

data <- data %>%
  filter(selling_price >= lower_bound, selling_price <= upper_bound)

# Factor conversions
data$os             <- factor(data$os)
data$price_category <- factor(data$price_category,
                              levels = c("Budget", "Mid-Range", "Flagship"))
data$brands         <- factor(data$brands, ordered = FALSE)
data$colors         <- factor(data$colors, ordered = FALSE)
data$camera         <- factor(data$camera, ordered = FALSE)
data$models         <- factor(data$models, ordered = FALSE)
data$mobile         <- factor(data$mobile, ordered = FALSE)


# MIN-MAX NORMALIZATION

minmax_norm <- function(x, new_min = 0, new_max = 1) {
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  if (mx == mn) return(rep(0, length(x)))
  ((x - mn) / (mx - mn)) * (new_max - new_min) + new_min
}

data$MinMax_SellingPrice <- minmax_norm(data$selling_price, 0, 100)
data$norm_selling_price  <- minmax_norm(data$selling_price)
data$norm_original_price <- minmax_norm(data$original_price)
data$norm_memory         <- minmax_norm(data$memory)
data$norm_storage        <- minmax_norm(data$storage)
data$norm_discount       <- minmax_norm(data$discount_percentage)
data$Discount_Scale      <- data$discount_percentage / 100


# TARGET VARIABLE

RATING_THRESH   <- 2
DISCOUNT_THRESH <- 1
STORAGE_THRESH  <- 16384   # 16 GB in MB

cond_rating   <- data$rating              > RATING_THRESH
cond_discount <- data$discount_percentage > DISCOUNT_THRESH
cond_storage  <- data$storage             >= STORAGE_THRESH

conditions_met <- as.integer(cond_rating) +
  as.integer(cond_discount) +
  as.integer(cond_storage)

data$Recommend <- factor(
  ifelse(conditions_met >= 2, "Yes", "No"),
  levels = c("No", "Yes")
)

cat("Recommend distribution:\n")
print(table(data$Recommend))
cat("Yes %:", round(mean(data$Recommend == "Yes") * 100, 1), "\n")


# TRAIN / TEST SPLIT  (70 / 30)

set.seed(42)
train_idx  <- createDataPartition(data$Recommend, p = 0.7, list = FALSE)
train_data <- data[ train_idx, ]
test_data  <- data[-train_idx, ]

features <- c("norm_selling_price", "norm_memory",
              "norm_storage",       "norm_discount",
              "rating")


# MODEL 1: DECISION TREE  

dt_model <- rpart(
  Recommend ~ norm_selling_price + norm_memory +
    norm_storage + norm_discount + rating,
  data    = train_data,
  method  = "class",
  parms   = list(loss = matrix(c(0, 4, 1, 0), nrow = 2,
                               dimnames = list(c("No","Yes"), c("No","Yes")))),
  control = rpart.control(cp = 0.001, minsplit = 5, maxdepth = 6)
)

dt_pred     <- predict(dt_model, newdata = test_data, type = "class")
dt_cm       <- confusionMatrix(dt_pred, test_data$Recommend, positive = "Yes")
dt_accuracy <- round(dt_cm$overall["Accuracy"] * 100, 2)


# MODEL 2: NAIVE BAYES

nb_model <- naiveBayes(
  Recommend ~ norm_selling_price + norm_memory +
    norm_storage + norm_discount + rating,
  data = train_data
)

nb_pred     <- predict(nb_model, newdata = test_data)
nb_cm       <- confusionMatrix(nb_pred, test_data$Recommend, positive = "Yes")
nb_accuracy <- round(nb_cm$overall["Accuracy"] * 100, 2)


# PERFORMANCE METRICS

compute_metrics <- function(cm_obj, positive = "Yes") {
  tbl <- as.matrix(cm_obj$table)
  TP <- tbl[positive, positive]
  TN <- tbl[setdiff(rownames(tbl), positive), setdiff(colnames(tbl), positive)]
  FP <- tbl[positive, setdiff(colnames(tbl), positive)]
  FN <- tbl[setdiff(rownames(tbl), positive), positive]
  
  TP <- as.numeric(TP); TN <- as.numeric(TN); FP <- as.numeric(FP); FN <- as.numeric(FN)
  
  sensitivity <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  specificity <- if ((TN + FP) == 0) NA else TN / (TN + FP)
  precision   <- if ((TP + FP) == 0) NA else TP / (TP + FP)
  recall      <- sensitivity
  f1_score    <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA else 2 * (precision * recall) / (precision + recall)
  accuracy    <- as.numeric(cm_obj$overall["Accuracy"])
  
  list(
    TP = TP, TN = TN, FP = FP, FN = FN,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Precision = precision,
    F1 = f1_score
  )
}

dt_metrics <- compute_metrics(dt_cm, "Yes")
nb_metrics <- compute_metrics(nb_cm, "Yes")

metrics_df <- data.frame(
  Model = c("Decision Tree", "Naive Bayes"),
  Accuracy = c(round(as.numeric(dt_metrics$Accuracy) * 100, 2), round(as.numeric(nb_metrics$Accuracy) * 100, 2)),
  Sensitivity = c(round(dt_metrics$Sensitivity * 100, 2), round(nb_metrics$Sensitivity * 100, 2)),
  Specificity = c(round(dt_metrics$Specificity * 100, 2), round(nb_metrics$Specificity * 100, 2)),
  Precision = c(round(dt_metrics$Precision * 100, 2), round(nb_metrics$Precision * 100, 2)),
  F1 = c(round(dt_metrics$F1 * 100, 2), round(nb_metrics$F1 * 100, 2)),
  stringsAsFactors = FALSE
)


# BUILD PREDICTION SUMMARY TABLE

build_prediction_summary <- function(df, dt_mod, nb_mod) {
  
  df$dt_pred <- predict(dt_mod, newdata = df, type = "class")
  df$nb_pred <- predict(nb_mod, newdata = df)
  
  df$models_agree <- df$dt_pred == df$nb_pred
  
  summary_tbl <- df %>%
    group_by(price_category) %>%
    summarise(
      Total_Phones       = n(),
      DT_Recommended     = sum(dt_pred == "Yes"),
      NB_Recommended     = sum(nb_pred == "Yes"),
      DT_Rec_Pct         = round(mean(dt_pred == "Yes") * 100, 1),
      NB_Rec_Pct         = round(mean(nb_pred == "Yes") * 100, 1),
      Both_Agree_Yes     = sum(dt_pred == "Yes" & nb_pred == "Yes"),
      Avg_Rating         = round(mean(rating,              na.rm = TRUE), 2),
      Avg_Discount_Pct   = round(mean(discount_percentage, na.rm = TRUE), 1),
      Avg_Storage_GB     = round(mean(storage / 1024,      na.rm = TRUE), 1),
      Avg_Selling_Price  = round(mean(selling_price,        na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    arrange(price_category)
  
  summary_tbl
}

prediction_summary <- build_prediction_summary(data, dt_model, nb_model)


# HELPER: classify a single new phone

classify_phone <- function(rating_val, discount_val, storage_mb,
                           selling_price_val, memory_mb) {
  
  safe_norm <- function(val, col) {
    mn <- min(data[[col]], na.rm = TRUE)
    mx <- max(data[[col]], na.rm = TRUE)
    if (mx == mn) return(0)
    (val - mn) / (mx - mn)
  }
  
  new_obs <- data.frame(
    norm_selling_price = safe_norm(selling_price_val, "selling_price"),
    norm_memory        = safe_norm(memory_mb,         "memory"),
    norm_storage       = safe_norm(storage_mb,        "storage"),
    norm_discount      = safe_norm(discount_val,      "discount_percentage"),
    rating             = rating_val
  )
  
  dt_result <- predict(dt_model, newdata = new_obs, type = "class")
  nb_result <- predict(nb_model, newdata = new_obs)
  
  c1  <- rating_val   > RATING_THRESH
  c2  <- discount_val > DISCOUNT_THRESH
  c3  <- storage_mb  >= STORAGE_THRESH
  met <- sum(c(c1, c2, c3))
  
  list(
    dt          = as.character(dt_result),
    nb          = as.character(nb_result),
    conds_met   = met,
    rule_result = ifelse(met >= 2, "Yes", "No"),
    c_rating    = c1,
    c_discount  = c2,
    c_storage   = c3
  )
}


# HELPER: Get matching phones from dataset

get_matching_phones <- function(rating_val, discount_val, storage_mb, 
                                selling_price_val, memory_mb) {
  
  # Start with full dataset
  matching <- data
  
  # Apply filters based on user input
  if (!is.null(rating_val) && rating_val > 0) {
    matching <- matching %>% filter(rating >= rating_val)
  }
  
  if (!is.null(discount_val) && discount_val > 0) {
    matching <- matching %>% filter(discount_percentage >= discount_val)
  }
  
  if (!is.null(storage_mb) && storage_mb > 0) {
    matching <- matching %>% filter(storage >= storage_mb)
  }
  
  if (!is.null(memory_mb) && memory_mb > 0) {
    matching <- matching %>% filter(memory >= memory_mb)
  }
  
  if (!is.null(selling_price_val) && selling_price_val > 0) {
    matching <- matching %>% filter(selling_price <= selling_price_val)
  }
  
  # Apply the recommendation rules to show status
  cond_rating   <- matching$rating > RATING_THRESH
  cond_discount <- matching$discount_percentage > DISCOUNT_THRESH
  cond_storage  <- matching$storage >= STORAGE_THRESH
  conditions_met <- as.integer(cond_rating) + 
    as.integer(cond_discount) + 
    as.integer(cond_storage)
  
  matching$Recommend <- ifelse(conditions_met >= 2, "Yes", "No")
  
  # Add model predictions
  matching$DT_Prediction <- predict(dt_model, newdata = matching, type = "class")
  matching$NB_Prediction <- predict(nb_model, newdata = matching)
  
  # Add criteria met breakdown
  matching$Criteria_Met <- case_when(
    conditions_met == 3 ~ "All 3",
    conditions_met == 2 ~ "2 of 3",
    conditions_met < 2  ~ "Less than 2"
  )
  
  return(matching)
}


# UI

ui <- dashboardPage(
  
  dashboardHeader(title = "Smartphone Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard",      icon = icon("dashboard")),
      menuItem("OS Standings",       tabName = "standings",      icon = icon("mobile-alt")),
      menuItem("Predictions",        tabName = "predictions",    icon = icon("chart-line")),
      menuItem("Interpretation",     tabName = "interpretation", icon = icon("lightbulb")),
      menuItem("About",              tabName = "about",          icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # DASHBOARD OVERVIEW
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("totalPhones"),
          valueBoxOutput("avgRating"),
          valueBoxOutput("avgPrice"),
          valueBoxOutput("avgDiscount")
        ),
        fluidRow(
          box(title = "Price vs Rating",             width = 6, plotlyOutput("priceRating")),
          box(title = "Top Brands by Rating",        width = 6, plotlyOutput("brandRating"))
        ),
        fluidRow(
          box(title = "Android vs iOS Distribution", width = 6, plotlyOutput("osChart")),
          box(title = "Price Category Distribution", width = 6, plotlyOutput("priceCategory"))
        )
      ),
      
      # OS STANDINGS
      tabItem(
        tabName = "standings",
        fluidRow(
          box(title = "Android Smartphones", width = 6, DTOutput("androidTable")),
          box(title = "iOS Smartphones",     width = 6, DTOutput("iosTable"))
        ),
        fluidRow(
          box(title = "Top Android Models", width = 6, DTOutput("androidModels")),
          box(title = "Top iOS Models",     width = 6, DTOutput("iosModels"))
        )
      ),
      
      # PREDICTIONS TAB
      tabItem(
        tabName = "predictions",
        fluidRow(
          box(
            title       = "Classify a New Smartphone",
            width       = 4,
            status      = "success",
            solidHeader = TRUE,
            sliderInput("predRating",   "Rating (0–5)",        min = 1,    max = 5,   value = 3.8, step = 0.1),
            sliderInput("predDiscount", "Discount (%)",        min = 0,    max = 80,  value = 8,   step = 0.5),
            numericInput("predStorage", "Storage (MB)",        value = 65536),
            numericInput("predMemory",  "Memory / RAM (MB)",   value = 4096),
            numericInput("predPrice",   "Selling Price (Rs)",  value = 15000),
            br(),
            actionButton("classifyBtn", "Classify Phone", class = "btn-primary btn-block"),
            br(),
            br(),
            actionButton("searchPhonesBtn", "Show Matching Phones", 
                         class = "btn-success btn-block")
          ),
          box(
            title       = "Classification Result",
            width       = 8,
            status      = "success",
            solidHeader = TRUE,
            br(),
            uiOutput("ruleCheckUI"),
            br(),
            h4(textOutput("dtPrediction")),
            h4(textOutput("nbPrediction")),
            br(),
            HTML(paste0("
              <small><b>Recommendation Rules (must meet ≥ 2 of 3):</b><br/>
              &bull; Rating &gt; ", RATING_THRESH, " &nbsp;|&nbsp;
              &bull; Discount &gt; ", DISCOUNT_THRESH, "% &nbsp;|&nbsp;
              &bull; Storage &ge; ", STORAGE_THRESH/1024, " GB<br/>
              The Decision Tree uses a 4:1 loss matrix — missing a true
              <i>Yes</i> is penalised 4&times; more than a false alarm.</small>
            "))
          )
        ),
        
        fluidRow(
          box(
            title       = "Matching Smartphones from Dataset",
            width       = 12,
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed   = FALSE,
            
            fluidRow(
              column(12,
                     h4(textOutput("matchCount"))
              )
            ),
            
            br(),
            
            fluidRow(
              column(12,
                     DTOutput("matchingPhonesTable")
              )
            )
          )
        )
      ),
      
      # INTERPRETATION
      tabItem(
        tabName = "interpretation",
        fluidRow(
          box(title = "Market Interpretation", width = 12, htmlOutput("marketInsights"))
        )
      ),
      
      # ABOUT
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "Project Information", width = 12,
            HTML(paste0("
              <h3>Smartphone Market Analytics Dashboard</h3>
              <p><b>Course:</b> Data Analytics</p>
              <p><b>Dataset:</b> Smartphone Sales Dataset</p>
              <p><b>Currency Used:</b> Indian Rupee (Rs)</p>
              <p>This dashboard analyzes smartphone brands, operating systems, ratings,
              prices, discounts, and market trends.</p>
              <p>The Predictions tab includes a <b>Decision Tree</b> (with 4:1 class-weight
              loss matrix) and a <b>Naive Bayes</b> classifier. Both models are applied to
              the full dataset and summarised by price category so you can see at a glance
              which tier is most recommended.</p>
              <p><b>Recommendation Criteria:</b> Phones are labelled <i>Recommend = Yes</i> 
              when they meet at least 2 of 3 conditions:<br/>
              &bull; Rating &gt; ", RATING_THRESH, "<br/>
              &bull; Discount &gt; ", DISCOUNT_THRESH, "%<br/>
              &bull; Storage &ge; ", STORAGE_THRESH/1024, " GB</p>
              <p>All numeric features are Min-Max normalised before training.</p>
            "))
          )
        )
      )
    )
  )
)


# SERVER

server <- function(input, output) {
  
  filtered <- reactive({ data })
  
  # VALUE BOXES — DASHBOARD
  output$totalPhones <- renderValueBox({
    valueBox(nrow(filtered()), "Total Smartphones",
             icon = icon("mobile-alt"), color = "aqua")
  })
  output$avgRating <- renderValueBox({
    valueBox(round(mean(filtered()$rating, na.rm = TRUE), 2),
             "Average Rating", icon = icon("star"), color = "green")
  })
  output$avgPrice <- renderValueBox({
    valueBox(
      paste0("Rs ", format(round(mean(filtered()$selling_price, na.rm = TRUE), 0), big.mark = ",")),
      "Average Price", icon = icon("money-bill"), color = "yellow"
    )
  })
  output$avgDiscount <- renderValueBox({
    valueBox(
      paste0(round(mean(filtered()$discount_percentage, na.rm = TRUE), 2), "%"),
      "Average Discount", icon = icon("tags"), color = "red"
    )
  })
  
  # DASHBOARD CHARTS
  output$priceRating <- renderPlotly({
    p <- ggplot(filtered(), aes(x = selling_price, y = rating, color = os, text = mobile)) +
      geom_point(size = 3) +
      labs(x = "Selling Price (Rs)", y = "Rating")
    ggplotly(p)
  })
  output$brandRating <- renderPlotly({
    df <- filtered() %>%
      group_by(brands) %>%
      summarise(avg_rating = mean(rating, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_rating)) %>% head(10)
    p <- ggplot(df, aes(reorder(brands, avg_rating), avg_rating)) +
      geom_col() + coord_flip() +
      labs(x = "Brand", y = "Average Rating")
    ggplotly(p)
  })
  output$osChart <- renderPlotly({
    df <- filtered() %>% count(os)
    plot_ly(df, labels = ~os, values = ~n, type = "pie")
  })
  output$priceCategory <- renderPlotly({
    df <- filtered() %>% count(price_category)
    plot_ly(df, labels = ~price_category, values = ~n, type = "pie")
  })
  
  # OS STANDINGS TABLES
  output$androidTable <- renderDT({
    datatable(
      data %>% filter(os == "Android") %>%
        select(Brand = brands, Model = models, Rating = rating, Price = selling_price),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  output$iosTable <- renderDT({
    datatable(
      data %>% filter(os == "iOS") %>%
        select(Brand = brands, Model = models, Rating = rating, Price = selling_price),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  output$androidModels <- renderDT({
    datatable(
      data %>% filter(os == "Android") %>% arrange(desc(rating)) %>%
        select(Brand = brands, Model = models, Rating = rating, Price = selling_price) %>% head(10),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  output$iosModels <- renderDT({
    datatable(
      data %>% filter(os == "iOS") %>% arrange(desc(rating)) %>%
        select(Brand = brands, Model = models, Rating = rating, Price = selling_price) %>% head(10),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # PREDICTION TAB — VALUE BOXES
  output$dtAccuracyBox <- renderValueBox({
    valueBox(
      paste0(dt_accuracy, "%"),
      "Decision Tree Accuracy",
      icon  = icon("tree"),
      color = "blue"
    )
  })
  output$nbAccuracyBox <- renderValueBox({
    valueBox(
      paste0(nb_accuracy, "%"),
      "Naive Bayes Accuracy",
      icon  = icon("calculator"),
      color = "purple"
    )
  })
  output$recYesPctBox <- renderValueBox({
    yes_pct <- round(mean(data$Recommend == "Yes") * 100, 1)
    valueBox(
      paste0(yes_pct, "%"),
      "Phones Recommended (Yes)",
      icon  = icon("thumbs-up"),
      color = "green"
    )
  })
  
  # MODEL METRICS TABLE
  output$modelMetricsTable <- renderDT({
    datatable(
      metrics_df,
      options = list(dom = 't', pageLength = 5),
      rownames = FALSE
    )
  })
  
  # PREDICTION SUMMARY TABLE
  output$predSummaryTable <- renderDT({
    datatable(
      prediction_summary,
      options  = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
      rownames = FALSE,
      colnames = c(
        "Price Category", "Total Phones",
        "DT: Rec. Yes", "NB: Rec. Yes",
        "DT Rec. %", "NB Rec. %",
        "Both Agree Yes",
        "Avg Rating", "Avg Discount %",
        "Avg Storage (GB)", "Avg Selling Price (Rs)"
      )
    ) %>%
      formatStyle("DT_Rec_Pct",
                  background = styleColorBar(c(0, 100), "#AED6F1"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("NB_Rec_Pct",
                  background = styleColorBar(c(0, 100), "#A9DFBF"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # RECOMMENDATION RATE BAR CHART
  output$recRateChart <- renderPlotly({
    df_long <- prediction_summary %>%
      select(price_category, DT_Rec_Pct, NB_Rec_Pct) %>%
      pivot_longer(cols = c(DT_Rec_Pct, NB_Rec_Pct),
                   names_to  = "Model",
                   values_to = "Rec_Pct") %>%
      mutate(Model = recode(Model,
                            "DT_Rec_Pct" = "Decision Tree",
                            "NB_Rec_Pct" = "Naive Bayes"))
    
    plot_ly(df_long,
            x      = ~price_category,
            y      = ~Rec_Pct,
            color  = ~Model,
            colors = c("Decision Tree" = "#2A9D8F", "Naive Bayes" = "#E9C46A"),
            type   = "bar") %>%
      layout(
        barmode = "group",
        xaxis   = list(title = "Price Category"),
        yaxis   = list(title = "% Recommended", range = c(0, 100)),
        legend  = list(title = list(text = "Model"))
      )
  })
  
  # FEATURE IMPORTANCE
  output$featureImportance <- renderPlotly({
    imp <- as.data.frame(dt_model$variable.importance)
    imp$Feature <- rownames(imp)
    colnames(imp)[1] <- "Importance"
    imp <- imp %>% arrange(desc(Importance))
    
    plot_ly(imp,
            x      = ~reorder(Feature, Importance),
            y      = ~Importance,
            type   = "bar",
            marker = list(color = "#2A9D8F")) %>%
      layout(
        xaxis  = list(title = "Feature"),
        yaxis  = list(title = "Importance"),
        margin = list(b = 80)
      )
  })
  
  # CONFUSION MATRICES
  plot_cm <- function(cm_table, title_text) {
    cm_df <- as.data.frame(cm_table)
    colnames(cm_df) <- c("Predicted", "Actual", "Freq")
    ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Freq), size = 6, fontface = "bold") +
      scale_fill_gradient(low = "#D6EAF8", high = "#1A5276") +
      labs(title = title_text, x = "Predicted", y = "Actual") +
      theme_minimal(base_size = 13)
  }
  output$dt_conf_matrix <- renderPlot({ plot_cm(dt_cm$table, "Decision Tree – Confusion Matrix") })
  output$nb_conf_matrix <- renderPlot({ plot_cm(nb_cm$table, "Naive Bayes – Confusion Matrix")   })
  
  # DECISION TREE PLOT
  output$dt_plot <- renderPlot({
    rpart.plot(
      dt_model,
      type          = 4,
      extra         = 104,
      box.palette   = c("#E74C3C", "#2ECC71"),
      fallen.leaves = TRUE,
      main          = "Decision Tree: Should this phone be Recommended?"
    )
  })
  
  # INTERACTIVE CLASSIFIER - Single phone
  result <- eventReactive(input$classifyBtn, {
    classify_phone(
      rating_val        = input$predRating,
      discount_val      = input$predDiscount,
      storage_mb        = input$predStorage,
      selling_price_val = input$predPrice,
      memory_mb         = input$predMemory
    )
  })
  
  output$ruleCheckUI <- renderUI({
    req(input$classifyBtn)
    r <- result()
    
    make_row <- function(label, passed) {
      color <- if (passed) "#2ECC71" else "#E74C3C"
      mark  <- if (passed) "✔ MET" else "✘ NOT MET"
      tags$tr(
        tags$td(style = "padding:6px 10px;", label),
        tags$td(style = paste0("padding:6px 10px;color:", color, ";font-weight:bold;"), mark)
      )
    }
    
    overall_color <- if (r$rule_result == "Yes") "#2ECC71" else "#E74C3C"
    
    tagList(
      tags$table(
        style = "border-collapse:collapse;width:100%;font-size:14px;",
        tags$thead(tags$tr(
          tags$th(style = "padding:6px 10px;background:#2C3E50;color:white;border-radius:4px 0 0 0;", "Condition"),
          tags$th(style = "padding:6px 10px;background:#2C3E50;color:white;border-radius:0 4px 0 0;", "Status")
        )),
        tags$tbody(
          make_row(paste0("Rating > ", RATING_THRESH),          r$c_rating),
          make_row(paste0("Discount > ", DISCOUNT_THRESH, "%"), r$c_discount),
          make_row(paste0("Storage ≥ ", STORAGE_THRESH/1024, " GB"), r$c_storage)
        )
      ),
      br(),
      tags$p(
        style = paste0("font-weight:bold;color:", overall_color,
                       ";font-size:16px;padding:8px;background:#F2F3F4;border-radius:4px;"),
        paste0("Rule check (≥ 2/3 met): ", r$rule_result,
               "  (", r$conds_met, " of 3 conditions met)")
      )
    )
  })
  
  output$dtPrediction <- renderText({
    req(result())
    paste0("Decision Tree  →  Recommend: ", result()$dt)
  })
  output$nbPrediction <- renderText({
    req(result())
    paste0("Naive Bayes    →  Recommend: ", result()$nb)
  })
  
  # INTERACTIVE - Matching phones from dataset
  
  # REACTIVE: Get matching phones (defined BEFORE outputs that use it)
  matching_results <- eventReactive(input$searchPhonesBtn, {
    req(input$searchPhonesBtn)  # Ensure button was clicked
    
    get_matching_phones(
      rating_val        = input$predRating,
      discount_val      = input$predDiscount,
      storage_mb        = input$predStorage,
      selling_price_val = input$predPrice,
      memory_mb         = input$predMemory
    )
  })
  
  # OUTPUT: Match count
  output$matchCount <- renderText({
    req(matching_results())
    df <- matching_results()
    count <- nrow(df)
    count_rec <- sum(df$Recommend == "Yes")
    count_dt <- sum(df$DT_Prediction == "Yes")
    count_nb <- sum(df$NB_Prediction == "Yes")
    
    paste0("Found ", count, " phones matching your criteria. ",
           count_rec, " (", round(count_rec/count*100, 1), "%) meet the recommendation rules. ",
           "Decision Tree recommends ", count_dt, " (", round(count_dt/count*100, 1), "%), ",
           "Naive Bayes recommends ", count_nb, " (", round(count_nb/count*100, 1), "%).")
  })
  
  # OUTPUT: Matching phones table
  output$matchingPhonesTable <- renderDT({
    req(matching_results())
    
    df <- matching_results() %>%
      select(
        Brand = brands,
        Model = models,
        OS = os,
        Price = selling_price,
        Discount = discount_percentage,
        Rating = rating,
        Storage_GB = storage,
        Memory_GB = memory,
        `Criteria Met` = Criteria_Met,
        Recommended = Recommend,
        `DT Predict` = DT_Prediction,
        `NB Predict` = NB_Prediction
      ) %>%
      mutate(
        Price = paste0("Rs ", format(Price, big.mark = ",")),
        Storage_GB = round(Storage_GB / 1024, 1),
        Memory_GB = round(Memory_GB / 1024, 1),
        Discount = paste0(Discount, "%")
      ) %>%
      arrange(desc(Recommended), desc(Rating))
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, 'desc')),
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      filter = 'top',
      style = 'bootstrap4'
    ) %>%
      formatStyle(
        'Recommended',
        backgroundColor = styleEqual(
          c("Yes", "No"),
          c("#A8E6CF", "#FF8B94")
        )
      ) %>%
      formatStyle(
        'Criteria Met',
        backgroundColor = styleEqual(
          c("All 3", "2 of 3", "Less than 2"),
          c("#81C784", "#FFD54F", "#E57373")
        )
      ) %>%
      formatStyle(
        'DT Predict',
        backgroundColor = styleEqual(
          c("Yes", "No"),
          c("#A8E6CF", "#FF8B94")
        )
      ) %>%
      formatStyle(
        'NB Predict',
        backgroundColor = styleEqual(
          c("Yes", "No"),
          c("#A8E6CF", "#FF8B94")
        )
      )
  })
  
  # INTERPRETATION
  output$marketInsights <- renderUI({
    android_count <- sum(data$os == "Android")
    ios_count     <- sum(data$os == "iOS")
    yes_count     <- sum(data$Recommend == "Yes")
    no_count      <- sum(data$Recommend == "No")
    yes_pct       <- round(yes_count / nrow(data) * 100, 1)
    avg_price     <- round(mean(data$selling_price, na.rm = TRUE), 0)
    avg_rating    <- round(mean(data$rating,        na.rm = TRUE), 2)
    
    HTML(paste0("
      <h3>Smartphone Market Analysis</h3>
      <ul>
        <li><b>Total Smartphones Analyzed:</b> ", nrow(data), "</li>
        <li><b>Average Selling Price:</b> Rs ", format(avg_price, big.mark = ","), "</li>
        <li><b>Average Rating:</b> ", avg_rating, "</li>
        <li><b>Android Devices:</b> ", android_count, "</li>
        <li><b>iOS Devices:</b> ", ios_count, "</li>
        <li><b>Recommended Phones (Yes):</b> ", yes_count,
                " (", yes_pct, "% of dataset)</li>
        <li><b>Not Recommended (No):</b> ", no_count, "</li>
        <li>Android smartphones dominate the dataset, indicating a larger
            variety of Android devices in the market.</li>
        <li>Devices with higher memory and storage generally command higher
            selling prices.</li>
        <li>Mid-range smartphones represent a significant portion of the
            available products.</li>
        <li>The Decision Tree uses a 4:1 loss matrix — it is penalised more
            for missing a recommended phone than for a false alarm, producing
            a more balanced tree with meaningful Yes branches.</li>
        <li>Recommendation thresholds: Rating &gt; ", RATING_THRESH, ",
            Discount &gt; ", DISCOUNT_THRESH, "%, Storage &ge; ", STORAGE_THRESH/1024, " GB</li>
      </ul>
    "))
  })
}

# RUN APP
shinyApp(ui, server)