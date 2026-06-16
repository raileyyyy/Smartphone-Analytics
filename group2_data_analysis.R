library(tidyverse)

# Load preprocessed data
final_dataset <- read.csv("group2_data_preprocess.csv",
                          stringsAsFactors = FALSE)

# Re-apply factor ordering
final_dataset$price_category <- factor(
  final_dataset$price_category,
  levels = c("Budget", "Mid-Range", "Flagship")
)
final_dataset$os <- factor(final_dataset$os,
                           levels = c("Android", "iOS"))


# FIGURE 1 – Price Category vs. Average Rating  (Table 1)
fig1_data <- final_dataset %>%
  group_by(price_category) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE),
            n = n(), .groups = "drop")

cor_fig1 <- cor(as.numeric(fig1_data$price_category),
                fig1_data$avg_rating)

bp1 <- barplot(
  height    = fig1_data$avg_rating,
  names.arg = as.character(fig1_data$price_category),
  main      = paste0("Figure 1. Price Category vs. Average Rating"),
  xlab      = "Price Category",
  ylab      = "Average Rating",
  ylim      = c(0, 5.5),
  col       = c("#F4A261", "#2A9D8F", "#264653"),
  border    = "white"
)
text(
  x      = bp1,
  y      = fig1_data$avg_rating + 0.15,
  labels = paste0(round(fig1_data$avg_rating, 2),
                  "\n(n = ", fig1_data$n, ")"),
  cex    = 0.9
)

# FIGURE 2 – Apple vs Top Android Brand by Average Rating  (Table 2)
fig2_data <- final_dataset %>%
  filter(brands %in% c("APPLE", "SAMSUNG")) %>%
  group_by(brands) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  arrange(brands)   # APPLE=1, SAMSUNG=2

cor_fig2 <- cor(seq_len(nrow(fig2_data)), fig2_data$avg_rating)

bp2 <- barplot(
  height    = fig2_data$avg_rating,
  names.arg = paste0(str_to_title(fig2_data$brands),
                     "\n(n = ", fig2_data$n, ")"),
  main      = paste0("Figure 2. Apple vs. Top Android Brand by Average Rating"),
  xlab      = "Brand",
  ylab      = "Average Rating",
  ylim      = c(0, 5.5),
  col       = c("#555555", "#1428A0"),
  border    = "white"
)
text(
  x      = bp2,
  y      = fig2_data$avg_rating + 0.15,
  labels = round(fig2_data$avg_rating, 2),
  cex    = 1.0,
  font   = 2
)

# FIGURE 3 – Operating System vs. Price Category  (Table 3)
os_num    <- ifelse(final_dataset$os == "iOS", 1, 0)
price_num <- as.numeric(final_dataset$price_category)
cor_fig3  <- cor(os_num, price_num, use = "complete.obs")

fig3_matrix <- final_dataset %>%
  count(os, price_category) %>%
  pivot_wider(names_from  = price_category,
              values_from = n,
              values_fill = 0) %>%
  column_to_rownames("os") %>%
  as.matrix()
fig3_matrix <- fig3_matrix[, c("Budget", "Mid-Range", "Flagship")]

bp3 <- barplot(
  fig3_matrix,
  beside      = TRUE,
  main        = paste0("Figure 3. Operating System vs. Price Category"),
  xlab        = "Price Category",
  ylab        = "Number of Devices",
  col         = c("#3DDC84", "#555555"),
  border      = "white",
  legend.text = rownames(fig3_matrix),
  args.legend = list(title = "OS", bty = "n", x = "topright"),
  ylim        = c(0, max(fig3_matrix) * 1.2)
)
text(
  x      = bp3,
  y      = fig3_matrix + 25,
  labels = fig3_matrix,
  cex    = 0.85,
  font   = 2
)

# Reset layout
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))