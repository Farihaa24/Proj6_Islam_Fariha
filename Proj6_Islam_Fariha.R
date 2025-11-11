# ========================= Project 6 =========================
# Name: Fariha Islam
# File: Proj6_Islam_Fariha.R

# ---- Libraries ----
library(tidyverse)
library(janitor)

# ---- Problem 1.5 Standing Height Dataset ----
heights <- tribble(
  ~y, ~x1, ~x2, ~x3, ~x4, ~x5, ~x6, ~x7,
  165.8,88.7,31.8,28.1,18.7,40.3,38.9,6.7,
  169.8,90.0,32.4,29.1,18.3,43.3,42.7,6.4,
  170.7,87.7,33.6,29.5,20.7,43.7,41.1,7.2,
  170.9,87.1,31.0,28.2,18.6,43.7,40.6,6.7,
  157.5,81.3,32.1,27.3,17.5,38.1,39.6,6.6,
  165.9,88.2,31.8,29.0,18.6,42.0,40.6,6.5,
  158.7,86.1,30.6,27.8,18.4,40.0,37.0,5.9,
  166.0,88.7,30.2,26.9,17.5,41.6,39.0,5.9,
  158.7,83.7,31.1,27.1,18.1,38.9,37.5,6.1,
  161.5,81.2,32.3,27.8,19.1,42.8,40.1,6.2,
  167.3,88.6,34.8,27.3,18.3,43.1,41.8,7.3,
  167.4,83.2,34.3,30.1,19.2,43.4,42.2,6.8,
  159.2,81.5,31.0,27.3,17.5,39.8,39.6,4.9,
  170.0,87.9,34.2,30.9,19.4,43.1,43.7,6.3,
  166.3,88.3,30.6,28.8,18.3,41.8,41.0,5.9
)

# ---- Scatterplot: Height vs Lower Leg ----
p1 <- ggplot(heights, aes(x = x6, y = y)) +
  geom_point() +
  labs(title="Standing Height vs Lower Leg",
       x="Lower Leg (cm)", y="Standing Height (cm)")

# ---- Scatterplot + Regression Line ----
p2 <- ggplot(heights, aes(x = x6, y = y)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Standing Height vs Lower Leg with Regression Line",
       x="Lower Leg (cm)", y="Standing Height (cm)")

# Comment:
# Standing height increases as lower leg length increases, showing a clear positive association.

# ---- Load Store Sales Data from Downloads ----
sales <- read_csv("~/Downloads/StoreSales.csv") |> clean_names()

# ---- Month Factor ----
month_levels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
sales <- sales |> mutate(month = factor(month, levels = month_levels))

# ---- Scatterplot: Revenue vs Month ----
p3 <- ggplot(sales, aes(x = month, y = revenue, color = store)) +
  geom_point() +
  labs(title="Monthly Revenue by Store",
       x="Month", y="Revenue") +
  theme(legend.position="bottom")

# Comment:
# Revenue varies by month and store. Some stores consistently perform better, showing performance differences.

# ---- Wide Dataset ----
sales_wide <- sales |> 
  pivot_wider(names_from = month, values_from = revenue, values_fn = list)

print(sales_wide)

# ---- Summary Stats ----
summary_stats <- sales |> 
  group_by(store) |> 
  summarise(
    Mean = mean(revenue),
    Median = median(revenue),
    SD = sd(revenue),
    IQR = IQR(revenue),
    Min = min(revenue),
    Q1 = quantile(revenue, .25),
    Q3 = quantile(revenue, .75),
    Max = max(revenue)
  )

print(summary_stats)

# ---- Save All Plots to PDF ----
pdf("Proj6_ALL_PLOTS.pdf", width=8, height=6)
print(p1)
print(p2)
print(p3)
dev.off()
