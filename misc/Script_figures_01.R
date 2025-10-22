fig <- d04 |> 
  ggplot(aes(x = factor(ae_hyperTAG_grade_3_4), 
             y = bmi, 
             color = factor(ae_hyperTAG_grade_3_4))) +
  geom_beeswarm(cex = 3.5, size = 3.5) +
  scale_color_brewer(
    palette = "Set1", 
    name = "Grade 3–4 Hypertriglyceridemia"
  ) +
  stat_summary(fun.data = ggpubr::median_mad, 
               geom = "errorbar", 
               color = "black", 
               width = 0.2, 
               size = 1) +
  stat_summary(fun = median, 
               geom = "crossbar", 
               color = "black", 
               size = 0.8, 
               width = 0.3) +
  labs(
    y = "Body mass index (kg/m²)",
    x = "Grade 3-4 hypertriglyceridemia",
    caption = "The error bars represent Median Absolute Deviations (MAD) intervals with the median."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  scale_x_discrete(
    breaks = c(0, 1),
    labels = c("NO", "YES")
  )

tiff("output/figures/250910_hTAG_bmi_01.tiff", compression = "lzw", res = 300,
     height = 1600, width = 1600)
fig
dev.off()



library(dplyr)
library(ggplot2)
library(ggbeeswarm)

# Prepare thresholds
d_fisher_plot <- d04 %>%
  mutate(
    Overweight = if_else(bmi >= 25, "YES", "NO"),
    Obesity    = if_else(bmi >= 30, "YES", "NO")
  ) %>%
  tidyr::pivot_longer(
    cols = c(Overweight, Obesity),
    names_to = "Threshold",
    values_to = "Category"
  ) %>%
  mutate(
    Threshold = factor(Threshold, levels = c("Overweight", "Obesity")),
    Category  = factor(Category, levels = c("NO", "YES")),
    HyperTG   = factor(ae_hyperTAG_grade_3_4, levels = c(0,1), labels = c("NO", "YES"))
  )

# Plot with dodge (HyperTG determines left/right within each column)
ggplot(d_fisher_plot, aes(x = Category, y = bmi,
                          color = HyperTG)) +
  geom_beeswarm(
    dodge.width = 0.6,   # separation of NO/YES inside columns
    cex = 3, size = 3.5
  ) +
  facet_wrap(~Threshold, scales = "free_x") +
  scale_color_brewer(
    palette = "Set1",
    name = "Grade 3–4 Hypertriglyceridemia"
  ) +
  labs(
    x = NULL,
    y = expression("Body mass index (kg/m"^2*")"),
    title = "BMI thresholds vs. Grade 3–4 Hypertriglyceridemia"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y   = element_text(face = "bold", size = 18),
    axis.text      = element_text(size = 14, face = "bold"),
    strip.text     = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

####################################
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)   # for median_mad

# Prepare thresholds
d_fisher_plot <- d04 %>%
  mutate(
    Overweight = if_else(bmi >= 25, "YES", "NO"),
    Obesity    = if_else(bmi >= 30, "YES", "NO")
  ) %>%
  tidyr::pivot_longer(
    cols = c(Overweight, Obesity),
    names_to = "Threshold",
    values_to = "Category"
  ) %>%
  mutate(
    Threshold = factor(Threshold, levels = c("Overweight", "Obesity")),
    Category  = factor(Category, levels = c("NO", "YES")),
    HyperTG   = factor(ae_hyperTAG_grade_3_4, levels = c(0,1), labels = c("NO", "YES"))
  )

# Define dodge width for consistency
pd <- position_dodge(width = 0.6)

# Plot with beeswarm + median + MAD
ggplot(d_fisher_plot, aes(x = Category, y = bmi, color = HyperTG)) +
  geom_beeswarm(dodge.width = 0.6, cex = 3, size = 3.5) +
  
  # MAD error bars
  stat_summary(
    aes(group = HyperTG),
    fun.data = ggpubr::median_mad,
    geom = "errorbar",
    position = pd,
    color = "black",
    width = 0.25,
    size = 1
  ) +
  # Median point
  stat_summary(
    aes(group = HyperTG),
    fun = median,
    geom = "point",
    position = pd,
    color = "black",
    size = 4,
    shape = 18
  ) +
  
  facet_wrap(~Threshold, scales = "free_x") +
  scale_color_brewer(
    palette = "Set1",
    name = "Grade 3–4 Hypertriglyceridemia"
  ) +
  labs(
    x = NULL,
    y = expression("Body mass index (kg/m"^2*")"),
    title = "BMI thresholds vs. Grade 3–4 Hypertriglyceridemia",
    caption = "Error bars = Median Absolute Deviation (MAD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.y   = element_text(face = "bold", size = 18),
    axis.text      = element_text(size = 14, face = "bold"),
    strip.text     = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

########################
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)   # for median_mad

# Prepare data with both BMI thresholds
d_fisher_plot <- d04 %>%
  mutate(
    Overweight = if_else(bmi >= 25, "YES", "NO"),
    Obesity    = if_else(bmi >= 30, "YES", "NO"),
    HyperTG    = factor(ae_hyperTAG_grade_3_4, levels = c(0, 1), labels = c("NO", "YES"))
  ) %>%
  pivot_longer(
    cols = c(Overweight, Obesity),
    names_to = "Threshold",
    values_to = "Category"     # BMI at threshold: NO/YES
  ) %>%
  mutate(
    Threshold = factor(Threshold, levels = c("Overweight", "Obesity")),
    Category  = factor(Category,  levels = c("NO", "YES")) # dodge/color by this
  )

# Consistent dodge for dots and summaries
pd <- position_dodge(width = 0.6)

ggplot(d_fisher_plot, aes(x = HyperTG, y = bmi, color = Category)) +
  # Beeswarm dots, dodged by BMI category
  geom_beeswarm(dodge.width = 0.6, cex = 3, size = 3.5, alpha = 0.5) +
  
  # MAD error bars per BMI category within each HyperTG column
  stat_summary(
    aes(group = Category),
    fun.data = ggpubr::median_mad,
    geom = "errorbar",
    position = pd,
    color = "black",
    width = 0.25,
    size = 1
  ) +
  # Median points (black diamonds)
  stat_summary(
    aes(group = Category),
    fun = median,
    geom = "point",
    position = pd,
    color = "black",
    size = 4,
    shape = 18
  ) +
  
  facet_wrap(~ Threshold, scales = "free_x") +
  scale_color_brewer(
    palette = "Set1",
    name = "BMI at threshold",
    labels = c("Below threshold (NO)", "Above threshold (YES)")
  ) +
  labs(
    x = "Grade 3–4 Hypertriglyceridemia",
    y = "Body mass index (kg/m²)",
    title = "BMI categorization vs. Grade 3–4 Hypertriglyceridemia",
    caption = "Dots: individuals; Diamonds: medians; Error bars: MAD."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x   = element_text(face = "bold", size = 18),
    axis.title.y   = element_text(size = 18),  # already bold via bquote()
    axis.text      = element_text(size = 14, face = "bold"),
    strip.text     = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

##############
library(dplyr)
library(ggplot2)

# Summarize to counts and proportions
d_counts <- d04 %>%
  mutate(
    Overweight = if_else(bmi >= 25, "YES", "NO"),
    Obesity    = if_else(bmi >= 30, "YES", "NO"),
    HyperTG    = factor(ae_hyperTAG_grade_3_4, levels = c(0, 1), labels = c("NO", "YES"))
  ) %>%
  tidyr::pivot_longer(
    cols = c(Overweight, Obesity),
    names_to = "Threshold",
    values_to = "Category"
  ) %>%
  group_by(Threshold, Category, HyperTG) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Threshold, Category) %>%
  mutate(prop = n / sum(n))

# Plot
fig2 <- ggplot(d_counts, aes(x = Category, y = prop, fill = HyperTG)) +
  geom_col(position = "fill", color = "black") +
  facet_wrap(~Threshold) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(
    palette = "Set1",
    name = "Grade 3–4 Hypertriglyceridemia"
  ) +
  labs(
    x = "BMI at threshold",
    y = "Proportion of patients",
    title = "Proportion of Hypertriglyceridemia\nacross BMI categories"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title.x   = element_blank(),
    axis.title.y   = element_text(face = "bold", size = 18),
    axis.text      = element_text(size = 14, face = "bold"),
    strip.text     = element_text(face = "bold", size = 16),
    legend.position = "top",
    plot.title     = element_text(face = "bold", size = 20, hjust = 0.5)
  )

tiff("output/figures/250910_hTAG_bmi_02.tiff", compression = "lzw", res = 300,
     height = 1600, width = 1800)
fig2
dev.off()
