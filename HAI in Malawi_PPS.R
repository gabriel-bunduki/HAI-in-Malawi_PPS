# HAI IN MALAWI: POINT PREVALENCE SURVEYS ANALYSIS 

# Upload and read the dataset

df <- read_excel("Documents/pps.xlsx")
df <- pps
View(df)

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales) # For better axis formatting


# Add site and ward labels
# Name of different study site and wards are added 
df$site_name <- factor(df$site,
                       levels = c(1, 2, 3),
                       labels = c("QECH", "ZCH", "CDH"))
df$ward_name <- factor(df$ward,
                       levels = c(1, 2),
                       labels = c("Surgical", "Medical"))


# CALCULATE OVERALL PREVALENCE BY SYNDROME


prev_bsi_overall_all <- df %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_bsi = sum(bsi, na.rm = TRUE),
    hcai_bsi = round(sum(bsi) / sum(total) * 100, 1)
  )

print (prev_bsi_overall_all)

prev_uti_overall_all <- df %>%
  summarise(
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    total_uti = sum(uti, na.rm = TRUE),
    hcai_uti = round(sum(uti) / sum(ucath) * 100, 1)
  )
print(prev_uti_overall_all)

prev_ssi_overall_all <- df %>%
  summarise(
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_ssi               = sum(ssi, na.rm = TRUE)
  ) %>%
  mutate(
    hcai_ssi = round(
      total_ssi / (total_patients_surgward + total_ssi_medward) * 100, 1
    )
  )
print(prev_ssi_overall_all)


# Combine into a single tidy table for plotting
overall_prev <- tibble(
  metric = c("BSI", "CAUTI", "SSI"),
  prevalence = c(
    prev_bsi_overall_all$hcai_bsi,
    prev_uti_overall_all$hcai_uti,
    prev_ssi_overall_all$hcai_ssi
  )
)
print("Combined prevalences into a single table")
print(overall_prev)

# Plot: bar chart of overall prevalence by metric
gg <- ggplot(overall_prev, aes(x = metric, y = prevalence, fill = metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(prevalence, "%")),
            vjust = -0.3, size = 4.2) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "",
    x = NULL,
    y = "Prevalence (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.line.x = element_line(),
    panel.grid.minor = element_blank()
  )

print("Rendering bar chart of overall prevalence")
print(gg)

# Script with 95% binomial CIs added for each metric and error bars included in the final figure

prev_bsi_overall_all <- df %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_bsi = sum(bsi, na.rm = TRUE),
    hcai_bsi = round(total_bsi / total_patients * 100, 1),
    ci_low = round(prop.test(total_bsi, total_patients)$conf.int[1] * 100, 1),
    ci_high = round(prop.test(total_bsi, total_patients)$conf.int[2] * 100, 1)
  )

print(prev_bsi_overall_all)

prev_uti_overall_all <- df %>%
  summarise(
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    total_uti = sum(uti, na.rm = TRUE),
    hcai_uti = round(total_uti / total_patients_ucath * 100, 1),
    ci_low = round(prop.test(total_uti, total_patients_ucath)$conf.int[1] * 100, 1),
    ci_high = round(prop.test(total_uti, total_patients_ucath)$conf.int[2] * 100, 1)
  )

print(prev_uti_overall_all)

prev_ssi_overall_all <- df %>%
  summarise(
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_ssi = sum(ssi, na.rm = TRUE)
  ) %>%
  mutate(
    ssi_denominator = total_patients_surgward + total_ssi_medward,
    hcai_ssi = round(total_ssi / ssi_denominator * 100, 1),
    ci_low = round(prop.test(total_ssi, ssi_denominator)$conf.int[1] * 100, 1),
    ci_high = round(prop.test(total_ssi, ssi_denominator)$conf.int[2] * 100, 1)
  )

print(prev_ssi_overall_all)


# Combine into a single tidy table for plotting
overall_prev <- tibble(
  metric = c("BSI", "CAUTI", "SSI"),
  prevalence = c(
    prev_bsi_overall_all$hcai_bsi,
    prev_uti_overall_all$hcai_uti,
    prev_ssi_overall_all$hcai_ssi
  ),
  ci_low = c(
    prev_bsi_overall_all$ci_low,
    prev_uti_overall_all$ci_low,
    prev_ssi_overall_all$ci_low
  ),
  ci_high = c(
    prev_bsi_overall_all$ci_high,
    prev_uti_overall_all$ci_high,
    prev_ssi_overall_all$ci_high
  )
)

print("Combined prevalences into a single table")
print(overall_prev)


# Plot: bar chart of overall prevalence by metric
gg <- ggplot(overall_prev, aes(x = metric, y = prevalence, fill = metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_text(
    aes(y=prevalence/2, label = paste0(prevalence, "%")),
    vjust = -0.6,
    size = 4.2,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "",
    x = NULL,
    y = "Prevalence (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.line.x = element_line(),
    panel.grid.minor = element_blank()
  )

print("Rendering bar chart of overall prevalence")
print(gg)


#Overall HCAI
prev_hcai_overall <- df %>%
  group_by(site_name) %>%
  summarise(
  total_patients = sum(total, na.rm = TRUE),
  total_hcai = sum(hcai, na.rm = TRUE),
  hcai_prev = round(sum(hcai) / sum(total) * 100, 1)
)

print(prev_hcai_overall)

#Overall bsi 
prev_bsi_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_bsi = sum(bsi, na.rm = TRUE),
    hcai_bsi = round(sum(bsi) / sum(total) * 100, 1)
  )

print(prev_bsi_overall)

#Overall CAUTI
prev_uti_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    total_uti = sum(uti, na.rm = TRUE),
    hcai_uti = round(sum(uti) / sum(ucath) * 100, 1)
  )

print(prev_uti_overall)


#Overall SSI
prev_ssi_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_ssi               = sum(ssi, na.rm = TRUE)
  ) %>%
  mutate(
    hcai_ssi = round(
      total_ssi / (total_patients_surgward + total_ssi_medward) * 100, 1
    )
  )

print(prev_ssi_overall)
  
  
# Plotting 

# Combine into a long-format table for plotting
# Combine into a long-format table for plotting
prev_all <- prev_hcai_overall %>%
  select(site_name, hcai_prev) %>%
  rename(value = hcai_prev) %>%
  mutate(metric = "HCAI") %>%
  bind_rows(
    prev_bsi_overall %>%
      select(site_name, hcai_bsi) %>%
      rename(value = hcai_bsi) %>%
      mutate(metric = "BSI")
  ) %>%
  bind_rows(
    prev_uti_overall %>%
      select(site_name, hcai_uti) %>%
      rename(value = hcai_uti) %>%
      mutate(metric = "CAUTI")
  ) %>%
  bind_rows(
    prev_ssi_overall %>%
      select(site_name, hcai_ssi) %>%
      rename(value = hcai_ssi) %>%
      mutate(metric = "SSI")
  ) %>%
  mutate(
    metric = factor(metric, levels = c("HCAI", "BSI", "CAUTI", "SSI"))
  )

# Optional: reorder sites by overall HCAI prevalence (or any metric) to improve readability
site_order <- prev_hcai_overall %>%
  arrange(desc(hcai_prev)) %>%
  pull(site_name)

prev_all <- prev_all %>%
  mutate(site_name = factor(site_name, levels = site_order))

# Grouped bar chart
p_bars_all <- ggplot(prev_all, aes(x = site_name, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "grey20") +
  geom_text(aes(label = ifelse(is.na(value), "", paste0(value, "%"))),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3, color = "grey15") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.05, 0.12))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "",
    x = "Sites",
    y = "Prevalence (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(p_bars_all)


# Calculates 95% binomial CIs for each site and metric

# Overall HCAI
prev_hcai_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_hcai = sum(hcai, na.rm = TRUE),
    hcai_prev = round(total_hcai / total_patients * 100, 1),
    ci_low = round(prop.test(total_hcai, total_patients)$conf.int[1] * 100, 1),
    ci_high = round(prop.test(total_hcai, total_patients)$conf.int[2] * 100, 1),
    .groups = "drop"
  )

print(prev_hcai_overall)

# Overall BSI
prev_bsi_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_bsi = sum(bsi, na.rm = TRUE),
    hcai_bsi = round(total_bsi / total_patients * 100, 1),
    ci_low = round(prop.test(total_bsi, total_patients)$conf.int[1] * 100, 1),
    ci_high = round(prop.test(total_bsi, total_patients)$conf.int[2] * 100, 1),
    .groups = "drop"
  )

print(prev_bsi_overall)

# Overall CAUTI
prev_uti_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    total_uti = sum(uti, na.rm = TRUE),
    hcai_uti = round(total_uti / total_patients_ucath * 100, 1),
    ci_low = round(prop.test(total_uti, total_patients_ucath)$conf.int[1] * 100, 1),
    ci_high = round(prop.test(total_uti, total_patients_ucath)$conf.int[2] * 100, 1),
    .groups = "drop"
  )

print(prev_uti_overall)

# Overall SSI
prev_ssi_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_ssi = sum(ssi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ssi_denominator = total_patients_surgward + total_ssi_medward,
    hcai_ssi = ifelse(
      ssi_denominator > 0,
      round(total_ssi / ssi_denominator * 100, 1),
      NA_real_
    )
  ) %>%
  rowwise() %>%
  mutate(
    ci_low = ifelse(
      ssi_denominator > 0 && total_ssi <= ssi_denominator,
      round(prop.test(total_ssi, ssi_denominator)$conf.int[1] * 100, 1),
      NA_real_
    ),
    ci_high = ifelse(
      ssi_denominator > 0 && total_ssi <= ssi_denominator,
      round(prop.test(total_ssi, ssi_denominator)$conf.int[2] * 100, 1),
      NA_real_
    )
  ) %>%
  ungroup()

print(prev_ssi_overall)


# Combine into a long-format table for plotting
prev_all <- prev_hcai_overall %>%
  select(site_name, hcai_prev, ci_low, ci_high) %>%
  rename(value = hcai_prev) %>%
  mutate(metric = "HAI") %>%
  bind_rows(
    prev_bsi_overall %>%
      select(site_name, hcai_bsi, ci_low, ci_high) %>%
      rename(value = hcai_bsi) %>%
      mutate(metric = "BSI")
  ) %>%
  bind_rows(
    prev_uti_overall %>%
      select(site_name, hcai_uti, ci_low, ci_high) %>%
      rename(value = hcai_uti) %>%
      mutate(metric = "CAUTI")
  ) %>%
  bind_rows(
    prev_ssi_overall %>%
      select(site_name, hcai_ssi, ci_low, ci_high) %>%
      rename(value = hcai_ssi) %>%
      mutate(metric = "SSI")
  ) %>%
  mutate(
    metric = factor(metric, levels = c("HAI", "BSI", "CAUTI", "SSI"))
  )

# Optional: reorder sites by overall HCAI prevalence
site_order <- prev_hcai_overall %>%
  arrange(desc(hcai_prev)) %>%
  pull(site_name)

prev_all <- prev_all %>%
  mutate(site_name = factor(site_name, levels = site_order))

# Grouped bar chart
p_bars_all <- ggplot(prev_all, aes(x = site_name, y = value, fill = metric)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "grey20"
  ) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6,
    color = "grey15"
  ) +
  geom_text(
    aes(y = value/2, label = ifelse(is.na(value), "", paste0(value, "%"))),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3,
    color = "grey15",
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "",
    x = "Sites",
    y = "Prevalence (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(p_bars_all)

# Merge figure A and B

plot_grid(gg, p_bars_all, labels = c(""), ncol = 1)


# Remove hcai prevalence from the chart as it is an overall system prevalence

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Overall BSI (per total patients)
prev_bsi_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_bsi = sum(bsi, na.rm = TRUE),
    prev_bsi = ifelse(total_patients > 0, round(total_bsi / total_patients * 100, 1), NA_real_),
    .groups = "drop"
  )

print(head(prev_bsi_overall))

# Overall CAUTI (per catheterized patients)
prev_uti_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_ucath = sum(ucath, na.rm = TRUE),
    total_uti = sum(uti, na.rm = TRUE),
    prev_cauti = ifelse(total_ucath > 0, round(total_uti / total_ucath * 100, 1), NA_real_),
    .groups = "drop"
  )

print(head(prev_uti_overall))

# Overall SSI with special denominator:
# numerator = SSI in surgical + SSI in medical
# denominator = patients in surgical + SSI in medical
prev_ssi_overall <- df %>%
  group_by(site_name) %>%
  summarise(
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_ssi_surgward      = sum(ssi[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi               = total_ssi_surgward + total_ssi_medward,
    prev_ssi = ifelse(
      (total_patients_surgward + total_ssi_medward) > 0,
      round(total_ssi / (total_patients_surgward + total_ssi_medward) * 100, 1),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  select(site_name, prev_ssi)

print(head(prev_ssi_overall))

# Combine metrics into a tidy frame
prev_all <- prev_ssi_overall %>%
  left_join(prev_bsi_overall %>% select(site_name, prev_bsi), by = "site_name") %>%
  left_join(prev_uti_overall %>% select(site_name, prev_cauti), by = "site_name") %>%
  pivot_longer(cols = c(prev_ssi, prev_bsi, prev_cauti),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric,
                    prev_ssi = "SSI",
                    prev_bsi = "BSI",
                    prev_cauti = "CAUTI")
  )

print(head(prev_all))

# Grouped bar chart: SSI, BSI, CAUTI per site
p_bars <- ggplot(prev_all, aes(x = fct_reorder(site_name, value, .fun = max, .desc = TRUE),
                               y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = ifelse(is.na(value), "", paste0(value, "%"))),
            position = position_dodge(width = 0.8), vjust = -0.2, size = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "",
    x = "Sites",
    y = "Prevalence (%)",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  ylim(0, max(prev_all$value, na.rm = TRUE) * 1.15)

print(p_bars)

# CALCULATE THE OVERALL PREVALENCE BY SITE AND WARD 


# Table 1: Distribution of overall infection prevalence by hospital sites and ward types

library(dplyr)
library(stringr)
library(tidyr)

# 0) Ensure ward classification (Surgical / Medical / Other if needed)
df_w <- df %>%
  mutate(
    ward_class = case_when(
      ward_name == "Surgical" ~ "Surgical",
      ward_name == "Medical"  ~ "Medical",
      TRUE ~ "Other"
    )
  )

# 1) Site-level special SSI prevalence (same for all wards within a site)
ssi_prev_site <- df_w %>%
  group_by(site_name) %>%
  summarise(
    total_patients_surgward = sum(total[ward_class == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_class == "Medical"], na.rm = TRUE),
    total_ssi               = sum(ssi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prev_ssi_site = ifelse(
      (total_patients_surgward + total_ssi_medward) > 0,
      round(total_ssi / (total_patients_surgward + total_ssi_medward) * 100, 1),
      NA_real_
    )
  ) %>%
  select(site_name, prev_ssi_site)

# 2) Prevalence by site and ward_class for HCAI, BSI, CAUTI
prev_by_site_ward <- df_w %>%
  group_by(site_name, ward_class) %>%
  summarise(
    total_patients = sum(total, na.rm = TRUE),
    total_hcai     = sum(hcai, na.rm = TRUE),
    total_bsi      = sum(bsi, na.rm = TRUE),
    total_ucath    = sum(ucath, na.rm = TRUE),
    total_uti      = sum(uti, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prev_hcai  = ifelse(total_patients > 0, round(total_hcai / total_patients * 100, 1), NA_real_),
    prev_bsi   = ifelse(total_patients > 0, round(total_bsi  / total_patients * 100, 1), NA_real_),
    prev_cauti = ifelse(total_ucath   > 0, round(total_uti  / total_ucath   * 100, 1), NA_real_)
  ) %>%
  left_join(ssi_prev_site, by = "site_name") %>%
  # Rename for clarity and select the distribution table columns
  transmute(
    site_name,
    ward_type = ward_class,
    prev_hcai = prev_hcai,
    prev_bsi = prev_bsi,
    prev_cauti = prev_cauti,
    prev_ssi = prev_ssi_site
  ) %>%
  arrange(site_name, ward_type)

# 3) Optional: produce a wide-format table (metrics as columns)
distribution_table <- prev_by_site_ward

# Show a peek
print(head(distribution_table, 10))


# Distribution table script 
create_infection_distribution_table <- function(df) {
  
  # Ensure ward classification
  df_w <- df %>%
    mutate(
      ward_class = case_when(
        ward_name == "Surgical" ~ "Surgical",
        ward_name == "Medical"  ~ "Medical",
        TRUE ~ "Other"
      )
    )
  
  # Site-level SSI prevalence (special denominator)
  ssi_prev_site <- df_w %>%
    group_by(site_name) %>%
    summarise(
      total_patients_surgward = sum(total[ward_class == "Surgical"], na.rm = TRUE),
      total_ssi_medward = sum(ssi[ward_class == "Medical"], na.rm = TRUE),
      total_ssi = sum(ssi, na.rm = TRUE),
      total_surgical_patients = total_patients_surgward + total_ssi_medward,
      prev_ssi_site = ifelse(total_surgical_patients > 0, 
                             round(total_ssi / total_surgical_patients * 100, 1), 
                             NA_real_),
      .groups = "drop"
    )
  
  # Ward-level calculations with totals
  distribution_table <- df_w %>%
    group_by(site_name, ward_class) %>%
    summarise(
      total_patients = sum(total, na.rm = TRUE),
      total_ucath = sum(ucath, na.rm = TRUE),
      total_hcai = sum(hcai, na.rm = TRUE),
      total_bsi = sum(bsi, na.rm = TRUE),
      total_uti = sum(uti, na.rm = TRUE),
      prev_hcai = ifelse(total_patients > 0, round(total_hcai / total_patients * 100, 1), NA_real_),
      prev_bsi = ifelse(total_patients > 0, round(total_bsi / total_patients * 100, 1), NA_real_),
      prev_cauti = ifelse(total_ucath > 0, round(total_uti / total_ucath * 100, 1), NA_real_),
      .groups = "drop"
    ) %>%
    left_join(ssi_prev_site %>% select(site_name, total_ssi, total_surgical_patients, prev_ssi_site), 
              by = "site_name") %>%
    select(
      site_name, 
      ward_type = ward_class,
      total_patients,
      total_ucath, 
      total_surgical_patients,
      total_hcai,
      total_ssi,
      total_bsi,
      total_uti,
      prev_hcai,
      prev_bsi,
      prev_cauti,
      prev_ssi = prev_ssi_site
    ) %>%
    arrange(site_name, ward_type)
  
  return(distribution_table)
}

result_table <- create_infection_distribution_table(df)
print(result_table)
View(result_table)


# ICC (inter cluster correlation coefficient) by hospital

library(lme4)
library(performance)

compute_icc_by_hospital <- function(df, outcome_var, total_var = "total") {
  
  # cbind(y, n-y) structure for binomial counts
  formula_str <- paste0("cbind(", outcome_var, ", ",
                        total_var, " - ", outcome_var, ") ~ 1 + (1 | site_name)")
  
  form <- as.formula(formula_str)
  
  fit <- glmer(
    form,
    data = df,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  ICC <- performance::icc(fit)$ICC_adjusted
  
  return(list(model = fit, ICC = ICC))
}

# Compute ICC by hospital
icc_hcai_hospital <- compute_icc_by_hospital(df, "hcai")
icc_hcai_hospital$ICC



# ICC by wards

df_icc <- df %>%
  mutate(
    ward_class = case_when(
      ward_name == "Surgical" ~ "Surgical",
      ward_name == "Medical"  ~ "Medical",
      TRUE ~ "Other"
    )
  )

compute_icc_by_ward <- function(df, outcome_var, total_var = "total") {
  
  formula_str <- paste0("cbind(", outcome_var, ", ",
                        total_var, " - ", outcome_var, ") ~ 1 + (1 | ward_class)")
  
  form <- as.formula(formula_str)
  
  fit <- glmer(
    form,
    data = df,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  ICC <- performance::icc(fit)$ICC_adjusted
  
  return(list(model = fit, ICC = ICC))
}

# compute ICC by wards
icc_hcai_ward <- compute_icc_by_ward(df_icc, "hcai")
icc_hcai_ward$ICC


# ICC for HCAI by ward type
icc_hcai_ward <- compute_icc_by_ward(df_icc, "hcai")
icc_hcai_ward$ICC


# ICC with two-level clustering (hospital + ward)

compute_icc_two_levels <- function(df, outcome_var, total_var = "total") {
  
  formula_str <- paste0(
    "cbind(", outcome_var, ", ", total_var, " - ", outcome_var,
    ") ~ 1 + (1 | site_name) + (1 | ward_class)"
  )
  
  fit <- glmer(
    as.formula(formula_str),
    data = df,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  varcomp <- as.data.frame(VarCorr(fit))
  
  var_hospital <- varcomp$vcov[varcomp$grp == "site_name"]
  var_ward     <- varcomp$vcov[varcomp$grp == "ward_class"]
  var_logistic <- (pi^2) / 3
  
  total_var <- var_hospital + var_ward + var_logistic
  
  list(
    model = fit,
    ICC_hospital = var_hospital / total_var,
    ICC_ward = var_ward / total_var
  )
}

# compute ICC at two level:
icc_two <- compute_icc_two_levels(df_icc, "hcai")
icc_two$ICC_hospital
icc_two$ICC_ward






# COMPUTE WEEKLY PREVALENCES
# For computing the weekely prevalence, I first order the weeks, from 1 to 57 as data were collected weekly at 57 point times.

# Create a proper week order
week_order <- paste0("Week", 1:57)
df$week <- factor(df$week, levels = week_order)

# Compute the overall weekly prevalence by aggregating total and hcai by week
# Prevalence of all the HCAI is computed, as well as all the HCAI types at all the study sites.
overall_weekly_prevalence <- df %>% 
  group_by(week) %>% 
  summarise(
    total = sum(total, na.rm = TRUE),
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    hcai = sum(hcai, na.rm = TRUE),
    ssi = sum(ssi, na.rm = TRUE),
    uti = sum(uti, na.rm = TRUE),
    bsi = sum(bsi, na.rm = TRUE),
    prev_hcai = round(hcai/total *100, 1),
    prev_ssi = round(ssi/(total_patients_surgward + total_ssi_medward) *100, 1),
    prev_uti = round(uti/total_patients_ucath *100, 1),
    prev_bsi = round(bsi/total *100, 1)
  )

print(overall_weekly_prevalence, n=57)


View(overall_weekly_prevalence)

# Time-series counting graph 
library(dplyr)
library(ggplot2)

weekly_hcai_counts <- weekly_hcai_counts %>%
  mutate(week = as.numeric(week)) %>%
  arrange(week)

# Compute weekly HCAI counts across the survey period
weekly_hcai_counts <- df %>%
  group_by(week) %>%
  summarise(
    hcai = sum(hcai, na.rm = TRUE),
    .groups = "drop"
  )

print(weekly_hcai_counts, n = Inf)

# Time series count graph: number of HCAI across survey weeks
hcai_count_plot <- ggplot(weekly_hcai_counts, aes(x = week, y = hcai, group = 1)) +
  geom_line(linewidth = 1, color = "#1f77b4") +
  geom_point(size = 2.5, color = "#1f77b4") +
  scale_y_continuous(
    name = "Number of HAI",
    breaks = scales::pretty_breaks()
  ) +
  labs(
    title = "",
    x = "Survey week"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

print(hcai_count_plot)


# Plotting the OVERALL weekly prevalence

# Convert 'week' variable to a numeric week value
overall_weekly_prevalence <- overall_weekly_prevalence %>%
  mutate(week_num = as.numeric(gsub("Week", "", week))) %>%
  arrange(week_num)

# Reshape the data to long format for plotting
overall_weekly_long <- overall_weekly_prevalence %>% 
  select(week_num, prev_hcai, prev_ssi, prev_uti, prev_bsi) %>% 
  pivot_longer(cols = starts_with("prev_"),
               names_to = "infection_type",
               values_to = "prevalence")

# Modify infection_type to be more readable
overall_weekly_long <- overall_weekly_long %>% 
  mutate(infection_type = factor(case_when(
    infection_type == "prev_hcai" ~ "HAI (Overall)",
    infection_type == "prev_ssi" ~ "Surgical Site Infection",
    infection_type == "prev_uti" ~ "Urinary Tract Infection",
    infection_type == "prev_bsi" ~ "Bloodstream Infection",
    TRUE ~ infection_type
  ), 
  levels = c("HAI (Overall)", "Surgical Site Infection", "Urinary Tract Infection", "Bloodstream Infection")))

# Create a Faceted plot - each infection type gets its own panel
facet_plot_all <- ggplot(overall_weekly_long, aes(x = week_num, y = prevalence)) +
  geom_area(fill = "white", alpha = 0.4) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, color = "darkblue", fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ infection_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 60, by = 3)) +
  labs(
       x = "Weeks",
       y = "Prevalence (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(facet_plot_all)


# Compute the QECH weekly prevalence by aggregating total and hcai by week

# Filter data for QECH (QECH=1)
qech_df <- df %>% filter(site == 1)

# Compute weekly prevalence for QECH

qech_weekly_prevalence <- qech_df %>% 
  group_by(week) %>% 
  summarise(
    total = sum(total, na.rm = TRUE),
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    hcai = sum(hcai, na.rm = TRUE),
    ssi = sum(ssi, na.rm = TRUE),
    uti = sum(uti, na.rm = TRUE),
    bsi = sum(bsi, na.rm = TRUE),
    prev_hcai = round(hcai/total *100, 1),
    prev_ssi = round(ssi/(total) *100, 1),
    prev_uti = round(uti/total_patients_ucath *100, 1),
    prev_bsi = round(bsi/total *100, 1)
  )

print(qech_weekly_prevalence, n=57)
View (qech_weekly_prevalence)

# Plotting the QECH overall weekly prevalence

# Convert 'week' variable to a numeric week value
qech_weekly_prevalence <- qech_weekly_prevalence %>%
  mutate(week_num = as.numeric(gsub("Week", "", week))) %>%
  arrange(week_num)

# Reshape the data to long format for plotting
qech_weekly_long <- qech_weekly_prevalence %>% 
  select(week_num, prev_hcai, prev_ssi, prev_uti, prev_bsi) %>% 
  pivot_longer(cols = starts_with("prev_"),
               names_to = "infection_type",
               values_to = "prevalence")

# Modify infection_type to be more readable
qech_weekly_long <- qech_weekly_long %>% 
  mutate(infection_type = factor(case_when(
    infection_type == "prev_hcai" ~ "HAI (Overall)",
    infection_type == "prev_ssi" ~ "Surgical Site Infection",
    infection_type == "prev_uti" ~ "Urinary Tract Infection",
    infection_type == "prev_bsi" ~ "Bloodstream Infection",
    TRUE ~ infection_type
  ), levels = c("HAI (Overall)", "Surgical Site Infection", "Urinary Tract Infection", "Bloodstream Infection")))

# Create a Faceted plot - each infection type gets its own panel
facet_plot_qech <- ggplot(qech_weekly_long, aes(x = week_num, y = prevalence)) +
  geom_area(fill = "white", alpha = 0.4) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, color = "darkblue", fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ infection_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 60, by = 3)) +
  labs(
    x = "Week",
    y = "Prevalence(%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(facet_plot_qech)


# Compute the ZCH weekly prevalence by aggregating total and hcai by week

# Filter data for ZCH (ZCH=2)
zch_df <- df %>% filter(site == 2)

# Compute weekly prevalence for ZCH
zch_weekly_prevalence <- zch_df %>% 
  group_by(week) %>% 
  summarise(
    total = sum(total, na.rm = TRUE),
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    hcai = sum(hcai, na.rm = TRUE),
    ssi = sum(ssi, na.rm = TRUE),
    uti = sum(uti, na.rm = TRUE),
    bsi = sum(bsi, na.rm = TRUE),
    prev_hcai = round(hcai/total *100, 1),
    prev_ssi = round(ssi/(total_patients_surgward + total_ssi_medward) *100, 1),
    prev_uti = round(uti/total_patients_ucath *100, 1),
    prev_bsi = round(bsi/total *100, 1)
  )

print(zch_weekly_prevalence, n=57)
View(zch_weekly_prevalence)

# Plotting the ZCH overall weekly prevalence

# Convert 'week' variable to a numeric week value
zch_weekly_prevalence <- zch_weekly_prevalence %>%
  mutate(week_num = as.numeric(gsub("Week", "", week))) %>%
  arrange(week_num)

# Reshape the data to long format for plotting
zch_weekly_long <- zch_weekly_prevalence %>% 
  select(week_num, prev_hcai, prev_ssi, prev_uti, prev_bsi) %>% 
  pivot_longer(cols = starts_with("prev_"),
               names_to = "infection_type",
               values_to = "prevalence")

# Modify infection_type to be more readable
zch_weekly_long <- zch_weekly_long %>% 
  mutate(infection_type = factor(case_when(
    infection_type == "prev_hcai" ~ "HAI (Overall)",
    infection_type == "prev_ssi" ~ "Surgical Site Infection",
    infection_type == "prev_uti" ~ "Urinary Tract Infection",
    infection_type == "prev_bsi" ~ "Bloodstream Infection",
    TRUE ~ infection_type
  ), levels = c("HAI (Overall)", "Surgical Site Infection", "Urinary Tract Infection", "Bloodstream Infection")))

# Create a Faceted plot - each infection type gets its own panel
facet_plot_zch <- ggplot(zch_weekly_long, aes(x = week_num, y = prevalence)) +
  geom_area(fill = "white", alpha = 0.4) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, color = "darkblue", fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ infection_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 60, by = 3)) +
  labs(
    x = "Week",
    y = "Prevalence (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(facet_plot_zch)



# Compute the CDH weekly prevalence by aggregating total and hcai by week

# Filter data for CDH (CDH=3)
cdh_df <- df %>% filter(site == 3)

# Compute weekly prevalence for CDH
cdh_weekly_prevalence <- cdh_df %>% 
  group_by(week) %>% 
  summarise(
    total = sum(total, na.rm = TRUE),
    total_patients_surgward = sum(total[ward_name == "Surgical"], na.rm = TRUE),
    total_ssi_medward       = sum(ssi[ward_name == "Medical"], na.rm = TRUE),
    total_patients_ucath = sum(ucath, na.rm = TRUE),
    hcai = sum(hcai, na.rm = TRUE),
    ssi = sum(ssi, na.rm = TRUE),
    uti = sum(uti, na.rm = TRUE),
    bsi = sum(bsi, na.rm = TRUE),
    prev_hcai = round(hcai/total *100, 1),
    prev_ssi = round(ssi/(total_patients_surgward + total_ssi_medward) *100, 1),
    prev_uti = round(uti/total_patients_ucath *100, 1),
    prev_bsi = round(bsi/total *100, 1)
  )

print(cdh_weekly_prevalence, n=57)
View(cdh_weekly_prevalence)

# Faceted plot for CDH

# Convert 'week' variable to a numeric week value
cdh_weekly_prevalence <- cdh_weekly_prevalence %>%
  mutate(week_num = as.numeric(gsub("Week", "", week))) %>%
  arrange(week_num)

# Reshape the data to long format for plotting
cdh_weekly_long <- cdh_weekly_prevalence %>% 
  select(week_num, prev_hcai, prev_ssi, prev_uti, prev_bsi) %>% 
  pivot_longer(cols = starts_with("prev_"),
               names_to = "infection_type",
               values_to = "prevalence")

# Modify infection_type to be more readable
cdh_weekly_long <- cdh_weekly_long %>% 
  mutate(infection_type = factor(case_when(
    infection_type == "prev_hcai" ~ "HAI (Overall)",
    infection_type == "prev_ssi" ~ "Surgical Site Infection",
    infection_type == "prev_uti" ~ "Urinary Tract Infection",
    infection_type == "prev_bsi" ~ "Bloodstream Infection",
    TRUE ~ infection_type
  ), levels = c("HAI (Overall)", "Surgical Site Infection", "Urinary Tract Infection", "Bloodstream Infection")))

# Create a Faceted plot - each infection type gets its own panel
facet_plot_cdh <- ggplot(cdh_weekly_long, aes(x = week_num, y = prevalence)) +
  geom_area(fill = "white", alpha = 0.4) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, color = "darkblue", fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ infection_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 60, by = 3)) +
  labs(
    x = "Week",
    y = "Prevalence(%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(facet_plot_cdh)

#MERGE FACETED PLOTS FOR OVERALL HCAI ACCROSS SITES

library(cowplot)
plot_grid(facet_plot_all, facet_plot_qech, facet_plot_zch, facet_plot_cdh, labels = c("A. All sites", "B. QECH", "C. ZCH", "D. CDH"), ncol = 4)


# COMPARISON OF PREVALENCE BY HOSPITAL AND BY WARDS

# Filter out rows with NA or zero total  
df_filtered <- df %>% filter(!is.na(total) & total > 0)  

# ------------------------------  
# Compare HCAI prevalence between wards  
# ------------------------------  
# Aggregate data by ward  
ward_data <- df_filtered %>%   
  group_by(ward_name) %>%   
  summarise(hcai_sum = sum(hcai),  
            total_sum = sum(total)) %>%  
  mutate(non_hcai = total_sum - hcai_sum)  

print(ward_data)  

# Create a contingency table for wards. Rows: ward, Columns: HCAI and No HCAI events.  
ward_table <- as.matrix(ward_data[, c("hcai_sum", "non_hcai")])  
rownames(ward_table) <- ward_data$ward_name  
print(ward_table)  

# Check if any expected cell frequency is low to decide on test type.  
# Typically, if any expected count < 5 then use Fisher's exact test.  

# Compute expected counts using chisq.test (without Yates' continuity correction)  
chisq_ward <- chisq.test(ward_table, correct = FALSE)  
if(any(chisq_ward$expected < 5)){  
  cat("Using Fisher's exact test for wards:\n")  
  ward_test <- fisher.test(ward_table)  
} else {  
  cat("Using Pearson's Chi-squared test for wards:\n")  
  ward_test <- chisq_ward  
}  
print(ward_test)  

# ------------------------------  
# Compare HCAI prevalence across hospitals  
# ------------------------------  
# Aggregate data by hospital (site_name)  
hospital_data <- df_filtered %>%   
  group_by(site_name) %>%   
  summarise(hcai_sum = sum(hcai),  
            total_sum = sum(total)) %>%  
  mutate(non_hcai = total_sum - hcai_sum)  

print(hospital_data)  

# Create a contingency table for hospitals.  
hospital_table <- as.matrix(hospital_data[, c("hcai_sum", "non_hcai")])  
rownames(hospital_table) <- hospital_data$site_name  
print(hospital_table)  

# Perform the test for hospitals  
chisq_hosp <- chisq.test(hospital_table, correct = FALSE)  
if(any(chisq_hosp$expected < 5)){  
  cat("Using Fisher's exact test for hospitals:\n")  
  hosp_test <- fisher.test(hospital_table)  
} else {  
  cat("Using Pearson's Chi-squared test for hospitals:\n")  
  hosp_test <- chisq_hosp  
}  
print(hosp_test)


# TRENDS ANALYSIS AND MODELLING OF HCAI PREVALENCE

# We'll prepare data, fit four models, compare via AIC, and plot predictions of best.

library(dplyr)
library(lme4)
library(ggplot2)
library(scales)

# Prepare data
pps_clean <- pps %>%
  mutate(
    hospital = as.factor(site),
    ward = as.factor(ward),
    week_num = as.integer(gsub("[^0-9]", "", as.character(week))),
    total = as.numeric(total),
    hcai = as.numeric(hcai)
  ) %>%
  filter(!is.na(week_num), total > 0)

# Aggregate to hospital-ward-week to align with random effects structure
agg <- pps_clean %>%
  group_by(hospital, ward, week_num) %>%
  summarise(hcai = sum(hcai, na.rm = TRUE), total = sum(total, na.rm = TRUE), .groups = 'drop') %>%
  mutate(prev_obs = hcai/total)

print("Head of aggregated hospital-ward-week data:")
print(head(agg))




# Fix: use glm for the time-only model (no random effects), and glmer for others

# 1) Fit models one-by-one
library(lme4)

# m_time: fixed week only via glm
m_time <- glm(cbind(hcai, total - hcai) ~ scale(week_num), data = agg, family = binomial)
print("Fit m_time (glm) ok")
summary(m_time)

# 95% CI for glm coefficients
ci_m_time <- confint.default(m_time)  # Wald CI
ci_m_time_prob <- plogis(ci_m_time)  # Transform to probability scale if needed
print("95% CI for m_time (glm) coefficients on logit scale:")
print(ci_m_time)
print("95% CI for m_time (glm) probabilities:")
print(ci_m_time_prob)

# m_time_hosp: fixed week + random intercept for hospital
m_time_hosp <- glmer(cbind(hcai, total - hcai) ~ scale(week_num) + (1 | hospital),
                     data = agg, family = binomial)
print("Fit m_time_hosp (glmer) ok")
summary(m_time_hosp)

# 95% CI for glmer fixed effects
ci_m_time_hosp <- confint(m_time_hosp, parm="beta_", method="Wald")  # Wald CI
ci_m_time_hosp_prob <- plogis(ci_m_time_hosp)
print("95% CI for m_time_hosp fixed effects on logit scale:")
print(ci_m_time_hosp)
print("95% CI for m_time_hosp probabilities:")
print(ci_m_time_hosp_prob)

# m_time_ward: fixed week + random intercept for ward
m_time_ward <- glmer(cbind(hcai, total - hcai) ~ scale(week_num) + (1 | ward),
                     data = agg, family = binomial)
print("Fit m_time_ward (glmer) ok")
summary(m_time_ward)

ci_m_time_ward <- confint(m_time_ward, parm="beta_", method="Wald")
ci_m_time_ward_prob <- plogis(ci_m_time_ward)
print("95% CI for m_time_ward fixed effects on logit scale:")
print(ci_m_time_ward)
print("95% CI for m_time_ward probabilities:")
print(ci_m_time_ward_prob)


# m_time_hosp_ward: fixed week + random intercepts for hospital and ward
m_time_hosp_ward <- glmer(cbind(hcai, total - hcai) ~ scale(week_num) + (1 | hospital) + (1 | ward),
                          data = agg, family = binomial, control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))
print("Fit m_time_hosp_ward (glmer) ok")
summary(m_time_hosp_ward)

ci_m_time_hosp_ward <- confint(m_time_hosp_ward, parm="beta_", method="Wald")
ci_m_time_hosp_ward_prob <- plogis(ci_m_time_hosp_ward)
print("95% CI for m_time_hosp_ward fixed effects on logit scale:")
print(ci_m_time_hosp_ward)
print("95% CI for m_time_hosp_ward probabilities:")
print(ci_m_time_hosp_ward_prob)


# 2) Compare AICs and BICs
model_comp <- AIC(m_time, m_time_hosp, m_time_ward, m_time_hosp_ward)
print("AIC comparison across specified models:")
print(model_comp)

BIC(m_time, m_time_hosp, m_time_ward, m_time_hosp_ward)

# Force/select m_time_hosp as the selected best model
best_name <- "m_time_hosp"
print(paste("Selected best model:", best_name))

# 3) Choose selected best model
best_mod <- m_time_hosp


# 4) Generate predictions on hospital-ward-week grid present in data
grid <- agg[, c("hospital", "ward", "week_num")]
grid <- unique(grid)

# use predict with type='response'
grid$pred <- predict(best_mod, newdata = grid, type = 'response', allow.new.levels = TRUE)

# Aggregate predictions to hospital-week for a clearer plot
library(dplyr)
obs_hosp <- agg %>% group_by(hospital, week_num) %>% summarise(prev = sum(hcai)/sum(total), .groups = 'drop')
pred_hosp <- grid %>% group_by(hospital, week_num) %>% summarise(pred = mean(pred), .groups = 'drop')

# 5) Plot

library(ggplot2)
library(scales)

p_pred <- ggplot() +
  geom_point(data = obs_hosp,
             aes(x = week_num, y = prev,
                 color = factor(hospital,
                                labels = c("QECH", "ZCH", "CDH"))),
             alpha = 0.5) +
  geom_line(data = pred_hosp,
            aes(x = week_num, y = pred,
                color = factor(hospital,
                               labels = c("QECH", "ZCH", "CDH"))),
            linewidth = 1) +
  scale_x_continuous(breaks = seq(min(obs_hosp$week_num),
                                  max(obs_hosp$week_num), by = 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     breaks = seq(0, 1, by = 0.05)) +
  labs(title = paste0("Predictions by selected model: ", best_name),
       x = "Week",
       y = "Prevalence",
       color = "Hospital") +
  theme_minimal(base_size = 10)

print(p_pred)


# Random slope by hospital
model_hosp <- glmer(
  cbind(hcai, total - hcai) ~ scale(week_num) + 
    (1 + scale(week_num) | hospital) + 
    (1 | ward),
  data = agg,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Extract hospital-specific β (with 95% CI)
library(broom.mixed)
library(dplyr)


colnames(ranef_hosp)

library(dplyr)
library(tibble)

library(dplyr)
library(tibble)

# Extract random effects
ranef_hosp <- ranef(model_hosp, condVar = TRUE)$hospital

# Fixed effect (overall slope)
fixef_week <- fixef(model_hosp)["scale(week_num)"]

# Random slope SD
vc <- VarCorr(model_hosp)
sd_slope <- attr(vc$hospital, "stddev")[2]

# Build hospital-specific estimates
beta_hosp <- ranef_hosp %>%
  rownames_to_column("hospital") %>%
  mutate(
    beta = fixef_week + `scale(week_num)`,
    se = sd_slope,
    z = beta / se,
    p_value = 2 * (1 - pnorm(abs(z))),
    lower = beta - 1.96 * se,
    upper = beta + 1.96 * se
  ) %>%
  select(hospital, beta, lower, upper, p_value)

beta_hosp



## Updated script with 4th trend + 95% CI

# ---- Overall observed prevalence per week ----
obs_all <- agg %>%
  group_by(week_num) %>%
  summarise(prev = sum(hcai) / sum(total), .groups = "drop")

# ---- Fit overall model (same structure as best model but no hospital or ward effects) ----
m_all <- glm(cbind(hcai, total - hcai) ~ scale(week_num),
             data = agg, family = binomial)

# ---- Predict overall prevalence with 95% CI ----
new_all <- data.frame(week_num = sort(unique(agg$week_num)))
pred_all <- predict(m_all, newdata = new_all, type = "link", se.fit = TRUE)
new_all$fit <- plogis(pred_all$fit)
new_all$lwr <- plogis(pred_all$fit - 1.96 * pred_all$se.fit)
new_all$upr <- plogis(pred_all$fit + 1.96 * pred_all$se.fit)

# ---- 95% CI for hospital-specific predictions ----
pred_grid <- predict(best_mod, newdata = grid, type = "link",
                     se.fit = TRUE, allow.new.levels = TRUE)

grid$fit <- plogis(pred_grid$fit)
grid$lwr <- plogis(pred_grid$fit - 1.96 * pred_grid$se.fit)
grid$upr <- plogis(pred_grid$fit + 1.96 * pred_grid$se.fit)

# Aggregate hospital-level predictions + CIs
pred_hosp_ci <- grid %>%
  group_by(hospital, week_num) %>%
  summarise(
    pred = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop"
  )

p_pred <- ggplot() +
  
  # ----- Observed hospital-level points -----
geom_point(data = obs_hosp,
           aes(x = week_num, y = prev,
               color = factor(hospital,
                              labels = c("QECH", "ZCH", "CDH"))),
           alpha = 0.5) +
  
  # ----- Hospital-specific 95% CI ribbons -----
geom_ribbon(data = pred_hosp_ci,
            aes(x = week_num, ymin = lwr, ymax = upr,
                fill = factor(hospital,
                              labels = c("QECH", "ZCH", "CDH"))),
            alpha = 0.15, color = NA) +
  
  # ----- Hospital-specific trend lines -----
geom_line(data = pred_hosp_ci,
          aes(x = week_num, y = pred,
              color = factor(hospital,
                             labels = c("QECH", "ZCH", "CDH"))),
          linewidth = 1) +
  
  # ----- Overall 95% CI ribbon (grey) -----
geom_ribbon(data = new_all,
            aes(x = week_num, ymin = lwr, ymax = upr),
            alpha = 0.2, fill = "black") +
  
  # ----- Overall trend line (black) -----
geom_line(data = new_all,
          aes(x = week_num, y = fit),
          color = "black", linewidth = 1.3) +
  
  # ----- Observed overall points -----
geom_point(data = obs_all,
           aes(x = week_num, y = prev),
           color = "black", shape = 21, fill = "white", size = 2) +
  
  scale_x_continuous(breaks = seq(min(obs_hosp$week_num),
                                  max(obs_hosp$week_num), by = 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     breaks = seq(0, 1, by = 0.05)) +
  labs(title = paste0("Predictions by best model: ", best_name),
       x = "Week",
       y = "Prevalence",
       color = "Hospital",
       fill  = "Hospital") +
  theme_minimal(base_size = 10)

p_pred


### Separate into 2 different plots

# ---- Overall observed per week ----
obs_all <- agg %>%
  group_by(week_num) %>%
  summarise(prev = sum(hcai) / sum(total), .groups = "drop")

# ---- Overall glm model ----
m_all <- glm(cbind(hcai, total - hcai) ~ scale(week_num),
             data = agg, family = binomial)

# ---- Predict overall with 95% CI ----
new_all <- data.frame(week_num = sort(unique(agg$week_num)))
pred_all <- predict(m_all, newdata = new_all, type = "link", se.fit = TRUE)

new_all$fit <- plogis(pred_all$fit)
new_all$lwr <- plogis(pred_all$fit - 1.96 * pred_all$se.fit)
new_all$upr <- plogis(pred_all$fit + 1.96 * pred_all$se.fit)

# ---- 95% CI for hospital-level predictions ----
pred_grid <- predict(best_mod, newdata = grid, type = "link",
                     se.fit = TRUE, allow.new.levels = TRUE)

grid$fit <- plogis(pred_grid$fit)
grid$lwr <- plogis(pred_grid$fit - 1.96 * pred_grid$se.fit)
grid$upr <- plogis(pred_grid$fit + 1.96 * pred_grid$se.fit)

pred_hosp_ci <- grid %>%
  group_by(hospital, week_num) %>%
  summarise(
    pred = mean(fit),
    lwr = mean(lwr),
    upr = mean(upr),
    .groups = "drop"
  )

p_hospitals <- ggplot() +
  
  # Observed points
  geom_point(data = obs_hosp,
             aes(x = week_num, y = prev,
                 color = factor(hospital,
                                labels = c("QECH", "ZCH", "CDH"))),
             alpha = 0.5) +
  
  # CI ribbons
  geom_ribbon(data = pred_hosp_ci,
              aes(x = week_num, ymin = lwr, ymax = upr,
                  fill = factor(hospital,
                                labels = c("QECH", "ZCH", "CDH"))),
              alpha = 0.15, color = NA) +
  
  # Predicted trend lines
  geom_line(data = pred_hosp_ci,
            aes(x = week_num, y = pred,
                color = factor(hospital,
                               labels = c("QECH", "ZCH", "CDH"))),
            linewidth = 1) +
  
  scale_x_continuous(breaks = seq(min(obs_hosp$week_num),
                                  max(obs_hosp$week_num), by = 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     breaks = seq(0, 1, by = 0.05)) +
  labs(title = "Predicted prevalence — Hospital-specific Trends",
       x = "Week",
       y = "HAI Prevalence",
       color = "Hospital",
       fill = "Hospital") +
  theme_minimal(base_size = 10)

p_hospitals

p_overall <- ggplot() +
  
  # Observed combined points
  geom_point(data = obs_all,
             aes(x = week_num, y = prev),
             color = "black", shape = 21, fill = "white", size = 2) +
  
  # Overall 95% CI ribbon
  geom_ribbon(data = new_all,
              aes(x = week_num, ymin = lwr, ymax = upr),
              alpha = 0.2, fill = "grey50") +
  
  # Overall trend line
  geom_line(data = new_all,
            aes(x = week_num, y = fit),
            color = "black", linewidth = 1.3) +
  
  scale_x_continuous(breaks = seq(min(obs_all$week_num),
                                  max(obs_all$week_num), by = 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     breaks = seq(0, 1, by = 0.05)) +
  
  labs(title = "Predicted prevalence — All hospitals combined",
       x = "Week",
       y = "HAI Prevalence") +
  
  theme_minimal(base_size = 10)

p_overall

# Overall temporal trend (β, 95% CI, p-value)
# Extract fixed effect for time
beta_overall <- fixef(model_hosp)["scale(week_num)"]

# Extract standard error
se_overall <- sqrt(vcov(model_hosp)["scale(week_num)", "scale(week_num)"])

# Wald statistics
lower <- beta_overall - 1.96 * se_overall
upper <- beta_overall + 1.96 * se_overall
z_value <- beta_overall / se_overall
p_value <- 2 * (1 - pnorm(abs(z_value)))

# Create summary table
overall_effect <- data.frame(
  beta = beta_overall,
  lower = lower,
  upper = upper,
  p_value = p_value
)

overall_effect


library(cowplot)

merged_plot <- plot_grid(
  p_hospitals,
  p_overall,
  labels = c("A", "B"),
  ncol = 2,
  align = "hv"
)

merged_plot


merged_plot_vertical <- plot_grid(
  p_hospitals,
  p_overall,
  labels = c("A", "B"),
  ncol = 1,
  align = "v",
  rel_heights = c(1.2, 1)
)

merged_plot_vertical


# Predicted prevalence for m_time (GLM – fixed effects only)

############################################################
## Predicted prevalence at baseline (week 1) and week 52
## Model: m_time (GLM)
############################################################

## Step 1: Extract coefficients and variance–covariance matrix
beta <- coef(m_time)
vc   <- vcov(m_time)

beta_0    <- beta["(Intercept)"]
beta_week <- beta["scale(week_num)"]

## Step 2: Define scaling parameters
week_mean <- attr(scale(agg$week_num), "scaled:center")
week_sd   <- attr(scale(agg$week_num), "scaled:scale")

# Scaled week values
w0  <- (1  - week_mean) / week_sd
w52 <- (52 - week_mean) / week_sd

## Step 3: Linear predictors (log-odds)
eta_0  <- beta_0 + beta_week * w0
eta_52 <- beta_0 + beta_week * w52

# Standard errors
se_0 <- sqrt(
  vc["(Intercept)", "(Intercept)"] +
    w0^2 * vc["scale(week_num)", "scale(week_num)"] +
    2 * w0 * vc["(Intercept)", "scale(week_num)"]
)

se_52 <- sqrt(
  vc["(Intercept)", "(Intercept)"] +
    w52^2 * vc["scale(week_num)", "scale(week_num)"] +
    2 * w52 * vc["(Intercept)", "scale(week_num)"]
)

## Step 4: Convert to prevalence (probability scale)
inv_logit <- function(x) exp(x) / (1 + exp(x))

P0  <- inv_logit(eta_0)
P52 <- inv_logit(eta_52)

P0_lwr  <- inv_logit(eta_0  - 1.96 * se_0)
P0_upr  <- inv_logit(eta_0  + 1.96 * se_0)

P52_lwr <- inv_logit(eta_52 - 1.96 * se_52)
P52_upr <- inv_logit(eta_52 + 1.96 * se_52)

## Step 5: Relative annual prevalence change
rel_change <- (P52 - P0) / P0 * 100
rel_lower  <- (P52_lwr - P0_upr) / P0_upr * 100
rel_upper  <- (P52_upr - P0_lwr) / P0_lwr * 100

## Step 6: Results table
results_glm <- data.frame(
  Metric = c("Baseline prevalence (P0)",
             "Prevalence at week 52 (P52)",
             "Relative annual change (%)"),
  Estimate = c(P0, P52, rel_change),
  Lower_95CI = c(P0_lwr, P52_lwr, rel_lower),
  Upper_95CI = c(P0_upr, P52_upr, rel_upper)
)

results_glm


# Hospital-Specific Predicted Prevalence

############################################################
## Predicted prevalence at baseline (week 1) and week 52
## Model: m_time_hosp (GLMM)
############################################################

## Step 1: Extract fixed effects and vcov
beta <- fixef(m_time_hosp)
vc   <- vcov(m_time_hosp)

beta_0    <- beta["(Intercept)"]
beta_week <- beta["scale(week_num)"]

## Step 2: Scaling parameters
week_mean <- attr(scale(agg$week_num), "scaled:center")
week_sd   <- attr(scale(agg$week_num), "scaled:scale")

w0  <- (1  - week_mean) / week_sd
w52 <- (52 - week_mean) / week_sd

## Step 3: Linear predictors (fixed-effects only)
eta_0  <- beta_0 + beta_week * w0
eta_52 <- beta_0 + beta_week * w52

# Standard errors (delta method)
se_0 <- sqrt(
  vc["(Intercept)", "(Intercept)"] +
    w0^2 * vc["scale(week_num)", "scale(week_num)"] +
    2 * w0 * vc["(Intercept)", "scale(week_num)"]
)

se_52 <- sqrt(
  vc["(Intercept)", "(Intercept)"] +
    w52^2 * vc["scale(week_num)", "scale(week_num)"] +
    2 * w52 * vc["(Intercept)", "scale(week_num)"]
)

## Step 4: Convert to prevalence
inv_logit <- function(x) exp(x) / (1 + exp(x))

P0  <- inv_logit(eta_0)
P52 <- inv_logit(eta_52)

P0_lwr  <- inv_logit(eta_0  - 1.96 * se_0)
P0_upr  <- inv_logit(eta_0  + 1.96 * se_0)

P52_lwr <- inv_logit(eta_52 - 1.96 * se_52)
P52_upr <- inv_logit(eta_52 + 1.96 * se_52)

## Step 5: Relative annual change
rel_change <- (P52 - P0) / P0 * 100
rel_lower  <- (P52_lwr - P0_upr) / P0_upr * 100
rel_upper  <- (P52_upr - P0_lwr) / P0_lwr * 100

## Step 6: Results table
results_glmm <- data.frame(
  Metric = c("Baseline prevalence (P0)",
             "Prevalence at week 52 (P52)",
             "Relative annual change (%)"),
  Estimate = c(P0, P52, rel_change),
  Lower_95CI = c(P0_lwr, P52_lwr, rel_lower),
  Upper_95CI = c(P0_upr, P52_upr, rel_upper)
)

results_glmm


# COUNT SENSITIVITY MODELS

# Negative binomial GLMM using the raw HAI counts and an offset
library(glmmTMB)

nb_model <- glmmTMB(
  hcai ~ scale(week_num) + (1 | hospital) +
    offset(log(total)),
  family = nbinom2,
  data = agg
)

summary(nb_model)
confint(nb_model, parm = "beta_", method = "Wald")












# OVERALL TREND — Average weekly change (with 95% CI)

# Create week range
week_min <- min(agg$week_num)
week_max <- max(agg$week_num)

# Prediction data for overall model
new_all <- data.frame(week_num = c(week_min, week_max))
new_all$pred <- predict(m_all, newdata = new_all, type = "response", se.fit = TRUE)$fit

# Also extract SE for CI
pred_info <- predict(m_all, newdata = new_all, type = "link", se.fit = TRUE)

# Convert from logit to probability with CI
p_start     <- plogis(pred_info$fit[1])
p_end       <- plogis(pred_info$fit[2])
p_start_lwr <- plogis(pred_info$fit[1] - 1.96*pred_info$se.fit[1])
p_start_upr <- plogis(pred_info$fit[1] + 1.96*pred_info$se.fit[1])
p_end_lwr   <- plogis(pred_info$fit[2] - 1.96*pred_info$se.fit[2])
p_end_upr   <- plogis(pred_info$fit[2] + 1.96*pred_info$se.fit[2])

# Overall change
overall_change <- data.frame(
  scenario = "All hospitals combined",
  change = p_end - p_start,
  lwr_95 = p_end_lwr - p_start_upr,
  upr_95 = p_end_upr - p_start_lwr
)

overall_change


#. Compute trend change for EACH hospital

# Create week range
week_min <- min(agg$week_num)
week_max <- max(agg$week_num)

# Function to compute prevalence change for a hospital
compute_hospital_change <- function(hosp) {
  
  # For glmer, need one existing ward to avoid new levels issue
  ward_example <- agg$ward[agg$hospital == hosp][1]
  
  new_hosp <- data.frame(
    hospital = factor(hosp, levels = levels(agg$hospital)),
    ward = factor(ward_example, levels = levels(agg$ward)),
    week_num = c(week_min, week_max)
  )
  
  # Predict on link scale (logit)
  pred_link <- predict(best_mod, newdata = new_hosp, type = "link", allow.new.levels = TRUE)
  
  # For CI: approximate using standard error of fixed effect slope
  beta <- fixef(best_mod)["scale(week_num)"]
  se <- sqrt(vcov(best_mod)["scale(week_num)", "scale(week_num)"])
  
  # Convert to probability
  p_start <- plogis(pred_link[1])
  p_end <- plogis(pred_link[2])
  
  # Approximate 95% CI for the change
  p_start_lwr <- plogis(pred_link[1] - 1.96*se)
  p_start_upr <- plogis(pred_link[1] + 1.96*se)
  p_end_lwr <- plogis(pred_link[2] - 1.96*se)
  p_end_upr <- plogis(pred_link[2] + 1.96*se)
  
  data.frame(
    hospital = hosp,
    change = p_end - p_start,
    lwr_95 = p_end_lwr - p_start_upr,
    upr_95 = p_end_upr - p_start_lwr
  )
}

# Apply to all hospitals
hospital_changes <- do.call(rbind, lapply(levels(agg$hospital), compute_hospital_change))

# View
hospital_changes




# Extract fixed-effect estimates, random-effect variances, and LRTs between models
# Assumes models m_time, m_time_hosp, m_time_ward, m_time_hosp_ward, and data agg exist from earlier

library(lme4)

# 1) Fixed effects (coefficients) with standard errors for each model
fx_m_time <- summary(m_time)$coefficients
fx_m_time_hosp <- summary(m_time_hosp)$coefficients
fx_m_time_ward <- summary(m_time_ward)$coefficients
fx_m_time_hosp_ward <- summary(m_time_hosp_ward)$coefficients

print("Fixed-effect estimates (glm time-only):")
print(fx_m_time)
print("Fixed-effect estimates (glmer time + hospital):")
print(fx_m_time_hosp)
print("Fixed-effect estimates (glmer time + ward):")
print(fx_m_time_ward)
print("Fixed-effect estimates (glmer time + hospital + ward):")
print(fx_m_time_hosp_ward)

# 2) Random effects variances (as data frames)
re_m_time_hosp <- as.data.frame(VarCorr(m_time_hosp))
re_m_time_ward <- as.data.frame(VarCorr(m_time_ward))
re_m_time_hosp_ward <- as.data.frame(VarCorr(m_time_hosp_ward))

print("Random-effect variances (time + hospital):")
print(re_m_time_hosp)
print("Random-effect variances (time + ward):")
print(re_m_time_ward)
print("Random-effect variances (time + hospital + ward):")
print(re_m_time_hosp_ward)

# 3) Likelihood Ratio Tests (nested model comparisons)
# Note: m_time (glm) can't be directly compared with glmer via anova; 
# We'll compare nested mixed models via ML fits (use REML=FALSE equivalent; binomial uses ML by default).

lrt_hosp_vs_both <- anova(m_time_hosp, m_time_hosp_ward, test = 'Chisq')
lrt_ward_vs_both <- anova(m_time_ward, m_time_hosp_ward, test = 'Chisq')

# For completeness: compare m_time_hosp vs m_time_ward (non-nested) via AIC already done; LRT isn't valid.

print("LRT: time + hospital vs time + hospital + ward:")
print(lrt_hosp_vs_both)
print("LRT: time + ward vs time + hospital + ward:")
print(lrt_ward_vs_both)


#AUTOCORRELATION 

library(lmtest)     # for tests like dwtest
library(ggfortify)
library(gridExtra)  # to arrange plots side by side

# Extract Pearson residuals
resid <- residuals(best_mod, type = "pearson")

# ACF plot
acf_plot <- autoplot(acf(resid, plot = FALSE)) +
  labs(title = "ACF of Model Residuals",
       x = "Lag", y = "Autocorrelation") +
  theme_minimal(base_size = 10)

# PACF plot
pacf_plot <- autoplot(pacf(resid, plot = FALSE)) +
  labs(title = "PACF of Model Residuals",
       x = "Lag", y = "Partial Autocorrelation") +
  theme_minimal(base_size = 10)

# Arrange side by side
grid.arrange(acf_plot, pacf_plot, ncol = 2)


# Optional: Durbin-Watson test for autocorrelation
dwtest(resid ~ 1)


# ICC CALCULATION

library(lme4)
library(dplyr)

#ICC by hospital
compute_icc_by_hospital <- function(df, outcome_var, total_var = "total") {
  
  # Model formula
  formula_str <- paste0(
    "cbind(", outcome_var, ", ", total_var, " - ", outcome_var, 
    ") ~ 1 + (1 | site_name)"
  )
  
  fit <- glmer(
    as.formula(formula_str),
    data = df,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  # Extract variance components
  var_hospital <- as.numeric(VarCorr(fit)$site_name)
  var_residual <- (pi^2) / 3
  
  ICC <- var_hospital / (var_hospital + var_residual)
  
  list(
    model = fit,
    ICC = ICC
  )
}

icc_hcai_hospital <- compute_icc_by_hospital(df, "hcai")
icc_hcai_hospital$ICC

# ICC by ward category
#Prepare ward grouping
df_icc <- df %>%
  mutate(
    ward_class = case_when(
      ward_name == "Surgical" ~ "Surgical",
      ward_name == "Medical"  ~ "Medical",
      TRUE ~ "Other"
    )
  )

#ICC function (ward level)
compute_icc_by_ward <- function(df, outcome_var, total_var = "total") {
  
  formula_str <- paste0(
    "cbind(", outcome_var, ", ", total_var, " - ", outcome_var,
    ") ~ 1 + (1 | ward_class)"
  )
  
  fit <- glmer(
    as.formula(formula_str),
    data = df,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  var_ward <- as.numeric(VarCorr(fit)$ward_class)
  var_residual <- (pi^2) / 3
  
  ICC <- var_ward / (var_ward + var_residual)
  
  list(
    model = fit,
    ICC = ICC
  )
}
icc_hcai_ward <- compute_icc_by_ward(df_icc, "hcai")
icc_hcai_ward$ICC

#ICC with two-level clustering (hospital + ward)
compute_icc_two_levels <- function(df, outcome_var, total_var = "total") {
  
  formula_str <- paste0(
    "cbind(", outcome_var, ", ", total_var, " - ", outcome_var,
    ") ~ 1 + (1 | site_name) + (1 | ward_class)"
  )
  
  fit <- glmer(
    as.formula(formula_str),
    data = df,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  # Extract variance components
  varcomp <- as.data.frame(VarCorr(fit))
  
  var_hospital <- varcomp$vcov[varcomp$grp == "site_name"]
  var_ward     <- varcomp$vcov[varcomp$grp == "ward_class"]
  var_residual <- (pi^2) / 3
  
  total_var <- var_hospital + var_ward + var_residual
  
  list(
    model = fit,
    ICC_hospital = var_hospital / total_var,
    ICC_ward     = var_ward / total_var
  )
}

icc_two <- compute_icc_two_levels(df_icc, "hcai")

icc_two$ICC_hospital
icc_two$ICC_ward


# SIMULATION OF NUMBER OF PPS TO BE DONE


# Simulation script: required number of PPS per year based on Model 1 estimates

# Mean patients surveyed per week
library(dplyr)

# Step 1: Aggregate total patients per week across hospitals and wards
weekly_total <- agg %>%
  group_by(week_num) %>%
  summarise(total_week = sum(total, na.rm = TRUE))  # sum across hospitals and wards

# Step 2: Compute mean and SD across 52 weeks
mean_weekly <- mean(weekly_total$total_week)
sd_weekly   <- sd(weekly_total$total_week)

# Step 3: Create a summary table
summary_weekly <- data.frame(
  mean_per_week = mean_weekly,
  sd_per_week = sd_weekly
)

summary_weekly



# Functions to simulate power 
# 1. simualate_power:m function for simulating power without cluster sampling

#inputs: num_surveys=number of prevalence surveys
#		:p0=prevalence at baseline/first survey
#		: rel_increase=expected % change in prevalence between the first survey & last survey
#		: sims=number of simulations
#		: alpha=significance level

simulate_power <- function(num_surveys, n_per_survey, p0, rel_increase,
                           sims = 10000, alpha = 0.05, seed = NULL, verbose = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  if (num_surveys < 2) stop("Need at least 2 surveys")
  
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(x) 1 / (1 + exp(-x))
  
  # Final prevalence after relative increase
  p_final <- min(p0 * (1 + rel_increase), 0.999)
  
  # Logit-linear slope across surveys
  slope <- (logit(p_final) - logit(p0)) / (num_surveys - 1) #Slope could also be taken from real data parameter estimate (beta coef)
  times <- 0:(num_surveys - 1)
  
  sig <- 0
  for (i in 1:sims) {
    ps <- inv_logit(logit(p0) + slope * times)
    counts <- rbinom(num_surveys, n_per_survey, ps)
    
    # Data in aggregated form
    df <- data.frame(
      y = counts,
      n = n_per_survey,
      time = times
    )
    
    # Fit logistic regression with binomial weights
    fit <- try(glm(cbind(y, n - y) ~ time, data = df, family = binomial), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      pval <- summary(fit)$coefficients["time", "Pr(>|z|)"]
      beta <- coef(fit)["time"]
      if (!is.na(pval) && pval < alpha) {
        sig <- sig + 1
      }
    }
    if (verbose && i %% 100 == 0) message("Simulation ", i, " of ", sims)
  }
  power_est <- sig / sims
  return(power_est)
}



# 2. Define a function:simulate_power_cluster for estimating power with cluster sampling 
#i.e account for clustering by hospital or hospital ward

#inputs: num_surveys=number of prevalence surveys
#		: n_cluster=number of clusters
#		: cluster_size=size of cluster (assuming equal size across all clusters)
#		:p0=prevalence at baseline/first survey
#		: rel_increase=expected % change in prevalence between the first survey & last survey
#		: ICC=intercluster correlation
#		: sims=number of simulations
#		: alpha=significance level

simulate_power_cluster <- function(num_surveys, n_clusters, cluster_size,
                                   p0, rel_increase, ICC = 0.01,
                                   sims = 10000, alpha = 0.05, seed = NULL,
                                   verbose = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  if (num_surveys < 2) stop("Need at least 2 surveys")
  
  logit <- function(p) log(p / (1 - p))
  inv_logit <- function(x) 1 / (1 + exp(-x))
  
  # Final prevalence after relative increase
  p_final <- min(p0 * (1 + rel_increase), 0.999)
  
  # Logit-linear slope across surveys
  slope <- (logit(p_final) - logit(p0)) / (num_surveys - 1)
  times <- 0:(num_surveys - 1)
  
  # Function to draw cluster counts from Beta-Binomial
  rbetabinom <- function(n, size, prob, ICC) {
    # Beta parameters from prob and ICC
    alpha <- prob * (1/ICC - 1)
    beta <- (1 - prob) * (1/ICC - 1)
    p_cluster <- rbeta(n, alpha, beta)
    rbinom(n, size, p_cluster)
  }
  
  sig <- 0
  for (i in 1:sims) {
    counts <- numeric(num_surveys)
    for (t in seq_along(times)) {
      p_t <- inv_logit(logit(p0) + slope * times[t])
      cluster_counts <- rbetabinom(n_clusters, cluster_size, p_t, ICC)
      counts[t] <- sum(cluster_counts)
    }
    n_total <- n_clusters * cluster_size
    
    # Build dataset for logistic regression
    df <- data.frame(
      y = counts,
      n = n_total,
      time = times
    )
    
    # Logistic regression with binomial weights
    fit <- try(glm(cbind(y, n - y) ~ time, data = df, family = binomial), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      pval <- summary(fit)$coefficients["time", "Pr(>|z|)"]
      beta <- coef(fit)["time"]
      if (!is.na(pval) && pval < alpha) {
        sig <- sig + 1
      }
    }
    if (verbose && i %% 100 == 0) message("Simulation ", i, " of ", sims)
  }
  power_est <- sig / sims
  return(power_est)
}


#Plots the power curve at for different number of pp surveys

library(ggplot2)
library(cowplot)

working_directory <- "~/Documents/Gabriel/"

setwd(working_directory)

#parse the file containing code for power estimated function: simulate_power_cluster
source("simulate_power_functions.r")
#Generate power curve with no cluster sampling


#Assign input values

n=390  # number of patients  per survey
prev_0=0.078 #baseline_prevalence (week1 prevalence)
relative_increase=-0.654 #(% change in prevalence from wk1 to final week52)

num_pps <- seq(from=2, to=52, by=2)

power_est <- NULL

#for each number of pps, estimate power and store it in the vector power_est
for(n_p in num_pps){
  power <- simulate_power(num_surveys =n_p,  n_per_survey=n, p0 = prev_0, 
                          rel_increase = relative_increase)
  power_est <- c(power_est, power)
}

#Create a dataframe of number of pps and corresponding power

npps_power_nocluster.df <- data.frame(n_pps=num_pps, power=power_est)

#Plot power curves

pc_nocluster <- ggplot(npps_power_nocluster.df, aes(x = n_pps, y = power)) +
  geom_line(color="blue") + 
  scale_x_continuous(breaks=seq(0,max(num_pps), by =2))+ylim(0, 1.0)+
  geom_hline(yintercept = 0.8, linetype="dashed", color="red")+ #show power threshold
  labs(x = "Number of Surveys", y = "Power", title = "A. Power curve without cluster sampling, rel_prev_change=-0.654") +
  theme_light()



#Generate power curve for cluster sampling
#Assign parameter values

K=3   #number of cluster
n=130  #Cluster size
prev=0.091 #baseline_prevalence (week1 prevalence)
relative_increase=-0.666 #(% change in prevalence from wk1 to final week)
ICC=0.05 #intercluster correlation

num_pps <- seq(from=2, to=52, by=2)

power_est <- NULL

#for each number of pps, estimate powe and store it in the vector power_est
for(n_p in num_pps){
  power <- simulate_power_cluster(num_surveys = n_p, n_clusters = K, cluster_size = n, p0 = prev, 
                                  rel_increase = relative_increase, ICC = ICC)
  power_est <- c(power_est, power)
}

#Create a dataframe of number of pps and corresponding power

npps_power.df <- data.frame(n_pps=num_pps, power=power_est)

#Plot power curves

pc_cluster <- ggplot(npps_power.df, aes(x = n_pps, y = power)) +
  geom_line(color="blue") + 
  scale_x_continuous(breaks=seq(0,max(num_pps), by =2))+ylim(0, 1.0)+
  geom_hline(yintercept = 0.8, linetype="dashed", color="red")+ #show power threshold
  labs(x = "Number of Surveys", y = "Power", title = "Power curve with cluster sampling, rel_prev_change=-0.666") +
  theme_light()

print(pc_cluster)
cowplot::plot_grid(pc_nocluster, pc_cluster)



# COMPUTING FOR DIFFERENT SCENARION from the 95% CI

library(ggplot2)
library(cowplot)

# Load power functions
source("simulate_power_functions.r")

# Parameters
n_simple  <- 390        # sample size per survey (no clustering)
p0        <- 0.083       # baseline prevalence
rel_list  <- c(-0.872, 0.163)   # scenarios

# Cluster parameters
K         <- 3          # number of clusters
cluster_n <- 130        # size of each cluster
ICC       <- 0.05

# Range of number of PPS
num_pps <- seq(2, 52, by = 2)

# A list to store all plots
plots_nocluster <- list()
plots_cluster   <- list()

# -----------------------------
# LOOP THROUGH ALL SCENARIOS
# -----------------------------
for (rel in rel_list) {
  
  # ----- 1. NO-CLUSTER POWER -----
  power_est <- c()
  for (n_p in num_pps) {
    pwr <- simulate_power(
      num_surveys = n_p,
      n_per_survey = n_simple,
      p0 = p0,
      rel_increase = rel
    )
    power_est <- c(power_est, pwr)
  }
  
  df_no_cluster <- data.frame(
    n_pps = num_pps,
    power = power_est,
    scenario = paste0("rel_increase = ", rel)
  )
  
  p_nc <- ggplot(df_no_cluster, aes(x = n_pps, y = power)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = num_pps) +
    ylim(0, 1) +
    labs(
      x = "Number of Surveys",
      y = "Power",
      title = paste("Power Curve (No Clustering), rel_prev_change =", rel)
    ) +
    theme_light()
  
  plots_nocluster[[paste0("rel_", rel)]] <- p_nc
  
  
  # ----- 2. CLUSTER-SAMPLING POWER -----
  power_est <- c()
  for (n_p in num_pps) {
    pwr <- simulate_power_cluster(
      num_surveys = n_p,
      n_clusters = K,
      cluster_size = cluster_n,
      p0 = p0,
      rel_increase = rel,
      ICC = ICC
    )
    power_est <- c(power_est, pwr)
  }
  
  df_cluster <- data.frame(
    n_pps = num_pps,
    power = power_est,
    scenario = paste0("rel_increase = ", rel)
  )
  
  p_cl <- ggplot(df_cluster, aes(x = n_pps, y = power)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = num_pps) +
    ylim(0, 1) +
    labs(
      x = "Number of Surveys",
      y = "Power",
      title = paste("Power Curve (Cluster Sampling), rel_prev_change =", rel)
    ) +
    theme_light()
  
  plots_cluster[[paste0("rel_", rel)]] <- p_cl
}

# Example: Display the first scenario (relative change = )

cowplot::plot_grid(plots_nocluster[[1]], plots_cluster[[1]])
cowplot::plot_grid(plots_nocluster[[2]], plots_cluster[[2]])




# Merge All Paired Plots Into One Multi-Panel Figure

library(cowplot)
library(ggplot2)

# -------------------------------------------------------------
# AUTOMATED MERGING OF PLOT PAIRS
# plots_nocluster and plots_cluster must already exist
# -------------------------------------------------------------

# Determine how many scenarios are available
n_plots <- length(plots_nocluster)

# Create a list to hold merged (side-by-side) panels
merged_pairs <- vector("list", n_plots)

# Loop through each index and merge the corresponding pair
for (i in seq_len(n_plots)) {
  merged_pairs[[i]] <- plot_grid(
    plots_nocluster[[i]],
    plots_cluster[[i]],
    ncol = 2,
    labels = c("", ""),
    label_size = 10
  )
}

# -------------------------------------------------------------
# Combine all merged pairs into a single multi-panel figure
# -------------------------------------------------------------
final_multifigure <- plot_grid(
  plotlist = merged_pairs,
  ncol = 1,
  labels = LETTERS[1:n_plots],  # A, B, C, ...
  label_size = 10,
  label_fontface = "bold"
)

# Save final figure
ggsave(
  "merged_power_curves_all_panels.png",
  final_multifigure,
  width = 12,
  height = 4 * n_plots,   # adjust height automatically
  dpi = 300
)

# Display in R
final_multifigure


# Merging all the scenario

library(cowplot)

#-----------------------------------------------------------
# 1. Merge the two comparison plots
#-----------------------------------------------------------

merged_pc <- cowplot::plot_grid(
  pc_nocluster,
  pc_cluster,
  ncol = 2,
  labels = c("", ""),
  label_size = 10
)

#-----------------------------------------------------------
# 2. Insert merged_pc into the list of merged pairs
#-----------------------------------------------------------

# Add the new merged_pc plot to the list
merged_pairs_extended <- c(list(merged_pc), merged_pairs)

# Number of total plots
n_plots <- length(merged_pairs_extended)

#-----------------------------------------------------------
# 3. Create the final multi-figure layout
#-----------------------------------------------------------

final_multifigure <- cowplot::plot_grid(
  plotlist = merged_pairs_extended,
  ncol = 1,
  labels = LETTERS[1:n_plots],
  label_size = 10,
  label_fontface = "bold"
)

# Display
final_multifigure



# SENSITIVITY ANALYSIS WITH NEW DATASET
# Packages
library(readxl)
library(dplyr)
install.packages("janitor")
library(janitor)

# Read data
pps <- read_excel("/Users/gabrielbunduki/Documents/pps2.xlsx", sheet = "PPS2") %>%
  clean_names()

# Recode site and ward
pps <- pps %>%
  mutate(
    site_name = case_when(
      site == 1 ~ "QECH",
      site == 2 ~ "ZCH",
      TRUE ~ NA_character_
    ),
    ward_name = case_when(
      ward == 1 ~ "Surgical",
      ward == 2 ~ "Medical",
      TRUE ~ NA_character_
    )
  )

# 1. Weekly summary per hospital and ward
weekly_summary <- pps %>%
  group_by(week, site_name, ward_name) %>%
  summarise(
    total_swound = sum(swound, na.rm = TRUE),
    total_admissions = sum(total, na.rm = TRUE),
    proportion_swound = total_swound / total_admissions,
    .groups = "drop"
  )

print(weekly_summary)

# 2. Weekly surgical ward proportions per hospital
surgical_weekly_prop <- pps %>%
  filter(ward == 1) %>%
  group_by(week, site_name) %>%
  summarise(
    total_swound = sum(swound, na.rm = TRUE),
    total_admissions = sum(total, na.rm = TRUE),
    proportion_swound = total_swound / total_admissions,
    .groups = "drop"
  )

print(surgical_weekly_prop)

# 3. Median and interquartile range of weekly surgical wound proportions
surgical_median_iqr <- surgical_weekly_prop %>%
  group_by(site_name) %>%
  summarise(
    median_proportion = median(proportion_swound, na.rm = TRUE),
    q1 = quantile(proportion_swound, 0.25, na.rm = TRUE),
    q3 = quantile(proportion_swound, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

print(surgical_median_iqr)

library(dplyr)
library(tibble)

# Median, Q1, Q3 proportions from surgical_weekly_prop
iqr_props <- surgical_weekly_prop %>%
  group_by(site_name) %>%
  summarise(
    q1 = quantile(proportion_swound, 0.25, na.rm = TRUE),
    median = median(proportion_swound, na.rm = TRUE),
    q3 = quantile(proportion_swound, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Hospital totals
hospital_totals <- tibble(
  hospital = c("QECH", "ZCH", "CDH"),
  total_patients_surgical_ward = c(6030, 5426, 1280),
  total_ssi = c(208, 154, 89),
  iqr_source = c("QECH", "ZCH", "ZCH") # CDH uses ZCH median/IQR
)

# Sensitivity analysis
sensitivity_analysis <- hospital_totals %>%
  left_join(iqr_props, by = c("iqr_source" = "site_name")) %>%
  tidyr::pivot_longer(
    cols = c(q1, median, q3),
    names_to = "scenario",
    values_to = "iqr_proportion"
  ) %>%
  mutate(
    adjusted_denominator = total_patients_surgical_ward * iqr_proportion,
    prevalence = total_ssi / adjusted_denominator,
    prevalence_percent = prevalence * 100
  )

print(sensitivity_analysis)





# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(janitor)
library(flextable)
install.packages("flextable")
library(officer)

# Read dataset
pps <- read_excel("/Users/gabrielbunduki/Documents/pps2.xlsx", sheet = "PPS2") %>%
  clean_names()

# Recode hospital and ward
pps <- pps %>%
  mutate(
    site_name = case_when(
      site == 1 ~ "QECH",
      site == 2 ~ "ZCH",
      TRUE ~ NA_character_
    ),
    ward_name = case_when(
      ward == 1 ~ "Surgical",
      ward == 2 ~ "Medical",
      TRUE ~ NA_character_
    )
  )

# Weekly surgical ward proportion of swound / total
surgical_weekly_prop <- pps %>%
  filter(ward == 1) %>%
  group_by(week, site_name) %>%
  summarise(
    total_swound = sum(swound, na.rm = TRUE),
    total_admissions = sum(total, na.rm = TRUE),
    proportion_swound = total_swound / total_admissions,
    .groups = "drop"
  )

# Median, Q1, Q3 proportions by hospital
iqr_props <- surgical_weekly_prop %>%
  group_by(site_name) %>%
  summarise(
    q1 = quantile(proportion_swound, 0.25, na.rm = TRUE),
    median = median(proportion_swound, na.rm = TRUE),
    q3 = quantile(proportion_swound, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Hospital totals
hospital_totals <- tibble(
  hospital = c("QECH", "ZCH", "CDH"),
  total_patients_surgical_ward = c(6030, 5426, 1280),
  total_ssi = c(208, 154, 89),
  iqr_source = c("QECH", "ZCH", "ZCH") # CDH uses ZCH median/IQR
)

# Sensitivity analysis
sensitivity_analysis <- hospital_totals %>%
  left_join(iqr_props, by = c("iqr_source" = "site_name")) %>%
  pivot_longer(
    cols = c(q1, median, q3),
    names_to = "scenario",
    values_to = "iqr_proportion"
  ) %>%
  mutate(
    scenario = case_when(
      scenario == "q1" ~ "Q1",
      scenario == "median" ~ "Median",
      scenario == "q3" ~ "Q3"
    ),
    adjusted_denominator = total_patients_surgical_ward * iqr_proportion,
    prevalence = total_ssi / adjusted_denominator,
    prevalence_percent = prevalence * 100
  ) %>%
  select(
    Hospital = hospital,
    `IQR Source` = iqr_source,
    Scenario = scenario,
    `IQR Proportion` = iqr_proportion,
    `Total Surgical Ward Patients` = total_patients_surgical_ward,
    `Total SSI` = total_ssi,
    `Adjusted Denominator` = adjusted_denominator,
    Prevalence = prevalence,
    `Prevalence (%)` = prevalence_percent
  )

# Format table for Word
sensitivity_table <- sensitivity_analysis %>%
  mutate(
    `IQR Proportion` = round(`IQR Proportion`, 3),
    `Adjusted Denominator` = round(`Adjusted Denominator`, 1),
    Prevalence = round(Prevalence, 3),
    `Prevalence (%)` = round(`Prevalence (%)`, 1)
  )

# Create flextable
ft <- flextable(sensitivity_table) %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  align(j = "Hospital", align = "left", part = "all") %>%
  bold(part = "header") %>%
  set_caption("Sensitivity analysis of SSI prevalence by hospital")

# Export to Word
doc <- read_docx() %>%
  body_add_par("Sensitivity Analysis of SSI Prevalence", style = "heading 1") %>%
  body_add_par(
    "Prevalence was calculated as total SSI divided by total surgical ward patients multiplied by the IQR proportion. CDH used the median, Q1, and Q3 proportions from ZCH.",
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc, target = "sensitivity_analysis_table.docx")


