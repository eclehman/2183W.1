library(here)
library(ggplot2)
library(tidyverse)
set.seed(783)

customer_df <- read.csv(here('customer.csv'))
customer_df <- customer_df %>% 
  mutate(x = c(1:20)) %>%
  mutate(across(c(A, B, C), sort)) %>% 
  select(x, A, B, C)

# --------------- Branch A -----------------
shapiro.test(customer_df$A)
# With W = 0.945, and p-value = 0.296, this sample can be reasonably assumed
# to be normal. A t-test will be used to confirm if the mean is 100.

t.test(customer_df$A, alternative = c('two.sided'), mu = 100)

# t.test resulted in a p-value > 0.05, therefore, the Null Hypothesis of 
# the µ = 100 will fail to be rejected.

plot_a <- customer_df %>%
  select(x, A) %>% 
  ggplot(aes(x = A)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = ceiling(sqrt(nrow(customer_df))),
    fill = '#F8E08E',
    color = '#033c5a',
    alpha = 0.5,
    show.legend = FALSE
  ) +
  stat_density(                       
    aes(color = 'Actual Distribution'),
    geom = 'line',                    
    linewidth = 1
  ) +
  stat_function(
    aes(color = 'Normal Distribution (µ = 100)'),
    fun = dnorm,
    args = list(mean = 100, sd = sd(customer_df$A)),
    linetype = 'dotted',
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 10)
  ) +
  scale_color_manual(
    name = '',
    values = c(
      'Actual Distribution' = '#0190DB',
      'Normal Distribution (µ = 100)' = '#A75523'
    ),
    guide = guide_legend(
      override.aes = list(
        linetype = c('solid', 'dotted'),
        linewidth = 1
      )
    )
  ) +
  labs(title = 'Customer Service Times',
       subtitle = 'Branch A',
       x = 'Minutes') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, family = 'Times New Roman'),
    plot.subtitle = element_text(hjust = 0.5, family = 'Georgia', face = 'italic'),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'Helvetica'),
    legend.position = c(0.85, 0.95)
  )
plot_a


# --------------- Branch B -----------------
shapiro.test(customer_df$B)
# With W = 0.784, and p-value = 0.000, this sample cannot be assumed to be
# normally distributed. A sign test for the median will be used to confirm if
# the median is 100.

m0 <- 100
diff <- customer_df$B - m0
diff <- diff[diff != 0]
n_pos <- sum(diff > 0)
n_neg <- sum(diff < 0)
n <- n_pos + n_neg

binom.test(n_pos, n, p = 0.5)
# The sign test resulted in a p-value of 1; therefore, the Null hypothesis of 
# the median = 100 will fail to be rejected.

plot_b <- customer_df %>%
  select(x, B) %>% 
  ggplot(aes(x = B)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = ceiling(sqrt(nrow(customer_df))),
    fill = '#F8E08E',
    color = '#033c5a',
    alpha = 0.5,
    show.legend = FALSE
  ) +
  stat_density(                       
    aes(color = 'Actual Distribution'),
    geom = 'line',                    
    linewidth = 1
  ) +
  stat_function(
    aes(color = 'Normal Distribution (M = 100)'),
    fun = dnorm,
    args = list(mean = 100, sd = sd(customer_df$B)),
    linetype = 'dotted',
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 10)
  ) +
  scale_color_manual(
    name = '',
    values = c(
      'Actual Distribution' = '#0190DB',
      'Normal Distribution (M = 100)' = '#A75523'
    ),
    guide = guide_legend(
      override.aes = list(
        linetype = c('solid', 'dotted'),
        linewidth = 1
      )
    )
  ) +
  labs(title = 'Customer Service Times',
       subtitle = 'Branch B',
       x = 'Minutes') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, family = 'Times New Roman'),
    plot.subtitle = element_text(hjust = 0.5, family = 'Georgia', face = 'italic'),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'Helvetica'),
    legend.position = c(0.85, 0.95)
  )
plot_b


# --------------- Branch C -----------------
shapiro.test(customer_df$C)
# With W = 0.939, and p-value = 0.233, this sample can be reasonably assumed
# to be normal. A t-test will be used to confirm if the mean is 100.

t.test(customer_df$C, alternative = c('two.sided'), mu = 100)
# t.test resulted in a p-value < 0.05, therefore, the Null Hypothesis of 
# the µ = 100 will be rejected.

plot_c <- customer_df %>%
  select(x, C) %>% 
  ggplot(aes(x = C)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = ceiling(sqrt(nrow(customer_df))),
    fill = '#F8E08E',
    color = '#033c5a',
    alpha = 0.5,
    show.legend = FALSE
  ) +
  stat_density(                       
    aes(color = 'Actual Distribution'),
    geom = 'line',                    
    linewidth = 1
  ) +
  stat_function(
    aes(color = 'Normal Distribution (µ = 100)'),
    fun = dnorm,
    args = list(mean = 100, sd = sd(customer_df$C)),
    linetype = 'dotted',
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 10)
  ) +
  scale_color_manual(
    name = '',
    values = c(
      'Actual Distribution' = '#0190DB',
      'Normal Distribution (µ = 100)' = '#A75523'
    ),
    guide = guide_legend(
      override.aes = list(
        linetype = c('solid', 'dotted'),
        linewidth = 1
      )
    )
  ) +
  labs(title = 'Customer Service Times',
       subtitle = 'Branch C',
       x = 'Minutes') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, family = 'Times New Roman'),
    plot.subtitle = element_text(hjust = 0.5, family = 'Georgia', face = 'italic'),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'Helvetica'),
    legend.position = c(0.85, 0.95)
  )
plot_c

# --------------- Summarized Plot -----------------
customer_long <- customer_df %>%
  select(A, B, C) %>%
  pivot_longer(
    cols = everything(),
    names_to = 'Branch',
    values_to = 'Minutes'
  )

violin_plot <- customer_long %>% 
  ggplot() +
  aes(x = Branch, y = Minutes, fill = Branch) +
  geom_violin( fill = '#F8E08E',
               color = '#033c5a',
               alpha = 0.5) +
  geom_boxplot(width = 0.1, 
               fill = 'white', 
               outlier.shape = NA) +
  geom_point(alpha = 0.5, color = '#0190DB') +
  geom_hline(yintercept = 100, color = '#A75523', linetype = 'dashed') +
  labs(title = 'Customer Service Times',
       subtitle = 'Cross Branch Comparison',
       y = 'Minutes') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, family = 'Times New Roman'),
    plot.subtitle = element_text(hjust = 0.5, family = 'Georgia', face = 'italic'),
    axis.title.x = element_blank(),
    legend.position = 'none'
  )

# --------------- Plot -----------------
plot_a
plot_b
plot_c
violin_plot



