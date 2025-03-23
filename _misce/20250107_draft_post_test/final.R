suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(gt)
  library(scales)
})


# define function for computing maximum bin count
max_bin_count <- function(x, binwidth = NULL, nbin = NULL) {
  min_bound <- x %>% min() %>% floor()
  max_bound <- x %>% max() %>% ceiling() + binwidth
  breaks <- seq(min_bound, max_bound, binwidth) - binwidth / 2
  bins = cut(x, breaks = breaks)
  # if you want max bins do bellow
  table(bins) %>% max()
}

# define bins
nbin <- 40
binwidth <- 0.02

# define theme for histogram
ct <- theme(
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(), 
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  strip.text = element_text(size = 12)
)

# Define function for simulating population
func <- c(
  "normal" = \(x) rnorm(x, mean = 0.5, sd = 0.1)
)

# Define the size of population
N <- 100000

# simulate populations
population <- 
  expand_grid(N, func) %>% # make a grid
  mutate(distribution = names(func), N = set_names(N, names(func))) %>%
  rowwise() %>% 
  mutate(population = map(N, func)) %>% 
  mutate(
    across(
      population,
      list(
        mu = mean,
        sigma = sd,
        min = min,
        max = max,
        max_bin = \(x) max_bin_count(x, binwidth = binwidth)
      ),
      .names = "{.fn}"
    )
  ) %>% 
  mutate(
    histo_data = list(tibble(value = population)),
    area_data = list(tibble(x = c(mu - sigma, mu + sigma),y = c(max_bin, max_bin))),
    segment_data = list(tibble(x = mu, y = 0, yend = max_bin)),
    limits = list(c(min, max))
  ) %>% ungroup()

population_plot <- population %>% 
  mutate(
    histogram = 
      pmap(list(histo_data, area_data, segment_data, limits, distribution), \(h,a,s,l,d) {
        h %>% 
          ggplot(aes(value)) + 
          geom_histogram(binwidth = binwidth, fill = "lightblue", color = "gray65") +
          geom_area(data = a, aes(x = x, y = y, fill = "mu %+-% sigma"), alpha = 0.4) +
          geom_segment(data = s, aes(x = x, y = y, yend = yend, linetype = "mu"), color = "red", linewidth = 0.8) +
          scale_fill_manual(values = "green3", name = "Spread", labels = label_parse()) +
          scale_linetype_manual(values = "22", name = "Centrality", labels = label_parse()) +
          scale_x_continuous(limits = l) +
          facet_grid(cols = vars({d})) +
          ct
      })) %>% 
  select(N, distribution, histogram)


n <- c(5)
sample_id <- paste0("X", 1:10000)

sample_grid <- 
  expand_grid(
    population %>% select(distribution, limits), 
    n, 
    sample_id
  )

sample_groups <- 
  sample_grid %>% 
  rowwise() %>% 
  mutate(
    sample = list(sample(population$population[[distribution]], size = n)), 
    xbar = mean(sample)
  ) %>% 
  ungroup()

# compute statistics
samples <- sample_groups %>%
  mutate(
    .by = c(distribution, n),
    sigma_xbar = sd(xbar),
    mu_xbar = mean(xbar),
    max_bin = max_bin_count(xbar, binwidth = binwidth)
  ) %>% 
  nest(.by = -matches("^(sample_id|sample|xbar)$"), histo_data = xbar) %>% 
  rowwise() %>% 
  mutate(
    area_data = list(tibble(x = c(mu_xbar - sigma_xbar, mu_xbar + sigma_xbar),y = c(max_bin, max_bin))),
    segment_data = list(tibble(x = mu_xbar, y = 0, yend = max_bin))
  ) %>% ungroup()

sample_plots <- samples %>% 
  mutate(
    histogram = 
      pmap(list(histo_data, area_data, segment_data, limits, n), \(h, a, s, l, n) {
        facet_row <- str_glue("n = {n}")
        h %>% 
          ggplot(aes(xbar)) + 
          geom_histogram(binwidth = binwidth, fill = "lightblue", color = "gray65") + 
          geom_area(data = a, aes(x = x, y = y, fill = "mu[bar(x)] %+-% S.E.M"), alpha = 0.35) + 
          geom_segment(data = s, aes(x = x, y = 0, yend = yend, linetype = "mu[bar(x)]"), color = "blue", linewidth = 0.8) +
          scale_fill_manual(values = "orange", name = "Spread", labels = label_parse()) +
          scale_linetype_manual(values = "22", name = "Centrality", labels = label_parse()) +
          # ggtitle(str_glue("n = {n}")) +
          facet_grid(rows = vars({facet_row}), switch = "y") +
          coord_cartesian(xlim = l) +
          ct
      })) %>% 
  select(n,distribution,histogram)

sample_plots <- 
  sample_plots %>% 
  mutate(
    histogram = case_when(
      distribution != "normal" ~ map(histogram, \(x) x + theme(strip.text.y = element_blank())),
      T ~ histogram
    )
  )

merge_plot <- bind_rows(population_plot,sample_plots) %>% arrange(distribution)
num_col <- merge_plot$distribution %>% unique() %>% length()
num_row <- nrow(merge_plot) / num_col

pfin <- wrap_plots(merge_plot$histogram, nrow = num_row, ncol = num_col, byrow = FALSE, guides = "collect")


sample_groups <- sample_groups %>% filter(row_number()<6)
sample_groups <- 
  sample_groups %>% 
  mutate(Samples = map(sample, \(x) x %>% round(2) %>% str_c(collapse = ", "))) %>% 
  mutate(Samples = str_glue("{sample_id} = [{Samples}]")) %>% 
  select(Samples, xbar)



sample_groups %>%
  gt() %>% 
  cols_label(xbar = "{{x\u0304}}") %>% 
  cols_align(align = "center") %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body()
  )
