library(ggplot2)

# =========================================================================
# 1. STATS
# =========================================================================

# Helper to close density polygon
close_density_poly <- function(df, lower, upper, label) {
  sub_df <- df[df$x >= lower & df$x <= upper, , drop = FALSE]
  if (nrow(sub_df) == 0) return(NULL)
  # Add anchors at y=0
  closed_df <- rbind(c(min(sub_df$x), 0), sub_df, c(max(sub_df$x), 0))
  closed_df$fill_group <- label
  return(closed_df)
}

StatDensitySpread <- ggproto("StatDensitySpread", Stat,
                             required_aes = c("x"),
                             compute_group = function(data, scales, sigmas = NULL) {
                               if (is.null(sigmas)) return(data.frame())
                               
                               stats <- list(mu = mean(data$x, na.rm=TRUE), sigma = sd(data$x, na.rm=TRUE))
                               dens  <- density(data$x, na.rm=TRUE)
                               df_base <- data.frame(x = dens$x, y = dens$y)
                               
                               sigmas <- sort(sigmas, decreasing = TRUE)
                               
                               polys <- lapply(sigmas, function(s) {
                                 close_density_poly(df_base, 
                                                    stats$mu - (stats$sigma * s), 
                                                    stats$mu + (stats$sigma * s), 
                                                    paste0("sigma_", s))
                               })
                               
                               final_df <- do.call(rbind, polys)
                               if (!is.null(final_df)) {
                                 final_df$fill_group <- factor(final_df$fill_group, levels = paste0("sigma_", sigmas))
                               }
                               return(final_df)
                             }
)

StatDensitySegmentMulti <- ggproto("StatDensitySegmentMulti", Stat,
                                   required_aes = c("x"),
                                   compute_group = function(data, scales, segments = NULL) {
                                     if (is.null(segments)) return(data.frame())
                                     if (!is.list(segments)) segments <- list(segments)
                                     
                                     dens <- density(data$x, na.rm = TRUE)
                                     df_base <- data.frame(x = dens$x, y = dens$y)
                                     
                                     polys <- lapply(seq_along(segments), function(i) {
                                       rng <- segments[[i]]
                                       if (length(rng) != 2) return(NULL)
                                       close_density_poly(df_base, min(rng), max(rng), paste0("segment_", i))
                                     })
                                     
                                     final_df <- do.call(rbind, polys)
                                     
                                     # FIX: Force factor levels to match input order (1, 2, 3...)
                                     # This prevents alphabetical sorting messing up the color mapping
                                     if (!is.null(final_df)) {
                                       final_df$fill_group <- factor(final_df$fill_group, levels = paste0("segment_", seq_along(segments)))
                                     }
                                     return(final_df)
                                   }
)

StatDensityMetric <- ggproto("StatDensityMetric", Stat,
                             required_aes = c("x"),
                             compute_group = function(data, scales, metrics = c(), annotation_type = "population") {
                               if (length(metrics) == 0) return(data.frame())
                               dens <- density(data$x, na.rm = TRUE)
                               
                               # Handle NULL annotation type safely
                               type_str <- if (is.null(annotation_type)) "default" else as.character(annotation_type)
                               
                               # Clean Label Map (No Bold)
                               label_map <- switch(type_str,
                                                   "population" = c(mean = "mu", median = "Median"),
                                                   "sample"     = c(mean = "x_bar", median = "Median"),
                                                   "sampling_distribution" = c(mean = "mu_x_bar", median = "Median"),
                                                   c(mean = "Mean", median = "Median")
                               )
                               
                               results <- lapply(metrics, function(m) {
                                 val <- if (m == "mean") mean(data$x, na.rm=TRUE) else median(data$x, na.rm=TRUE)
                                 lbl <- if (!is.null(label_map[[m]])) label_map[[m]] else tools::toTitleCase(m)
                                 
                                 data.frame(
                                   x = val, xend = val, 
                                   y = 0, yend = approx(dens$x, dens$y, xout = val)$y,
                                   metric_label = lbl
                                 )
                               })
                               do.call(rbind, results)
                             }
)

# =========================================================================
# 2. MASTER GEOM
# =========================================================================

geom_density_analysis <- function(mapping = NULL, data = NULL, ...,
                                  sigmas = c(1, 2), 
                                  segments = NULL, 
                                  show_mean = FALSE, show_median = FALSE,
                                  alpha = 0.6, 
                                  line_color = "black", line_alpha = 1.0, 
                                  sigma_colors = c("#1f77b4", "#6baed6", "#c6dbef"),
                                  segment_fill = "orange", 
                                  annotation_type = "population") {
  
  layers <- list(geom_density(mapping = mapping, data = data, fill = NA, ...))
  palette_vals <- character(0) # Start empty
  
  # --- 1. Build Color Palette (Robust Naming) ---
  if (!is.null(sigmas)) {
    s_sorted <- sort(sigmas)
    cols <- rep_len(sigma_colors, length(s_sorted))
    names(cols) <- paste0("sigma_", s_sorted)
    palette_vals <- c(palette_vals, cols)
  }
  
  if (!is.null(segments)) {
    if (!is.list(segments)) segments <- list(segments)
    # Explicitly map segment_1 -> Color 1, segment_2 -> Color 2
    cols <- rep_len(segment_fill, length(segments))
    names(cols) <- paste0("segment_", seq_along(segments))
    palette_vals <- c(palette_vals, cols)
  }
  
  # --- 2. Labeller (No Bold) ---
  master_labeller <- function(breaks) {
    lapply(breaks, function(brk) {
      if (grepl("sigma_", brk)) {
        s <- as.numeric(gsub("sigma_", "", brk))
        if (is.null(annotation_type)) return(paste0("Mean \u00B1 ", s, " sd"))
        
        # Standard expressions (No bold)
        if (annotation_type == "population") return(bquote(mu %+-% .(s)*sigma))
        if (annotation_type == "sample") return(bquote(bar(x) %+-% .(s)*s))
        if (annotation_type == "sampling_distribution") return(bquote(mu[bar(x)] %+-% .(s)*sigma[bar(x)]))
        return(paste0("Mean \u00B1 ", s, " sd"))
      }
      
      if (grepl("segment_", brk)) {
        idx <- as.numeric(gsub("segment_", "", brk))
        if (idx <= length(segments)) {
          nm <- names(segments)[idx]
          if (!is.null(nm) && nm != "") return(nm)
          rng <- segments[[idx]]
          return(paste0("[", round(min(rng), 2), " \u2013 ", round(max(rng), 2), "]"))
        }
      }
      return(brk)
    })
  }
  
  add_layer <- function(stat, group_aes = FALSE, params) {
    aes_args <- list(fill = quote(after_stat(fill_group)))
    if (group_aes) aes_args$group <- quote(after_stat(fill_group))
    new_map <- do.call(aes, aes_args)
    if (!is.null(mapping)) new_map <- modifyList(mapping, new_map)
    do.call(geom_polygon, c(list(stat = stat, mapping = new_map, data = data, color = NA), params))
  }
  
  if (!is.null(sigmas)) {
    layers[[length(layers)+1]] <- add_layer(StatDensitySpread, TRUE, list(sigmas=sigmas, alpha=alpha))
  }
  if (!is.null(segments)) {
    layers[[length(layers)+1]] <- add_layer(StatDensitySegmentMulti, FALSE, list(segments=segments, alpha=alpha))
  }
  
  if (length(palette_vals) > 0) {
    layers[[length(layers)+1]] <- scale_fill_manual(
      name = "Area", values = palette_vals, labels = master_labeller,
      guide = guide_legend(override.aes = list(linetype = 0, color = NA))
    )
  }
  
  metrics <- c(if(show_mean) "mean", if(show_median) "median")
  if (length(metrics) > 0) {
    lmap <- aes(linetype = after_stat(metric_label))
    if (!is.null(mapping)) lmap <- modifyList(mapping, lmap)
    layers[[length(layers)+1]] <- geom_segment(
      stat = StatDensityMetric, mapping = lmap, data = data,
      metrics = metrics, annotation_type = annotation_type,
      linewidth = 1, color = line_color, alpha = line_alpha, show.legend = TRUE
    )
    
    lt_vals <- c("Median" = "22", "mu" = "solid", "x_bar" = "solid", "mu_x_bar" = "solid", "Mean" = "solid")
    
    # Label Map (No Bold)
    lbl_map <- list("Median" = "Median")
    if (is.null(annotation_type)) {
      lbl_map[["Mean"]] <- "Mean"
    } else if (annotation_type == "population") {
      lbl_map[["mu"]] <- expression(mu)
    } else if (annotation_type == "sample") {
      lbl_map[["x_bar"]] <- expression(bar(x))
    } else if (annotation_type == "sampling_distribution") {
      lbl_map[["mu_x_bar"]] <- expression(mu[bar(x)])
    } else {
      lbl_map[["Mean"]] <- "Mean"
    }
    
    layers[[length(layers)+1]] <- scale_linetype_manual(
      name = "Statistics", values = lt_vals, labels = lbl_map
    )
  }
  
  return(layers)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

set.seed(123)
df <- data.frame(x = rnorm(1000))

ggplot(df, aes(x = x)) +
  geom_density_analysis(
    sigmas = NULL, 
    
    # Two separate segments
    segments = list("Rejection Region" = c(1.645, Inf), 
                    "Warning"          = c(1, 1.645)),
    
    # Two distinct colors -> Should map correctly now
    segment_fill = c("red", "orange"),
    
    show_mean = TRUE,
    show_median = TRUE,
    annotation_type = "population"
  ) +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  
  # Increases text size, but does NOT bold the symbols
  theme(legend.text = element_text(size = 12)) +
  
  labs(title = "Final Verification", 
       subtitle = "Rejection (Red) vs Warning (Orange) | No Bold Symbols")
