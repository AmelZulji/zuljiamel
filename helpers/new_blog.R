create_new_blog_post <- function(
    title,
    author = "Amel Zulji",
    description = "Describe blog here",
    date = Sys.Date(),
    categories = NULL,
    draft = TRUE,
    output_path = "posts"
) {
  # Create a safe file name
  file_slug <- gsub(" ", "-", tolower(title))
  file_slug <- gsub("[^a-z0-9\\-]", "", file_slug)
  post_dir <- file.path(output_path, file_slug)
  dir.create(post_dir, showWarnings = FALSE, recursive = TRUE)
  
  # YAML front matter
  yaml_lines <- c(
    "---",
    paste0("author: ", author),
    paste0('title: "', title, '"'),
    paste0("description: ", description),
    paste0("date: ", format(date, "%Y-%m-%d")),
    if (!is.null(categories))
      paste0("categories: [", paste(categories, collapse = ", "), "]"),
    paste0("draft: ", tolower(as.character(draft))),
    "---",
    "",
    "```{r}",
    'renv::use(lockfile = "renv.lock", verbose = FALSE)',
    "```",
    "",
    "```{r}",
    "suppressPackageStartupMessages({",
    "",
    "})",
    "```",
    "",
    "## Introduction",
    "",
    "Write your post content here..."
  )
  
  # Write to index.qmd
  index_path <- file.path(post_dir, "index.qmd")
  writeLines(yaml_lines, index_path)
  message("Blog post created at: ", index_path)
}

get_blog_dependencies <- function(x) {
  fp <- rstudioapi::getActiveDocumentContext()[["path"]]
  fp_dir <- dirname(fp)
  renv_lock_path <- file.path(fp_dir, "renv.lock")
  dep <- renv::dependencies(fp)[["Package"]]
  renv::snapshot(prompt = FALSE,
                 packages = dep,
                 lockfile = renv_lock_path)
}
