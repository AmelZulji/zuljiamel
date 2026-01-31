#!/bin/bash

# 1. Capture Arguments
if [ "$#" -ne 4 ]; then
echo "Usage: $0 <target_dir> <web_name> <blog_name> <book_name>"
echo "Example: $0 . 'My Professional Site' 'posts' 'guidebook'"
exit 1
fi

TARGET_DIR=$1
WEB_NAME=$2
BLOG_NAME=$3
BOOK_NAME=$4

echo "--- Initializing Meta-Project: $WEB_NAME in $TARGET_DIR ---"

# Create the projects using [dir] [name], this avoids "$TARGET_DIR/$BLOG_NAME" as blog name in metadata
quarto create project website "$TARGET_DIR" "$WEB_NAME" --quiet --no-prompt
quarto create project blog "$TARGET_DIR/$BLOG_NAME" "$BLOG_NAME" --quiet --no-prompt
quarto create project book "$TARGET_DIR/$BOOK_NAME" "$BOOK_NAME" --quiet --no-prompt

# Create the render_book.R to prerender book
cat <<EOF > "$TARGET_DIR/render_book.R"
var1 <- Sys.getenv("QUARTO_PROJECT_RENDER_ALL")

if (var1 == "") {
  quit()
}

# Renders the book from its subdirectory relative to this script
quarto::quarto_render(input = "$BOOK_NAME/")
EOF

# configure main _quarto.yml to include blog and book
cat <<EOF > "$TARGET_DIR/_quarto.yml"
project:
  type: website
pre-render: render_book.R
render:
  - "*.qmd"
- "!$BOOK_NAME/"

resources: 
  - "$BOOK_NAME/_book"

website:
  title: "$WEB_NAME"
site-url: https://your-website-url.example.com
navbar:
  left:
  - href: index.qmd
text: Home
- about.qmd
- href: $BLOG_NAME/index.qmd
text: Blog
- href: $BOOK_NAME/_book/index.html
text: Book

format:
  html:
  theme: cosmo
css: styles.css
toc: true
EOF

echo "setup complete"