var1 <- Sys.getenv("QUARTO_PROJECT_RENDER_ALL")

if (var1 == "") {
    quit()
}

# Renders the book from its subdirectory relative to this script
quarto::quarto_render(input = "Notes/")
