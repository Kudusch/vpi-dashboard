rmarkdown::render(
    paste0("report.Rmd"), 
    knit_root_dir = "/Users/kudusch/Desktop/vpi-dashboard",
    output_format = rmarkdown::html_document(
        theme = NULL,
        mathjax = NULL,
        highlight = NULL,
        css = "style.css"
    ),
    output_file = "index.html"
)

