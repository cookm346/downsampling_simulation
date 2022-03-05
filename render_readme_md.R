library(rmarkdown)

render("downsampling_simulation.Rmd", 
       md_document(variant = "markdown_github"), 
       output_file = "README.md")