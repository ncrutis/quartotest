directory <- "/home/ni3853/Documents/quartoproj"
#prerun templates
names(templates)[which(names(templates) == "Rproj")] <-
  glue::glue("{basename(directory)}.Rproj")
names(templates) <- file.path(directory, names(templates))
project_files <- names(templates)

# Create subdirectories
subdirs <- file.path(directory, c("analysis", "code", "data", "docs",
                                  "output"))

fs::dir_create(subdirs)

for (fname in project_files) {

    cat(glue::glue(templates[[fname]]), file = fname)
  }

