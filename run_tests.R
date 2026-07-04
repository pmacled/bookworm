files <- list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE)
fail <- FALSE
for (f in files) {
  cat("\n== ", f, " ==\n"); r <- tryCatch({ source(f); TRUE },
    error = function(e) { message(conditionMessage(e)); FALSE })
  if (!r) fail <- TRUE
}
if (fail) quit(status = 1)
