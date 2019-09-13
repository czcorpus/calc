#!/usr/bin/env Rscript

# get port number from env var; if unset, a random available port will
# be used
port <- as.numeric(Sys.getenv("SHINY_APP_PORT"))
port <- if (is.na(port)) NULL else port
shiny::runApp(host = "0.0.0.0", port = port)
