#!/usr/bin/env Rscript

Sys.setenv(TAR_PROJECT = "data")
targets::tar_make()
Sys.setenv(TAR_PROJECT = "ppi")
targets::tar_make()
Sys.setenv(TAR_PROJECT = "pvi")
targets::tar_make()
Sys.setenv(TAR_PROJECT = "tourism")
targets::tar_make()
Sys.setenv(TAR_PROJECT = "post_mortem")
targets::tar_make()
