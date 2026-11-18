
## Objects for the Quarto report, saved so that the report can be rendered without running the analysis.
## Read by the setup chunk of 'Gambia-NFI-2026-report.qmd'.
## - tab : tables of the 05a-05i scripts
## - fig : figures (ggplot objects) of the 05a-05i scripts
## - info: values quoted in the report text

report_obj <- list(
  tab  = tab,
  fig  = fig,
  info = list(
    ph1_n     = nrow(ph1_data),
    iucn_date = file.mtime("data-anci/iucn/taxon.txt"),
    run_time  = Sys.time()
  )
)

saveRDS(report_obj, "results/report-objects.rds")
