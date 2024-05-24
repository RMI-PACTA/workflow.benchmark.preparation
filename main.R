# load necessary packages ------------------------------------------------------

logger::log_info("Checking for necessary packages.")
is_installed <- function(packagename) {
  packagename %in% utils::installed.packages()[, "Package"]
}

necessary_pkgs <-
  unlist(
    strsplit(
      x = as.data.frame(read.dcf("DESCRIPTION"))$Imports,
      split = ",\\n"
    )
  )

installed_pkgs <- unlist(lapply(X = necessary_pkgs, is_installed))

if (!all(installed_pkgs)) {
  missing_pkgs <- necessary_pkgs[!installed_pkgs]
  logger::log_error("Necessary packages are not installed: {missing_pkgs}")
  stop()
}


logger::log_info("Loading necessary packages.")
suppressPackageStartupMessages({
  library("dplyr")
})


# set general i/o paths --------------------------------------------------------

benchmarks_preparation_inputs_path <-
  Sys.getenv(
    "BENCHMARKS_PREPARATION_INPUTS_PATH",
    "./inputs"
  )

if (dir.exists(benchmarks_preparation_inputs_path)) {
  logger::log_info("Setting benchmarks preparation inputs path: {benchmarks_preparation_inputs_path}")
} else {
  logger::log_error("Benchmarks preparation inputs path does not exist: {benchmarks_preparation_inputs_path}")
  stop()
}

benchmarks_preparation_outputs_path <- Sys.getenv(
  "BENCHMARKS_PREPARATION_OUTPUTS_PATH",
  "./outputs"
)

if (dir.exists(benchmarks_preparation_outputs_path)) {
  logger::log_info("Setting benchmarks preparation outputs path: {benchmarks_preparation_outputs_path}")
} else {
  logger::log_error("Benchmarks preparation outputs path does not exist: {benchmarks_preparation_outputs_path}")
  stop()
}


# load config ------------------------------------------------------------------

logger::log_info("Loading config.")
config <-
  config::get(
    file = "config.yml",
    config = Sys.getenv(x = "R_CONFIG_ACTIVE", unset = "2023Q4"),
    use_parent = FALSE
  )


# set and check paths ----------------------------------------------------------

msci_xlsx_path <-
  file.path(
    benchmarks_preparation_inputs_path,
    config[["msci_filename"]]
  )

if (!dir.exists(msci_xlsx_path) && file.exists(msci_xlsx_path)) {
  logger::log_info("Setting MSCI XLSX input filepath: {msci_xlsx_path}")
} else {
  logger::log_warn("MSCI XLSX input filepath does not exist: {msci_xlsx_path}")
  msci_xlsx_path <- ""
}


# create output list ------------------------------------------------------

benchmark_portfolios <- list()


# scrape iShares indices data --------------------------------------------------

logger::log_info("Scraping iShares indices data")

logger::log_debug("Scraping iShares indices bonds data.")
ishares_indices_bonds <-
  dplyr::bind_rows(
    lapply(
      seq_along(config[["bonds_indices_urls"]]), function(index) {
        pacta.data.scraping::get_ishares_index_data(
          config[["bonds_indices_urls"]][[index]],
          names(config[["bonds_indices_urls"]])[[index]],
          config[["ishares_date"]]
        )
      }
    )
  ) %>%
  pacta.data.scraping::process_ishares_index_data()

logger::log_debug("Scraping iShares indices equity data.")
ishares_indices_equity <-
  dplyr::bind_rows(
    lapply(
      seq_along(config[["equity_indices_urls"]]), function(index) {
        pacta.data.scraping::get_ishares_index_data(
          config[["equity_indices_urls"]][[index]],
          names(config[["equity_indices_urls"]])[[index]],
          config[["ishares_date"]]
        )
      }
    )
  ) %>%
  pacta.data.scraping::process_ishares_index_data()

logger::log_debug("Combining iShares indices data.")
ishares_indices <- bind_rows(ishares_indices_bonds, ishares_indices_equity)

benchmark_portfolios <- c(benchmark_portfolios, list(ishares_indices))


# load MSCI indices ------------------------------------------------------------

if (nzchar(msci_xlsx_path)) {
  sheet_names <- readxl::excel_sheets(path = msci_xlsx_path)[-1]
  logger::log_info("Setting MSCI XLSX sheet names: {sheet_names}")

  logger::log_debug("Reading and combining MSCI indices data.")
  msci_indices <-
    dplyr::bind_rows(
      lapply(sheet_names, function(sheet_name) {
        index_name <- paste0("MSCI - ", sub("_[0-9]*$", "", sheet_name))

        readxl::read_excel(path = msci_xlsx_path, sheet = sheet_name) %>%
          dplyr::mutate(investor_name = "Benchmark Portfolios") %>%
          dplyr::mutate(portfolio_name = index_name) %>%
          dplyr::mutate(currency = "USD") %>%
          dplyr::select(
            investor_name,
            portfolio_name,
            isin = `Isin Code`,
            market_value = `Security Closing Weight`,
            currency
          )
      })
    )

  benchmark_portfolios <- c(benchmark_portfolios, list(msci_indices))
} else {
  logger::log_info("Skipping MSCI XLSX import because input file is not available.")
}


# combine all benchmarks -------------------------------------------------------

benchmark_portfolios <- dplyr::bind_rows(benchmark_portfolios)

if (nzchar(config[["project_code"]])) {
  run_id <-
    paste(
      config[["pacta_financial_timestamp"]],
      config[["project_code"]],
      sep = "_"
    )
} else {
  run_id <- config[["pacta_financial_timestamp"]]
}

benchmarks_investor_name <-
  paste0(
    "Benchmark Portfolios ",
    run_id
  )

benchmark_portfolios <-
  dplyr::mutate(
    benchmark_portfolios,
    investor_name = benchmarks_investor_name
  )


# save output to output path ---------------------------------------------------

run_timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

output_filename <-
  paste0(
    paste(
      run_id,
      "benchmark_portfolios",
      run_timestamp,
      sep = "_"
    ),
    ".rds"
  )

output_filepath <-
  file.path(
    benchmarks_preparation_outputs_path,
    output_filename
  )

logger::log_info("Saving benchmark output file: {output_filepath}")

saveRDS(
  object = benchmark_portfolios,
  file = output_filepath
)

logger::log_info("Benchmark preparation complete!")
