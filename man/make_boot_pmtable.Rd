% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-boot-pmtable.R
\name{make_boot_pmtable}
\alias{make_boot_pmtable}
\title{Generate a complete bootstrap parameter table ready for rendering via \code{pmtables}}
\usage{
make_boot_pmtable(
  .df,
  .pmtype = c("full", "fixed", "structural", "covariate", "random"),
  .span_model_label = "Final model",
  .span_boot_label = "Non-parametric bootstrap",
  .drop_model_ci = TRUE,
  .width = 1
)
}
\arguments{
\item{.df}{Combined dataset of model and bootstrap parameter estimates. See
examples.}

\item{.pmtype}{Parameter table type. Options include:
\itemize{
\item \code{"full"} (all rows in \code{.df} retained in pmtable). This is the default.
\item \code{"fixed"} (all rows with type = "Struct" or "effect"),
\item \code{"structural"} (all rows with type = "Struct"),
\item \code{"covariate"} (all rows with type = "effect"),
\item \code{"random"} (all rows with greek = "Omega" or type = "Resid").
}}

\item{.span_model_label}{A label for the span above columns relating to the
model that was bootstrapped.}

\item{.span_boot_label}{A label for the span above columns relating to the
confidence interval of bootstrap estimates.}

\item{.drop_model_ci}{Logical (\code{T/F}). If \code{TRUE} (the default), drop original
CI columns (\code{ci_[x]}).}

\item{.width}{Notes width. Defaults to 1.}
}
\description{
Generate a complete bootstrap parameter table ready for rendering via \code{pmtables}
}
\details{
Generates specific parameter tables by filtering and using \code{pmtables}. This
function expects a data.frame with both the regular parameter estimates \emph{and}
the bootstrap parameter estimates. See "Examples" for more detail.

This function:
\enumerate{
\item Filters to columns needed for specific parameter tables
\item Panels by "type"
\item Makes \code{"abb"}, \code{"greek"}, \code{"desc"} blank (no title)
\itemize{
\item Note that description is removed when \code{.pmtype = "random"}. See
\code{?pmtables::st_mutate()} if you want to add it back in.
}
\item Attaches notes
\item Rename "value" to "Estimate" and "shrinkage" to "Shrinkage (\%)", if
applicable
}

\strong{Note:}
If these \code{pmtables} settings do not work for your parameter table, you can
overwrite them afterwards using desired \code{pmtables} commands.
}
\examples{

\dontrun{

model_dir <- system.file("model/nonmem", package = "pmparams")
paramKey <-  file.path(model_dir, "pk-parameter-key-new.yaml")

# Parameter estimates
mod <- bbr::read_model(file.path(model_dir, "106"))
param_df <- define_param_table(
 .estimates = mod,
 .key = paramKey,
) \%>\% format_param_table(.prse = TRUE)

# Bootstrap estimates
boot_run <- bbr::read_model(file.path(model_dir, "106-boot"))
boot_df <- define_boot_table(
 .boot_estimates = bbr::bootstrap_estimates(boot_run),
 .key = paramKey
) \%>\% format_boot_table()

# Combine parameter estimates with bootstrap estimates
combine_df <- dplyr::left_join(param_df, boot_df)

# Fixed effects table
make_boot_pmtable(.df = combine_df, .pmtype = "fixed") \%>\%
 pmtables::stable() \%>\%
 # preview in Rstudio viewer (requires `magick` and `pdftools`)
 pmtables::st_as_image(border = "0.8cm 0.7cm 1.8cm 0.8cm")


# Random effects table
make_boot_pmtable(.df = combine_df, .pmtype = "random") \%>\%
 pmtables::stable() \%>\%
 pmtables::st_as_image(border = "0.8cm 0.7cm")
}
}
\seealso{
\code{\link[=make_pmtable]{make_pmtable()}}, \code{\link[=boot_notes]{boot_notes()}}
}
