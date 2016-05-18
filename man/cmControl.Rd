\name{cmControl}
\alias{cmControl}
\title{cmControl}
\usage{
cmControl(min_perf, metric, num_mined, model)
}
\description{
Controls the parameters used for context mining.
}
\examples{
cm_ctrl <- cmControl(min_perf = 0.7, metric = "acc", num_mined = 5, model = "rf")
}
