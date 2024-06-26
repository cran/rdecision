#' @title A parametrized log Normal probability distribution
#' @description An R6 class representing a log Normal distribution.
#' @details A parametrized Log Normal distribution inheriting from class
#' \code{Distribution}. Swat (2017) defined seven parametrizations of the log
#' normal distribution.
#' These are linked, allowing the parameters of any one to be derived from any
#' other. All 7 parametrizations require two parameters as follows:
#' \describe{
#' \item{LN1}{\eqn{p_1=\mu}, \eqn{p_2=\sigma}, where \eqn{\mu} and \eqn{\sigma}
#' are the mean and standard deviation, both on the log scale.}
#' \item{LN2}{\eqn{p_1=\mu}, \eqn{p_2=v}, where \eqn{\mu} and \eqn{v} are the
#' mean and variance, both on the log scale.}
#' \item{LN3}{\eqn{p_1=m}, \eqn{p_2=\sigma}, where \eqn{m} is the median on the
#' natural scale and \eqn{\sigma} is the standard deviation on the log scale.}
#' \item{LN4}{\eqn{p_1=m}, \eqn{p_2=c_v}, where \eqn{m} is the median on the
#' natural scale and \eqn{c_v} is the coefficient of variation on the natural
#' scale.}
#' \item{LN5}{\eqn{p_1=\mu}, \eqn{p_2=\tau}, where \eqn{\mu} is the mean on the
#' log scale and \eqn{\tau} is the precision on the log scale.}
#' \item{LN6}{\eqn{p_1=m}, \eqn{p_2=\sigma_g}, where \eqn{m} is the median on
#' the natural scale and \eqn{\sigma_g} is the geometric standard deviation on
#' the natural scale.}
#' \item{LN7}{\eqn{p_1=\mu_N}, \eqn{p_2=\sigma_N}, where \eqn{\mu_N} is the mean
#' on the natural scale and \eqn{\sigma_N} is the standard deviation on the
#' natural scale.}
#' }
#' @references{
#'  Briggs A, Claxton K and Sculpher M. Decision Modelling for Health
#'  Economic Evaluation. Oxford 2006, ISBN 978-0-19-852662-9.
#'
#'  Leaper DJ, Edmiston CE and Holy CE. Meta-analysis of the potential
#'  economic impact following introduction of absorbable antimicrobial
#'  sutures. \emph{British Journal of Surgery} 2017;\bold{104}:e134-e144.
#'
#'  Swat MJ, Grenon P and Wimalaratne S. Ontology and Knowledge Base of
#'  Probability Distributions. \emph{Bioinformatics} 2016;\bold{32}:2719-2721,
#'  \doi{10.1093/bioinformatics/btw170}.
#' }
#' @note The log normal distribution may be used to model the uncertainty in
#' an estimate of relative risk (Briggs 2006, p90). If a relative risk
#' estimate is available with a 95\% confidence interval, the \verb{"LN7"}
#' parametrization
#' allows the uncertainty distribution to be specified directly. For example,
#' if RR = 0.67 with 95\% confidence interval 0.53 to 0.84 (Leaper, 2016), it
#' can be modelled with
#' \code{LogNormModVar$new("rr", "RR", p1=0.67,
#' p2=(0.84-0.53)/(2*1.96)), "LN7")}.
#' @docType class
#' @author Andrew J. Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
LogNormDistribution <- R6::R6Class(
  classname = "LogNormDistribution",
  lock_class = TRUE,
  inherit = Distribution,
  private = list(
    meanlog = NULL,
    sdlog = NULL,
    parametrization = NULL
  ),
  public = list(

    #' @description Create a log normal distribution.
    #' @param p1 First hyperparameter, a measure of location. See
    #' \emph{Details}.
    #' @param p2 Second hyperparameter, a measure of spread. See \emph{Details}.
    #' @param parametrization A character string taking one of the values
    #' \verb{"LN1"} (default) through \verb{"LN7"} (see \emph{Details}).
    #' @return A \code{LogNormDistribution} object.
    initialize = function(p1, p2, parametrization = "LN1") {
      # initialize the base class
      super$initialize("LogNorm", K = 1L)
      # check that p1 and p2 are numeric
      abortifnot(is.numeric(p1),
        message = "Argument 'p1' must be numeric",
        class = "p1_not_numeric"
      )
      abortifnot(is.numeric(p2),
        message = "Argument 'p2' must be numeric",
        class = "p2_not_numeric"
      )
      # transform parameters according to parametrization
      private$parametrization <- parametrization
      if (parametrization == "LN1") {
        private$meanlog <- p1
        private$sdlog <- p2
      } else if (parametrization == "LN2") {
        private$meanlog <- p1
        private$sdlog <- sqrt(p2)
      } else if (parametrization == "LN3") {
        private$meanlog <- log(p1)
        private$sdlog <- p2
      } else if (parametrization == "LN4") {
        private$meanlog <- log(p1)
        private$sdlog <- sqrt(log(p2 ^ 2L + 1L))
      } else if (parametrization == "LN5") {
        private$meanlog <- p1
        private$sdlog <- 1L / sqrt(p2)
      } else if (parametrization == "LN6") {
        private$meanlog <- log(p1)
        private$sdlog <- log(p2)
      } else if (parametrization == "LN7") {
        private$meanlog <- log(p1 / sqrt(1L + (p2 ^ 2L) / (p1 ^ 2L)))
        private$sdlog <- sqrt(log(1L + (p2 ^ 2L) / (p1 ^ 2L)))
      } else {
        abortif(TRUE,
          message = "'parametrization' must be one of 'LN1' through 'LN7'",
          class = "parametrization_not_supported"
        )
      }
      # initial sample
      self$sample(TRUE)
      # return new object
      return(invisible(self))
    },

    #' @description Accessor function for the name of the distribution.
    #' @return Distribution name as character string (\verb{"LN1"}, \verb{"LN2"}
    #' etc.).
    distribution = function() {
      rv <- paste0(
        "LN(", round(private$meanlog, 3L), ",", round(private$sdlog, 3L), ")"
      )
      return(rv)
    },

    #' @description Draw a random sample from the model variable.
    #' @param expected If TRUE, sets the next value retrieved by a call to
    #' \code{r()} to be the mean of the distribution.
    #' @return Updated \code{LogNormDistribution} object.
    sample = function(expected = FALSE) {
      if (expected) {
        private$.r[[1L]] <- self$mean()
      } else {
        private$.r[[1L]] <- rlnorm(
          n = 1L, mean = private$meanlog, sd = private$sdlog
        )
      }
      return(invisible(self))
    },

    #' @description Return the expected value of the distribution.
    #' @return Expected value as a numeric value.
    mean = function() {
      E <- exp(private$meanlog + 0.5 * private$sdlog ^ 2L)
      return(E)
    },

    #' @description Return the point estimate of the variable.
    #' @return Point estimate (mode) of the log normal distribution.
    mode = function() {
      return(exp(private$meanlog - private$sdlog ^ 2L))
    },

    #' @description Return the standard deviation of the distribution.
    #' @return Standard deviation as a numeric value
    SD = function() {
      S <- exp(private$meanlog + 0.5 * private$sdlog ^ 2L) *
        sqrt(exp(private$sdlog ^ 2L) - 1L)
      return(S)
    },

    #' @description Return the quantiles of the log normal distribution.
    #' @param probs Vector of probabilities, in range [0,1].
    #' @return Vector of quantiles.
    quantile = function(probs) {
      # test argument
      vapply(probs, FUN.VALUE = TRUE, FUN = function(x) {
        abortif(is.na(x),
          message = "All elements of 'probs' must be defined",
          class = "probs_not_defined"
        )
        abortifnot(is.numeric(x),
          message = "Argument 'probs' must be a numeric vector",
          class = "probs_not_numeric"
        )
        abortifnot(x >= 0.0 && x <= 1.0,
          message = "Elements of 'probs' must be in range[0,1]",
          class = "probs_out_of_range"
        )
        return(TRUE)
      })
      q <- qlnorm(probs, mean = private$meanlog, sd = private$sdlog)
      return(q)
    }
  )
)
