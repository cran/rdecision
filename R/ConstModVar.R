#' @title \verb{ConstModVar} class
#' 
#' @description 
#' An R6 class for a constant in a model
#' 
#' @details
#' A \verb{ModVar} with no uncertainty in its value. It has no
#' distribution and there are no hyperparameters. Its 
#' benefit over using a regular numeric variable in a model is that
#' it will appear in automatic tabulations of the model variables
#' associated with a model and therefore be explicitly documented
#' as a model input.
#' 
#' @docType class
#' @author Andrew Sims \email{andrew.sims@@newcastle.ac.uk}
#' @export
#' 
ConstModVar <- R6::R6Class(
  classname = "ConstModVar",
  lock_class = TRUE,
  inherit = ModVar,
  private = list(
    val = NULL
  ),
  public = list(
    
    #' @description 
    #' Create a new constant model variable
    #' @param description A character string description of the variable
    #' and its role in the model. This description will be used in a tabulation
    #' of the variables linked to a model.
    #' @param units A character string description of the units, e.g. "GBP",
    #' "per year".
    #' @param const The constant numerical value of the object.
    #' @return A new \verb{ConstModVar} object.
    initialize = function(description, units, const) {
      super$initialize(description, units)
      if (!is.numeric(const)) {
        rlang::abort(
          "Argument 'const' must be numeric", 
          class="const_not_numeric"
        )
      }
      private$val <- const
      # initialize next get() call
      self$set("expected")
      # return object
      return(invisible(self))
    },
    
    #' @description 
    #' Tests whether the model variable is probabilistic, i.e. a random
    #' variable that follows a distribution, or an expression involving
    #' random variables, some of which follow distributions. 
    #' @return TRUE if probabilistic
    is_probabilistic = function() {
      return(FALSE)
    },

    #' @description 
    #' Accessor function for the name of the uncertainty distribution.
    #' @return Distribution name as character string.
    distribution = function() {
      rv <- paste('Const(', 
                  format(private$val, digits=4, scientific=F),
                  ')', sep='')
      return(rv)
    },
    
    #' @description 
    #' Return the mode of the distribution.
    #' @return Value of the constant.
    mode = function() {
      return(private$val)
    },

    #' @description 
    #' Return the expected value of the distribution. 
    #' @return Expected value as a numeric value.
    mean = function() {
      return(private$val)
    },
    
    #' @description 
    #' Return a random sample from the distribution. 
    #' @param n Number of samples to draw.
    #' @return Constant value as a numeric value.
    r = function(n=1) {
      return(rep(private$val, times=n))
    },
    
    #' @description 
    #' Return the standard deviation of the distribution. 
    #' @return Standard deviation as a numeric value
    SD = function() {
      return(0)
    },
    
    #' @description 
    #' Quantiles of the uncertainty distribution; for a constant all
    #' quantiles are returned as the value of the constant.
    #' @param probs Numeric vector of probabilities, each in range [0,1].
    #' @return Vector of numeric values of the same length as \verb{probs}.
    quantile = function(probs) {
      # test argument
      sapply(probs, FUN=function(x) {
        if (is.na(x)) {
          rlang::abort("All elements of 'probs' must be defined",
                       class="probs_not_defined")
        }
        if (!is.numeric(x)) {
          rlang::abort("Argument 'probs' must be a numeric vector",
                       class="probs_not_numeric")
        }
        if (x<0 || x>1) {
          rlang::abort("Elements of 'probs' must be in range[0,1]",
                       class="probs_out_of_range")
        }
      })
      q <- rep(private$val, times=length(probs))
      return(q)
    }

  )
)