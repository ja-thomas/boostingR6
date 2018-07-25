Loss = R6::R6Class("Loss",
  public = list(
    loss = function(y, scores) NULL,
    gradient = function(y, scores) NULL,
    hessian = function(y, scores) NULL,
    initialize = function() NULL
  )
)

L2 = R6::R6Class("L2",
  inherit = Loss,
  public = list(
    loss = function(y, scores) (y - scores)^2,
    gradient = function(y, scores) y - scores,
    hessian = function(y, scores) rep(1, length(y))
    )
  )
