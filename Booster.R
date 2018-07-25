Booster = R6::R6Class("Booster",
  public = list(
    params = NULL,
    loss = NULL,
    models = list(),
    best.iteration = NULL,
    initialize = function(gamma = 0, lambda = 1, min_split_gain = 0.1, max_depth = 5, shrinkage = 0.3, loss) {
      self$params = list(gamma = gamma, lambda = lambda, min_split_gain = min_split_gain, max_depth = max_depth, shrinkage = shrinkage)
      self$loss = loss
      return(NULL)
    },
    train = function(data, iterations = 20) {
      self$models = list()
      scores = self$predict(data$x)
      for (m in seq_len(iterations)) {
        grad = self$loss$gradient(data$y, scores)
        hessian = self$loss$hessian(data$y, scores)
        self$models[[m]] = Tree$new(data$x, grad, hessian, param = self$params)
        scores = self$predict(data$x)
        print(sprintf("Loss:%f", mean(self$loss$loss(data$y, scores))))
      }
    },
    predict = function(newdata) {
      scores = numeric(nrow(newdata))
      if (length(self$models) == 0)
        return(scores)
      for (obs in seq_len(nrow(newdata))) {
        scores[obs] = sum(sapply(self$models, function(x) x$predict(newdata[obs, ])))
      }
      return(scores)
    }
  )
)
