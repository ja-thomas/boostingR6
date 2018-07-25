Tree = R6::R6Class("Tree",
  public = list(
    is_leaf = FALSE,
    left_child = NULL,
    right_child = NULL,
    split_feature_id = NULL,
    split_val = NULL,
    weight = NULL,
    initialize = function(instances, grad, hessian, depth = 0, param) {
      if (depth > param$max_depth) {
        self$is_leaf = TRUE
        self$weight = private$calcLeafWeight(grad, hessian, param$lambda) * param$shrinkage
        return(NULL)
      }
      G = sum(grad)
      H = sum(hessian)
      best_gain = 0
      best_feature_id = NULL
      best_val = 0
      best_left_instance_ids = NULL
      best_right_instance_ids = NULL
      for (feat in colnames(instances)) {
        G_l = 0
        H_l = 0
        sorted_instances = order(instances[, feat])
        for (i in seq_along(sorted_instances)) {
          inst = sorted_instances[i]
          G_l = G_l + grad[inst]
          H_l = H_l + hessian[inst]
          G_r = G - G_l
          H_r = H - H_l
          gain = private$calcSplitGain(G, H, G_l, H_l, G_r, H_r, param$lambda)
          if (gain > best_gain) {
            best_gain = gain
            best_feature_id = feat
            best_val = instances[inst, feat]
            best_left_instance_ids = sorted_instances[1:i]
            best_right_instance_ids = sorted_instances[(i+1):length(sorted_instances)]
          }
        }
      }
      if (best_gain < param$min_split_gain) {
        self$is_leaf = TRUE
        self$weight = private$calcLeafWeight(grad, hessian, param$lambda) * param$shrinkage
      } else {
        print(sprintf("Split at %s=%f", best_feature_id, best_val))
        self$split_feature_id = best_feature_id
        self$split_val = best_val
        self$left_child = Tree$new(instances[best_left_instance_ids, ],
          grad[best_left_instance_ids], hessian[best_left_instance_ids], depth + 1, param)
        self$right_child = Tree$new(instances[best_right_instance_ids, ],
          grad[best_right_instance_ids], hessian[best_right_instance_ids], depth + 1, param)

      }
    },
    predict = function(x) {
      if (self$is_leaf)
        return (self$weight)
      else {
        if (x[self$split_feature_id] <= self$split_val) {
          return (self$left_child$predict(x))
        } else {
          return (self$right_child$predict(x))
        }
      }
    }),
  private = list(
    calcSplitGain = function(G, H, G_l, H_l, G_r, H_r, lambda) {
    terms = mapply(function(grad, hess) grad^2 / (sum(hess) + lambda),
      grad = list(G_l, G_r, G), hess = list(H_l, H_r, H))
      res = terms[[1]] + terms[[2]] - terms[[3]]
      return(res)
    },
    calcLeafWeight = function(grad, hessian, lambda) {
      sum(grad) / (sum(hessian) + lambda)
    }
  )
)
