library(smoof)

canon_example_1 = makeSingleObjectiveFunction(name = "XOR", fn = function(x) {
        - dmvnorm(x, mean = c(-1.5, -1.5), sigma = diag(2)) - dmvnorm(x, mean = c(1.5, 1.5), sigma = diag(2))
    }, 
    par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 2, lower = - 2, upper = 2))
)

canon_example_2 = makeSingleObjectiveFunction(name = "VERTICAL_INFO", fn = function(x) {
        - dmvnorm(x, mean = c(-1.5, -1.5), sigma = diag(2)) - dmvnorm(x, mean = c(-1.5, 1.5), sigma = diag(2))
    }, 
    par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 2, lower = - 2, upper = 2))
)

canon_example_3 = makeSingleObjectiveFunction(name = "HORIZONTAL_INFO", fn = function(x) {
        - dmvnorm(x, mean = c(-1.5, -1.5), sigma = diag(2)) - dmvnorm(x, mean = c(1.5, - 1.5), sigma = diag(2))
    }, 
    par.set = makeParamSet(makeNumericVectorParam(id = "x", len = 2, lower = - 2, upper = 2))
)

canon_examples = list(XOR = canon_example_1, VERTICAL = canon_example_2, HORIZONTAL = canon_example_3)

binary_example_1 = makeSingleObjectiveFunction(name = "binary_example_1", fn = function(x) {
        (1 / 2 * sum(x$lambda1^4 - 16 * x$lambda1^2 + 5 * x$lambda1) + 60) / (125 + 60) - 0.1 * x$lambda2
    }, 
    par.set = makeParamSet(
    	makeNumericParam(id = "lambda1", lower = - 4, upper = 4), 
    	makeDiscreteParam(id = "lambda2", values = c(0, 1))
    	), 
    has.simple.signature = FALSE
)

binary_example_2 = makeSingleObjectiveFunction(name = "binary_example_2", fn = function(x) {
#         (1 / 2 * sum(x$lambda1^4 - 16 * x$lambda1^2 + 5 * x$lambda1) + 60) / (125 + 60) - (- x$lambda1 + 4) / 50 * x$lambda2
	      ifelse(x$lambda2 == 0, x$lambda1^2 + 1, (x$lambda1 - 1)^2)
    }, 
    par.set = makeParamSet(
    	makeNumericParam(id = "lambda1", lower = - 3, upper = 3), 
    	makeDiscreteParam(id = "lambda2", values = c(0, 1))
    	), 
    has.simple.signature = FALSE
)