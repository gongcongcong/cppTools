print("\n")
dyn.load("./src/cppTools.dll")
t_statistic = numeric(1)
        p_value = numeric(1)
        df = numeric(1)
        x_mean = numeric(1)
        y_mean = numeric(1)
        method = character(1)
n <- 4
x <- rnorm(n)
y <- rnorm(n)
.Call("R_auto_test", x = as.double(x), y = as.double(y), 
                t_statistic = t_statistic, p_value = p_value, df = df,
                x_mean = x_mean, y_mean = y_mean, method = method
        ) 
list(
                t_statistic = t_statistic, p_value = p_value, df = df,
                x_mean = x_mean, y_mean = y_mean, method = method
        ) |> print()
dyn.unload("./src/cppTools.dll")
t_test(1:3, 1:3)
