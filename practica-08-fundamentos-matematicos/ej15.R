f  <- function(x) x^4 - 3*x^2 + 2
df <- -2                                  # f'(1) exacto
h  <- 10^-(1:4)
adelante <- (f(1 + h) - f(1)) / h
centrada <- (f(1 + h) - f(1 - h)) / (2 * h)
tabla <- data.frame(h, adelante, err_adel = abs(adelante - df),
                    centrada, err_cent = abs(centrada - df))
print(format(tabla, digits = 10), row.names = FALSE)
