# Ejecuta RLM.R, Logistica.R, CP.R y Conglomerados.R bloque por bloque y guarda la
# salida de consola de cada bloque "# >> nombre" en salidas/<nombre>.txt, y escribe
# rangos.tex (lineas de cada bloque y el script al que pertenece) para los informes.
# Uso:  Rscript generar-salidas.R
options(useFancyQuotes = FALSE)
dir.create("salidas", showWarnings = FALSE)
rangos <- character()
for (script in c("RLM.R", "Logistica.R", "CP.R", "Conglomerados.R")) {
  src <- readLines(script)
  ini <- grep("^# >> ", src); fin <- grep("^# <<", src)
  env <- new.env(parent = globalenv())
  eval(parse(text = src[1:(ini[1] - 1)]), envir = env)
  for (i in seq_along(ini)) {
    nom <- sub("^# >> ", "", src[ini[i]])
    cod <- src[(ini[i] + 1):(fin[i] - 1)]
    out <- capture.output(source(textConnection(cod), local = env, echo = FALSE,
                                 print.eval = TRUE, max.deparse.length = Inf))
    writeLines(out, file.path("salidas", paste0(nom, ".txt")))
    rangos <- c(rangos,
      sprintf("\\expandafter\\def\\csname r@%s\\endcsname{%d-%d}", nom, ini[i] + 1, fin[i] - 1),
      sprintf("\\expandafter\\def\\csname s@%s\\endcsname{%s}", nom, script))
  }
}
writeLines(rangos, "rangos.tex")
