windows = function(...) {
  switch(.Platform$OS.type,
    "unix" = quartz(...),
    "windows" = windows(...)
  )
}
