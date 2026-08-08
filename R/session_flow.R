storage_for_identity <- function(identity,
                                 configured = supabase_configured,
                                 driver_available = function() requireNamespace("RPostgres", quietly = TRUE),
                                 connect = supabase_connect) {
  guest <- function(degraded = FALSE, reason = "")
    list(storage = make_storage("guest"), con = NULL,
         degraded = degraded, reason = reason)

  if (!identical(identity$mode, "user")) return(guest())

  configured_ok <- tryCatch(isTRUE(configured()), error = function(e) FALSE)
  if (!configured_ok)
    return(guest(TRUE, "Saving is not configured on this deployment."))

  # Check the driver before ever calling connect(), so a deployment that's missing
  # RPostgres (a packaging/manifest problem) is never mistaken for a network outage —
  # the two need different messages and, eventually, different fixes.
  driver_ok <- tryCatch(isTRUE(driver_available()), error = function(e) FALSE)
  if (!driver_ok)
    return(guest(TRUE, "The database driver is not available on this deployment. This game will not be saved."))

  con <- tryCatch(connect(), error = function(e) e)
  if (inherits(con, "error"))
    return(guest(TRUE, "Could not reach the database. This game will not be saved."))

  list(storage = make_storage("supabase", con = con, user_id = identity$user_id),
       con = con, degraded = FALSE, reason = "")
}
