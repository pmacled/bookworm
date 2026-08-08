storage_for_identity <- function(identity,
                                 configured = supabase_configured,
                                 connect = supabase_connect) {
  guest <- function(degraded = FALSE, reason = "")
    list(storage = make_storage("guest"), con = NULL,
         degraded = degraded, reason = reason)

  if (!identical(identity$mode, "user")) return(guest())

  configured_ok <- tryCatch(isTRUE(configured()), error = function(e) FALSE)
  if (!configured_ok)
    return(guest(TRUE, "Saving is not configured on this deployment."))

  con <- tryCatch(connect(), error = function(e) e)
  if (inherits(con, "error"))
    return(guest(TRUE, "Could not reach the database. This game will not be saved."))

  list(storage = make_storage("supabase", con = con, user_id = identity$user_id),
       con = con, degraded = FALSE, reason = "")
}
