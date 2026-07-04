storage_for_identity <- function(identity) {
  if (identical(identity$mode, "user") && supabase_configured()) {
    con <- supabase_connect()
    return(list(storage = make_storage("supabase", con = con, user_id = identity$user_id),
                con = con))
  }
  list(storage = make_storage("guest"), con = NULL)
}
