# Applying the Bookworm schema

1. Open your Supabase project → SQL Editor.
2. Paste the contents of `supabase_schema.sql` and run it.
3. In `.Renviron` (copy from `.Renviron.template`), set the SUPABASE_* values.
4. RLS is intentionally left disabled in slice one; the app enforces
   `owner_id = <signed-in user>` scoping. Enable the commented policies in Phase 3.
