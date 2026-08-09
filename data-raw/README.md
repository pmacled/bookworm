# Applying the Bookworm schema

1. Open your Supabase project → SQL Editor.
2. Paste the contents of `supabase_schema.sql` and run it. It enables the
   `pgcrypto` and `citext` extensions.
3. Set the `SUPABASE_*` secret values in Posit Connect Cloud (not in the app).
   End users authenticate against the `users` table and never see the database.

## Auth notes

- **Usernames** are case-insensitive (`citext`): `MIKE` and `mike` are the same
  user. Allowed: 3–30 chars, letters/digits/underscore.
- **Passwords** are stored only as bcrypt hashes. Create/verify in the app:

  ```sql
  -- create
  insert into users (username, password_hash)
  values (:username, crypt(:password, gen_salt('bf')));

  -- verify
  select id, is_admin from users
  where username = :username
    and password_hash = crypt(:password, password_hash);
  ```

- **Admins** (`is_admin = true` / `role = 'admin'`) are intended to eventually
  see all games; enforce this in the app layer.
- **Sharing**: insert into `game_shares (game_id, user_id, can_edit)` after
  looking up the target by exact username. Only the owner can edit for now;
  `can_edit` is reserved for shared editing later.

## Access control

RLS via `auth.uid()` does not apply here: the app connects with a single
Supabase service credential and identifies users itself. All owner/share
scoping is enforced in the R application. Keep the service key server-side.
