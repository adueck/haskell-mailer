# haskell-mailer

![E2E Testing](https://github.com/adueck/haskell-mailer/actions/workflows/ci.yml/badge.svg)

A simple personal newsletter mailing app.

### 🚧 TODO: IN PROGRESS

- get cabal caching and caching with npm working in CI actions
- get testing working in CI
- Email preview
- sign-in and security
- DEPLOY!

## Requires

- [Cabal](https://hackage.haskell.org/package/Cabal)
- [Postgres](https://www.postgresql.org/) instance
- An SMTP account for sending mail

## Running

The following environment variables need to be set for production:

- `MYMAILER_URL`: The URL of the app (use `http://localhost:8080` for dev) 
- `MYMAILER_SENDER`: FROM Sender Address
- `MYMAILER_ADMIN_EMAIL`: The e-mail address to send admin messages to
- `MYMAILER_DOMAIN`: SMTP domain
- `MYMAILER_LOGIN`: SMTP login name
- `MYMAILER_PASSWORD`: SMTP password
- `MYMAILER_DB`: Postgres DB name
- `MYMAILER_DB_HOST`: Postgres DB host
- `MYMAILER_DB_PASSWORD`: Postgres DB password

For testing, leave them blank.

Then run the app using cabal

```bash
$ cabal run
```

## End to end testing

Requires [Mailpit](https://mailpit.axllent.org/) to be installed in path OR to be running on `localhost:8025` (API) and `localhost:1025` (SMTP).

```
$ npx playwright test
```
