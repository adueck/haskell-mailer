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

The following environment variables need to be set **for production**:

- `MYMAILER_URL`: The URL of the app
- `MYMAILER_SENDER`: FROM Sender Address
- `MYMAILER_ADMIN_EMAIL`: The e-mail address to send admin messages to
- `MYMAILER_DOMAIN`: SMTP domain
- `MYMAILER_LOGIN`: SMTP login name
- `MYMAILER_PASSWORD`: SMTP password
- `MYMAILER_DB`: Postgres DB name
- `MYMAILER_DB_HOST`: Postgres DB host
- `MYMAILER_DB_PASSWORD`: Postgres DB password

For development and testing, they can be left blank.

Then run the app using cabal

```bash
$ cabal run
```

This will launch a dev server at `http://localhost:8080`. Postgres and mailpit will need to be running for dev.

## End to end testing

Requires [Mailpit](https://mailpit.axllent.org/) to be installed in path OR to be running on `localhost:8025` (API) and `localhost:1025` (SMTP).

```
$ npm 
```
