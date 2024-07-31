# haskell-mailer

![E2E Testing](https://github.com/adueck/haskell-mailer/actions/workflows/playwright.yml/badge.svg)

A simple personal newsletter mailing app.

### 🚧 TODO: IN PROGRESS

- ability to update address info
- display sends information
- CSV contacts import / export
- Email preview
- sign-in and security
- DEPLOY!

## Requires

- [Cabal](https://hackage.haskell.org/package/Cabal)
- [Postgres](https://www.postgresql.org/) instance
- An SMTP account for sending mail

## Running

The following environment variables need to be set:

- `MYMAILER_URL`: The URL of the app (use `http://localhost:8080` for dev) 
- `MYMAILER_SENDER`: FROM Sender Address
- `MYMAILER_ADMIN_EMAIL`: The e-mail address to send admin messages to
- `MYMAILER_DOMAIN`: SMTP domain
- `MYMAILER_LOGIN`: SMTP login name
- `MYMAILER_PASSWORD`: SMTP password
- `MYMAILER_DB`: Postgres DB name
- `MYMAILER_DB_HOST`: Postgres DB host
- `MYMAILER_DB_PASSWORD`: Postgres DB password

Optional:
- `MYMAILER_PORT`: SMTP Port (defaults to `"465"`)

For E2E testing:
- `MAILPIT_DOMAIN`: base url (domain) for Mailpit

Then run the app using cabal

```bash
$ cabal run
```

## Setting up a Mailpit server for testing 

These are the instructions for setting up a [Mailpit](https://mailpit.axllent.org/) server for SMTP testing on FreeBSD

Install Mailpit

```
$ sudo pkg install mailpit
```

Setup domain and HTTP proxy with NGINX to `http://localhost:8025`

Get certbot certificates:

```
$ sudo certbot certonly -d example.com
```

Create a [password file](https://mailpit.axllent.org/docs/configuration/passwords/) somewhere

Add the following lines to `/etc/rc.conf`

```
mailpit_enable="YES"
mailpit_runtimeuser="root"
mailpit_args="--smtp-tls-cert LETSENCRYPTCERT --smtp-tls-key LETSENCRYPTKEY --smtp-auth-file AUTHPASSWORDFILE --smtp-require-tls"
```

In the file `/usr/local/etc/rc.d/mailpit`, adjust the `command_args` line to the following:

```
command_args="-c -r -f -P ${pidfile} -u ${mailpit_runtimeuser} /usr/local/bin/${name} --listen ${mailpit_bind_addr}:${mailpit_api_port} ${mailpit_args}"
```

This is because `--smtp ${mailpit_bind_addr}:${mailpit_smtp_port}` in the original file causes a problem.