DROP TABLE IF EXISTS sends;
DROP TABLE IF EXISTS mailings;
DROP TABLE IF EXISTS contacts;

CREATE TABLE contacts (
    contact_id uuid DEFAULT gen_random_uuid() PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(100) NOT NULL,
    contact_group VARCHAR(100) NOT NULL,
    notes TEXT NOT NULL
);

CREATE TABLE mailings (
    mailing_id uuid DEFAULT gen_random_uuid() PRIMARY KEY,
    subject VARCHAR(100) NOT NULL,
    content TEXT NOT NULL,
    created_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    published BOOLEAN NOT NULL DEFAULT false
);

CREATE TABLE IF NOT EXISTS sends (
    send_id uuid DEFAULT gen_random_uuid() PRIMARY KEY,
    mailing_id uuid,
    contact_id uuid,
    email VARCHAR(100),
    send_error TEXT,
    sent_on TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT fk_mailing FOREIGN KEY (mailing_id) REFERENCES mailings(mailing_id) ON DELETE CASCADE,
    CONSTRAINT fk_contact FOREIGN KEY (contact_id) REFERENCES contacts(contact_id) ON DELETE CASCADE
);
