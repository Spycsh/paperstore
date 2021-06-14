CREATE TABLE papers (
    doi varchar(40) PRIMARY KEY
    title varchar(256) NOT NULL,
    author varchar(256) NOT NULL,
);

DROP TABLE papers;

-- add a paper
INSERT INTO papers  (doi, title, author)
    VALUES          ($1,  $2,   $3);


