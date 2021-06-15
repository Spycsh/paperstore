-- :setup_paper_store
CREATE TABLE papers (
    doi varchar(40) PRIMARY KEY
    title varchar(256) NOT NULL,
    author varchar(256) NOT NULL,
    lock boolean,
    price int,
    link varchar(256)
);

-- :teardown_paper_store
DROP TABLE papers;

-- add a paper
-- :add_paper
INSERT INTO papers  (doi, title, author)
    VALUES          ($1,  $2,   $3);

-- :find_by_author
SELECT * FROM papers WHERE author LIKE $1;

-- :find_by_title
SELECT * FROM papers WHERE title LIKE $1;

-- :find_by_doi
SELECT * FROM papers WHERE doi = $1;
