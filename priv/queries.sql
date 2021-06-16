-- :setup_table_papers
CREATE TABLE papers (
    doi varchar(40) PRIMARY KEY,
    title varchar(256) NOT NULL,
    author varchar(256) NOT NULL,
    lock boolean DEFAULT false,
    price int DEFAULT 0,
    link varchar(256)
);

-- :teardown_table_papers
DROP TABLE papers;

-- :add_paper
INSERT INTO papers  (doi, title, author, lock, price, link)
    VALUES          ($1,  $2,   $3,     $4,    $5,    $6);

-- :find_by_author
SELECT * FROM papers WHERE author LIKE $1;

-- :find_by_title
SELECT * FROM papers WHERE title LIKE $1;

-- :find_by_doi
SELECT * FROM papers WHERE doi = $1;