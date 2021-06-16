-module(paper_shim).
-compile(export_all).

add_paper_existing(DOI, Title, Author, Lock, Price, Link) ->
    paperstore_db:add_paper(DOI, Title, Author, Lock, Price, Link).
add_paper_new(DOI, Title, Author, Lock, Price, Link) ->
    paperstore_db:add_paper(DOI, Title, Author, Lock, Price, Link).

find_paper_by_doi_exists(DOI) ->
    paperstore_db:find_paper_by_doi(DOI).
find_paper_by_doi_unknown(DOI) ->
    paperstore_db:find_paper_by_doi(DOI).

find_paper_by_author_matching(Author) ->
    paperstore_db:find_paper_by_author(Author).
find_paper_by_author_unknown(Author) ->
    paperstore_db:find_paper_by_author(Author).

find_paper_by_title_matching(Author) ->
    paperstore_db:find_paper_by_title(Author).
find_paper_by_title_unknown(Author) ->
    paperstore_db:find_paper_by_title(Author).

