-- psql postgres -c "CREATE DATABASE opallib"
-- psql opallib -f setup.sql

BEGIN;

CREATE TABLE book
( isbn     BIGINT       NOT NULL PRIMARY KEY
, title    VARCHAR(255) NOT NULL
);

CREATE TABLE book_keyword 
( book_isbn BIGINT       NOT NULL REFERENCES book(isbn)
, keyword   VARCHAR(255) NOT NULL
, PRIMARY KEY (book_isbn,keyword)
);

CREATE TABLE accession
( id        SERIAL  NOT NULL PRIMARY KEY
, book_isbn BIGINT  NOT NULL REFERENCES book(isbn)
);

CREATE TABLE person
( id   SERIAL       NOT NULL PRIMARY KEY
, name VARCHAR(255) NOT NULL 
);

CREATE TABLE loan
( id           SERIAL      NOT NULL PRIMARY KEY
, person_id    INTEGER     NOT NULL REFERENCES person(id)
, accession_id INTEGER     NOT NULL REFERENCES accession(id)
, borrowed     TIMESTAMPTZ NOT NULL
, due          TIMESTAMPTZ NOT NULL
, returned     TIMESTAMPTZ
);

INSERT INTO book (isbn,title) VALUES
 (9780262510875,'Structure and Interpretation of Computer Programs')
,(9780262032933,'Introduction to Algorithms')
,(9780131103627,'The C Programming Language')
,(9780132158718,'A Discipline of Programming')
,(9781593272838,'Learn you a Haskell for Great Good!: A Beginner''s Guide')
,(9780130340740,'Computer Systems: A Programmer''s Perspective')
,(9780767907699,'Slack: Getting Past Burnout, Busywork, and the Myth of Total Efficiency')
,(9780596514983,'Real World Haskell')
,(9781449335946,'Parallel and Concurrent Haskell')
,(9780521631246,'Purely Functional Data Structures')
,(9781107452640,'Thinking Functionally with Haskell')
,(9780521513388,'Pearls of Functional Algorithm Design')
,(9780262162098,'Types and Programming Languagues')
,(9780521675994,'How to Prove It')
,(9780521719162,'Conceptual Mathematics')
,(9781781252871,'Cakes, Custard and Category Theory: Easy Recipes for Understanding Complex Maths')
;

INSERT INTO book_keyword (book_isbn,keyword) VALUES
 (9780262510875,'Programming')
,(9780262510875,'Computer Science')
,(9780262510875,'LISP')
,(9780262032933,'Computer Science')
,(9780262032933,'Programming')
,(9780131103627,'Programming')
,(9780131103627,'C')
,(9780132158718,'Programming')
,(9780132158718,'Computer Science')
,(9781593272838,'Programming')
,(9781593272838,'Haskell')
,(9780130340740,'Operating Systems')
,(9780767907699,'Management')
,(9780596514983,'Programming')
,(9780596514983,'Haskell')
,(9781449335946,'Programming')
,(9781449335946,'Haskell')
,(9781449335946,'Concurrent Programming')
,(9781449335946,'Parallel Programming')
,(9780521631246,'Computer Science')
,(9781107452640,'Programming')
,(9781107452640,'Computer Science')
,(9780521513388,'Programming')
,(9780521513388,'Computer Science')
,(9780521513388,'Haskell')
,(9780262162098,'Computer Science')
,(9780521675994,'Mathematics')
,(9780521719162,'Mathematics')
,(9781781252871,'Mathematics')
;

INSERT INTO accession (book_isbn) (SELECT isbn FROM book);
INSERT INTO accession (book_isbn) VALUES (9781593272838);

INSERT INTO person (name) VALUES
  ('Ben Kolera')
, ('Bruce Wayne')
, ('Peter Parker')
, ('Hank McCoy')
, ('Rachel Roth')
, ('Oliver Queen')
;

INSERT INTO loan (person_id,accession_id,borrowed,due,returned)
( SELECT person.id,accession.id,'2015-07-01','2015-08-01','2015-07-22'
  FROM person , accession
  WHERE person.name = 'Hank McCoy'
);

INSERT INTO loan (person_id,accession_id,borrowed,due)
( SELECT person.id,accession.id,'2015-08-01','2015-09-01'
FROM person , accession 
WHERE person.name = 'Oliver Queen' AND book_isbn = 9780767907699
);

COMMIT;
