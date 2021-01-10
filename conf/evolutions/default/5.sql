# --- 4th database schema
# --- !Ups
create table "addresses" ("hostName" VARCHAR NOT NULL,"lastIP" VARCHAR NOT NULL,
    "description" VARCHAR,
    "addTime" TIMESTAMP NOT NULL,
    "id" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);
create index "addTimeIPIndex" on "addresses" ("addTime","lastIP");

# --- !Downs
drop table if exists "addresses";