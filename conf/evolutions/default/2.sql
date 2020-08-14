# --- First database schema
# --- !Ups
create table "records" ("day" TIMESTAMP NOT NULL,"calorie" INTEGER NOT NULL,"note" VARCHAR,"id" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);

# --- !Downs
drop table if exists "records"
