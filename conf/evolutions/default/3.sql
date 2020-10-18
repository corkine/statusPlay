# --- First database schema
# --- !Ups
create table "goods" ("name" VARCHAR NOT NULL,
    "picture" VARCHAR,"description" VARCHAR,
    "kind" VARCHAR,"currentState" VARCHAR NOT NULL,
    "importance" VARCHAR NOT NULL,"validUntil" TIMESTAMP,
    "estimatedLiftTime" BIGINT,"message" VARCHAR,"place" VARCHAR,
    "addTime" TIMESTAMP NOT NULL,"updateTime" TIMESTAMP NOT NULL,
    "id" VARCHAR NOT NULL PRIMARY KEY);

create table "goodLogs" (
    "name" VARCHAR NOT NULL,
    "description" VARCHAR,
    "createAt" TIMESTAMP NOT NULL,
    "goodId" VARCHAR NOT NULL, "logId" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);

alter table "goodLogs"
    add constraint "good_log_fk" foreign key("goodId")
    references "goods"("id") on update RESTRICT on delete CASCADE;

# --- !Downs
drop table if exists "goods";
drop table if exists "goodLogs";
