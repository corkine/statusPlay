# --- 4th database schema
# --- !Ups
create table "foods" ("name" VARCHAR NOT NULL,
    "picture" VARCHAR,"description" VARCHAR,
    "kind" VARCHAR,"buyEatIntervalDay" INTEGER NOT NULL,"evilDegree" INTEGER,
    "hungerDegree" INTEGER,"addTime" TIMESTAMP NOT NULL, "finishTime" TIMESTAMP,
    "id" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT)


# --- !Downs
drop table if exists "foods"