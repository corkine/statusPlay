# --- First database schema
# --- !Ups
create table "websites" ("name" VARCHAR NOT NULL,"url" VARCHAR NOT NULL,"note" VARCHAR,"priority" INTEGER NOT NULL,"id" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);

create table "activities" ("website_id" BIGINT NOT NULL,"checkTime" TIMESTAMP NOT NULL,"status" INTEGER NOT NULL,"note" VARCHAR,"id" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);

alter table "activities" add constraint "website" foreign key("website_id") references "websites"("id") on update RESTRICT on delete CASCADE;

create table "datas" ("category" VARCHAR NOT NULL,"value" INTEGER NOT NULL,"unit" VARCHAR NOT NULL,"startTime" TIMESTAMP NOT NULL,"endTime" TIMESTAMP NOT NULL,"duration" BIGINT NOT NULL,"id" BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);


insert into "websites" ("name", "url", "note", "priority")
values ( '主页', 'http://www.mazhangjing.com', null, 1 );

insert into "websites" ("name", "url", "note", "priority")
values ( '博客', 'http://blog.mazhangjing.com', null, 1 );

insert into "websites" ("name", "url", "note", "priority")
values ( '代码存储服务', 'http://git.mazhangjing.com', null, 1 );

insert into "websites" ("name", "url", "note", "priority")
values ( '图床服务', 'http://static2.mazhangjing.com', null, 1 );

insert into "websites" ("name", "url", "note", "priority")
values ( '短链接服务', 'http://go.mazhangjing.com', null, 1 );

insert into "websites" ("name", "url", "note", "priority")
values ( '成才教育', 'http://edu.mazhangjing.com', null, 1 );

insert into "websites" ("name", "url", "note", "priority")
values ( 'Corkine & We', 'http://love.mazhangjing.com', null, 1 );

# --- !Downs
drop table if exists "websites";
drop table if exists "activities";
drop table if exists "datas"
