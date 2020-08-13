# --- First database schema
# --- !Ups
create table IF NOT EXISTS `Users` (
                                       `username` VARCHAR NOT NULL,
                                       `password` VARCHAR NOT NULL,
                                       `userType` VARCHAR NOT NULL,
                                       `id` BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);

insert into "Users" ("username", "password", "userType")
values ('corkine', 'mi960032', 'Admin');

insert into "Users" ("username", "password", "userType")
values ('marvin', 'marvin123', 'Common');

# --- !Downs
drop table if exists Users;
