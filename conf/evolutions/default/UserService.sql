# --- First database schema
# --- !Ups
create table IF NOT EXISTS `users` (
                                       `username` VARCHAR NOT NULL,
                                       `password` VARCHAR NOT NULL,
                                       `userType` VARCHAR NOT NULL,
                                       `id` BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT);

insert into "users" ("username", "password", "userType")
values ('corkine', 'mi960032', 'Admin');

insert into "users" ("username", "password", "userType")
values ('marvin', 'marvin123', 'Common');

# --- !Downs
drop table if exists users;
