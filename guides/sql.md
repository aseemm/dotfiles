# mysql --local-infile=1 -u root -p
> show databases;
> use <database>;
> show tables;
> describe <table> (or) show fields from <table> (or) show columns from <table>
> show index from <table>

create database library; 
use library;
create table book (id INT NOT NULL AUTO_INCREMENT PRIMARY KEY, title VARCHAR(256), author VARCHAR(256), link TEXT) ENGINE = MYISAM CHARACTER SET utf8 COLLATE utf8_general_ci;
LOAD DATA LOCAL INFILE 'books.tsv' INTO TABLE book FIELDS TERMINATED BY '\t' ENCLOSED BY '"' LINES TERMINATED BY '\n';
select * from book;

# backup & restore sql databases
mysqldump -u root -p<password> <database> > <database_backup>.sql
mysqldump -u root -p<password> <database> < <database_backup>.sql

# backup & restore sqlite databases
sqllite3 sample.db .dump > sample.bak
sqlite3 sample.db < sample.bak