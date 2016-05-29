DROP DATABASE cs;
CREATE DATABASE cs;
use cs;

CREATE TABLE tbl_user (
	username varchar(30) PRIMARY KEY,
	password VARCHAR(30) NOT NULL,
	fullname NVARCHAR(30) NOT NULL,
	phone VARCHAR(13),
	email VARCHAR(30),
	avatar VARCHAR(30),
	create_date varchar(20)
);

CREATE TABLE tbl_friend (
	friend_id varchar(30) PRIMARY KEY,
	user1 VARCHAR(30) NOT NULL,
	user2 NVARCHAR(30) NOT NULL,
	create_date varchar(20)
);

CREATE TABLE tbl_friend_request (
	request_id varchar(30) PRIMARY KEY,
	from_user VARCHAR(30) NOT NULL,
	to_user NVARCHAR(30) NOT NULL,
	create_date varchar(20)
);
