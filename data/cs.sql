DROP DATABASE cs;
CREATE DATABASE cs;
use cs;

CREATE TABLE tbl_user (
	username varchar(30) PRIMARY KEY,
	password VARCHAR(30) NOT NULL,
	fullname NVARCHAR(30) NOT NULL,
	phone VARCHAR(13),
	email VARCHAR(30),
	create_date varchar(20)
);

SELECT * FROM tbl_user