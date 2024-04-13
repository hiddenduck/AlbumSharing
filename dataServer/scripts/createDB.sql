CREATE DATABASE album;

\c album;

CREATE TABLE data (
	hash CHAR(32) PRIMARY KEY,
	data BYTEA
);



