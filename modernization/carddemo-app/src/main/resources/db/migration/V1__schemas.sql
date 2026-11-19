-- One schema per bounded context (target state DATA profile); `batch` holds the Spring Batch JobRepository and job staging.
CREATE SCHEMA IF NOT EXISTS account;
CREATE SCHEMA IF NOT EXISTS card;
CREATE SCHEMA IF NOT EXISTS customer;
CREATE SCHEMA IF NOT EXISTS transaction;
CREATE SCHEMA IF NOT EXISTS reference;
CREATE SCHEMA IF NOT EXISTS batch;
