psql postgres -c 'DROP DATABASE opallib';
psql postgres -c 'CREATE DATABASE opallib';
psql opallib -f setup.sql
