DROP TABLE IF EXISTS agencies;
DROP TABLE IF EXISTS externaltours;

CREATE TABLE agencies (
    a_id       integer primary key,
    a_name     text,
    a_based_in text,
    a_phone    text
) WITH OIDS;

CREATE TABLE externaltours (
    et_id          integer primary key,
    et_name        text,
    et_destination text,
    et_type        text,
    et_price       integer
) WITH OIDS;

insert into agencies (a_id, a_name, a_based_in, a_phone) values
  (1, 'EdinTours', 'Edinburgh', '412 1200'),
  (2, 'Burns''s' , 'Glasgow'  , '607 3000');

insert into externaltours (et_id, et_name, et_destination, et_type, et_price) values
  (3, 'EdinTours', 'Edinburgh'     , 'bus'  ,  20),
  (4, 'EdinTours', 'Loch Ness'     , 'bus'  ,  50),
  (5, 'EdinTours', 'Loch Ness'     , 'boat' , 200),
  (6, 'EdinTours', 'Firth of Forth', 'boat' ,  50),
  (7, 'Burns''s' , 'Islay'         , 'boat' , 100),
  (8, 'Burns''s' , 'Mallaig'       , 'train',  40);
