use ims_catalog
go

select
	substring((convert(char(20),NEAR_START_LAT)),1,6), 
	substring((convert(char(20),NEAR_START_LON)),1,7), 
	substring((convert(char(20),NEAR_END_LAT)),1,6), 
	substring((convert(char(20),NEAR_END_LON)),1,7), 
	substring((convert(char(20),FAR_START_LAT)),1,6), 
	substring((convert(char(20),FAR_START_LON)),1,7), 
	substring((convert(char(20),FAR_END_LAT)),1,6), 
	substring((convert(char(20),FAR_END_LON)),1,7) 
	from granules_112 where REVOLUTION = 8165
order by
	FRAME_ID
go
