with voy as(
	select 
		a.vessel,
		a.date_depart,
		a.poi_depart,
		b.cd_name as poiname_depart,
		c.name as port_depart,
		c.country as country_depart,
		c.area as region_depart,
		a.date_arrive,
		a.poi_arrive,
		d.cd_name as poiname_arrive,
		e.name as port_arrive,
		e.country as country_arrive,
		e.area as region_arrive,
		f.crude_code as crude_name,
		probability
	from 
		v_global_crude a,
		as_poi b,
		port c,
		as_poi d,
		port e,
		cat_crude f
	where
		f.crude_code = a.grade
		and a.poi_arrive = b.poi
		and b.port = c.code
		and a.poi_depart = d.poi
		and d.port = e.code
		and date_depart >= date '2017-08-01'
		and date_arrive <= date '2017-08-31'
), dv as (
	select 
		distinct(vessel),
		tanker.name
	from 
		asvt_position,
		tanker
	where
		tanker.imo = asvt_position.imo
		and tanker.name ilike '%new%creation%'
)select
	dv.name,
	voy.vessel,
	voy.date_depart,
	voy.poiname_depart,
	voy.port_depart,
	voy.country_depart,
	voy.region_depart,
	voy.date_arrive,
	voy.poiname_arrive,
	voy.port_arrive,
	voy.country_arrive,
	voy.region_arrive,
	voy.crude_name,
	probability
from
	dv,
	voy
where
	voy.vessel in (select vessel from dv)
/*
---permission denied---
create function testleycan(
	tanker varchar(255),
	date_arrive date,
	date_depart date
) returns void as $$
begin
	return select * from v_global_crude limit 2;
end;$$ language plpsql;
*/

select distinct(vessel), tanker.imo from asvt_position, tanker 
	where asvt_position.imo = tanker.imo and 
select * from tanker where tanker.name ilike 'new crEation'
select * from  where imo = 9340635
select * from v_global_crude where vessel = 100306 and date_depart>=date'2017-08-01' and date_arrive<=date'2017-08-17';

--select vessel from asvt_position where vessel in (select vessel from)

/*
select * from as_poi limit 50;
select * from port limit 50;
select count(*) from asvt_position
select * from tanker limit 2;

select 
		poi_depart,
		date_depart,
		poi_arrive,
		date_arrive,
		region_arrive,
		b.name as crude_name,
		region_arrive,
		a.source
	from 
		v_global_crude a,
		cat_crude b,
		asvt_position c,
		tanker d
	where 
		b.crude_code = a.grade
		and a.vessel = c.vessel
		and c.imo = d.imo
		and date_depart>=date'2017-08-01'
		and d.name ilike '%v.%k.%eddie%'


/*
select * from dev.zas_lukoil_vessels limit 5 - Source file from client
select * from live.v_global_crude limit 5 - All of our crude voyages
select * from live.tanker limit 5 - tanker information (names/IMOs)
select * from live.as_vessel_exp limit 5 - matches IMO -> vessel ID
*/

/*
select * from v_global_crude limit 2;
select * from cat_crude limit 2;

with result as (
	select 
		poi_depart,
		date_depart,
		poi_arrive,
		date_arrive,
		region_arrive,
		name as crude_name,
		region_arrive,
		source
	from 
		v_global_crude a,
		cat_crude b,
		asvt_position c,
		tanker d,
	where 
		b.crude_code = a.grade
		and a.vessel = c.vessel
		and c.imo = d.imo
		and date_depart>=date'2017-08-01'
		and d.name ilike '%v.%k.%eddie%')