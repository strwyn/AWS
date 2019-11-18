select 
	reason,
	vessel
	imo,
	operation_date,
	report_date,
	archive_time,
	grade,
	grade_raw,
	quantity,
	unit
	port,
	operation_raw,
	operation,
	rank_operation,
	direction,
	lo_country_code,
	lo_city_code,
	charterer,
	supplier,
	receiver,
	source_file,
	source_sheet,
	source_imo,
	min_dist,
	f_get_bargo(imo,(operation_date - interval '10 days')::date,(operation_date + interval '10 days')::date) bargo
from
	alert.inchcape_iss_process_discard
where
	reason !~* 'dup' and
	reason !~* 'no vessel in tanker table' and
	source_file = 'gs101' AND
	grade in (select PRODUCT_CODE from cat_product where cd_report in ('GASOLINE','MIDDLE DISTILLATES','BIOFUELS')) 
order by 1,quantity desc