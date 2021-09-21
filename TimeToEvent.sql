SELECT t.subject_id,
	 t.cohort_start_date,
	 coalesce(min(o.cohort_start_date), max(t.cohort_end_date)) AS event_date,
	 CASE WHEN min(o.cohort_start_date) IS NULL THEN 0 ELSE 1 END AS event
FROM alex_alexeyuk_results.union_table t
LEFT JOIN alex_alexeyuk_results.union_table o
  ON t.subject_id = o.subject_id
	  AND o.cohort_start_date >= t.cohort_start_date
	  AND o.cohort_start_date <= t.cohort_end_date
	  AND o.cohort_definition_id = 3
WHERE t.cohort_definition_id = 1
GROUP BY t.subject_id, t.cohort_start_date