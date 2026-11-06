CREATE TABLE eduvos_qualifications (
	qualification_id SERIAL PRIMARY KEY,
	title TEXT NOT NULL,
	degree_type TEXT NOT NULL,
	faculty TEXT NOT NULL,
	description TEXT,
	start_date DATE,
	nqf_level INTEGER,
	desc_words INTEGER,
	title_chars INTEGER,
	days_to_start INTEGER,
	description_missing BOOLEAN,
	start_missing BOOLEAN,
	url TEXT);

SELECT COUNT(*) AS rows_loaded FROM eduvos_qualifications;
SELECT * FROM eduvos_qualifications LIMIT 5;

-- Making correction to the dataset
UPDATE eduvos_qualifications SET nqf_level = 8 WHERE title ILIKE '%Honours%';
UPDATE eduvos_qualifications SET nqf_level = 10 WHERE title ILIKE '%Doctor of Philosophy%';
UPDATE eduvos_qualifications SET nqf_level = NULL WHERE degree_type = 'Access';

SELECT degree_type, nqf_level, COUNT(*) AS qualifications
FROM eduvos_qualifications
GROUP BY degree_type, nqf_level
ORDER BY degree_type, nqf_level;

--Investigation 1: WHat does each faculty offer?
SELECT faculty,
	COUNT(*) AS qualifications,
	ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS pct_of_catalogue,
	ROUND(AVG(nqf_level), 1) AS avg_nqf_level,
	COUNT(*) FILTER (WHERE nqf_level >= 8) AS postgraduate_level
FROM eduvos_qualifications
GROUP BY faculty
ORDER BY qualifications DESC;

--Results
--There are 66 qualifications:
--Commerce 21, avg nqf = 7,5 and 5 postgraduate programs
--Technology 20, avg nqf = 6,8 and 7 postgraduate programs
--Humanities and Arts 25, avg nqf = 6,9 and 1 postgraduate program
--Science 4, avg nqf = 6,3 and 0 postgraduate programe
--Law 3, avg nqf = 7,7 and 1 postgraduate program
--Unknown 3, avg nqf = 0 because the short courses are not recognised

--INTERPRETATION:
--   Commerce and Technology together make up about 62% of the catalogue, so Eduvos is
--   mainly a business and IT institution. Technology has the deepest postgraduate offering
--   (7 of the 12 postgraduate-level programmes: honours and masters), which gives its
--   students the clearest progression from higher certificate to masters. Commerce has the
--   highest average level among the large faculties (7.5) because it has degrees, honours,
--   postgraduate diplomas and a masters. Law has only 3 programmes but the highest average
--   NQF level (7.7), since it consists of degree and masters qualifications. Humanities and
--   Arts is large (22.7%) but almost entirely undergraduate (1 postgraduate programme), and
--   Science is small and offers no postgraduate programme. The "Unknown" faculty rows are
--   short courses whose titles do not name a faculty, so they could not be classified.

--Investigation 2: When are programmes starting?
SELECT start_date,
       COUNT(*) AS programmes_starting,
       ROUND(AVG(desc_words), 1) AS avg_description_words,
       COUNT(*) FILTER (WHERE description_missing) AS missing_descriptions,
       STRING_AGG(DISTINCT degree_type, ', ' ORDER BY degree_type) AS programme_types
FROM   eduvos_qualifications
GROUP  BY start_date
ORDER  BY start_date;

--RESULT:
--54 programmes under Degrees, honours, masters, higher certificates, access and pre-degree programmes start: 2027-02-15
--10 programmes under Corporate Training only start: 2027-01-11
--2 programmes under Diploma start: 2027-01-15

--INTERPRETATION:
--   Intake is heavily concentrated: 54 of the 66 programmes (about 82%) start on 15 February
--   2027, so February is the main intake for every qualification type from higher
--   certificates to masters. The 10 corporate-training short courses start earlier, on
--   11 January, and the 2 postgraduate diploma programmes in accounting start on 15 January,
--   so these are the only programmes a student can begin in January. No programme is missing
--   a description, and the average length is similar for every date