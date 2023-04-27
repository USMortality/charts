# Data Format
## Input Table
Fields: iso3c, date, age_group, deaths, population
- iso3c: <string>, e.g.: ['USA', 'DEU'] or ['USA-CA', 'DEU-BW']
- date: daily e.g. <date>'2021-08-10'
- age_group: <string>, 'all' and e.g. optionally by age group ['0-14', ..., '85+']
- deaths: <number>
- population: <number>

### Input Table Validation
- check for gaps
- check for age_group: if 'all' not available, make 'all' rows

## Output Table
Fields: iso3c, date, deaths, population, cmr, asmr, asmr_esp2013, asmr_usa2000, asmr_can2011, asmr_country2020
