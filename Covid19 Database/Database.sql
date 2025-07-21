CREATE TABLE Country(
CountryName TEXT,
CountryISO TEXT PRIMARY KEY NOT NULL,
SourceName TEXT,
URL TEXT,
LastUpdated DATE
);

CREATE TABLE Vaccine(
Vaccine TEXT PRIMARY KEY NOT NULL
);

CREATE TABLE AgeGroup(
AgeGroup TEXT PRIMARY KEY NOT NULL
);

CREATE TABLE VaccineCountry(
VaccineName TEXT,
CountryISO TEXT,
FOREIGN KEY (CountryISO) REFERENCES Country(CountryISO),
FOREIGN KEY (VaccineName) REFERENCES Vaccine(Vaccine),
PRIMARY KEY (VaccineName, CountryISO)
);

CREATE TABLE VaccineStatAge(
Date DATE NOT NULL,
PeopleVaccinatedPerHundred NUMERIC,
PeopleFullyVaccinatedPerHundred NUMERIC,
PeopleWBoosterPerHundred NUMERIC,
AgeGroup TEXT NOT NULL,
CountryISO TEXT NOT NULL,
FOREIGN KEY (AgeGroup) REFERENCES AgeGroup(Agegroup)
FOREIGN KEY (CountryISO) REFERENCES Country(CountryISO),
PRIMARY KEY (Date, AgeGroup, CountryISO)
);

CREATE TABLE VaccineStatOverall(
Date DATE NOT NULL,
CountryISO TEXT NOT NULL,
TotalVaccinations NUMERIC,
PeopleVaccinated NUMERIC,
PeopleFullyVaccinated NUMERIC,
TotalBoosters NUMERIC,
DailyVaccinationsRaw NUMERIC,
DailyVaccinations NUMERIC,
TotalVaccPerHundred NUMERIC,
PeopleVaccPerHundred NUMERIC,
PeopleFullyVaccPerHundred NUMERIC,
TotalBoostersPerHundred NUMERIC,
DailyVaccPerMillion NUMERIC,
DailyPeopleVaccinated NUMERIC,
DailyPeopleVaccinatedPerHundred NUMERIC,
FOREIGN KEY (CountryISO) REFERENCES Country(CountryISO),
PRIMARY KEY (Date, CountryISO)
);

CREATE TABLE VaccinationStatRegion(
Date DATE NOT NULL,
CountryISO TEXT NOT NULL,
RegionName TEXT NOT NULL,
TotalVaccinations NUMERIC,
TotalDistributed NUMERIC,
PeopleVaccinated NUMERIC,
PeopleFullyVaccPerHundred NUMERIC,
TotalVaccPerHundred NUMERIC,
PeopleFullyVaccinated NUMERIC,
PeopleVaccPerHundred NUMERIC,
DistributedPerHundred NUMERIC,
DailyVaccinationsRaw NUMERIC,
DailyVaccinations NUMERIC,
DailyVaccPerMillion NUMERIC,
ShareDosesUsed NUMERIC,
TotalBoosters NUMERIC,
TotalBoostersPerHundred NUMERIC,
FOREIGN KEY (CountryISO) REFERENCES Country(CountryISO),
PRIMARY KEY (Date, CountryISO, RegionName)
);

CREATE TABLE VaccineByManufacturer(
Date DATE NOT NULL,
TotalVaccinations NUMERIC,
VaccineName TEXT NOT NULL,
CountryISO TEXT NOT NULL,
FOREIGN KEY (VaccineName) REFERENCES Vaccine(Vaccine),
FOREIGN KEY (CountryISO) REFERENCES Country(CountryISO),
PRIMARY KEY (Date, VaccineName, CountryISO)
);

