SELECT Id, ActivityDate, TotalSteps, Calories
INTO Daily_Activity
FROM dbo.dailyActivity_merged

SELECT *
INTO Daily_Steps
FROM dbo.dailySteps_merged

SELECT *
INTO Daily_Sleep
FROM dbo.sleepDay_merged

SELECT *
INTO Hourly_Steps
FROM dbo.hourlySteps_merged




----- Daily Activity


EXEC sp_rename 'Daily_Activity.Id', 'id', 'COLUMN';
EXEC sp_rename 'Daily_Activity.ActivityDate', 'date', 'COLUMN';
EXEC sp_rename 'Daily_Activity.TotalSteps', 'steps', 'COLUMN';
EXEC sp_rename 'Daily_Activity.Calories', 'calories', 'COLUMN';

UPDATE Daily_Activity
SET date = CONVERT(date, date, 101)




----- Daily Sleep

EXEC sp_rename 'Daily_Sleep.Id', 'id', 'COLUMN';
EXEC sp_rename 'Daily_Sleep.TotalMinutesAsleep', 'minutes_asleep', 'COLUMN';
EXEC sp_rename 'Daily_Sleep.TotalTimeInBed', 'minutes_in_bed', 'COLUMN';


ALTER TABLE Daily_Sleep
ADD [date] DATE,
    [time] TIME(0);

UPDATE Daily_Sleep
SET [date] = CONVERT(DATE, SleepDay, 101),
    [time] = CONVERT(TIME(0), SleepDay);

ALTER TABLE Daily_Sleep
DROP COLUMN SleepDay;

ALTER TABLE Daily_Sleep
DROP COLUMN TotalSleepRecords



----- Daily Steps


EXEC sp_rename 'Daily_Steps.Id', 'id', 'COLUMN';
EXEC sp_rename 'Daily_Steps.ActivityDay', 'date', 'COLUMN';
EXEC sp_rename 'Daily_Steps.StepTotal', 'steps', 'COLUMN';

UPDATE Daily_Steps
SET date = CONVERT(date, date, 101)




----- Hourly Steps


EXEC sp_rename 'Hourly_Steps.Id', 'id', 'COLUMN';
EXEC sp_rename 'Hourly_Steps.StepTotal', 'steps', 'COLUMN';

ALTER TABLE Hourly_Steps
ADD [date] DATE,
    [time] TIME(0);

UPDATE Hourly_Steps
SET [date] = CONVERT(DATE, ActivityHour, 101),
    [time] = CONVERT(TIME(0), ActivityHour);

ALTER TABLE Hourly_Steps
DROP COLUMN ActivityHour;



