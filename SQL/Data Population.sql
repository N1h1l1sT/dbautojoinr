
-- Populating [DIM_Region]
INSERT INTO [dbautojoinr].[dbo].[DIM_Region] (Region_Name)
VALUES (N'Region #1'),
       (N'Region #2'),
	   (N'Region #3'),
	   (N'Region #4');

	   
-- Populating [DIM_Site]
INSERT INTO [dbautojoinr].[dbo].[DIM_Site] (Site_RegionID, Site_Name, Site_Useless1, Site_Useless3)
VALUES (1, N'OR', 0,0),
	   (1, N'Offices', 0,0),
	   (1, N'Central Building', 0,0),
       (2, N'North Bullding', 0,0),
	   (3, N'South Bullding', 0,0),
	   (2, N'Clinic', 0,0);


-- Populating [DIM_Employee]
INSERT INTO [dbautojoinr].[dbo].[DIM_Employee] (Employee_FirstName, Employee_LastName, Employee_SiteID)
VALUES (N'Ioannis', N'Mamalikidis', 6),
       (N'John', N'Doe', 2),
	   (N'Jane', N'Doh', 2),
	   (N'Rich', N'DotCom', 5);

	   
-- Populating [[FACT_Hours]]
INSERT INTO [dbautojoinr].[dbo].[FACT_Hours] (Hours_Value, Hours_Date, Hours_SiteID, Hours_EmployeeID, Hours_Useless1, Hours_Useless2, Hours_Useless3, Hours_Useless4)
VALUES (4.7, GETUTCDATE(), 1, 1, 0,0,0,0),
       (5.7, GETUTCDATE(), 2, 2, 0,0,0,0),
	   (8, GETUTCDATE(), 3, 3, 0,0,0,0),
	   (4.2, GETUTCDATE(), 4, 4, 0,0,0,0),
	   (2.6, DATEADD(DD,-1,GETUTCDATE()), 5, 1, 0,0,0,0),
	   (6.3, DATEADD(DD,-1,GETUTCDATE()), 6, 2, 0,0,0,0),
	   (7, DATEADD(DD,-1,GETUTCDATE()), 3, 3, 0,0,0,0),
	   (1.5, DATEADD(DD,-1,GETUTCDATE()), 6, 4, 0,0,0,0);
