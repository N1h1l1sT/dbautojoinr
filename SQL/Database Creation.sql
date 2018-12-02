USE [master]
GO

CREATE DATABASE [dbautojoinr]
-- CONTAINMENT = NONE
-- ON  PRIMARY 
--( NAME = N'dbautojoinr', FILENAME = N'C:\Users\GiannisM\Dropbox\Data Science\Code Pre-Made\dbautojoinr\Database\dbautojoinr.mdf' , SIZE = 8192KB , MAXSIZE = UNLIMITED, FILEGROWTH = 65536KB )
-- LOG ON 
--( NAME = N'dbautojoinr_log', FILENAME = N'C:\Users\GiannisM\Dropbox\Data Science\Code Pre-Made\dbautojoinr\Database\dbautojoinr_log.ldf' , SIZE = 8192KB , MAXSIZE = 2048GB , FILEGROWTH = 65536KB )
GO

--ALTER DATABASE [dbautojoinr] MODIFY FILE
--( NAME = N'dbautojoinr', SIZE = 512MB, MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB )
--GO

--ALTER DATABASE [dbautojoinr] MODIFY FILE
--( NAME = N'dbautojoinr_log', SIZE = 256MB, MAXSIZE = UNLIMITED, FILEGROWTH = 10% )
--GO

ALTER DATABASE [dbautojoinr] SET COMPATIBILITY_LEVEL = 140
GO

IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [dbautojoinr].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO

ALTER DATABASE [dbautojoinr] SET ANSI_NULL_DEFAULT OFF 
GO

ALTER DATABASE [dbautojoinr] SET ANSI_NULLS OFF 
GO

ALTER DATABASE [dbautojoinr] SET ANSI_PADDING OFF 
GO

ALTER DATABASE [dbautojoinr] SET ANSI_WARNINGS OFF 
GO

ALTER DATABASE [dbautojoinr] SET ARITHABORT OFF 
GO

ALTER DATABASE [dbautojoinr] SET AUTO_CLOSE OFF 
GO

ALTER DATABASE [dbautojoinr] SET AUTO_SHRINK OFF 
GO

ALTER DATABASE [dbautojoinr] SET AUTO_UPDATE_STATISTICS ON 
GO

ALTER DATABASE [dbautojoinr] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO

ALTER DATABASE [dbautojoinr] SET CURSOR_DEFAULT  GLOBAL 
GO

ALTER DATABASE [dbautojoinr] SET CONCAT_NULL_YIELDS_NULL OFF 
GO

ALTER DATABASE [dbautojoinr] SET NUMERIC_ROUNDABORT OFF 
GO

ALTER DATABASE [dbautojoinr] SET QUOTED_IDENTIFIER OFF 
GO

ALTER DATABASE [dbautojoinr] SET RECURSIVE_TRIGGERS OFF 
GO

ALTER DATABASE [dbautojoinr] SET  DISABLE_BROKER 
GO

ALTER DATABASE [dbautojoinr] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO

ALTER DATABASE [dbautojoinr] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO

ALTER DATABASE [dbautojoinr] SET TRUSTWORTHY OFF 
GO

ALTER DATABASE [dbautojoinr] SET ALLOW_SNAPSHOT_ISOLATION OFF 
GO

ALTER DATABASE [dbautojoinr] SET PARAMETERIZATION SIMPLE 
GO

ALTER DATABASE [dbautojoinr] SET READ_COMMITTED_SNAPSHOT OFF 
GO

ALTER DATABASE [dbautojoinr] SET HONOR_BROKER_PRIORITY OFF 
GO

ALTER DATABASE [dbautojoinr] SET RECOVERY FULL 
GO

ALTER DATABASE [dbautojoinr] SET  MULTI_USER 
GO

ALTER DATABASE [dbautojoinr] SET PAGE_VERIFY CHECKSUM  
GO

ALTER DATABASE [dbautojoinr] SET DB_CHAINING OFF 
GO

ALTER DATABASE [dbautojoinr] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO

ALTER DATABASE [dbautojoinr] SET TARGET_RECOVERY_TIME = 60 SECONDS 
GO

ALTER DATABASE [dbautojoinr] SET DELAYED_DURABILITY = DISABLED 
GO

ALTER DATABASE [dbautojoinr] SET QUERY_STORE = OFF
GO

ALTER DATABASE [dbautojoinr] SET  READ_WRITE 
GO

USE [dbautojoinr]
GO
-- /Database creation


--DIM_Region TABLE Creation
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DIM_Region](
	[Region_ID] [int] IDENTITY(1,1) NOT NULL,
	[Region_Name] [nvarchar](50) NOT NULL,
	[Region_Useless1] [nvarchar](255) NULL,
	[Region_Useless2] [int] NULL,
	[Region_EntryOn] [datetime2](0) NOT NULL,
	[Region_EntryBy] [nvarchar](60) NOT NULL,
 CONSTRAINT [PK_DIM_Region] PRIMARY KEY CLUSTERED 
(
	[Region_ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[DIM_Region] ADD  CONSTRAINT [DF_Region_Region_EntryOn]  DEFAULT (getutcdate()) FOR [Region_EntryOn]
GO

ALTER TABLE [dbo].[DIM_Region] ADD  CONSTRAINT [DF_Region_Region_EntryBy]  DEFAULT (suser_name()) FOR [Region_EntryBy]
GO
--/DIM_Region TABLE Creation


--DIM_Site TABLE Creation
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DIM_Site](
	[Site_ID] [int] IDENTITY(1,1) NOT NULL,
	[Site_RegionID] [int] NOT NULL,
	[Site_Name] [nvarchar](120) NOT NULL,
	[Site_Useless1] [int] NOT NULL,
	[Site_Useless2] [nvarchar](10) NULL,
	[Site_Useless3] [int] NOT NULL,
	[Site_EntryOn] [datetime2](0) NOT NULL,
	[Site_EntryBy] [nvarchar](60) NOT NULL,
 CONSTRAINT [PK_DIM_Site] PRIMARY KEY CLUSTERED 
(
	[Site_ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[DIM_Site] ADD  CONSTRAINT [DF_DIM_Site_Site_EntryOn]  DEFAULT (getutcdate()) FOR [Site_EntryOn]
GO

ALTER TABLE [dbo].[DIM_Site] ADD  CONSTRAINT [DF_DIM_Site_Site_EntryBy]  DEFAULT (suser_name()) FOR [Site_EntryBy]
GO

ALTER TABLE [dbo].[DIM_Site]  WITH CHECK ADD  CONSTRAINT [FK_DIM_Site_DIM_Region] FOREIGN KEY([Site_RegionID])
REFERENCES [dbo].[DIM_Region] ([Region_ID])
GO

ALTER TABLE [dbo].[DIM_Site] CHECK CONSTRAINT [FK_DIM_Site_DIM_Region]
GO

EXEC sys.sp_addextendedproperty @name=N'MS_Description', @value=N'on the input form there will only be one ObjectName, Text and Letter will be extracted' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'DIM_Site', @level2type=N'COLUMN',@level2name=N'Site_Useless1'
GO

EXEC sys.sp_addextendedproperty @name=N'MS_Description', @value=N'on the input form there will only be one ObjectName, Text and Letter will be extracted' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'DIM_Site', @level2type=N'COLUMN',@level2name=N'Site_Useless2'
GO
--/DIM_Site TABLE Creation


--DIM_Employee TABLE Creation
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[DIM_Employee](
	[Employee_ID] [int] IDENTITY(1,1) NOT NULL,
	[Employee_FirstName] [nvarchar](60) NOT NULL,
	[Employee_LastName] [nvarchar](60) NOT NULL,
	[Employee_SiteID] [int] NOT NULL,
	[Employee_EntryOn] [datetime2](0) NOT NULL,
	[Employee_EntryBy] [nvarchar](60) NOT NULL,
 CONSTRAINT [PK_DIM_Employee] PRIMARY KEY CLUSTERED 
(
	[Employee_ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[DIM_Employee] ADD  CONSTRAINT [DF_Employee_Employee_EntryOn]  DEFAULT (getutcdate()) FOR [Employee_EntryOn]
GO

ALTER TABLE [dbo].[DIM_Employee] ADD  CONSTRAINT [DF_Employee_Employee_EntryBy]  DEFAULT (suser_name()) FOR [Employee_EntryBy]
GO

ALTER TABLE [dbo].[DIM_Employee]  WITH CHECK ADD  CONSTRAINT [FK_DIM_Employee_DIM_Site] FOREIGN KEY([Employee_SiteID])
REFERENCES [dbo].[DIM_Site] ([Site_ID])
GO

ALTER TABLE [dbo].[DIM_Employee] CHECK CONSTRAINT [FK_DIM_Employee_DIM_Site]
GO
--/DIM_Employee TABLE Creation


--FACT_Hours TABLE Creation
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[FACT_Hours](
	[Hours_ID] [int] IDENTITY(1,1) NOT NULL,
	[Hours_Value] [decimal](4, 2) NOT NULL,
	[Hours_Date] [date] NOT NULL,
	[Hours_SiteID] [int] NOT NULL,
	[Hours_EmployeeID] [int] NOT NULL,
	[Hours_Useless1] [bit] NOT NULL,
	[Hours_Useless2] [bit] NOT NULL,
	[Hours_Useless3] [bit] NOT NULL,
	[Hours_Useless4] [bit] NOT NULL,
	[Hours_EntryOn] [datetime2](0) NOT NULL,
	[Hours_EntryBy] [nvarchar](60) NOT NULL,
 CONSTRAINT [PK_FACT_Hours] PRIMARY KEY CLUSTERED 
(
	[Hours_ID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[FACT_Hours] ADD  CONSTRAINT [DF_FACT_Hours_Hours_EntryOn]  DEFAULT (getutcdate()) FOR [Hours_EntryOn]
GO

ALTER TABLE [dbo].[FACT_Hours] ADD  CONSTRAINT [DF_FACT_Hours_Hours_EntryBy]  DEFAULT (suser_name()) FOR [Hours_EntryBy]
GO

ALTER TABLE [dbo].[FACT_Hours]  WITH CHECK ADD  CONSTRAINT [FK_FACT_Hours_DIM_Employee] FOREIGN KEY([Hours_EmployeeID])
REFERENCES [dbo].[DIM_Employee] ([Employee_ID])
GO

ALTER TABLE [dbo].[FACT_Hours] CHECK CONSTRAINT [FK_FACT_Hours_DIM_Employee]
GO

ALTER TABLE [dbo].[FACT_Hours]  WITH CHECK ADD  CONSTRAINT [FK_FACT_Hours_DIM_Site] FOREIGN KEY([Hours_SiteID])
REFERENCES [dbo].[DIM_Site] ([Site_ID])
GO

ALTER TABLE [dbo].[FACT_Hours] CHECK CONSTRAINT [FK_FACT_Hours_DIM_Site]
GO

EXEC sys.sp_addextendedproperty @name=N'MS_Description', @value=N'In each row, only 1 of the 4 (Hours worked, overtime, absence, sickness) will be filled in at a time. But one will be.' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'FACT_Hours', @level2type=N'COLUMN',@level2name=N'Hours_Value'
GO
--/FACT_Hours TABLE Creation