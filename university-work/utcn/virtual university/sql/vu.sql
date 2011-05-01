-- trebuie inlocuite cele doua cai 'c:\program files\...\data\'
-- cu cele dorite
create database vu
	on (
		name = vu,
		filename = 'c:\program files\microsoft sql server\mssql\data\vu.mdf',
		maxsize = unlimited
	)
	log on (
		name = logVu,
		filename = 'c:\program files\microsoft sql server\mssql\data\vu.ldf',
		maxsize = unlimited
	)

use vu

create table Users (
	user_ID uniqueidentifier not null primary key,
	first_name varchar(50),
	last_name varchar(50),
	address varchar(250),
	city varchar(25),
	country varchar(25),
	phone_nr varchar(20),
	e_mail varchar(100)
)

create table Role (
	role_ID uniqueidentifier not null primary key,
	description varchar(50)
)

create table User_Role (
	user_ID uniqueidentifier not null foreign key references Users(user_ID) on update cascade on delete cascade,
	role_ID uniqueidentifier not null foreign key references Role(role_ID) on update cascade,
	login_name varchar(50),
	password varchar(50),
	primary key (user_ID, role_ID)
	unique (login_name, password)
)

create table CourseAuthor (
	author_ID uniqueidentifier not null primary key,
	first_name varchar(50),
	last_name varchar(50),
	e_mail varchar(100)
)

create table Course (
	course_ID uniqueidentifier not null primary key,
	author_ID uniqueidentifier not null foreign key references CourseAuthor(author_ID) on update cascade,
	name varchar(50),
	description varchar(500),	-- va fi un URI
	bibliography varchar(500),	-- va fi un URI
	content	varchar(500),		-- va fi un URI
	objectives varchar(500),	-- va fi un URI
	prerequisites varchar(500),	-- va fi un URI
	start_page varchar(500)		-- va fi un URI
)

create table ResourceType (
	resource_ID uniqueidentifier not null primary key,
	description char(3) check (description = 'url' or description = 'txt' or description = 'fil')
)

insert into ResourceType
values (newID(), 'url')

insert into ResourceType
values (newID(), 'txt')

insert into ResourceType
values (newID(), 'fil')

create table Class (
	class_ID uniqueidentifier not null primary key,
	name varchar(50) not null,
	course_ID uniqueidentifier not null foreign key references Course(course_ID) on update cascade,
	start_date datetime not null,
	end_date datetime not null,
	teacher_ID uniqueidentifier not null foreign key references Users(user_ID),
	assistent_ID uniqueidentifier foreign key references Users(user_ID),
	schedule_type uniqueidentifier foreign key references ResourceType(resource_ID),
	schedule varchar(1400),
	policies_type uniqueidentifier foreign key references ResourceType(resource_ID),
	policies varchar(1400),
	grading_info_type uniqueidentifier foreign key references ResourceType(resource_ID),
	grading_info varchar(1400),
	references_type uniqueidentifier foreign key references ResourceType(resource_ID),
	references varchar(1400),
	additional_info_type uniqueidentifier foreign key references ResourceType(resource_ID),
	additional_info varchar(1400),
	check (start_date < end_date)
)

create table Student_Class (
	student_ID uniqueidentifier not null primary key,
	user_ID uniqueidentifier not null foreign key references Users(user_ID) on update cascade on delete cascade,
	class_ID uniqueidentifier not null foreign key references Class(class_ID) on update cascade on delete cascade,
)

create table Conference (
	class_ID uniqueidentifier not null foreign key references Class(class_ID) on update cascade on delete cascade,
	idx smallint not null,
	name varchar(50) not null,
	description varchar(5000),
	visibility tinyint not null check (visibility <= 2),
	start_date datetime,
	student_interactivity tinyint not null check (student_interactivity <= 3),
	creation_date datetime not null,
	last_modification_date datetime not null,
	primary key (class_ID, idx)
)

create table Message (
	message_ID uniqueidentifier not null primary key,
	main_topic_flag bit not null,
	title varchar(50) not null,
	message varchar(7900),
	parent_ID uniqueidentifier foreign key references Message(message_ID),
	class_ID uniqueidentifier not null,
	idx smallint not null,
	author_ID uniqueidentifier not null foreign key references Users(user_ID),
	creation_date datetime not null,
	foreign key (class_ID, idx) references Conference(class_ID, idx) on update cascade,
	check (((main_topic_flag = 0) and (parent_ID is not null)) or (main_topic_flag = 1))
)

create table User_Message (
	user_ID uniqueidentifier not null foreign key references Users(user_ID) on update cascade on delete cascade,
	message_ID uniqueidentifier not null foreign key references Message(message_ID) on update cascade on delete cascade,
	primary key (user_ID, message_ID)
)

create table Announcement (
	class_ID uniqueidentifier not null foreign key references Class(class_ID) on update cascade on delete cascade,
	announcement_ID uniqueidentifier not null primary key,
	announcement varchar(7000) not null,
	creation_date datetime not null,
	last_modification_date datetime not null,
	start_date datetime,
	end_date datetime
)

create table Task (
	class_ID uniqueidentifier not null foreign key references Class(class_ID) on update cascade on delete cascade,
	idx int not null,
	title varchar(50) not null,
	description varchar(5000),
	difficulty varchar(100),
	creation_date datetime not null,
	last_modification_date datetime not null,
	primary key (class_ID, idx)
)

create table Assignment (
	class_ID uniqueidentifier not null,
	idx int not null,
	task_idx int not null,
	student_ID uniqueidentifier not null foreign key references Student_Class(student_ID),
	assigned_on datetime not null,
	due_on datetime,
	submitted_on datetime,
	allow_late_submission bit, -- 0 (no), 1 (yes)
	status tinyint not null check (status >= 1 and status <= 4), -- 1 (unsubmitted), 2 (submitted), 3 (read), 4 (graded)
	grade varchar(20),
	grade_comments varchar(1500),
	teacher_comments varchar(1500),
	teacher_notes varchar(1500),
	answer varchar(3400),
	primary key (class_ID, idx),
	foreign key (class_ID, task_idx) references Task(class_ID, idx) on update cascade on delete cascade,
)

create table GradeColumns(
	class_ID uniqueidentifier not null foreign key references Class(class_ID)
		on update cascade on delete cascade,
	column_ID uniqueidentifier not null primary key,
	column_name varchar(30) not null,
	creation_date Datetime not null
)

create table StudentGrades(
	student_ID uniqueidentifier not null foreign key references 
		Student_Class(student_ID),
	column_ID uniqueidentifier not null foreign key references
		GradeColumns(column_ID) on delete cascade on update cascade,
	grade varchar(20),
	primary key(student_ID, column_ID)
)
