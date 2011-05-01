-- insert data in Users table
insert into Users values (newid(), 'Bogdan', 'Dumitriu', 'Str. Malinului nr. 11', 'Cluj-Napoca', 'Romania', '092629000', 'bdumitriu@bdumitriu.ro')

-- insert data in CourseAuthor table
insert into CourseAuthor values (newid(), 'Ioan', 'Salomie', 'Ioan.Salomie@cs.utcluj.ro')

-- insert data in Role table
insert into Role values (newid(), 'admin')
insert into Role values (newid(), 'user')
insert into Role values (newid(), 'visitor')

-- insert data in User_Role table
declare @uid uniqueidentifier, @rid uniqueidentifier
set @uid = (select user_ID from Users where last_name = 'Dumitriu')
set @rid = (select role_ID from Role where description = 'user')
insert into User_Role values (@uid, @rid, 'bdumitriu', 'bdumitriu')

-- insert data in Course table
declare @aid uniqueidentifier
set @aid = (select author_ID from CourseAuthor where last_name = 'Salomie')
insert into Course values (newid(), @aid, 'Object Oriented Programming', 'html/courses/oop/course-description.html', 'html/courses/oop/course-bibliography.html', 'html/courses/oop/course-contents.html', 'html/courses/oop/course-objectives.html', 'html/courses/oop/course-prerequisites.html', 'html/courses/oop/index.html')

-- insert data in Class table
declare @uid uniqueidentifier, @cid uniqueidentifier
set @uid = (select user_ID from Users where last_name = 'Dumitriu')
set @cid = (select course_ID from Course where name = 'object oriented programming')
insert into Class (class_id, course_id, name, start_date, end_date, teacher_id) values (newid(), @cid, 'OOP1', '02/01/2002', '06/30/2002', @uid)

-- insert data in Student_Class table
declare @uid uniqueidentifier, @cid uniqueidentifier
set @uid = (select user_ID from Users where last_name = 'Marian')
set @cid = (select class_ID from Class where name = 'OOP1')
insert into Student_Class
values (newid(), @uid, @cid)

-- insert data in ResourceType table
insert into resourcetype
values(newid(), 'url')

insert into resourcetype
values(newid(), 'txt')

insert into resourcetype
values(newid(), 'fil')
