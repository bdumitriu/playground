-- This file contains all stored procedures currently used
-- by the Virtual University web application.

------------------------------------------------
-- BEGIN Conference related stored procedures --
------------------------------------------------

-- begin procedure
create procedure sp_insert_conference
	@class_id uniqueidentifier,
	@idx smallint output,
	@name varchar(50),
	@visibility tinyint,
	@student_interactivity tinyint,
	@start_date datetime = null,
	@description varchar(5000) = '' as
select @idx = max(idx)
from Conference
where class_ID = @class_id
if @idx is null
	set @idx = 0
set @idx = @idx + 1
insert into Conference values (@class_id, @idx, @name, @description, @visibility, @start_date, @student_interactivity, getdate(), getdate())
-- end procedure


-- begin procedure
create procedure sp_update_conference
	@class_id uniqueidentifier,
	@idx smallint,
	@name varchar(50),
	@visibility tinyint,
	@student_interactivity tinyint,
	@start_date datetime = null,
	@description varchar(5000) = '' as
update Conference
set name = @name,
	description = @description,
	visibility = @visibility,
	start_date = @start_date,
	student_interactivity = @student_interactivity,
	last_modification_date = getdate()
where class_id = @class_id and idx = @idx
-- end procedure


-- begin procedure
create procedure sp_delete_conference
	@class_id uniqueidentifier,
	@idx smallint as

delete from Conference
where class_id = @class_id and idx = @idx

declare @ret_val int
set @ret_val = @@rowcount

update Conference
set idx = idx - 1
where class_id = @class_id and idx > @idx

return @ret_val
-- end procedure


-- begin procedure
create procedure sp_list_conferences
	@class_id uniqueidentifier as
select class_id, idx, name, visibility, student_interactivity, start_date
from Conference
where class_ID = @class_id
order by idx
-- end procedure


-- begin procedure
create procedure sp_get_conference
	@class_id uniqueidentifier,
	@idx smallint as
select class_id, idx, name, description, visibility, student_interactivity, start_date
from Conference
where class_id = @class_id and @idx = idx
-- end procedure


-- begin procedure
create procedure sp_increase_conference_index
	@class_id uniqueidentifier,
	@incValue smallint as
update Conference
set idx = idx + @incValue
where class_id = @class_id
-- end procedure


-- begin procedure
create procedure sp_update_conference_index
	@class_id uniqueidentifier,
	@old_idx smallint,
	@new_idx smallint as
update Conference
set idx = @new_idx
where class_id = @class_id and idx = @old_idx
-- end procedure

----------------------------------------------
-- END Conference related stored procedures --
----------------------------------------------


---------------------------------------------------------
-- BEGIN Conference Messages related stored procedures --
---------------------------------------------------------

-- begin procedure
create procedure sp_insert_message
	@message_id uniqueidentifier,
	@title varchar(50),
	@author_id uniqueidentifier,
	@contents varchar(5000) = '' as

declare @class_id uniqueidentifier
declare @idx smallint

select @class_id = class_id, @idx = idx
from Message
where message_ID = @message_id

insert into Message values (newid(), 0, @title, @contents, @message_id, @class_id, @idx, @author_id, getdate())
-- end procedure


-- begin procedure
create procedure sp_insert_main_topic
	@class_id uniqueidentifier,
	@idx smallint,
	@title varchar(50),
	@author_id uniqueidentifier,
	@contents varchar(5000) = '' as
insert into Message values (newid(), 1, @title, @contents, null, @class_id, @idx, @author_id, getdate())
-- end procedure


-- updates the title and contents of the message identified
-- by @message_id and deletes all records from User_Message
-- that have their message_id equal to @message_id (thus
-- making this message a new message for all users
-- begin procedure
create procedure sp_update_message
	@message_id uniqueidentifier,
	@title varchar(50),
	@contents varchar(5000) = '' as

delete from User_Message
where message_id = @message_id

update Message
set title = @title, message = @contents
where message_id = @message_id
-- end procedure


-- the number returned by this procedure represents the
-- number of messages belonging to conference identified by
-- @_class_id and @_idx which the user identified by @_user_id
-- hasn't read yet
-- begin procedure
create procedure sp_unread_messages
	@_class_id uniqueidentifier,
	@_idx smallint,
	@_user_id uniqueidentifier as
update Message
set idx = idx
where class_id = @_class_id and idx = @_idx and
	not exists
	(select *
	 from User_Message um
	 where Message.message_id = um.message_id and um.user_id = @_user_id)
return @@rowcount
-- end procedure


-- returns a result set cotaining the following columns:
-- idx, name, mainTopicsFlag, postAnswersFlag, newMessagesFlag
-- ordered by idx. The flags are bit values (1 => students can post
-- main topics / students can post answers / conference contains
-- new messages for the specified user; 0 => the opposite).
-- begin procedure
create procedure sp_get_conferences
	@class_id uniqueidentifier,
	@user_id uniqueidentifier as

declare @idx smallint
declare @name varchar(50)
declare @stint tinyint	-- student_interactivity
declare @mtf bit		-- mainTopicFlag
declare @paf bit		-- postAnswersFlag
declare @rows bigint

create table #TempTable (
	idx smallint,
	name varchar(50),
	mainTopicsFlag bit,
	postAnswersFlag bit,
	newMessagesFlag bit
)

declare conf cursor for
select idx, name, student_interactivity
from conference
where class_id = @class_id and ((visibility = 0) or (visibility = 2 and start_date <= getdate()))

open conf
fetch next from conf into @idx, @name, @stint
while @@FETCH_STATUS = 0
begin
	if (@stint = 0) or (@stint = 2)
		set @mtf = 1
	else
		set @mtf = 0
	if (@stint = 0) or (@stint = 1)
		set @paf = 1
	else
		set @paf = 0

	execute @rows = sp_unread_messages @class_id, @idx, @user_id

	if (@rows != 0)
		set @rows = 1

	insert into #TempTable values (@idx, @name, @mtf, @paf, @rows)
	fetch next from conf into @idx, @name, @stint
end
close conf
deallocate conf

select *
from #TempTable
order by idx

drop table #TempTable
-- end procedure


-- in addition to returning data about the specified message
-- this procedure also inserts a (user, message) pair in the
-- User_Message table, indicating that the user has read the
-- specified message
-- begin procedure
create procedure sp_get_message
	@user_id uniqueidentifier,
	@message_id uniqueidentifier as

update User_Message
set user_id = user_id
where user_id = @user_id and message_id = @message_id

if (@@rowcount = 0)
	insert into User_Message values (@user_id, @message_id)

select message_id, title, message, first_name, last_name, m.parent_id, m.idx, m.creation_date, student_interactivity
from Message m, Users, Conference c
where author_id = user_id and message_id = @message_id and m.class_id = c.class_id and m.idx = c.idx
-- end procedure


-- returns a result set cotaining the following columns:
-- message_id, user_id, title, first_name, last_name,
-- creation_date, newFlag ordered by creation_date.
-- The messages returned are the replies to message
-- identified by @parent_id. @user_id is used for creating
-- the newFlag column which is 1 if the specified user
-- has not read this message yet and 0 if he/she has.
-- begin procedure
create procedure sp_get_messages
	@user_id uniqueidentifier,
	@parent_id uniqueidentifier as

declare @mid uniqueidentifier
declare @uid uniqueidentifier
declare @tit varchar(50)
declare @fnm varchar(50)
declare @lnm varchar(50)
declare @cdt datetime
declare @rows bit

create table #TempTable (
	message_id uniqueidentifier,
	user_id uniqueidentifier,
	title varchar(50),
	first_name varchar(50),
	last_name varchar(50),
	creation_date datetime,
	newFlag bit
)

declare mess cursor for
select message_id, title, user_id, first_name, last_name, creation_date
from Message, Users
where author_id = user_id and parent_id = @parent_id
order by creation_date

open mess
fetch next from mess into @mid, @tit, @uid, @fnm, @lnm, @cdt
while @@FETCH_STATUS = 0
begin
	update User_Message
	set user_id = user_id
	where user_id = @user_id and message_id = @mid

	set @rows = @@ROWCOUNT

	if (@rows != 0)
		set @rows = 0
	else
		set @rows = 1

	insert into #TempTable values (@mid, @uid, @tit, @fnm, @lnm, @cdt, @rows)
	fetch next from mess into @mid, @tit, @uid, @fnm, @lnm, @cdt
end
close mess
deallocate mess

select *
from #TempTable
order by creation_date

drop table #TempTable
--end procedure

-- If @message_id identifies a main topic then @parent_id
-- will be null and idx will be the index of the conference
-- this main topic belongs to. If @message_id identifies
-- a regular message (i.e. not a main topic) then @parent_id
-- will identifiy the parent of this message (another message
-- or a main topic).
--begin procedure
create procedure sp_get_grandparent
	@message_id uniqueidentifier,
	@parent_id uniqueidentifier output,
	@conference_idx smallint output as

set @parent_id =
	(select parent_id
	from Message
	where message_id = @message_id)

set @conference_idx =
	(select idx
	from Message
	where message_id = @message_id)
--end procedure

-- returns a result set cotaining the following columns:
-- message_id, user_id, title, first_name, last_name,
-- creation_date, newFlag ordered by creation_date.
-- The messages returned are the main topics in conference
-- identified by @class_id & @idx. @user_id is used creating
-- the newFlag column which is 1 if the specified user
-- has not read this message yet and 0 if he/she has.
-- begin procedure
create procedure sp_get_main_topics
	@user_id uniqueidentifier,
	@class_id uniqueidentifier,
	@idx smallint as

declare @mid uniqueidentifier
declare @pid uniqueidentifier
declare @uid uniqueidentifier
declare @tit varchar(50)
declare @fnm varchar(50)
declare @lnm varchar(50)
declare @cdt datetime
declare @rows bit

create table #TempTable (
	message_id uniqueidentifier,
	user_id uniqueidentifier,
	title varchar(50),
	first_name varchar(50),
	last_name varchar(50),
	creation_date datetime,
	newFlag bit
)

declare mess cursor for
select message_id, title, user_id, first_name, last_name, creation_date
from Message, Users
where author_id = user_id and class_id = @class_id and idx = @idx and main_topic_flag = 1
order by creation_date

open mess
fetch next from mess into @mid, @tit, @uid, @fnm, @lnm, @cdt
while @@FETCH_STATUS = 0
begin
	update User_Message
	set user_id = user_id
	where user_id = @user_id and message_id = @mid

	set @rows = @@ROWCOUNT

	if (@rows != 0)
		set @rows = 0
	else
		set @rows = 1

	insert into #TempTable values (@mid, @uid, @tit, @fnm, @lnm, @cdt, @rows)
	fetch next from mess into @mid, @tit, @uid, @fnm, @lnm, @cdt
end
close mess
deallocate mess

select *
from #TempTable
order by creation_date

drop table #TempTable
-- end procedure

-------------------------------------------------------
-- END Conference Messages related stored procedures --
-------------------------------------------------------


-------------------------------------------
-- BEGIN Login related stored procedures --
-------------------------------------------

-- begin procedure
create procedure sp_get_login
	@login_name varchar(50) as
select user_ID, password, description
from user_role, role
where @login_name = login_name and role.role_ID = user_role.role_ID
-- end procedure

-----------------------------------------
-- END Login related stored procedures --
-----------------------------------------


--------------------------------------------------
-- BEGIN Class Choice related stored procedures --
--------------------------------------------------

-- begin procedure
create procedure sp_get_classes
	@user_ID uniqueidentifier as

create table #MyTempTable (
	class_name varchar(50),
	course_name varchar(50),
	class_ID uniqueidentifier,
	position varchar(50)
)

insert into #MyTempTable
select class.name, course.name, class.class_ID, 'teacher'
from Class, Course
where teacher_ID = @user_ID and class.course_ID = course.course_ID

insert into #MyTempTable
select class.name, course.name, class.class_ID, 'student'
from class, course, student_class
where student_class.user_ID = @user_ID and
	class.class_ID = student_class.class_ID	and
	class.course_ID = course.course_ID

select *
from #MyTempTable
order by position desc

drop table #MyTempTable
-- end procedure

------------------------------------------------
-- END Class Choice related stored procedures --
------------------------------------------------


--------------------------------------------------
-- BEGIN Announcement related stored procedures --
--------------------------------------------------

-- begin procedure
create procedure sp_insert_announcement
	@cid uniqueidentifier,
	@announcement varchar (7000),
	@startdate datetime = null,
	@enddate datetime = null as
insert into announcement
values (@cid, newid(), @announcement, getdate(), getdate(), @startdate, @enddate)
-- end procedure


-- begin procedure
create procedure sp_update_announcement
	@aid uniqueidentifier,
	@announcement varchar (7000),
	@startdate datetime = null,
	@enddate datetime = null as
update Announcement
set announcement = @announcement,
	last_modification_date = getDate(),
	start_date = @startdate,
	end_date = @enddate
where announcement_ID = @aid
-- end procedure


-- begin procedure
create procedure sp_delete_announcement
	@aid uniqueidentifier as
delete from announcement
where @aid = announcement_ID
-- end procedure


-- begin procedure
create procedure sp_list_announcements
	@cid uniqueidentifier as
select announcement_ID, announcement, creation_date, start_date, end_date
from announcement
where @cid = class_ID
order by creation_date desc
-- end procedure


-- begin procedure
create procedure sp_get_announcements
	@cid uniqueidentifier as
select announcement, creation_date, start_date, end_date
from announcement
where @cid = class_ID and
	(
		((start_date is null) and (end_date is null)) or
		((start_date is null) and (end_date is not null) and (end_date > getdate())) or
		((start_date is not null) and (end_date is null) and (start_date < getdate())) or
		((start_date is not null) and (end_date is not null) and (start_date < getdate()) and (end_date > getdate()))
	)
order by creation_date desc
-- end procedure


-- begin procedure
create procedure sp_get_announcement
	@cid uniqueidentifier, @aid uniqueidentifier as
select announcement, creation_date, start_date, end_date, announcement_ID
from announcement
where @cid = class_ID and @aid = announcement_ID
-- end procedure

------------------------------------------------
-- END Announcement related stored procedures --
------------------------------------------------

------------------------------------------
-- BEGIN Chat related stored procedures --
------------------------------------------

create procedure sp_get_chat_related_info
	@uid uniqueidentifier,
	@cid uniqueidentifier as

select first_name, last_name, name

from	users, class, student_class

where	@cid = class.class_ID and
	@uid = users.user_ID and (
	(users.user_ID = student_class.user_ID and student_class.class_ID = class.class_ID)
	or
	(@uid = class.teacher_ID))

----------------------------------------
-- END Chat related stored procedures --
----------------------------------------

--------------------------------------------
-- BEGIN E-mail related stored procedures --
--------------------------------------------

create procedure sp_get_email_addresses
	@cid uniqueidentifier as

select user_ID, e_mail, first_name, last_name
from users u
where exists
	(select *
	 from Class
	 where class_id = @cid and teacher_id = u.user_id) or
	exists
	(select *
	 from Student_Class
	 where class_id = @cid and user_id = u.user_id)

------------------------------------------
-- END E-mail related stored procedures --
------------------------------------------

-----------------------------------------------------
-- BEGIN Task management related stored procedures --
-----------------------------------------------------

-- begin procedure
create procedure sp_insert_task
	@class_id uniqueidentifier,
	@idx int output,
	@title varchar(50),
	@description varchar(5000) = '',
	@difficulty varchar(100) = '' as
select @idx = max(idx)
from Task
where class_ID = @class_id
if @idx is null
	set @idx = 0
set @idx = @idx + 1
insert into Task values (@class_id, @idx, @title, @description, @difficulty, getdate(), getdate())
-- end procedure

-- begin procedure
create procedure sp_update_task
	@class_id uniqueidentifier,
	@idx int,
	@title varchar(50),
	@description varchar(5000) = '',
	@difficulty varchar(100) = '' as
update Task
set title = @title,
	description = @description,
	difficulty = @difficulty,
	last_modification_date = getdate()
where class_id = @class_id and idx = @idx
-- end procedure

-- begin procedure
create procedure sp_delete_task
	@class_id uniqueidentifier,
	@idx int as

delete from Task
where class_id = @class_id and idx = @idx

declare @ret_val int
set @ret_val = @@rowcount

update Task
set idx = idx - 1
where class_id = @class_id and idx > @idx

return @ret_val
-- end procedure

-- begin procedure
create procedure sp_list_tasks
	@class_id uniqueidentifier as
select class_id, idx, title, difficulty
from Task
where class_ID = @class_id
order by idx
-- end procedure

-- begin procedure
create procedure sp_get_task
	@class_id uniqueidentifier,
	@idx int as
select class_id, idx, title, description, difficulty
from Task
where class_id = @class_id and @idx = idx
-- end procedure

---------------------------------------------------
-- END Task management related stored procedures --
---------------------------------------------------

-----------------------------------------------------
-- BEGIN Task assignment related stored procedures --
-----------------------------------------------------

-- begin procedure
create procedure sp_get_students
	@class_id uniqueidentifier as
select student_ID, first_name, last_name
from Student_Class sc, Users u
where class_id = @class_id and sc.user_id = u.user_id
-- end procedure

-- begin procedure
create procedure sp_insert_assignment
	@class_id uniqueidentifier,
	@task_idx int,
	@stud_id uniqueidentifier,
	@due_on datetime,
	@allow_late_submission bit,
	@comments_for_teacher varchar(1500),
	@comments_for_student varchar(1500) as

declare @idx int

select @idx = max(idx)
from Assignment
where class_ID = @class_id

if @idx is null
	set @idx = 0
set @idx = @idx + 1

insert into Assignment values (@class_id, @idx, @task_idx, @stud_id, getdate(), @due_on, null, @allow_late_submission, 1, null, null, @comments_for_teacher, @comments_for_student, null)
-- end procedure

-- begin procedure
create procedure sp_submit_answer
	@class_id uniqueidentifier,
	@idx int,
	@answer varchar(5000) as
update Assignment
set status = 2, answer = @answer, submitted_on = getdate()
where class_id = @class_id and idx = @idx
-- end procedure

-- begin procedure
create procedure sp_mark_as_read
	@class_id uniqueidentifier,
	@idx int as
update Assignment
set status = 3
where class_id = @class_id and idx = @idx
-- end procedure

-- begin procedure
create procedure sp_grade_assignment
	@class_id uniqueidentifier,
	@idx int,
	@grade varchar(20),
	@gradeCom varchar(1500) as
update Assignment
set grade = @grade, grade_comments = @gradeCom, status = 4
where class_id = @class_id and idx = @idx
-- end procedure

-- begin procedure
create procedure sp_delete_assignment
	@class_id uniqueidentifier,
	@idx int as

delete from Assignment
where class_id = @class_id and idx = @idx

declare @ret_val int
set @ret_val = @@rowcount

update Assignment
set idx = idx - 1
where class_id = @class_id and idx > @idx

return @ret_val
-- end procedure

-- begin procedure
create procedure sp_list_student_assignments
	@class_id uniqueidentifier,
	@user_id uniqueidentifier as

declare @student_id uniqueidentifier

select @student_id = student_id
from Student_Class
where user_id = @user_id and class_id = @class_id

select t.class_ID, t.title, a.idx, a.assigned_on, a.due_on, a.submitted_on, a.allow_late_submission, a.status, a.grade, a.grade_comments
from Task as t inner join Assignment as a
on t.class_ID = a.class_id and t.idx = a.task_idx
where student_id = @student_id
order by status, due_on
-- end procedure

-- begin procedure
create procedure sp_list_teacher_assignments
	@class_id uniqueidentifier,
	@status int as

select t.class_ID, t.title, u.first_name, u.last_name, a.idx, a.assigned_on, a.due_on, a.submitted_on, a.allow_late_submission, a.status, a.grade, a.grade_comments
from Assignment as a inner join Student_Class as sc on sc.student_id = a.student_id
	inner join Users as u on u.user_id = sc.user_id
	inner join Task as t on a.class_ID = t.class_id and a.task_idx = t.idx
where a.class_id = @class_id and a.status = @status
order by
	case @status
		when 1 then due_on
		when 2 then submitted_on
		when 3 then submitted_on
		when 4 then submitted_on
	end
-- end procedure

-- begin procedure
create procedure sp_get_student_assignment
	@class_id uniqueidentifier,
	@idx int as
select t.class_ID, t.idx, t.title, t.description, t.difficulty, a.teacher_notes
from Task as t inner join Assignment as a
on t.class_ID = a.class_id and t.idx = a.task_idx
where a.class_id = @class_id and a.idx = @idx
-- end procedure

-- begin procedure
create procedure sp_get_teacher_assignment
	@class_id uniqueidentifier,
	@idx int as
select t.class_ID, t.idx, t.title, t.description, t.difficulty, a.teacher_notes, a.teacher_comments
from Task as t inner join Assignment as a
on t.class_ID = a.class_id and t.idx = a.task_idx
where a.class_id = @class_id and a.idx = @idx
-- end procedure

-- begin procedure
create procedure sp_get_answer
	@class_id uniqueidentifier,
	@idx int as
select class_id, idx, task_idx, answer, assigned_on, due_on, submitted_on, allow_late_submission, status
from Assignment
where class_id = @class_id and idx = @idx
-- end procedure

-- begin procedure
create procedure sp_get_grade
	@class_id uniqueidentifier,
	@idx int as
select class_id, idx, task_idx, grade, grade_comments
from Assignment
where class_id = @class_id and idx = @idx
-- end procedure

---------------------------------------------------
-- END Task assignment related stored procedures --
---------------------------------------------------

------------------------------------------------------------
-- BEGIN static course material related stored procedures --
------------------------------------------------------------

-- begin procedure
create procedure sp_get_course_data
	@cid uniqueidentifier as

select course.name, course.start_page, courseauthor.first_name,
	courseauthor.last_name, courseauthor.e_mail, course.course_ID,
	course.description, course.bibliography, course.content,
	course.objectives, course.prerequisites
from class, course, courseauthor
where class.class_id = @cid and
		class.course_id = course.course_id and
		course.author_ID = courseauthor.author_ID
-- end procedure

----------------------------------------------------------
-- END static course material related stored procedures --
----------------------------------------------------------

-------------------------------------------------------------
-- BEGIN dynamic course material related stored procedures --
-------------------------------------------------------------

-- begin procedure
create procedure sp_get_schedule
	@cid uniqueidentifier as
select class.schedule, resourcetype.[description]
from class, resourcetype
where @cid = class.class_ID and
	class.schedule_type = resourcetype.resource_ID
-- end procedure

-- begin procedure
create procedure sp_get_policies
	@cid uniqueidentifier as
select class.policies, resourcetype.[description]
from class, resourcetype
where @cid = class.class_ID and
	class.policies_type = resourcetype.resource_ID
-- end procedure

-- begin procedure
create procedure sp_get_references
	@cid uniqueidentifier as
select class.[references], resourcetype.[description]
from class, resourcetype
where @cid = class.class_ID and
	class.references_type = resourcetype.resource_ID
-- end procedure

-- begin procedure
create procedure sp_get_grading_info
	@cid uniqueidentifier as
select class.grading_info, resourcetype.[description]
from class, resourcetype
where @cid = class.class_ID and
	class.grading_info_type = resourcetype.resource_ID
-- end procedure

-- begin procedure
create procedure sp_get_add_info
	@cid uniqueidentifier as
select class.additional_info, resourcetype.[description]
from class, resourcetype
where @cid = class.class_ID and
	class.additional_info_type = resourcetype.resource_ID
-- end procedure

-- begin procedure
create procedure sp_get_all_classtm_info
	@cid uniqueidentifier as
create table #MyTempTable (
	schedule varchar(1400),
	schedule_description char(3),
	policies varchar(1400),
	policies_description char(3),
	[references] varchar(1400),
	references_description char(3),
	grading_info varchar(1400),
	grading_info_description char(3),
	additional_info varchar(1400),
	additional_info_description char(3)
)

insert into #MyTempTable(schedule, policies, [references], grading_info, additional_info)
 	select c.schedule, c.policies, c.[references], c.grading_info, c.additional_info
	from class c
	where c.class_ID = @cid

if exists (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.schedule_type = rt.resource_ID
	)
	update #MyTempTable
	set schedule_description = (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.schedule_type = rt.resource_ID
	)

if exists (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.policies_type = rt.resource_ID
	)
	update #MyTempTable
	set policies_description = (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.policies_type = rt.resource_ID
	)

if exists (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.references_type = rt.resource_ID
	)
	update #MyTempTable
	set references_description = (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.references_type = rt.resource_ID
	)

if exists (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.grading_info_type = rt.resource_ID
	)
	update #MyTempTable
	set grading_info_description = (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.grading_info_type = rt.resource_ID
	)

if exists (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.additional_info_type = rt.resource_ID
	)
	update #MyTempTable
	set additional_info_description = (select rt.[description]
		from class, resourcetype rt
		where class.class_ID = @cid and
			class.additional_info_type = rt.resource_ID
	)

select *
from #MyTempTable
drop table #MyTempTable
-- end procedure

-- begin procedure
create procedure sp_remove_class_resource_schedule
	@cid uniqueidentifier as
update class
set schedule = null, schedule_type = null
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_remove_class_resource_references
	@cid uniqueidentifier as
update class
set [references] = null, references_type = null
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_remove_class_resource_policies
	@cid uniqueidentifier as
update class
set policies = null, policies_type = null
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_remove_class_resource_grading_info
	@cid uniqueidentifier as
update class
set grading_info = null, grading_info_type = null
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_remove_class_resource_add_info
	@cid uniqueidentifier as
update class
set additional_info = null, additional_info_type = null
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_update_class_resource_schedule
	@cid uniqueidentifier, @resource varchar(1400) as
update class
set class.schedule = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_update_class_resource_references
	@cid uniqueidentifier, @resource varchar(1400) as
update class
set class.[references] = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_update_class_resource_policies
	@cid uniqueidentifier, @resource varchar(1400) as
update class
set class.policies = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_update_class_resource_grading_info
	@cid uniqueidentifier, @resource varchar(1400) as
update class
set class.grading_info = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_update_class_resource_add_info
	@cid uniqueidentifier, @resource varchar(1400) as
update class
set class.additional_info = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_create_class_resource_schedule
	@cid uniqueidentifier, @resource varchar(1400), @res_type char(3) as
update class
set schedule_type =
	(select resource_ID
	from resourcetype
	where [description] = @res_type),
	 [schedule] = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_create_class_resource_policies
	@cid uniqueidentifier, @resource varchar(1400), @res_type char(3) as
update class
set policies_type =
	(select resource_ID
	from resourcetype
	where [description] = @res_type),
	 [policies] = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_create_class_resource_references
	@cid uniqueidentifier, @resource varchar(1400), @res_type char(3) as
update class
set references_type =
	(select resource_ID
	from resourcetype
	where [description] = @res_type),
	 [references] = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_create_class_resource_grading_info
	@cid uniqueidentifier, @resource varchar(1400), @res_type char(3) as
update class
set grading_info_type =
	(select resource_ID
	from resourcetype
	where [description] = @res_type),
	 [grading_info] = @resource
where class.class_ID = @cid
-- end procedure

-- begin procedure
create procedure sp_create_class_resource_add_info
	@cid uniqueidentifier, @resource varchar(1400), @res_type char(3) as
update class
set additional_info_type =
	(select resource_ID
	from resourcetype
	where [description] = @res_type),
	 [additional_info] = @resource
where class.class_ID = @cid
-- end procedure
-----------------------------------------------------------
-- END dynamic course material related stored procedures --
-----------------------------------------------------------

------------------------------------------------
-- BEGIN grade book related stored procedures --
------------------------------------------------

create procedure sp_get_grades
	@cid uniqueidentifier
as
select tmp.student_ID, usr.first_name, usr.last_name,
	tmp.column_name, sg.grade, tmp.column_ID, tmp.creation_date
from StudentGrades sg, Student_Class s_c, Users usr,
	(select gc.column_name, gc.column_ID, s.student_ID,
		 gc.creation_date
	from GradeColumns gc, Class c, Student_Class s
	where	gc.class_ID = c.class_ID
		and c.class_ID = @cid
		and c.class_ID = s.class_ID
	) tmp
where
	tmp.student_ID *= sg.student_ID and
	tmp.column_ID *= sg.column_ID and
	tmp.student_ID = s_c.student_ID and
	s_c.user_ID = usr.user_ID
	order by tmp.creation_date

create procedure sp_remove_grade
	@sid uniqueidentifier, @cid uniqueidentifier
as
delete from StudentGrades
where student_ID = @sid and column_ID = @cid

create procedure sp_edit_grade
	@sid uniqueidentifier, @cid uniqueidentifier, @grd varchar(20) as
update StudentGrades
set grade = @grd
where student_ID = @sid and column_ID = @cid

create procedure sp_add_grade
	@sid uniqueidentifier, @cid uniqueidentifier, @grd varchar(20) as
insert into StudentGrades
values (@sid, @cid, @grd)

create procedure sp_add_grade_column
	@cid uniqueidentifier, @col_name varchar(30), @date datetime as
insert into GradeColumns values(@cid, newID(), @col_name, @date)

create procedure sp_delete_grade_column
	@cid uniqueidentifier, @colid uniqueidentifier as
delete from GradeColumns
where class_ID = @cid and column_ID = @colid

create procedure sp_get_grade_columns
	@cid uniqueidentifier as
select *
from GradeColumns
where @cid = class_ID

create procedure sp_get_student_grades
	@cid uniqueidentifier, @uid uniqueidentifier
as
select tmp.student_ID, usr.first_name, usr.last_name,
	tmp.column_name, sg.grade, tmp.column_ID, tmp.creation_date
from StudentGrades sg, Student_Class s_c, Users usr,
	(select gc.column_name, gc.column_ID, s.student_ID,
		 gc.creation_date
	from GradeColumns gc, Class c, Student_Class s
	where	gc.class_ID = c.class_ID
		and c.class_ID = @cid
		and c.class_ID = s.class_ID
	) tmp
where
	tmp.student_ID *= sg.student_ID and
	tmp.column_ID *= sg.column_ID and
	tmp.student_ID = s_c.student_ID and
	s_c.user_ID = usr.user_ID and
	usr.user_ID = @uid
	order by tmp.creation_date asc
----------------------------------------------
-- END grade book related stored procedures --
----------------------------------------------
