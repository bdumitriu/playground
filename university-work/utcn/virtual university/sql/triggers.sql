-- this is for testing purposes only for now
-- DO *NOT* run on the database!

/*
 * This trigger updates the Class table when a user_id changes
 * in the Users database. It is the equivalent of 'on update cascade'
 * for the teacher_id and assistent_id fields. It is needed because
 * 'on update cascade' cannot be used in this configuration because
 * SQL Server says that infinite loops might appear and thus disallows
 * the usage.
 */
create trigger Users_Update on Users instead of update as
if update (user_ID)
begin
	declare
		@old_ID uniqueidentifier,
		@new_ID uniqueidentifier,
		@temp_ID uniqueidentifier

	set @temp_ID = newid()
	insert into Users (user_ID) values (@temp_ID)

	declare cur_del cursor for
	select user_ID
	from deleted

	declare cur_ins cursor for
	select user_ID
	from inserted
	
	open cur_del
	open cur_ins

	fetch next from cur_del into @old_id
	fetch next from cur_ins into @new_id
	while @@FETCH_STATUS = 0
	begin
		update Class
		set teacher_ID = @temp_ID
		where teacher_ID = @old_ID

		update Class
		set assistent_ID = @temp_ID
		where assistent_ID = @old_ID

		update Users
		set Users.user_ID = i.user_ID, Users.name = i.name
		from inserted i
		where Users.user_ID = @old_ID and i.user_ID = @new_id

		update Class
		set teacher_ID = @new_ID
		where teacher_ID = @temp_ID

		update Class
		set assistent_ID = @new_ID
		where assistent_ID = @temp_ID

		fetch next from cur_del into @old_id
		fetch next from cur_ins into @new_id
	end
	
	close cur_del
	close cur_ins
	deallocate cur_del
	deallocate cur_ins

	delete from Users
	where user_ID = @temp_ID
end
go