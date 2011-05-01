import java.util.Date;

@DBMapping("Person")
public class Person
{
	@DBMapping("person_id")
	private int id;

	@DBMapping("person_first_name")
	private String firstName;

	@DBMapping("person_last_name")
	private String lastName;

	@DBMapping("person_date_of_birth")
	private Date dateOfBirth;

	private String nickName;

	public void setId(int id)
	{
		this.id = id;
	}

	public void setFirstName(String firstName)
	{
		this.firstName = firstName;
	}

	public void setLastName(String lastName)
	{
		this.lastName = lastName;
	}

	public void setDateOfBirth(Date dateOfBirth)
	{
		this.dateOfBirth = dateOfBirth;
	}

	public void setNickName(String nickName)
	{
		this.nickName = nickName;
	}
}
