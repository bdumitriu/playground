import com.sun.mirror.apt.*;
import com.sun.mirror.declaration.*;
import com.sun.mirror.util.*;

import java.util.Collection;
import java.util.ArrayList;
import java.sql.*;

public class DBAnnotationProcessor implements AnnotationProcessor
{
	private final AnnotationProcessorEnvironment env;

	public DBAnnotationProcessor(AnnotationProcessorEnvironment env)
	{
		this.env = env;
	}

	public void process()
	{
		// create a filter which will eliminate declarations
		// unannotated with the DBMapping annotation
		DeclarationFilter annFilter = new DeclarationFilter()
		{
			public boolean matches(Declaration d)
			{
				return d.getAnnotation(DBMapping.class) != null;
			}
		};

		// eliminate classes wihch are not annotated with DBMapping
		Collection<TypeDeclaration> types = annFilter.filter(env.getSpecifiedTypeDeclarations());
		for (TypeDeclaration typeDecl : types)
		{
			// eliminate fields from current class wihch are not annotated with DBMapping
			Collection<FieldDeclaration> fields = annFilter.filter(typeDecl.getFields());

			ArrayList<String> fieldNames = new ArrayList<String>(fields.size());
			ArrayList<String> fieldMappings = new ArrayList<String>(fields.size());

			// extract the field names and their associated mappings into two separate collections
			for (FieldDeclaration fieldDecl : fields)
			{
				fieldNames.add(fieldDecl.getSimpleName());
				fieldMappings.add(fieldDecl.getAnnotation(DBMapping.class).value());
			}

			createFinder(typeDecl.getSimpleName(), typeDecl.getAnnotation(DBMapping.class).value(), fieldNames, fieldMappings);
		}

	}

	private void createFinder(String className, String classMapping, ArrayList<String> fieldNames, ArrayList<String> fieldMappings)
	{
		System.out.println("class name: " + className);
		System.out.println("class mapping: " + classMapping);
		System.out.println("fields:");
		for (int i = 0; i < fieldNames.size(); i++)
		{
			System.out.println("  " + fieldNames.get(i) + ": " + fieldMappings.get(i));
		}
	}
}

class PersonFinder
{
	private static final String findQuery =
			"SELECT person_id, person_first_name, person_last_name, person_date_of_birth" +
			"FROM Person" +
			"WHERE id = ?";

	public Person find(Connection conn, int id) throws SQLException
	{
		PreparedStatement stmt = conn.prepareCall(findQuery);
		stmt.setInt(1, id);
		ResultSet rs = stmt.executeQuery();

		Person result = null;
		if (rs.next())
		{
			result = new Person();
			result.setId(rs.getInt("person_id"));
			result.setFirstName(rs.getString("person_first_name"));
			result.setLastName(rs.getString("person_last_name"));
			result.setDateOfBirth(rs.getDate("person_date_of_birth"));
		}

		return result;
	}
}
