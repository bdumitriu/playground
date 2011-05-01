import com.sun.mirror.apt.*;
import com.sun.mirror.declaration.*;

import java.util.Collection;
import java.util.Set;
import java.util.Collections;
import java.util.Arrays;

public class DBAnnotationProcessorFactory implements AnnotationProcessorFactory
{
	private static final Collection<String> supportedAnnotations =
			Collections.unmodifiableCollection(Arrays.asList("DBMapping"));

	private static final Collection<String> supportedOptions = Collections.emptySet();

	public Collection<String> supportedOptions()
	{
		return supportedOptions;
	}

	public Collection<String> supportedAnnotationTypes()
	{
		return supportedAnnotations;
	}

	public AnnotationProcessor getProcessorFor(Set<AnnotationTypeDeclaration> atds, AnnotationProcessorEnvironment env)
	{
        return new DBAnnotationProcessor(env);
	}
}
