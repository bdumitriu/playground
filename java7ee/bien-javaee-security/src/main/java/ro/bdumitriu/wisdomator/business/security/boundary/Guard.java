package ro.bdumitriu.wisdomator.business.security.boundary;

import ro.bdumitriu.wisdomator.business.security.entity.Permission;
import ro.bdumitriu.wisdomator.business.security.entity.User;

import javax.enterprise.inject.Instance;
import javax.inject.Inject;
import javax.interceptor.AroundInvoke;
import javax.interceptor.InvocationContext;
import java.lang.reflect.Method;
import java.util.Arrays;

/**
 * @author Bogdan Dumitriu
 */
public class Guard {

    @Inject
    Instance<User> users;

    @AroundInvoke
    public Object validatePermissions(InvocationContext invocationContext) throws Exception {
        final Method method = invocationContext.getMethod();
        final User user = users.get();
        if (!isAllowed(method, user)) {
            throw new SecurityException("User " + user + " is not allowed to execute the method " + method);
        }
        return invocationContext.proceed();
    }

    boolean isAllowed(Method method, User user) {
        final AllowedTo annotation = method.getAnnotation(AllowedTo.class);
        if (annotation == null) {
            return true;
        }
        final Permission[] permissions = annotation.value();
        return Arrays.stream(permissions).anyMatch(user::isAllowed);
    }
}
