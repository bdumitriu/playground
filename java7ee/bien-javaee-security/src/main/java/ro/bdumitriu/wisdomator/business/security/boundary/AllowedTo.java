package ro.bdumitriu.wisdomator.business.security.boundary;

import ro.bdumitriu.wisdomator.business.security.entity.Permission;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Bogdan Dumitriu
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface AllowedTo {

    Permission[] value();
}
