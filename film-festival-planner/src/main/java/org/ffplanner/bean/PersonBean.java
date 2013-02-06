package org.ffplanner.bean;

import org.ffplanner.entity.Person;
import org.ffplanner.entity.Person_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class PersonBean extends BasicEntityBean<Person> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Override
    protected Class<Person> getEntityClass() {
        return Person.class;
    }

    @Override
    protected SingularAttribute<Person, Long> getIdAttribute() {
        return Person_.id;
    }

    public Person findOrCreateActorBy(String actorName) {
        return findOrCreateBy(actorName);
    }

    public Person findOrCreateDirectorBy(String directorName) {
        return findOrCreateBy(directorName);
    }

    private Person findOrCreateBy(String personName) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Person> query = criteriaBuilder.createQuery(Person.class);
        final Root<Person> root = query.from(Person.class);
        query.where(criteriaBuilder.equal(root.get(Person_.name), personName));
        final TypedQuery<Person> countryQuery = entityManager.createQuery(query);
        final List<Person> result = countryQuery.getResultList();
        final Person person;
        if (result.isEmpty()) {
            person = new Person(personName);
        } else {
            assert result.size() == 1;
            person = result.get(0);
        }
        return person;
    }
}
