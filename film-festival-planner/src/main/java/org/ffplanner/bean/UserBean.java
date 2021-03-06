package org.ffplanner.bean;

import org.ffplanner.controller.auth.AuthData;
import org.ffplanner.entity.User;
import org.ffplanner.entity.UserToken;
import org.ffplanner.entity.UserToken_;
import org.ffplanner.entity.User_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.List;
import java.util.Locale;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserBean extends BasicEntityBean<User> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Override
    protected Class<User> getEntityClass() {
        return User.class;
    }

    @Override
    protected SingularAttribute<User, Long> getIdAttribute() {
        return User_.id;
    }

    public User findBy(String openId) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<UserToken> query = criteriaBuilder.createQuery(UserToken.class);
        final Root<UserToken> root = query.from(UserToken.class);
        query.where(criteriaBuilder.equal(root.get(UserToken_.token), openId));
        final TypedQuery<UserToken> userTokenQuery = entityManager.createQuery(query);
        final List<UserToken> result = userTokenQuery.getResultList();
        if (result.isEmpty()) {
            return null;
        } else {
            assert result.size() == 1;
            final User user = result.get(0).getUser();
            user.loadLazyFields();
            return user;
        }
    }

    public User createWith(AuthData authData) {
        final User user = new User();
        user.setFirstName(authData.getFirstName());
        user.setLastName(authData.getLastName());
        user.setEmailAddress(authData.getEmailAddress());
        user.setLocale(Locale.ENGLISH);
        final UserToken userToken = new UserToken();
        userToken.setUser(user);
        userToken.setToken(authData.getOpenId());
        entityManager.persist(user);
        entityManager.persist(userToken);
        return user;
    }
}
