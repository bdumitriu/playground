package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public class FestivalEditionBean extends BasicEntityBean<FestivalEdition> implements Serializable {

    private static final long serialVersionUID = -9047566575453952764L;

    @Override
    protected SingularAttribute<FestivalEdition, Long> getIdAttribute() {
        return FestivalEdition_.id;
    }

    @Override
    protected Class<FestivalEdition> getEntityClass() {
        return FestivalEdition.class;
    }
}
