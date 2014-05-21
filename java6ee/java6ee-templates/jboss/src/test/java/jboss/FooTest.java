/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package jboss;

import org.junit.Test;

import javax.ejb.EJB;

/**
 * @author Bogdan Dumitriu
 */
public class FooTest {

    @EJB
    private FooEJB fooEJB;

    @Test
    public void testAdd() {
//        fooEJB.addFoo();
    }
}
