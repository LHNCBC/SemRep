/* $Id$
 * Copyright (c) 2000 SICS 
 */

package se.sics.jasper;

// 3.8.5 [PM]
/**
 * Thrown if trying to use a (no longer) valid SPTerm.
 *
 * <p>You should not create or inherit from
 * <code>IllegalTermException</code>.
 *
 * <p>
 * You should not catch an IllegalTermException. Catch SPException instead,
 * since IllegalTermException is not guaranteed to exist in future versions
 * of Jasper.
 *
 * @see se.sics.jasper.SPTerm#isValid
 */
public class IllegalTermException extends SPException
{
    IllegalTermException(SICStus sp) {
	super(sp, 0, "Illegal SPTerm");
    }
}
