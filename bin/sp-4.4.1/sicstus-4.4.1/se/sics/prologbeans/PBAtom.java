/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

/**
 * <code>PBAtom</code> is the representation of Prolog atoms.
 */

class PBAtom extends PBAtomic
{
    /**
     * Creates a new <code>PBAtom</code> instance with the specified name.
     */
    PBAtom(String name)
    {
        super(name);
    }

    int getType()
    {
        return ATOM;
    }

    public boolean isAtom()
    {
        return true;
    }

    String toPrologString()
    {
        return stuffAtom(name);
    }

    public String toString()
    {
        return name;
    }

}
