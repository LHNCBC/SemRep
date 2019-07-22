/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

/**
 * <code>PBNil</code> is the representation of the empty list, [].
 */

class PBNil extends PBAtom
{
    /**
     * Creates a new <code>PBNil</code> instance with the specified name.
     */
    PBNil()
    {
        super("[]");
    }

    public boolean isEmptyList()
    {
        return true;
    }

    public boolean isProperList()
    {
        return true;
    }

    public int length()
    {
        return 0;
    }

    public String getString()
    {
        return "";
    }
}
