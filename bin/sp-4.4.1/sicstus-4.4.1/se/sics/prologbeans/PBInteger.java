/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

/**
 * <code>PBInteger</code> is the representation of Prolog integers.
 */

class PBInteger extends PBAtomic
{
    long longValue;

    /**
     * Creates a new <code>PBInteger</code> instance with the specified value.
     */
    PBInteger(long value)
    {
        super(Long.toString(value));
        longValue = value;
    }

    PBInteger(long value, String name)
    {
        super(name);
        longValue = value;
    }

    int getType()
    {
        return INTEGER;
    }

    public boolean isInteger()
    {
        return true;
    }

    public long intValue()
    {
        return longValue;
    }

    String toPrologString()
    {
        return toString();
    }

    public String toString()
    {
        return Long.toString(longValue);
    }

}
