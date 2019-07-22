/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;
import java.math.BigInteger;

/**
 * <code>PBBignum</code> is the representation of large Prolog integers.
 */

class PBBignum extends PBAtomic
{
    BigInteger bigIntValue;

    /**
     * Creates a new <code>PBBignum</code> instance with the specified value.
     */
    PBBignum(BigInteger value)
    {
        super(value.toString());
        bigIntValue = value;
    }

    PBBignum(BigInteger value, String name)
    {
        super(name);
        bigIntValue = value;
    }

    int getType()
    {
        return INTEGER;
    }

    public boolean isBignum()
    {
        return true;
    }

    public BigInteger bigIntegerValue()
    {
        return bigIntValue;
    }

    String toPrologString()
    {
        return toString();
    }

    public String toString()
    {
        return bigIntValue.toString();
    }

}
