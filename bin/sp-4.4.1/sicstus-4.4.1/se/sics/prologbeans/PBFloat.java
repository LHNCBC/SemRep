/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

/**
 * <code>PBFloat</code> is the representation of Prolog integers.
 */

class PBFloat extends PBAtomic
{
    double doubleValue;

    /**
     * Creates a new <code>PBFloat</code> instance with the specified value.
     */
    PBFloat(double value)
    {
        super(Double.toString(value));
        doubleValue = value;
    }

    PBFloat(double value, String name)
    {
        super(name);
        doubleValue = value;
    }

    int getType()
    {
        return FLOAT;
    }

    public boolean isFloat()
    {
        return true;
    }

    public double floatValue()
    {
//      return Double.parseDouble(name);
        return doubleValue;
    }

    String toPrologString()
    {
        return toString();
    }

    public String toString()
    {
        return Double.toString(doubleValue);
    }

}
