/**********************************************************
**
**  FILE
**    ConversionFailedException.java
**
**  AUTHOR
**    Jesper Eskilson <jojo@sics.se>
** 
**  CREATED
**    Fri Jul 31 14:23:26 1998
**    
**  DESCRIPTION
**    
**
***********************************************************
*/

package se.sics.jasper;
/** Thrown if a Java value canot be converted to a Prolog term.
 * <p>
 * You should not catch a ConversionFailedException. Catch SPException instead,
 * since ConversionFailedException is not guaranteed to exist in future versions
 * of Jasper.
 * @see se.sics.jasper.SPTerm
 */
public class ConversionFailedException extends SPException
{
    /**
     * Create an instance
     * @param sp the Prolog instance
     * @param msg the message
     */
    public ConversionFailedException(SICStus sp, String msg)
    {
        super(sp, msg);
    }

    // [PM] 3.8.5 obsolete
//     public ConversionFailedException(String msg)
//     {
//         super(msg);
//     }

}
