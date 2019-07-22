/**********************************************************
**
**  FILE
**    NativeCodeException.java
**
**  AUTHOR
**    Jesper Eskilson <jojo@sics.se>
** 
**  CREATED
**    Fri Jul 31 14:24:06 1998
**    
**  DESCRIPTION
**    
**
***********************************************************
*/

package se.sics.jasper;

/** Used internally to signal errors from native code.
 *
 * <p> You should not create or inherit from
 * <code>NativeCodeException</code>.
 */
class NativeCodeException extends SPException
{

  // [PM] 3.8.5 added SICStus arg
    /* public */ NativeCodeException(SICStus sp)// called from glue (spnative.c)
  {
      super(sp, (String)null);
  }

    // [PM] 3.8.5 
//   public NativeCodeException(String msg)
//   {
//     super(msg);
//   }
}
