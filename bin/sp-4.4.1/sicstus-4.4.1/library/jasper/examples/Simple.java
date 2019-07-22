/*
 * Simple.java: a couple of simple Prolog-Java examples
 */

// [PM] 3.10.1 Not all platforms use Latin 1 as default. Always use only the 7-bit ASCII range.

// 0xC5 == capital A ring, 0xC4 == capital A diaresis, 0xD6 == capital O diaresis
// 0xE5 == small a ring, 0xE4 == small a dieresis, 0xF6 == small o dieresis, 

import se.sics.jasper.*;
import java.util.HashMap;

public class Simple
{
    private String a;
    private int x;
	
    public Simple(String a, int b) {
	this.a = a + " " + b;
    }

    public static int square(int x) {
	return x*x;
    }

    public String get() {
	return a;
    }

    public void set(String b) {
	a = b;
    }

    public void append(String b) {
	a = new String(a + b);
    }

    public static SPTerm train(String filename)
    {
	SICStus sp;
	/* Necessary for old style, see below
	SPTerm from, to, way = null;
	*/
        SPTerm way = null;
	HashMap varMap = new HashMap();
	SPQuery query;
	int i;
	
	try {
	    if (null == (sp = SICStus.getInitializedSICStus())) {
		sp = new SICStus();
            }

	    if (filename != null) {
                // You probably want to use sp.restore() instead
                sp.load(filename);
            }
	    
	    /* Old style
	    to = new SPTerm(sp, "\u00D6rebro");
	    from = new SPTerm(sp, "Stockholm");
	    way = new SPTerm(sp).putVariable();	    
            sp.query("user", "connected", new SPTerm[] { from, to, way, way });
	    */
	    /* New style */
	    if (!sp.query("connected('Stockholm','\u00D6rebro',Way,Way).", varMap))
                {
                    System.out.println("ERROR: connected/4 failed"); // [PM] 3.10.1
                }
            else
                {
                    way = ((SPTerm)varMap.get("Way"));
                }
	} catch ( Exception e ) {
	    e.printStackTrace();
	}
	return way;
    }
    // [PM] 3.8.5 train when called from prolog and thus the prolog code is already loaded.
    public static SPTerm train()
    {
        return train(null);
    }

    
    public static void main(String argv[])
    {
	SPTerm result = train("simple");
        if (result != null) {
            System.out.println(result.toString());
        } else {
            System.out.println("ERROR: Did not find any solutions"); // [PM] 3.10.1
        }
    }
}
