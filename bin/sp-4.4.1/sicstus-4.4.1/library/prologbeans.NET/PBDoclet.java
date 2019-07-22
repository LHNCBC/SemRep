import java.io.File;
import java.io.PrintStream;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.ConstructorDoc;
import com.sun.javadoc.DocErrorReporter;
import com.sun.javadoc.ExecutableMemberDoc;
import com.sun.javadoc.MethodDoc;
import com.sun.javadoc.ParamTag;
import com.sun.javadoc.Parameter;
import com.sun.javadoc.RootDoc;
import com.sun.javadoc.Tag;

/**
 * Converts javadoc comments to XML.
 *
 * Compile with:
 * /c/PROGRA~1/Java/JDK15~1.0_0/bin/javac -classpath C:/PROGRA~1/Java/JDK15~1.0_0/lib/tools.jar PBDoclet.java
 *
 * Invoke with:
 * /c/PROGRA~1/Java/JDK15~1.0_0/bin/javadoc -assembly prologbeans -doclet PBDoclet -docletpath . <java-files>
 *
 */
public class PBDoclet
{
    static int indentationIncrement = 4;
    static int indentationLevel = 0;
    static String assemblyName = null;
//    static PrintStream out = System.out;
    static PrintStream out;

    public static int optionLength(String option)
    {
	if (option.equals("-assembly")) {
	    return 2;
	}
	return 0;
    }

    public static boolean validOptions(String options[][], 
				       DocErrorReporter reporter) {
	boolean foundAssemblyOption = false;
        for (int i = 0; i < options.length; i++) {
            String[] opt = options[i];
            if (opt[0].equals("-assembly")) {
	        if (foundAssemblyOption) {
	            reporter.printError("Only one -assembly option allowed.");
		    return false;
	        } else { 
		    foundAssemblyOption = true;
		}
	    } 
	}
	if (!foundAssemblyOption) {
	    reporter.printError("Usage: javadoc -assembly assemblyname -doclet PBDoclet ...");
	}
	return foundAssemblyOption;
    }

    public static boolean start(RootDoc root)
    {
	String[][] options = root.options();
	for (int i = 0; i < options.length; i++) {
	    String[] opt = options[i];
	    if (opt[0].equals("-assembly")) {
		assemblyName = opt[1];
	    }
	}
	if (assemblyName == null) {
	    System.err.println("No assembly name - aborting");
	    return false;
	}
	try {
	    out = new PrintStream(new File(assemblyName + ".xml"));
	} catch (java.io.FileNotFoundException fnfex) {
	    System.err.println("Can't open output file");
	    return false;
	}
	printDoc(root);
	out.close();
	return true;
    }

    static void printDoc(RootDoc root)
    {
	printTagln("?xml version=\"1.0\"?");
	startTagln("doc");
	printPackageDoc(root);
	endTagln("doc");
    }

    static void printPackageDoc(RootDoc root)
    {
	printAssembly(assemblyName);
	startTagln("members");
	ClassDoc[] cla = root.classes();
	for (int i = 0; i < cla.length; i++) {
	    if (cla[i].isPublic()) {
		printClassDoc(cla[i]);
	    } else {
		System.out.println("    " + cla[i].toString() + " is not public");
	    }
	}
	endTagln("members");
    }

    static void printAssembly(String ass)
    {
	startTagln("assembly");
	startTag("name");
	print(ass);
	endTagln("name");
	endTagln("assembly");
    }

    static void printClassDoc(ClassDoc cd)
    {
	if (! cd.containingPackage().name().equals("se.sics.prologbeans")) {
	    System.out.println("Skipping " + cd.qualifiedTypeName());
	    return;
	}
	startTagln("member", "name", "T:" + cd.qualifiedTypeName());
	startTag("remarks");
	print(cd.commentText());
	endTagln("remarks");
	endTagln("member");
	ConstructorDoc[] consda = cd.constructors();
	for (int i = 0; i < consda.length; i++) {
	    printConstructor(consda[i]);
	}
	MethodDoc[] methda = cd.methods();
	for (int i = 0; i < methda.length; i++) {
	    printMethod(methda[i]);
	}
    }

    static void printConstructor(ConstructorDoc cd)
    {
	printExecutableMember(cd, ".#ctor");
	endTagln("member");
    }

    static void printMethod(MethodDoc meth)
    {
	printExecutableMember(meth, "");
	Tag[] ret = meth.tags("return");
	for (int i = 0; i < ret.length; i++) {
	    startTag("returns");
	    print(ret[i].text());
	    endTagln("returns");
	}
	endTagln("member");

    }

    static void printExecutableMember(ExecutableMemberDoc emd, String suffix)
    {
	String name = "M:" + emd.qualifiedName() + suffix;
	Parameter[] pma = emd.parameters();
	if (pma.length > 0) {
	    name += "(";
	    for (int i = 0; i < pma.length; i++) {
		name += pma[i].type().simpleTypeName();
		if (i == pma.length - 1) {
		    name += ")";
		} else {
		    name += ", ";
		}
	    }
	}
    	startTagln("member", "name", name);
	startTag("summary");
	print(emd.commentText());
	endTagln("summary");
	ParamTag[] pmta = emd.paramTags();
	for (int i = 0; i < pmta.length; i++) {
	    startTag("param", "name", pmta[i].parameterName());
	    print(pmta[i].parameterComment());
	    endTagln("param");
	}
    }

    static void print(String str)
    {
	out.print(str);
    }

    static void println(String str)
    {
	print(str); out.println();
    }

    static void startTag(String tag)
    {
	out.print("<" + tag + ">");
    }

    static void startTagln(String tag)
    {
	startTag(tag); out.println();
	indent();
	printIndentation();
    }

    static void startTag(String tag, String attribute, String value)
    {
	out.print("<" + tag + " " + attribute + "=" + '"' + value + '"' + ">");
    }

    static void startTagln(String tag, String attribute, String value)
    {
	startTag(tag, attribute, value); out.println();
	indent();
	printIndentation();
    }

    static void endTag(String tag)
    {
	out.print("</" + tag + ">");
    }

    static void endTagln(String tag)
    {
	endTag(tag); out.println();
	unindent();
	printIndentation();
    }

    static void printTag(String tag)
    {
	out.print("<" + tag + ">");
    }

    static void printTagln(String tag)
    {
	printTag(tag);
	out.println();
	printIndentation();
    }

    static void indent()
    {
//	indentationLevel += indentationIncrement;
    }

    static void unindent() 
    {
// 	indentationLevel -= indentationIncrement;
// 	if (indentationLevel < 0) {
// 	    System.err.println("Warning: indentationLevel==" + indentationLevel);
// 	}
    }

    static void printIndentation()
    {
// 	for (int i = indentationLevel; i > 0; i--) {
// 	    out.write(' ');
//	}
    }

}
