package javax.naming;

public class InitialContext implements Context
{
    public Object lookup(String lookupString)
    {
	throw new Error("InitialContext is not implemented in Prologbeans.NET");
    }
}
