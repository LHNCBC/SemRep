/*
 * Copyright (c) 2002 SICS AB
 */

package se.sics.jasper;

interface Server
{
    void run();
//    void setSICStus(SICStus sp);
    SICStus getSICStus() throws InterruptedException;
    void stopServer();
/*
    TokenQueue getServerqueue();
    TokenQueue getClientqueue();
    void methodServer(TokenQueue srvq) throws InterruptedException;
    void callMethod(Token request);
    void callObjectMethod(Token request) throws Exception;
    void callBooleanMethod(Token request) throws Exception;
    void callByteMethod(Token request);
    void callCharMethod(Token request);
    void callShortMethod(Token request);
    void callIntMethod(Token request);
    void callLongMethod(Token request);
    void callFloatMethod(Token request);
    void callDoubleMethod(Token request);
    public Token callBack(Object obj, String methname,
			  Object[] args, String typesig)
	throws InterruptedException, Exception;
    public void callBackVoid(Object obj, String methname,
			     Object[] args, String typesig)
	throws InterruptedException, Exception;
    public Object callBackObject(Object obj, String methname,
				 Object[] args, String typesig)
	throws InterruptedException, Exception;
    public boolean callBackBoolean(Object obj, String methname,
				   Object[] args, String typesig)
	throws InterruptedException, Exception;
    public byte callBackByte(Object obj, String methname,
			     Object[] args, String typesig)
	throws InterruptedException, Exception;
    public char callBackChar(Object obj, String methname,
			     Object[] args, String typesig)
	throws InterruptedException, Exception;
    public short callBackShort(Object obj, String methname,
			       Object[] args, String typesig)
	throws InterruptedException, Exception;
    public int callBackInt(Object obj, String methname,
			   Object[] args, String typesig)
	throws InterruptedException, Exception;
    public long callBackLong(Object obj, String methname,
			     Object[] args, String typesig)
	throws InterruptedException, Exception;
    public float callBackFloat(Object obj, String methname,
			       Object[] args, String typesig)
	throws InterruptedException, Exception;
    public double callBackDouble(Object obj, String methname,
				 Object[] args, String typesig)
	throws InterruptedException, Exception;
*/
}
