/*
 * Copyright (c) 2002 SICS AB
 */

package se.sics.jasper;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
//import java.util.LinkedList;
import java.util.Map;
//import java.util.Set;
import java.util.Stack;


/**
 * This class provides the multi threaded functionality of the Java-Prolog
 * interface. The User's Manual documents how to use this class.
 *
 * <p> The interfaces {@link se.sics.jasper.Prolog},
 * {@link se.sics.jasper.Query} and {@link se.sics.jasper.Term} declare
 * the methods available to the Java programmer for access to SICStus runtime.
 *
 */
public class Jasper
{
    // *** FIX? remove the counters when non-Beta.
    private Counter tokencounter;
    private Counter queuecounter;
    private JasperProlog jasperProlog;
    private PrologServer jasperServer;
 
    private static int jasperDebug = Integer.getInteger("se.sics.jasper.debugLevel", 0).intValue();

    final boolean debugging()
    {
        return debugging(1);
    }

    final boolean debugging(int level)
    {
        return jasperDebug >= level;
    }

    static void debugPrintln(String msg)
    {
        System.err.println("Thread " + Thread.currentThread().getName() + ": "
                           + msg);
        System.err.flush();
    }

    static void debugPrint(String msg)
    {
        System.err.print("Thread " + Thread.currentThread().getName() + ": "
                         + msg);
        System.err.flush();
    }

    // *** FIX? remove the counters when non-Beta.
    //          Or at least remove most of the usage.
    class Counter
    {
        private int c = 0;
        public int next()
        {
            return c += 1;
        }
    }

    class TokenQueue
    {
        private String name;
//L     private LinkedList qlist = new LinkedList();
        private Stack qstack = new Stack();
        synchronized public void put(Token t)
        {
            if (debugging(5)) debugPrintln("Entering TokenQueue.put()");
            if (debugging(5)) debugPrintln("    this queue: " + this);
            boolean wasEmpty = isEmpty();
            if (debugging(5)) debugPrintln("    wasEmpty==" + wasEmpty);
            if (debugging(5)) debugPrintln("    putting token: " + t);
            t.debugPrintToken(10);
//L         qlist.addFirst(t);
            qstack.push(t);
//L         if (debugging(10)) debugPrintln("    queue size: " + qlist.size());
            if (debugging(10)) debugPrintln("    queue size: " + qstack.size());
            if (wasEmpty) { notifyAll(); }
        }
        synchronized public Token get() throws InterruptedException
        { 
            if (debugging(5)) debugPrintln("Entering TokenQueue.get()");
            if (debugging(5)) debugPrintln("    this queue: " + this);
            waitfor();
            if (debugging(5)) debugPrintln("in TokenQueue.get(), after waitfor()");
            if (debugging(5)) debugPrintln("    this queue:" + this);
//L         if (debugging(10)) debugPrintln("    queue size: " + qlist.size());
            if (debugging(10)) debugPrintln("    queue size: " + qstack.size());
//L         Token tok = (Token)qlist.removeLast();
            Token tok = (Token)qstack.pop();
            if (debugging(5)) debugPrintln("    getting token: " + tok);
            tok.debugPrintToken(10);
            return tok; 
        }
        synchronized public boolean isEmpty()
        { 
//L         return qlist.isEmpty(); 
            return qstack.empty(); 
        }
        synchronized public void waitfor() throws InterruptedException
        {
            if (debugging(5)) debugPrintln("Entering TokenQueue.waitfor()");
            if (debugging(5)) debugPrintln("    this queue: " + this);
            if (debugging(5)) debugPrintln("    isEmpty():  " + isEmpty());
            while (isEmpty()) {
                wait();
            }
            if (debugging(5)) debugPrintln("Exiting TokenQueue.waitfor()");
        }
        public String toString()
        {
            return this.name;
        }
        TokenQueue(String name)
        {
            // *** FIX? Remove the counters when non-Beta.
            //          Use toString() here instead of the counter value?
            //          Well, let's leave the counters for a while ...
            this.name = "TokenQueue-" + queuecounter.next() + "-" + name;
            if (debugging(5)) debugPrintln( "Creating TokenQueue(): " + this.name);
        }
    }

    class Token
    {
        private String name;
        private TokenQueue myqueue;     // Return adress.
        private Object obj;
        private String methodname;
        private Class[] typesig;
        private Object[] args;
        private boolean staticP;        // static or instance
        private boolean hasResult;
        private Object result;
        private Exception excp;

        boolean isStopServerMessage()   { return false; }
        TokenQueue getQueue()           { return myqueue; }
        Object    getObject()           { return obj; }
        String    getMethodname()       { return methodname; }
        Class[]   getTypesig()          { return typesig; }
        Object[]  getArgs()             { return args; }
        boolean   getStaticP()          { return staticP; }
        void      setResult(Object res) { this.result = res; hasResult = true; }
        Object    getResult()           { return result; }
        void      setException(Exception ex) { this.excp = ex; }
        boolean   checkException()      { return (excp != null); }
        Exception getException()        { return excp; }
        boolean   checkResult()         { return hasResult; }

        public String toString()
        {
            return this.name;
        }
        void debugPrintToken(int debugLevel)
        {
            if (debugging(debugLevel)) {
                debugPrintln("  *Token*:");
                debugPrintln("    token:      " + name);
                debugPrintln("    myqueue:    " + myqueue);
                debugPrintln("    obj.getClass().getName():  " + obj.getClass().getName());
                debugPrintln("    obj.hashCode(): " + obj.hashCode());
                debugPrintln("    methodname: " + methodname);
//              debugPrintln("    typesig:    " + typesig);
                // [PD] 4.0.5 Can't use this in JDK 1.3.1
//              debugPrintln("    typesig:    " + Arrays.toString(typesig));
                // [PD] 4.0.5 Use this instead.
                debugPrintln("    typesig:    " + Arrays.asList(typesig).toString());
                debugPrintln("    staticP:    " + staticP);
                if (result == null) {
                    debugPrintln("    result is null");
                } else {
                    debugPrintln("    result is " + result.getClass().getName());
                }
                if (excp == null) {
                    debugPrintln("    excp is null");
                } else {
                    debugPrintln("    excp is not null");
                }
            }
        }
        Token(String name, TokenQueue tq, Object obj, String methodname,
                  Object[] args, Class[] typesig, boolean staticP)
        {
            // *** FIX? Remove the counters when non-Beta.
            //          Use toString() here instead of the counter value.
            // or maybe not ...
            this.name = "Token-" + tokencounter.next() + "-" + name;
            this.myqueue = tq;
            this.obj = obj;
            this.methodname = methodname;
            this.args = args;
            this.typesig = typesig;
            this.staticP = staticP;
            this.result = null;
            this.hasResult = false;
            this.excp = null;
        }
    }

    class NewObjectToken extends Token
    {
        NewObjectToken(String name, TokenQueue tq, Object obj,
                       String methodname, Object[] args, Class[] typesig)
        {
            super(name, tq, obj, methodname, args, typesig, true);
        }
    }

    class StopToken extends Token
    {
        boolean isStopServerMessage()   { return true; }

        void debugPrintToken(int debugLevel)
        {
            if (debugging(debugLevel)) debugPrintln("  *StopToken*");
        }

        StopToken()
        {
            super(null, null, null, null, null, null, false);
        }
    }

    class PrologServer implements Runnable, Server
    {
        private String bootPath;
        private String[] argv;
        private String savFile;
        private SICStus sp = null;
        private TokenQueue serverqueue = null;
        private TokenQueue clientqueue = null;
        private boolean runFlag = true;

        void initPrologServer()
        {
            serverqueue = new TokenQueue("Server");
            clientqueue = new TokenQueue("Client");
            runFlag = true;
        }
        PrologServer(String[] argv, String bootPath, String savFile)
        {
            this.argv = argv;
            this.bootPath = bootPath;
            this.savFile = savFile;
            sp = null;
            initPrologServer();
        }
        PrologServer(SICStus sp)
        {
            setSICStus(sp);
            initPrologServer();
        }

        public void run()
        {
            if (sp == null) {
                SICStus sp;
                try {
                    if (debugging(10)) debugPrintln("in PrologServer.run(): trying to create sp");
                    sp = new SICStus(argv, bootPath);
                    if (debugging(10)) debugPrintln("in PrologServer.run(): sp created");
                    if (savFile != null) { sp.restore(savFile); }
                } catch (Throwable th) {
                    setNoSICStus();
                    throw new Error("Cannot create a SICStus object. " + th);
                }
                setSICStus(sp);
            }
            if (!sp.spSetThreadServerMode(true)) {
                throw new Error("Cannot set thread server mode.");
            }
            TokenQueue srvq = getServerqueue();
            try {
                while (runFlag) {
                    if (debugging(5)) debugPrintln("in PrologServer.run(); runFlag-loop");
                    Token message = srvq.get();
                    boolean stop = message.isStopServerMessage();
                    if (stop) break;
                    while (!message.checkResult()) {
                        if (debugging(10)) debugPrintln("    message:");
                        message.debugPrintToken(10);
                        callMethod(sp, message);
                        clientqueue.put(message);
                        message = srvq.get();
                        stop = message.isStopServerMessage();
                        if (stop) break;
                    }
                    if (stop) break;
                }
            } catch (Exception ex) {
                if (debugging(10)) debugPrint("Exception in PrologServer.run():");
                ex.printStackTrace(System.err);
            } finally {
                sp.spSetThreadServerMode(false);
            }
        }

        public void stopServer()
        {
            if (runFlag) {
                TokenQueue srvq = getServerqueue();
                Token stopToken = new StopToken();
                runFlag = false;
                srvq.put(stopToken);
            }
        }

// [PD] 3.9 Does not have to be public?
//      synchronized public void setSICStus(SICStus sp)
        synchronized void setSICStus(SICStus sp)
        {
            this.sp = sp;
            sp.setServer(this);
            notifyAll();
        }
        synchronized private void setNoSICStus()
        {
        	notifyAll();
        }
        synchronized public SICStus getSICStus() throws InterruptedException
        {
            while (sp == null) {
                wait();
            }
            return sp;
        }
        TokenQueue getServerqueue() { return serverqueue; }
        TokenQueue getClientqueue() { return clientqueue; }

        class TypeSigConversionException extends Exception
        {
            TypeSigConversionException(String str, int i)
            {
                super(str + " " + i);
            }
        }

        ClassLoader ourClassLoader = null;

        Class forName(String name) throws ClassNotFoundException
        {
            if (ourClassLoader == null) {
                ourClassLoader = this.getClass().getClassLoader();
            }
            return Class.forName(name, false, ourClassLoader);
        }

        // Convert a JNI-style type signature, a String, to a type signature
        // acceptable for Method.invoke(), a Class[].
        // 
        Class[] convertTypesig(String typeSigString)
            throws TypeSigConversionException, ClassNotFoundException
        {
//          ArrayList<Class> al = new ArrayList<Class>();
            ArrayList al = new ArrayList();
            String argTypeString = typeSigString.substring(typeSigString.indexOf("(") + 1, typeSigString.indexOf(")")).replace('/', '.');
            for (int i = 0; i < argTypeString.length(); i++) {
                int arrayCount = 0;
                char tc = argTypeString.charAt(i);
                Class cc = null;
                switch(tc) {
                case '[':
                    arrayCount++;
                    char ac = argTypeString.charAt(++i);
                    while (ac == '[') {
                        arrayCount++;
                        ac = argTypeString.charAt(++i);
                    }
                    char[] arrayPrefix = new char[arrayCount];
                    Arrays.fill(arrayPrefix, '[');
                    switch (ac) {
                    case 'L':
                        int ii = argTypeString.indexOf(";", i + 1);
                        cc = forName(new String(arrayPrefix)
                                     + argTypeString.substring(i, ii + 1));
                        i = ii;
                        break;
                    case 'Z': case 'B': case 'C': case 'S': case 'I': case 'J': case 'F': case 'D':
                        cc = forName(new String(arrayPrefix)
                                     + argTypeString.substring(i, i + 1));
                        break;
                    default: throw new TypeSigConversionException(argTypeString, i);
                    }
                    break;
                case 'L':
                    int ii = argTypeString.indexOf(";", i + 1);
                    cc = forName(argTypeString.substring(i + 1, ii));
                    i = ii;
                    break;
                case 'Z': cc = boolean.class; break;
                case 'B': cc = byte.class; break;
                case 'C': cc = char.class; break;
                case 'S': cc = short.class; break;
                case 'I': cc = int.class; break;
                case 'J': cc = long.class; break;
                case 'F': cc = float.class; break;
                case 'D': cc = double.class; break;
                default: throw new TypeSigConversionException(argTypeString, i);
                }
                if (cc == null) { throw new TypeSigConversionException(argTypeString, i); }
                al.add(cc);
            }
            return (Class[])al.toArray(new Class[]{});
        }


        // The callBack<Foo> methods are called from jasper.c
        // *NOTE*: They are therefore called in the server thread!
        // When they are called the PrologClient is waiting for
        // the PrologServer to service a request. The client
        // checks for callbacks, executes one or more callbacks
        // and then continues to wait for a notification from the
        // server.

        public Token callBack(Object obj, String methname,
                              Object[] args, String typesig, boolean staticP)
            throws InterruptedException, Exception
        {
            Class[] ctypesig = convertTypesig(typesig);
            if (debugging(5)) debugPrintln("Entering PrologServer.callBack()");
            Token cbtok = new Token("Server",serverqueue,obj,methname,args,ctypesig, staticP);
            return callBackLoop(cbtok);
            
        }

        Token callBackLoop(Token cbtok) throws InterruptedException, Exception
        {
            clientqueue.put(cbtok);
            cbtok = serverqueue.get();
            while (!cbtok.checkResult()) {
                callMethod(sp, cbtok);
                clientqueue.put(cbtok);
                cbtok = serverqueue.get();
            }
// cbtok now has a return value, and possibly an exception value.
// Same as in PrologClient.sendMessage(): throw the exception. It will be
// caught by callMethod() and propagated further.
// [PD] 3.9.1. No, no! That will break things. We must not throw an exception
// here, since that will cause disrupt the calling sequence in the server
// thread. Or maybe not? Throwing the exception is the only way to propagate it,
// isn't it?
// Simply pass the token to the client, and let it handle the exception.
// *** FIX? We should perhaps not propagate InterruptedException?

            if (cbtok.checkException()) {
                Exception ex = cbtok.getException();
                throw (Exception)ex.fillInStackTrace();
            }

            return cbtok;
        }

        public Object callBackNewObject(Object obj, String methname,
                                        Object[] args, String typesig)
            throws InterruptedException, Exception
        {
            Class[] ctypesig = convertTypesig(typesig);
            Token cbtok = new NewObjectToken("Server",serverqueue,obj,methname,args,ctypesig);
            Token cbtok2 = callBackLoop(cbtok);
            return cbtok2.getResult();
        }

        public void callBackVoid(Object obj, String methname,
                                 Object[] args, String typesig,
                                 boolean staticP)
            throws InterruptedException, Exception
        {
            if (debugging(5)) debugPrintln("Entering PrologServer.callBackVoid()");
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
        }

        public Object callBackObject(Object obj, String methname,
                                     Object[] args, String typesig,
                                     boolean staticP)
            throws InterruptedException, Exception
        {
            if (debugging(5)) debugPrintln("Entering PrologServer.callBackObject()");
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return cbtok.getResult();
        }

        public boolean callBackBoolean(Object obj, String methname,
                                       Object[] args, String typesig,
                                       boolean staticP)
            throws InterruptedException, Exception
        {
            if (debugging(5)) debugPrintln("Entering PrologServer.callbackBoolean()");
            if (debugging(10)) debugPrintln("    obj:      " + obj);
            if (debugging(10)) debugPrintln("    methname: " + methname);
            if (debugging(10)) debugPrintln("    args:     " + args);
            if (debugging(10)) debugPrintln("    typesig:  " + typesig);
            if (debugging(10)) {
                debugPrintln("x"+methname+"y");
                if (methname.equals("barbool")) {
                     debugPrintln("["+methname+"]");
                     debugPrintln("    args[6]==" + args[6]);
                }
            }
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Boolean)cbtok.getResult()).booleanValue();
        }

        public byte callBackByte(Object obj, String methname,
                                 Object[] args, String typesig,
                                 boolean staticP)
            throws InterruptedException, Exception
        {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Byte)cbtok.getResult()).byteValue();
        }

        public char callBackChar(Object obj, String methname,
                                 Object[] args, String typesig,
                                 boolean staticP)
            throws InterruptedException, Exception
        {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Character)cbtok.getResult()).charValue();
        }

        public short callBackShort(Object obj, String methname,
                                   Object[] args, String typesig,
                                   boolean staticP)
            throws InterruptedException, Exception
        {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Short)cbtok.getResult()).shortValue();
        }

        public int callBackInt(Object obj, String methname,
                               Object[] args, String typesig,
                               boolean staticP)
            throws InterruptedException, Exception
        {
            if (debugging(5)) debugPrintln("Entering PrologServer.callBackInt()");
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            if (debugging(10)) debugPrintln("in PrologServer.callBackInt(), after callBack()");
            if (debugging(10)) debugPrintln("    cbtok.getResult()==" + cbtok.getResult());
            if (debugging(10)) debugPrintln("    intvalue: " + ((Integer)cbtok.getResult()).intValue());
            return ((Integer)cbtok.getResult()).intValue();
        }

        public long callBackLong(Object obj, String methname,
                                 Object[] args, String typesig,
                                 boolean staticP)
            throws InterruptedException, Exception
        {
            if (debugging(5)) debugPrintln("Entering PrologServer.callbackLong()");
            if (debugging(10)) debugPrintln("    obj:      " + obj);
            if (debugging(10)) debugPrintln("    methname: " + methname);
            if (debugging(10)) debugPrintln("    args:     " + args);
            if (debugging(10)) debugPrintln("    typesig:  " + typesig);
            if (debugging(10)) debugPrintln("    staticP:  " + staticP);
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            cbtok.debugPrintToken(2);
            return ((Long)cbtok.getResult()).longValue();
        }

        public float callBackFloat(Object obj, String methname,
                                   Object[] args, String typesig,
                                   boolean staticP)
            throws InterruptedException, Exception
        {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Float)cbtok.getResult()).floatValue();
        }

        public double callBackDouble(Object obj, String methname,
                                         Object[] args, String typesig,
                                     boolean staticP)
            throws InterruptedException, Exception
        {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Double)cbtok.getResult()).doubleValue();
        }

        // called by "glue" (i.e. spnative), in the server thread.
        JasperTerm newGlueJasperTerm(SPTerm spt)
            throws InterruptedException, Exception
        {
            if (debugging()) debugPrintln("Entering newGlueJasperTerm()");
            if (debugging()) {
                Object obj0[] = new Object[] {null};
                debugPrintln("in newGlueJasperTerm() (1)");
                debugPrintln("    spt==" + spt);
                debugPrintln("    spt.hashCode()==" + spt.hashCode());
            }       
            Object obj[] = new Object[] {spt};
            if (debugging()) debugPrintln("in newGlueJasperTerm() (2)");
            return (JasperTerm)callBackObject(this,
                                              "newGlueJasperTermFromClient",
                                              obj,
                                              "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;",
                                              false);
        }

        // Must be called from the client thread!
        // [PD] 4.0.5 This has to be public.
        public JasperTerm newGlueJasperTermFromClient(SPTerm spt)
            throws InterruptedException //, Exception
        {
            return new JasperTerm(jasperProlog, this, spt);
        }
    }

    class PrologClient
    {
        PrologServer myServer;
        SICStus sp = null;
        private TokenQueue serverqueue = null;
        TokenQueue clientqueue = null;
        Thread initialThread;
        
        public Server getServer() { return myServer; }

        void pushCaller(Prolog caller)
        {
// [PD] 3.9.1 SPRM 3305
//          ((Stack)(JasperGlobalCallerStack.get())).push(this);
            ((Stack)(JasperGlobalCallerStack.get())).push(caller);
        }
        void popCaller()
        {
            ((Stack)(JasperGlobalCallerStack.get())).pop();
        }

// [PD] 3.9.1 SPRM 3305
//      Token sendMessage(String typesig, Object obj, String methodName,
// [PD] 3.10.1 Make synchronized so multiple threads can use the same
//             PrologClient object.
        synchronized Token sendMessage(PrologClient owner,
                          Class[] typesig, Object obj, String methodName,
                          Object[] args) throws Exception
        {
// [PD] 3.10.1 Allow multiple threads to use the same PrologClient object
//          if (debugging()) {
//              Thread callingThread = Thread.currentThread();
//              if (initialThread != callingThread) {
//                  throw new Error("Illegal calling thread in PrologClient.sendMessage(); initialThread==" + initialThread + "; callingThread==" + callingThread);
//              }
//          }
            Token reply;
            Token request = new Token(Thread.currentThread().getName(), clientqueue, obj, methodName, args, typesig, false);
            serverqueue.put(request);
            reply = clientqueue.get();
            while (!reply.checkResult()) {
// [PD] Can we do this cast just like that? What if `this' is e.g. a
//      JasperTerm?
//      [PD, later] No, of course we can't. See SPRM 3305.
//              pushCaller((Prolog)this);
                if (debugging(5)) debugPrintln("In sendMessage(), owner==" + owner);
                pushCaller((Prolog)owner);
                callMethod(sp, reply);
                popCaller();
                serverqueue.put(reply);
                reply = clientqueue.get();
            }
// *** FIX? We should perhaps not propagate InterruptedException?
            if (debugging(5)) debugPrintln("In sendMessage(), checking for exception (this is expected)");
            reply.debugPrintToken(5);
            if (reply.checkException()) {
                Exception ex = reply.getException();
                throw (Exception)ex.fillInStackTrace();
            }
            return reply;
        }

// *** FIX? Consider making this private.
// [PD] 3.9.1 SPRM 3305
        Token sendMessage(Class[] typesig, Object obj, String methodName,
                          Object[] args) throws Exception
        {
            if (debugging(5)) debugPrintln("Entering sendMessage(), this.getClass()==" + this.getClass());
            return sendMessage(this, typesig, obj, methodName, args);
        }


        private void initQueues(PrologServer server)
        {
            this.myServer = server;
            serverqueue = myServer.getServerqueue();
            clientqueue = myServer.getClientqueue();
        }
        PrologClient(PrologServer server)
        {
            if (debugging()) debugPrintln("Creating a PrologClient from thread " + Thread.currentThread());
            initQueues(server);
            initialThread = Thread.currentThread();
        }
        PrologClient(PrologServer server, SICStus sp)
        {
/*          initQueues(server);
            initialThread = Thread.currentThread();
*/
            this(server);
            this.sp = sp;
        }
    }

    class JasperProlog extends PrologClient implements Prolog
    {
        public Query openPrologQuery(String string, Map varMap)
            throws Exception
        {
            if (debugging(5)) debugPrintln("entering JasperProlog.openPrologQuery()");
// [PD] 3.9.1 SPRM 3305
//          return new JasperQuery(myServer, string, varMap);
            return new JasperQuery(this, myServer, string, varMap);
        }

        private boolean queryHelper(String methodName, String string,
                                    Map varMap)
            throws Exception
        {
            if (debugging(5)) debugPrintln("entering JasperProlog.queryHelper()");
            if (debugging(10)) debugPrintln("    string==" + string);
            if (varMap != null) {
                // Unwrap all SPTerms
                Iterator it = varMap.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry me = (Map.Entry)it.next();
                    Object val = me.getValue();
                    if (debugging(10)) debugPrintln( "    val.getClass()==" + val.getClass());
                    if (val instanceof JasperTerm) {
                        me.setValue(((JasperTerm)val).getSPTerm());
                    }
                }
            if (debugging(10)) debugPrintln("JasperProlog.queryHelper(), after unwrap");
            }
            Token request =
                this.sendMessage(new Class[]{String.class, Map.class},
                                 sp, methodName,
                                 new Object[] {string, varMap});
            if (varMap != null) {
                // Wrap all SPTerms in JasperTerm objects
                Iterator it = varMap.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry me = (Map.Entry)it.next();
                    me.setValue(new JasperTerm(this, myServer, (SPTerm)me.getValue()));
                }
            }
            if (debugging(10)) debugPrintln("JasperProlog.queryHelper(), after wrap");
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean query(String string, Map varMap)
            throws Exception
        {
            if (debugging(5)) debugPrintln("entering JasperProlog.prologQuery()");
            return queryHelper("query", string, varMap);
        }

        public boolean queryCutFail(String string, Map varMap)
            throws Exception
        {
            if (debugging(5)) debugPrintln("entering JasperProlog.prologQueryCutFail()");
            return queryHelper("queryCutFail", string, varMap);
        }

        public Term newTerm() throws InterruptedException, Exception
        {
            if (debugging(5)) debugPrintln("Entering JasperProlog.newTerm()");
            Token request = this.sendMessage(new Class[]{},
                                             sp, "newTerm",
                                             new Object[] {});
            SPTerm spt = (SPTerm)request.getResult();
// [PD] 3.11.0 SPTerm.toString() cannot be called from the client thread
//          if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt==" + spt.toString());
            if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt==" + spt.superToString());
            if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt.isValid()==" + spt.isValid());
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(Term t) throws InterruptedException, Exception
        {
            Token request = this.sendMessage(new Class[]{Term.class},
                                             sp, "newTerm",
                                             new Object[] {((JasperTerm)t).getSPTerm()});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(int i) throws InterruptedException, Exception // integer
        {
            Token request = this.sendMessage(new Class[]{Integer.class},
                                             sp, "newTerm",
                                             new Object[] {new Integer(i)});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(long j) throws InterruptedException, Exception // integer
        {
            Token request = this.sendMessage(new Class[]{Long.class},
                                             sp, "newTerm",
                                             new Object[] {new Long(j)});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(double d) throws InterruptedException, Exception // float
        {
            Token request = this.sendMessage(new Class[]{Double.class},
                                             sp, "newTerm",
                                             new Object[] {new Double(d)});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(float f) throws InterruptedException, Exception // float
        {
            Token request = this.sendMessage(new Class[]{Float.class},
                                             sp, "newTerm",
                                             new Object[] {new Float(f)});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(String a) throws InterruptedException, Exception // atom
        {
            Token request = this.sendMessage(new Class[]{String.class},
                                             sp, "newTerm",
                                             new Object[] {a});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newTerm(String functor, Term args[]) throws InterruptedException, Exception // functor
        {
            if (debugging(5)) debugPrintln("Entering JasperProlog.newTerm(String,Term[])");
            // *** FIX? Is `new SPTerm[]' OK?
            SPTerm[] sptargs = new SPTerm[args.length];
            if (debugging(10)) debugPrintln("in JasperProlog.newTerm(String,Term[]), sptargs created");
            for (int i = 0; i < args.length; i++) {
                if (debugging(10)) debugPrintln("in JasperProlog.newTerm(String,Term[]), i==" + i);
                sptargs[i] = ((JasperTerm)args[i]).getSPTerm();
            }
            if (debugging(10)) debugPrintln("in JasperProlog.newTerm(String,Term[]), before sendMessage");
            Token request = this.sendMessage(new Class[]{String.class, Term[].class},
                                             sp, "newTerm",
                                             new Object[] {functor, sptargs});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        

        public Term prologReadFromString(String string, java.util.Map varMap) throws InterruptedException, Exception
        {
            Token request = sendMessage(new Class[]{String.class, Map.class},
                                        sp, "prologReadFromString",
                                        new Object[] {string, varMap});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term newVariable() throws InterruptedException, Exception
        {
            Token request = sendMessage(new Class[]{},
                                        sp, "newVariable",
                                        new Object[] {});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term consFunctor(String functor, Term[] args) throws InterruptedException, Exception
        {
            SPTerm[] sptargs = new SPTerm[args.length];
            for (int i = 0; i < args.length; i ++) {
                sptargs[i] = ((JasperTerm)args[i]).getSPTerm();
            }
            Token request = sendMessage(new Class[]{String.class, Term[].class},
                                        sp, "consFunctor",
                                        new Object[] {functor, sptargs});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }
        public Term consList(Term head, Term tail) throws InterruptedException, Exception
        {
            SPTerm spthead = ((JasperTerm)head).getSPTerm();
            SPTerm spttail = ((JasperTerm)tail).getSPTerm();
            Token request = sendMessage(new Class[]{Term.class, Term.class},
                                        sp, "consList",
                                        new Object[] {spthead, spttail});
            SPTerm spt = (SPTerm)request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        public Term newObjectTerm(Object obj) // [PD] 3.9.2 SPRM 3141
            throws InterruptedException, ConversionFailedException, Exception
        {
            Token request = this.sendMessage(new Class[]{},
                                             sp, "newTerm",
                                             new Object[] {});
            SPTerm spt = (SPTerm)request.getResult();
            // [PD] 3.10.2 We can't call putObject directly since we are not
            // in the server thread.
            //spt.putObject(obj);
            this.sendMessage(new Class[]{Object.class},
                             spt, "putObject",
                             new Object[] {obj});
            return new JasperTerm(this, myServer, spt);
        }

        public Term numberFromString(String str) // [PD] 3.9.2 SPRM 3141
            throws InterruptedException, ConversionFailedException, Exception
        {
            Token request = this.sendMessage(new Class[]{},
                                             sp, "newTerm",
                                             new Object[] {});
            SPTerm spt = (SPTerm)request.getResult();
            spt.putNumberChars(str);
            return new JasperTerm(this, myServer, spt);
        }

        public Term listFromString(String str) // [PD] 3.9.2 SPRM 3141
            throws InterruptedException, ConversionFailedException, Exception
        {
            Token request = this.sendMessage(new Class[]{},
                                             sp, "newTerm",
                                             new Object[] {});
            SPTerm spt = (SPTerm)request.getResult();
            spt.putListChars(str);
            return new JasperTerm(this, myServer, spt);
        }

        void initProlog(PrologServer server) throws InterruptedException

        {
            this.sp = server.getSICStus();
            if (this.sp == null) {
                throw new Error("Can't get SICStus from server");
            }
            if (debugging(5)) debugPrint("In JasperProlog.initProlog(); clientqueue==" + clientqueue);
        }

        JasperProlog(PrologServer server) throws InterruptedException
        {
            super(server);
            if (debugging(5)) debugPrintln( "Creating a JasperProlog");
        }
        JasperProlog(PrologServer server, SICStus sp)
            throws InterruptedException
        {
            super(server, sp);
            if (debugging(5)) debugPrintln( "Creating a JasperProlog");
        }
    }


// [PD] 3.10.2
//    class JasperQuery extends PrologClient implements Query
    class JasperQuery implements Query
    {
        SPQuery spq;
        Map myMap;
        JasperProlog owner;     // [PD] 3.9.1 SPRM 3305

        public void close() throws Exception
        {
            Token dummy = owner.sendMessage(owner, new Class[]{}, spq, "close",
                                           new Object[]{});
        }

        public void cut() throws Exception
        {
            Token dummy = owner.sendMessage(owner, new Class[]{}, spq, "cut",
                                           new Object[] {});
        }

        public boolean nextSolution()
            throws Exception
        {
            Token request = owner.sendMessage(owner, new Class[]{}, spq, "nextSolution",
                                             new Object[]{});
            boolean result = ((Boolean)request.getResult()).booleanValue();
            if (result && myMap != null) {
                // Wrap all SPTerms in JasperTerm objects
                Iterator it = myMap.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry me = (Map.Entry)it.next();
                    Object val = me.getValue();
                    if (val instanceof SPTerm) {
                        me.setValue(new JasperTerm(owner, jasperServer, (SPTerm)val));
                    }
                }
            }
            return result;
        }

// [PD] 3.9.1 SPRM 3305
//      JasperQuery(PrologServer server, String string, Map varMap)
        JasperQuery(JasperProlog owner, PrologServer server, String string,
                    Map varMap) throws Exception
        {
// [PD] 3.10.2
//          super(server, server.getSICStus());
            if (debugging(5)) debugPrintln( "Creating a JasperQuery");
            this.owner = owner; // [PD] 3.9.1 SPRM 3305
            myMap = varMap;
            if (varMap != null) {
                // Unwrap all SPTerms
                Iterator it = varMap.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry me = (Map.Entry)it.next();
                    Object val = me.getValue();
                    if (val instanceof JasperTerm) {
                        me.setValue(((JasperTerm)val).getSPTerm());
                    }
                }
            }
            Token request = owner.sendMessage(new Class[]{String.class, Map.class},
                                             owner.sp, "openQuery",
                                             new Object[] {string, varMap});
            spq = (SPQuery)request.getResult();
        }
    }


/* [PD] 3.9 Should not have to be public. */
//    public class JasperTerm extends PrologClient implements Term
    class JasperTerm implements Term
    {
        private SPTerm spt;
        SPTerm getSPTerm() { return spt; }
        JasperProlog owner;     // [PD] 3.10.2

        // [PD] 3.9.1 Used by "glue code" (jasper.c). Must be called in the
        //            server thread.
        long GetNativeTermRef() throws IllegalTermException
        {
            return getSPTerm().GetNativeTermRef();
        }

        public int compare(Term with) throws IllegalTermException, Exception
        {
            SPTerm sptwith = ((JasperTerm)with).getSPTerm();
            Token request = owner.sendMessage(new Class[]{Term.class},
                                              spt, "compare",
                                              new Object[] {sptwith});
            return ((Integer)request.getResult()).intValue();
        }
        public void delete() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "delete",
                                              new Object[] {});
        }
        public Term getArg(int i, Term arg) throws InterruptedException, Exception
        {
            SPTerm sptarg = ((JasperTerm)arg).getSPTerm();
            Token request = owner.sendMessage(new Class[]{Integer.class, Term.class},
                                        spt, "getArg",
                                        new Object[] {new Integer(i), sptarg});
            return this;
        }
        public double getDouble() throws ConversionFailedException, IllegalTermException, Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "getDouble",
                                              new Object[] {});
            return ((Double)request.getResult()).doubleValue();
        }
        public int getFunctorArity() throws ConversionFailedException, IllegalTermException, Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "getFunctorArity",
                                              new Object[] {});
            return ((Integer)request.getResult()).intValue();
        }
        public String getFunctorName() throws ConversionFailedException, IllegalTermException, Exception
        {
            Token request = owner.sendMessage(new Class[]{},
                                              spt, "getFunctorName",
                                              new Object[] {});
            return (String)request.getResult();
        }
        public long getInteger() throws Exception
        {
            if (debugging(5)) debugPrintln( "entering JasperTerm.getInteger()");
            Token request = owner.sendMessage(new Class[]{}, spt, "getInteger",
                                              new Object[] {});
            if (debugging(10)) debugPrintln( "in JasperTerm.getInteger(), returning");
            return ((Long)request.getResult()).longValue();
        }
        public Term getList(Term head, Term tail) throws InterruptedException, Exception
        {
            SPTerm spthead = ((JasperTerm)head).getSPTerm();
            SPTerm spttail = ((JasperTerm)tail).getSPTerm();
            Token request = owner.sendMessage(new Class[]{Term.class, Term.class},
                                              spt, "getList",
                                              new Object[] {spthead, spttail});
            return this;
        }
        public String getListChars() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{},
                                              spt, "getListChars",
                                              new Object[] {});
            return (String)request.getResult();
        }
        public String getNumberChars() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{},
                                              spt, "getNumberChars",
                                              new Object[] {});
            return (String)request.getResult();
        }
        public Object getObject() throws Exception
        {
// [PD] 3.9 There is no putObject() yet, so this can't be used. Using unify()
//          instead of putObject() (as with the other put<Foo>() methods is
//          not acceptable since we need to create a GlobalRef for the object
//          before it is stored in a prolog term.
// [PD] 3.9.2 JasperProlog.newObjectTerm() now exists, so this code can
//            safely be used.
            Token request = owner.sendMessage(new Class[]{},
                                              spt, "getObject",
                                              new Object[] {});
            return request.getResult();
        }
        public String getString() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{},
                                              spt, "getString",
                                              new Object[] {});
            return (String)request.getResult();
        }
        public boolean isAtom() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isAtom",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isAtomic() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isAtomic",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isCompound() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isCompound",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isEmptyList() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isEmptyList",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isFloat() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isFloat",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isInteger() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isInteger",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isList() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isList",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isNumber() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isNumber",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isValid() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isValid",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
        public boolean isVariable() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "isVariable",
                                              new Object[] {});
            return ((Boolean)request.getResult()).booleanValue();
        }
/* We don't want the put methods. Use unify instead.
        public Term putEmptyList()
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putFloat(double value)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putFloat(float value)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
// *** FIX? Perhaps we need putFunctor after all?
        public Term putFunctor(String functor, int arity)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putInteger(int value)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putList()
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putListChars(String string)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putNumberChars(String string)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putObject(Object obj)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putString(String value)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putTerm(Term new_term)
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
        public Term putVariable()
        {
            return new JasperTerm(new PrologServer());  // *** temp
        }
*/
        public String toString()
//          If we call SPTerm.toString() (via sendMessage()), this method must
//          throw Exception. The compiler will complain about that since
//          Object.toString() does not throw Exception.
//          Solution: catch the Exception from sendMessage() and return the
//          result from super.toString (which is Object.toString) with the
//          exception string appended.
        {
            if (debugging(5)) debugPrintln("Entering JasperTerm.toString()");
            try {
                Token request = owner.sendMessage(new Class[]{},
                                                  spt, "toString",
                                                  new Object[] {});
                return (String)request.getResult();
            } catch (Exception ex) {
                if (debugging(2)) debugPrintln("Warning: SPTerm.toString() failed. Using Object.toString instead.");
                if (debugging(5)) {
                    debugPrintln("    exception:" + ex);
                    ex.printStackTrace(System.err);
                }
                return super.toString() + ex.toString();
            }
        }
        public String toString(Term options) throws Exception
        {
            if (debugging(5)) debugPrintln("Entering JasperTerm.toString(Term options)");
            if (debugging(5)) debugPrintln("    spt.getClass()==" + spt.getClass());
            SPTerm sptoptions = ((JasperTerm)options).getSPTerm();
            Token request = owner.sendMessage(new Class[]{Term.class},
                                             spt, "toString",
                                             new Object[] {sptoptions});
            if (debugging(5)) debugPrintln("Returning from JasperTerm.toString(Term options)");
            return (String)request.getResult();
        }
        public Term[] toPrologTermArray() throws InterruptedException, Exception
        {
            Token request = owner.sendMessage(new Class[]{},
                                              spt, "toTermArray",
                                              new Object[] {});
            SPTerm[] spta = (SPTerm[])request.getResult();
            Term[] ta = new Term[spta.length];
            for (int i = 0; i < ta.length; i++) {
                ta[i] = new JasperTerm(owner, jasperServer, spta[i]);
            }
            return ta;
        }
        public int type() throws Exception
        {
            Token request = owner.sendMessage(new Class[]{}, spt, "type",
                                              new Object[] {});
            return ((Integer)request.getResult()).intValue();
        }
        public boolean unify(Term with) throws Exception
        {
            SPTerm sptwith = ((JasperTerm)with).getSPTerm();
            Token request = owner.sendMessage(new Class[]{Term.class},
                                              spt, "unify",
                                              new Object[] {sptwith});
            return ((Boolean)request.getResult()).booleanValue();
        }

        JasperTerm(JasperProlog owner, PrologServer server, SPTerm spt)
            throws InterruptedException
        {
// [PD] 3.10.2
//          super(server, server.getSICStus());
            if (debugging(5)) debugPrintln( "Creating a JasperTerm");
            this.owner = owner; // [PD] 3.10.2
            this.spt = spt;
        }
    }

    /* Meta-method */
    void callMethod(SICStus sp, Token request)
    {
        if (debugging(5)) debugPrintln( "Entering callMethod()");
        try {
            Object result = null;
            Object obj = request.getObject();
            String methodname = request.getMethodname();
            Class[] ts = request.getTypesig();
            Object[] args = request.getArgs();
            if (methodname.equals("<init>")) {
                Class clazz = (Class)(obj);
                if (debugging(5)) {
                    debugPrintln("                     clazz==" + clazz);
                }
                Constructor cons = clazz.getConstructor(ts);
                result =  cons.newInstance(args);
            } else {
                Class clazz = ( request.getStaticP() ? (Class)obj : obj.getClass() );
                Method m = clazz.getMethod(methodname,
                                           request.getTypesig());
                result= m.invoke(obj, args);
            }
            if (result == null) {
                request.setResult(java.lang.Void.TYPE);
            } else {
                request.setResult(result);
            }
        } catch (Exception ex) {
            if (debugging(5)) debugPrint("in callMethod(), caught an Exception ");
            if (debugging(5)) {
                if (ex instanceof InvocationTargetException) {
                    System.err.print(", InvocationTargetException with cause: ");
                    // [PD] 4.0.5 Can't use this in JDK 1.3.1
//                  Throwable cause = ex.getCause();
                    // [PD] 4.0.5 Use this instead
                    Throwable cause = ((InvocationTargetException)ex).getTargetException();
                    System.err.println(cause.toString());
                    cause.printStackTrace(System.err);
                } else {
                    System.err.println(ex.toString());
                    ex.printStackTrace(System.err);
                }
            } else {
                if (debugging(5)) debugPrintln(""); // *** ???
            }
            request.setResult(java.lang.Void.TYPE);
            request.setException(ex);
        } 
    }

    /* [PD] 3.9 Should not have to be public. */
//    public Server getServer()
    Server getServer()
    {
        return jasperServer;
    }

    static ThreadLocal JasperGlobalCallerStack = new java.lang.ThreadLocal()
        {
            protected Object initialValue()
            {
                Stack st = new Stack();
                st.push(null);
                return st;
            }
        };

    // *** [PD] FIX: This needs more detailed documentation.
    /**
     * Returns the Prolog interface corresponding to the SICStus runtime that
     * called us.
     * If Java is the toplevel, this method returns null,
     * @return the calling instance
     */
    public static Prolog getCaller()
    {
        return (Prolog)((Stack)(JasperGlobalCallerStack.get())).peek();
    }

    /**
     * Sets the calling Prolog interface. This is done automatically for each
     * call to the Prolog runtime (e.g. a query or similar), but can also be
     * done explicitly with this method.
     */
// [PD] Does this have to be public? *** FIX: Is it necessary to keep it at all?
//    public static void setCaller(Prolog p)
    static void setCaller(Prolog p)
    {
        ((Stack)(JasperGlobalCallerStack.get())).push(p);
    }


    // The arguments to newProlog must match the constructors for
    // class SICStus.
    /**
     * Creates a {@link se.sics.jasper.Prolog} interface object. Equivalent to
     * {@link #newProlog(String[], String, String) newProlog(null, null, null)}.
     * @return a new prolog instance
     * @throws InterruptedException if the thread was interrupted
     */
    public static Prolog newProlog() throws InterruptedException
    {
        return newProlog(null, null, null);
    }
    /**
     * Creates a {@link se.sics.jasper.Prolog} interface object. Equivalent to
     * {@link #newProlog(String[], String, String) newProlog(null, bootPath, null)}.
     * @param bootPath The path where SICStus should look for its start-up
     * files.
     * @return a new prolog instance
     * @throws InterruptedException if the thread was interrupted
     */
    public static Prolog newProlog(String bootPath) throws InterruptedException
    {
        return newProlog(null, bootPath, null);
    }
    /**
     * Creates a {@link se.sics.jasper.Prolog} interface object. Starts a server
     * thread which will serve that {@link se.sics.jasper.Prolog}. The server
     * thread takes care of all interaction with the Prolog runtime, making sure
     * that all calls to the Prolog runtime will be done from one and the same
     * thread.
     *
     * @param argv Argument vector to the emulator.
     * @param bootPath The path where SICStus should look for its  start-up
     * files.
     * @param savFile A .sav-file to restore. See
     * {@link se.sics.jasper.SICStus#restore}
     * @return a new prolog instance
     * @throws InterruptedException if the thread was interrupted
     */
    public static Prolog newProlog(String[] argv,String bootPath,String savFile)
        throws InterruptedException
    {
        Jasper js = new Jasper(argv, bootPath, savFile);
        return js.jasperProlog;
    }
    /**
     * Multiple threads must synchronize when calling this method.
     *
     *
     */
/* [PD] 3.9 Should not have to be public. */
//    public static Prolog newProlog(SICStus sp) throws InterruptedException
    static Prolog newProlog(SICStus sp) throws InterruptedException
    {
        Jasper js = new Jasper(sp);
        return js.jasperProlog;
    }

    void startServer()
    {
        Thread serverThread = new Thread((PrologServer)jasperServer,
                                         "ServerThread");
        serverThread.setDaemon(true);
        serverThread.start();
    }

    Jasper(String[] argv, String bootPath, String savFile)
        throws InterruptedException
    {
        // *** FIX? Remove the counters when non-Beta.
        tokencounter = new Counter();
        queuecounter = new Counter();
        jasperServer = new PrologServer(argv, bootPath, savFile);
        // Order is important here. jasperServer must exist when
        // a new JasperProlog is created.
        jasperProlog = new JasperProlog(jasperServer);
        // Order is important here, too. The server must be running
        // before jasperProlog is initialized.
        startServer(); 
        jasperProlog.initProlog(jasperServer);
    }
    Jasper(SICStus sp) throws InterruptedException
    {
        tokencounter = new Counter();
        queuecounter = new Counter();
        jasperServer = new PrologServer(sp);
        // Order is important here. jasperServer must exist when
        // a new JasperProlog is created.
        jasperProlog = new JasperProlog(jasperServer, sp);
    }

    /* Test Jasper.
        java -Djava.library.path=/usr/local/lib -Dsicstus.path=/usr/local/lib/sicstus-3.8.6 se.sics.jasper.Jasper
    */
    /** This is a small test function. It will try to load Jasper by creating
     * a Jasper object and prints a message if it succeeded.
     * @param argv the command line arguments
     */

    public static void main(String argv[])
    {
        try {
            System.out.print("Trying to start Jasper...");
            Prolog j = newProlog();
            System.out.println("OK");
        } catch (Exception ex) {
            System.err.println("Failed to start Jasper: ");
            ex.printStackTrace(System.err);
        }
    }

}
