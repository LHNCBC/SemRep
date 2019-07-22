namespace se.sics.prologbeans
{
	using System;

	/// <summary> <code>PrologSession</code> handles the connection with the Prolog Server.
	/// Currently only synchronous connections with the server are supported.
	/// </summary>
	// [PM] FIXME: when Java is closed it should do whatever it takes to
	// prevent the Prolog side from blocking forever. Presumably it would
	// be enough to close the Java-side socket.
	// [JE] Java always close sockets and streams when it closes down.
	// * What exactly does "Java is closed" mean?
	// [PM] When I close the window for the evaluate demo SICStus will
	// hang (in WriteFile() !!). At this point the Java process has
	// exited so this appears to be more of an OS bug.
	
	public class PrologSession
	{
		/// <summary> The timeout in milliseconds before the connection to the Prolog
		/// server is reset (when a query is not answered). Setting the timeout to
		/// <code>0</code> will disable timeouts for this prolog session. Default is
		/// 2000 milliseconds.
		/// 
		/// When a connection times out it will be closed silently.
		/// 
		/// </summary>
		virtual public int Timeout {
			get {
				if (parentSession != null) {
					return parentSession.Timeout;
				}
				return timeout;
			}
			
			set {
				if (parentSession != null) {
					parentSession.Timeout = value;
				} else {
					timeout = value;
				}
			}
		}


		/// <summary> The timeout in milliseconds before the connection to the Prolog
		/// server is reset (when a query is not answered). Setting the timeout to
		/// <code>0</code> will disable timeouts for this prolog session. Default is
		/// 2000 milliseconds.
		/// 
		/// When a connection times out it will be closed silently.
		/// 
		/// </summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public int getTimeout ()
		{
			return Timeout;
		}

		/// <summary> The timeout in milliseconds before the connection to the Prolog
		/// server is reset (when a query is not answered). Setting the timeout to
		/// <code>0</code> will disable timeouts for this prolog session. Default is
		/// 2000 milliseconds.
		/// 
		/// When a connection times out it will be closed silently.
		/// 
		/// </summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public void setTimeout (int value)
		{
			Timeout = value;
		}

		/// <summary> the port of the Prolog server (default <code>8066</code>).</summary>
		virtual public int Port {
			get {
				if (parentSession != null) {
					return parentSession.Port;
				}
				return port;
			}
			
			set {
				if (parentSession != null) {
					parentSession.Port = value;
				} else {
					port = value;
				}
			}
			
		}

		/// <summary> the port of the Prolog server (default <code>8066</code>).</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public int getPort ()
		{
			return Port;
		}

		/// <summary> the port of the Prolog server (default <code>8066</code>).</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public void setPort (int value)
		{
			Port = value;
		}

		/// <summary> The host of the Prolog server (default is <code>localhost</code>).
		/// The host can be specified as either IP-address or host name.
		/// </summary>
		virtual public string Host {
			get {
				if (parentSession != null) {
					return parentSession.Host;
				}
				return host;
			}
			
			set {
				if (parentSession != null) {
					parentSession.Host = value;
				} else {
					host = value;
				}
			}
		}

		/// <summary> The host of the Prolog server (default is <code>localhost</code>).
		/// The host can be specified as either IP-address or host name.
		/// </summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public String getHost ()
		{
			return Host;
		}


		/// <summary> The host of the Prolog server (default is <code>localhost</code>).
		/// The host can be specified as either IP-address or host name.
		/// </summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public void setHost (String prologServerHost)
		{
			Host = prologServerHost;
		}

		/// <summary> whether to close connections after each send. Default <tt>false</tt>.</summary>
		virtual public bool AlwaysClose {
			set {
				if (parentSession != null) {
					parentSession.AlwaysClose = value;
				} else if (value) {
					flags = flags | ALWAYS_CLOSE;
				} else {
					flags = flags & ~ALWAYS_CLOSE;
				}
			}

			get {
				if (parentSession != null) {
					return parentSession.AlwaysClose;
				}
				return (flags & ALWAYS_CLOSE) != 0;
			}
		}

		/// <summary> whether to close connections after each send. Default <tt>false</tt>.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isAlwaysClosing ()
		{
			return AlwaysClose;
		}

		/// <summary> whether to close connections after each send. Default <tt>false</tt>.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public void setAlwaysClose (bool close)
		{
			AlwaysClose = close;
		}

		/// <summary> The connection mode of this <code>PrologSession</code>. If set to
		/// <code>true</code> it will ensure that it is connected to the Prolog
		/// server as soon as a call to
		/// {@link se.sics.prologbeans.PrologSession#executeQuery executeQuery()} or
		/// anything else causing a need for communication happens. This is by
		/// default set to <code>false</code>.
		/// </summary>
		virtual public bool AutoConnect {
			set {
				if (parentSession != null) {
					parentSession.AutoConnect = value;
				} else if (value) {
					flags = flags | AUTO_CONNECT;
				} else {
					flags = flags & ~AUTO_CONNECT;
				}
			}
			get {
				if (parentSession != null) {
					return parentSession.AutoConnect;
				}
				return (flags & AUTO_CONNECT) != 0;
			}
		}

		/// <summary> The connection mode of this <code>PrologSession</code>. If set to
		/// <code>true</code> it will ensure that it is connected to the Prolog
		/// server as soon as a call to
		/// {@link se.sics.prologbeans.PrologSession#executeQuery executeQuery()} or
		/// anything else causing a need for communication happens. This is by
		/// default set to <code>false</code>.
		/// </summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isAutoConnecting ()
		{
			return AutoConnect;
		}

		/// <summary> The connection mode of this <code>PrologSession</code>. If set to
		/// <code>true</code> it will ensure that it is connected to the Prolog
		/// server as soon as a call to
		/// {@link se.sics.prologbeans.PrologSession#executeQuery executeQuery()} or
		/// anything else causing a need for communication happens. This is by
		/// default set to <code>false</code>.
		/// </summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public void setAutoConnect (bool autoConnect)
		{
			AutoConnect = autoConnect;
		}



		/// <summary> Returns <code>true</code> if a connection with the Prolog server is open
		/// and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether there is a connection to a Prolog server
		/// </returns>
		virtual public bool Connected {
			get {
				if (parentSession != null) {
					return parentSession.Connected;
				}
				return connection != null;
			}
			
		}

		/// <summary> Returns <code>true</code> if a connection with the Prolog server is open
		/// and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether there is a connection to a Prolog server.</returns>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isConnected ()
		{
			return Connected;
		}


		virtual internal long QueryStartTime {
			// -------------------------------------------------------------------
			// API towards PBMonitor.
			// The monitor is used to supervise and cancel prolog queries that
			// takes too long time.
			// -------------------------------------------------------------------
			
			
			get {
				return sendTime;
			}
			
		}

		static int debugLevel = 0;

		/// <summary> whether debugging of this package is in effect.</summary>
		public static bool Debugging {
			get {
				return debugLevel >= 1;
			}

		}

		/// <summary> whether debugging of this package is in effect.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public static bool debugging ()
		{
			return Debugging;
		}

		internal static bool debugging (int level)
		{
			return debugLevel >= level;
		}

		const int ALWAYS_CLOSE = 1;
		const int AUTO_CONNECT = 2;
		
		long sendTime = -1L;
		int timeout = 2000;
		// Wait for an answer max 2000 millis
		
		FastParser parser;
		// private string query;
		
		System.IO.Stream input;
		FastWriter output;
		System.Net.Sockets.TcpClient connection;
		
		int port = 8066;
		string host = "localhost";
		// [PD] 4.0 No autoconnect
		// private int flags = AUTO_CONNECT; // ALWAYS_CLOSE;
		int flags = 0;
		// AUTO_CONNECT; // ALWAYS_CLOSE;
		
		PrologSession parentSession;
		string prologSessionID;
		
		bool isAddedToMonitor = false;

		/// <summary> Creates a new <code>PrologSession</code> instance with default Prolog
		/// server settings.
		/// </summary>
		public PrologSession ()
		{
			parser = new FastParser ();
			// new Monitor(this);
		}
		// PrologSession constructor
		
		PrologSession (PrologSession parent, string sessionID)
		{
			parentSession = parent;
			prologSessionID = sessionID;
		}
		// PrologSession constructor
		
		/// <summary>FIXME: CS1591: Missing XML comment for publicly visible type or member `se.sics.prologbeans.PBTerm.bigIntegerValue()'</summary>
		public static PrologSession getPrologSession (string name)
		{
			if (name == null) {
				throw new ArgumentNullException ("name");
			}
			return null;
		}

		internal virtual void endSession ()
		{
			if (prologSessionID != null) {
				sendAtom ("end_session", prologSessionID);
			}
		}


		/// <summary>FIXME: CS1591: Missing XML comment for publicly visible type or member `se.sics.prologbeans.PBTerm.bigIntegerValue()'</summary>
		public virtual QueryAnswer executeQuery (string query)
		{
			return new QueryAnswer (send (query, null, prologSessionID), null);
		}

		/// <summary>FIXME: CS1591: Missing XML comment for publicly visible type or member `se.sics.prologbeans.PBTerm.bigIntegerValue()'</summary>
		public virtual QueryAnswer executeQuery (string query, Bindings bindings)
		{
			return new QueryAnswer (send (query, bindings, prologSessionID), bindings);
		}

		/// <summary>FIXME: CS1591: Missing XML comment for publicly visible type or member `se.sics.prologbeans.PBTerm.bigIntegerValue()'</summary>
		public virtual QueryAnswer executeQuery (string query, Bindings bindings, string sessionID)
		{
			return new QueryAnswer (send (query, bindings, sessionID), bindings);
		}

		static bool is_valid_latin1 (string str)
		{
			for (int i = 0, len = str.Length; i < len; i++) {
				if (str [i] > 255) {
					return false;
				}
			}
			return true;
		}

		PBTerm send (string query, Bindings bindings, string sessionID)
		{
			lock (this) {
				if (parentSession != null) {
					return parentSession.send (query, bindings, sessionID);
				}
				// [PM] 4.1.3 Moved check to before I/O
				if (!is_valid_latin1 (query)) {
					throw new ArgumentException ("Non ISO-8859-1 character in query: " + query);
				}
				try {
					initSend ();
					
					int len = sessionID == null ? 2 : 3;
					
					output.writeCompound ("query", len);
					// [PD] Quintus 3.5; read in Quintus can be confused unless there
					// is whitespace after the full stop.
					output.writeString (query + ". ");
					if (bindings == null) {
						output.writeNIL ();
					} else {
						bindings.fastWrite (output);
					}
					if (sessionID != null) {
						output.writeAtom (sessionID);
					}
					output.commit ();
					return parser.parseProlog (input);
				} catch (System.IO.IOException e) {
					close ();
					throw e;
				} finally {
					finishSend ();
				}
			}
		}

		PBTerm sendAtom (string commandName, string argument)
		{
			lock (this) {
				if (parentSession != null) {
					return parentSession.sendAtom (commandName, argument);
				}
				
				try {
					initSend ();
					
					// Write a fastrw term
					output.writeCompound (commandName, 1);
					output.writeAtom (argument);
					output.commit ();
					return parser.parseProlog (input);
				} catch (System.IO.IOException e) {
					if (PrologSession.debugging ()) {
						PrologSession.WriteStackTrace (e, Console.Error);
					}
					close ();
					return null;
				} finally {
					finishSend ();
				}
			}
		}

		void  initSend ()
		{
			if ((flags & AUTO_CONNECT) != 0) {
				connectToServer ();
			}
			if (output == null) {
				throw new System.IO.IOException ("no connection to Prolog Server");
			}
			
			sendTime = (DateTime.Now.Ticks - 621355968000000000) / 10000;
			if (timeout > 0) {
				PBMonitor.Default.queryStarted (this);
				isAddedToMonitor = true;
			}
		}

		void  finishSend ()
		{
			sendTime = -1L;
			if (isAddedToMonitor) {
				PBMonitor.Default.queryFinished (this);
				isAddedToMonitor = false;
			}
			if ((flags & ALWAYS_CLOSE) > 0) {
				close ();
			}
		}

		/// <summary> Connects to the Prolog server. By default
		/// {@link se.sics.prologbeans.PrologSession#executeQuery executeQuery()}
		/// will automatically connect to the server when called.
		/// </summary>
		/// <throws>  IOException if there is an error connecting </throws>
		public virtual void  connect ()
		{
			if (parentSession != null) {
				parentSession.connect ();
			} else {
				connectToServer ();
			}
		}
		
		// DEBUG
		internal class debugStream:System.IO.Stream
		{
			internal System.IO.Stream ostream;

			internal debugStream (System.IO.Stream os)
			{
				ostream = os;
			}

			public override void  Close ()
			{
				ostream.Close ();
			}

			public override void  Flush ()
			{
				ostream.Flush ();
			}

			public override void  Write (Byte[] buffer, int offset, int count)
			{
				Console.Error.Write (ToCharArray (buffer), offset, count);
				Console.Error.Flush ();
				ostream.Write (buffer, offset, count);
			}

			static char[] ToCharArray (byte[] byteArray)
			{
				return System.Text.Encoding.UTF8.GetChars (byteArray);
			}

			public override  void  WriteByte (byte value)
			{
				Console.Error.Write ((Char)value);
				Console.Error.Flush ();
				ostream.WriteByte (value);
			}

			public override Int64 Seek (Int64 offset, System.IO.SeekOrigin origin)
			{
				return ostream.Seek (offset, origin);
			}

			public override void  SetLength (Int64 value)
			{
				ostream.SetLength (value);
			}

			public override Int32 Read (Byte[] buffer, Int32 offset, Int32 count)
			{
				return ostream.Read (buffer, offset, count);
			}

			public override Boolean CanRead {
				get {
					return ostream.CanRead;
				}
			}

			public override Boolean CanSeek {
				get {
					return ostream.CanSeek;
				}
			}

			public override Boolean CanWrite {
				get {
					return ostream.CanWrite;
				}
			}

			public override Int64 Length {
				get {
					return ostream.Length;
				}
			}

			public override Int64 Position {
				get {
					return ostream.Position;
				}
				
				set {
					ostream.Position = value;
				}
			}
		}

		void  connectToServer ()
		{
			if (connection == null) {
				System.Net.Sockets.TcpClient conn = new System.Net.Sockets.TcpClient (host, port);
				if (debugging (2)) {
					output = new FastWriter (new debugStream (conn.GetStream ()));
				} else {
					output = new FastWriter (conn.GetStream ());
				}
				input = new System.IO.BufferedStream (conn.GetStream ());
				connection = conn;
			}
		}

		/// <summary> Closes the connection with the Prolog server. The connection can be
		/// opened again with {@link se.sics.prologbeans.PrologSession#connect
		/// connect()}.
		/// </summary>
		public virtual void disconnect ()
		{
			if (parentSession != null) {
				parentSession.close ();
			} else {
				close ();
			}
		}
		
		// Close connection when things have gone wrong...
		void  close ()
		{
			// System.out.println("Closing Connection...");
			try {
				sendTime = -1L;
				if (output != null) {
					output.close ();
					output = null;
				}
				if (input != null) {
					input.Close ();
					input = null;
				}
				if (connection != null) {
					connection.Close ();
					connection = null;
				}
			} catch (Exception e) {
				if (PrologSession.debugging ()) {
					PrologSession.WriteStackTrace (e, Console.Error);
				}
			}
		}

		/// <summary> Called by the timeout monitor when a query takes too long. Override this
		/// method to handle the timeout in some way other than silently closing the
		/// connection.
		/// </summary>
		protected internal virtual void  cancelQuery ()
		{
			isAddedToMonitor = false;
			if (sendTime != -1L) {
				close ();
			}
		}

		static PrologSession ()
		{
			// We use environment variables instead of (Java) system properties with .NET
			string stringValue = Environment.GetEnvironmentVariable ("se.sics.prologbeans.debugLevel");
			if (stringValue != null) {
				try {
					debugLevel = Convert.ToInt32 (stringValue);
				} catch (FormatException) {
					// ignore parse errors
				} catch (OverflowException) {
					// ignore parse errors
				}
			}
		}


		internal static void WriteStackTrace (Exception throwable, System.IO.TextWriter stream)
		{
			stream.WriteLine (throwable);
			stream.Write (throwable.StackTrace);
			stream.Flush ();
		}

	}
}
