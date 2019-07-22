// Copyright (c) 2004 SICS AB. All rights reserved.
// 
namespace se.sics.prologbeans
{
	using System;

	class FastWriter
	{
		// [PM] 4.3.2 Use a strict encoder
		static internal readonly System.Text.Encoding UTF8encoder = new System.Text.UTF8Encoding (false, true);

		readonly System.IO.Stream output;
		bool isWritingTerm = false;

		// null, or a, not necessarily thread safe, mapping from variable to the (bytes of its) DFS number.
		System.Collections.Hashtable variableTable = null;

		public FastWriter (System.IO.Stream output)
		{
			this.output = output;
		}

		void initOutput ()
		{
			if (!isWritingTerm) {
				isWritingTerm = true;
				variableTable = null; // [PM] 4.3.2 redundant, should be null already.
				output.WriteByte (FastParser.VERSION);
			}
		}

		public virtual void  writeCompound (string name, int arguments)
		{
			writeTaggedBytes (FastParser.COMPOUND, UTF8encoder.GetBytes (name));
			output.WriteByte ((Byte)arguments);
		}

		public virtual void  writeList ()
		{
			initOutput ();
			output.WriteByte (FastParser.LIST);
		}

		public virtual void  writeNIL ()
		{
			initOutput ();
			output.WriteByte (FastParser.NIL);
		}

		public virtual void  writeString (string value)
		{
			initOutput ();
			writeCharList (value);
			output.WriteByte (FastParser.NIL);
		}
			
		// [PM] 4.3.2 Writes the (possibly empty) list prefix corresponding to the string. In either case the caller should write the tail (typically NIL).
		void writeCharList (string value)
		{
			bool in_compact_list = false;
			for (int i = 0; i < value.Length; i++) {
				char c = value [i];
				if (0 < c && c < 256) {
					if (!in_compact_list) {
						output.WriteByte (FastParser.STRING);
					}
					output.WriteByte ((Byte)c);
					in_compact_list = true;
				} else {
					if (in_compact_list) {
						output.WriteByte (0);
						in_compact_list = false;
					}
					output.WriteByte (FastParser.LIST);
					writeTaggedBytes (FastParser.INTEGER, UTF8encoder.GetBytes (Convert.ToString (((int)c))));
				}
			}
			if (in_compact_list) {
				output.WriteByte (0);
			}
		}

		public virtual void  writeAtom (string value)
		{
			writeTaggedBytes (FastParser.ATOM, UTF8encoder.GetBytes (value));
		}

		public virtual void  writeAtomic (PBAtomic atomic)
		{
			byte tag;

			int type = atomic.Type;
			switch (type) {
				
			case PBAtomic.ATOM: 
				tag = FastParser.ATOM;
				break;
				
			case PBAtomic.FLOAT: 
				tag = FastParser.FLOAT;
				break;
				
			case PBAtomic.INTEGER: 
				tag = FastParser.INTEGER;
				break;
				
			default: 
				throw new SystemException ("illegal type: " + type);
			}

			writeTaggedBytes (tag, UTF8encoder.GetBytes (atomic.Name));
		}
		
		// [PM] 4.3.2
		public virtual void  writeVariable (PBVariable var)
		{
			if (variableTable == null) {
				variableTable = new System.Collections.Hashtable ();
			}
			byte[] variableNameBytes = (byte[])variableTable [var];
			if (variableNameBytes == null) {
				variableNameBytes = UTF8encoder.GetBytes (Convert.ToString (variableTable.Count));
				variableTable [var] = variableNameBytes;
			}
			writeTaggedBytes (FastParser.VARIABLE, variableNameBytes);
		}

		// [PM] 4.3.2 Init output, write the tag, write the byteArray bytes, terminate with a single zero byte.
		void writeTaggedBytes (byte tag, byte[] byteArray)
		{
			initOutput ();
			output.WriteByte (tag);
			output.Write (byteArray, 0, byteArray.Length);
			output.WriteByte (0);
		}


		public virtual void  commit ()
		{
			output.Flush ();
			isWritingTerm = false;
			variableTable = null;
		}

		public virtual void  close ()
		{
			commit ();
			output.Close ();
		}

		class WriteWork
		{
			virtual public WriteWork Next {
				get {
					return next;
				}
				
			}

			int i;
			readonly PBTerm[] args;
			readonly WriteWork next;

			public WriteWork (PBTerm[] args, WriteWork next)
			{
				// assert args != null;
				// assert args.length > 0;
				i = 0;
				this.args = args;
				this.next = next;
			}

			/// <summary> Get next term to process at this level or null. If null is returned
			/// then use getNext() to pop this level
			/// </summary>
			internal virtual PBTerm bump ()
			{
				if (i < args.Length) {
					return args [i++];
				}
				return null;
			}

			public virtual WriteWork push (PBTerm[] args)
			{
				// assert args != null && args.length > 0;
				return new WriteWork (args, prune ());
			}

			/// <summary> Pop exhausted stack entries and return the resulting stack (which may
			/// be null).
			/// </summary>
			internal virtual WriteWork prune ()
			{
				WriteWork tmp = this;
				while (tmp != null && tmp.i == tmp.args.Length) {
					tmp = tmp.Next;
				}
				return tmp;
			}
		}
		
		internal virtual void  writeTerm (PBTerm term)
		{
			WriteWork stack = new WriteWork (new []{ term }, null);
			do {
				PBTerm t = stack.bump ();
				if (t != null) {
					PBTerm[] args = t.fastWritePrefix (this);
					if (args.Length > 0) {
						stack = stack.push (args);
					}
				} else {
					// top-most entry exhausted, pop it.
					stack = stack.prune ();
				}
			} while (stack != null);
		}

	}
}
