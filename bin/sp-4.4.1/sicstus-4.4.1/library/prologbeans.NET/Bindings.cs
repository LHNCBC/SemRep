// Copyright (c) 2004 SICS AB. All rights reserved.
//
namespace se.sics.prologbeans
{
	using System;

	/// <summary> <c>Bindings</c> handles the variable bindings in the
	/// communication with the prolog server. Using variable bindings
	/// ensures that the values are properly quoted when sent to the
	/// prolog server.
	/// </summary>
	public class Bindings
	{

		// [PM] 4.3.2 never null
		readonly System.Collections.Hashtable bindings;

		/// <summary> Creates a new <c>Bindings</c> instance with no variable bindings.
		/// </summary>
		public Bindings ()
		{
			bindings = System.Collections.Hashtable.Synchronized (new System.Collections.Hashtable ());
		}

		/// <summary> Creates a new <c>Bindings</c> instance and copies all existing
		/// variable bindings from the specified bindings.
		/// </summary>
		/// <param name="binds">the variable bindings to copy
		/// 
		/// </param>
		public Bindings (Bindings binds)
		{
			if (binds != null) {
				bindings = System.Collections.Hashtable.Synchronized (new System.Collections.Hashtable (binds.bindings));
			} else {
				// [PM] 4.3.2 Why not throw an error instead?
				bindings = System.Collections.Hashtable.Synchronized (new System.Collections.Hashtable ());
			}
		}

		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bind (string name, int value)
		{
			return bind (name, PBTerm.makeTerm (value));
		}

		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bind (string name, long value)
		{
			return bind (name, PBTerm.makeTerm (value));
		}

		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bind (string name, float value)
		{
			return bind (name, PBTerm.makeTerm (value));
		}

		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bind (string name, double value)
		{
			return bind (name, PBTerm.makeTerm (value));
		}

		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bind (string name, string value)
		{
			return bind (name, PBTerm.makeTerm (value));
		}


		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bind (string name, PBTerm value)
		{
			checkVar (name);
			bindings [name] = value;
			return this;

		}

		/// <summary> Adds the specified variable binding. The variable name must start
		/// with an upper case letter or '_'. The value will be bound as an
		/// atom.
		/// </summary>
		/// <param name="name">a prolog variable name
		/// </param>
		/// <param name="value">the value to bind to the variable as an atom
		/// </param>
		/// <returns> a reference to this <c>Bindings</c> object
		/// </returns>
		/// <exception cref="System.ArgumentException"> if the name is not a
		/// valid prolog variable name
		/// </exception>
		public virtual Bindings bindAtom (string name, string value)
		{
			return bind (name, PBTerm.makeAtom (value));
		}

		// Throws an error if name is not a valid variable name.
		static void checkVar (string name)
		{
			if (name.Length > 0) {
				char c = name [0];

				if (Char.IsUpper (c)) {
					return;
				}

				if ((c == '_') && name.Length > 1) {
					return;
				}
			}
			throw new ArgumentException ("Variable names must start with uppercase letter or '_' : " + name);
		}


		/// <summary> Returns the value for the specified variable or <c>null</c>
		/// if the variable is not bound.
		/// </summary>
		/// <param name="name">the name of the variable
		/// </param>
		/// <returns> the value of the variable as a <c>Term</c> or
		/// <c>null</c> if the variable is not bound
		/// 
		/// </returns>
		public virtual PBTerm getValue (string name)
		{
			return (PBTerm)bindings [name];
		}

		/// <summary> For debugging.
		/// </summary>
		public override string ToString ()
		{
			System.Text.StringBuilder buffer = new System.Text.StringBuilder ();
			buffer.Append ('[');
			string prefix = "";
			foreach (string key in bindings.Keys) {
				buffer.Append (prefix);
				prefix = ",";
				PBTerm value = (PBTerm)bindings [key];
				buffer.Append (key).Append ('=').Append (value.toPrologString ());
			}
			buffer.Append (']');
			return buffer.ToString ();
		}

		internal virtual void  fastWrite (FastWriter writer)
		{
			foreach (string key in bindings.Keys) {
				PBTerm value = (PBTerm)bindings [key];
				writer.writeList ();
				writer.writeCompound ("=", 2);
				// Arg 1
				writer.writeAtom (key);
				// Arg 2
				writer.writeTerm (value);
			}
			writer.writeNIL ();
		}
	}
}
