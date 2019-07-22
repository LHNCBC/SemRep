package gov.nih.nlm.nls.util.trie;

import java.io.Serializable;


public class Trie <T> implements Serializable
{
  /** serialization version unique identifier for this class. */ 
  static final long serialVersionUID = -1613143146291047522L;

  private Node <T> root = new Node <T> ();
	
  public void insert (String key, T value)
  { insert(key.getBytes(), 0, value, root); }

  private Node <T> findChild(Node <T> child, char value)
  {
	if (child.c == value)
	{ return child; }
	else
	{
	  if (child.sibling != null)
	  { return findChild(child.sibling, value); }
	}
	
	return null;
  }
  
  private void insert (byte [] keyArray, int index, T value, Node <T> node)
  {
	if (index == keyArray.length)
	// Add the value, if the key is repeated, modify it
	{ node.value = value; }
	else  
	{
	  if (node.child == null)
 	  {
	    // Create the child node
		Node <T> child = new Node <T> ();
	    // Add the character
		child.c = (char)keyArray[index];
		// Add it to the current node
		node.child = child;
	    // Go for the next one
		insert (keyArray, index+1, value, child);
	  }
	  else
	  {
		// Find node with char
		Node <T> fc = findChild(node.child, (char)keyArray[index]);
		
		if (fc != null)
		{ insert (keyArray, index+1, value, fc); }
		else
		// If not, then create a new one
		{
	      // Create the child node
		  Node <T> child = new Node <T> ();
		  // Add the character
		  child.c = (char)keyArray[index];
		  // Add a sibling
		  child.sibling = node.child;
		  // Add it to the current node
		  node.child = child;
		  // Go for the next one
		  insert (keyArray, index+1, value, child);
		}  
	  }
	}
  }

  public T get (String key)
  {
	if (root.child == null)
	{ return null; }
	else
	{ return get(root.child, key.getBytes(),0); }
  }

  private T get(Node <T> node, byte [] keyArray, int index)
  {
	if (node.c != (char)keyArray[index])
	{
	  if (node.sibling != null)
	  { return get(node.sibling, keyArray, index); }
	  else
	  { return null; }
	}
	else
	{
	  if (index == keyArray.length - 1)
	  { return node.value; }
	  else
	  {
		if (node.child != null)
		{ return get(node.child, keyArray, index + 1); }
		else
		{ return null; }
	  }
	}
  }

  public void traverse()
  { traverse(root.child, ""); }
  
  private void traverse (Node <T> child, String space)
  {
	System.out.println(space + child.c + "/" + child.value);
	
	// Look for children
	if (child.child != null)
	{ traverse(child.child, space + "  "); }
	  
	// Look for siblings
	if (child.sibling != null)
	{ traverse(child.sibling, space); }
  }
}