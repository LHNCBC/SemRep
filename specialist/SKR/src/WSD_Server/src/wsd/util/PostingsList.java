
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

package wsd.util;

import java.util.*;
import java.io.*;

/**
 * Implementation of an Unmodifiable AbstractList for lists of "IR"
 * document postings.
 *
 *
 * Created: Fri Aug 31 17:22:28 2001
 *
 * @author <a href="mailto:wrogers@nlm.nih.gov">Willie Rogers</a>
 * @version $Id: PostingsList.java,v 1.1 2006/09/25 18:34:05 wrogers Exp $
 */

public class PostingsList extends AbstractList implements List {

  /** offset in file to beginning of postings list */
  private int address;
  /** number of postings in list */
  private int count;
  /** absolute offset (addresses) in postings file of postings in this
   * list. */
  private int[] offsets;
  /** byte lengths of of postings in this list */
  private int[] lengths;
  /** random access file object for postings file */
  private RandomAccessFile file;

  /**
   * Constructor.
   * @param postingFile random access file object for postings file 
   * @param postingsAddress offset in file to beginning of postings list
   * @param postingsCount number of postings in list
   */
  public PostingsList (RandomAccessFile postingsFile, int postingsAddress, int postingsCount)
    throws IOException
  {
    this.address = postingsAddress;
    this.count = postingsCount;
    this.file = postingsFile;
    // System.out.println("this.file: " + this.file);
    this.offsets = new int[postingsCount];
    this.lengths = new int[postingsCount];
    postingsFile.seek(this.address); int offset = this.address;
    for (int i = 0; i < count; i++)
      {
	this.offsets[i] = offset;
	int postingsLen = postingsFile.readInt();
	this.lengths[i] = postingsLen;
	// System.out.println("postingsLen : " + postingsLen);
	byte[] databuf = new byte[postingsLen];
	postingsFile.read(databuf);
	offset = offset + 4 + postingsLen;
      }
    
  }
  /** @return size of postings list. */
  public int size()
  {
    return this.count;
  }
  public void     add(int index, Object element) {  }
  public boolean  add(Object o)  { return false; }
  public boolean  addAll(int index, Collection c) { return false; }
  public void     clear() {}
  public boolean  equals(Object o) { return false; }
  /** 
   * get posting at index. 
   *  @return a posting (A String object)
   */
  public Object   get(int index) 
  { 
    try {
      this.file.seek(this.offsets[index] );
      int postingsLen = this.file.readInt(); 
      byte[] databuf = new byte[postingsLen];
      this.file.read(databuf);
      return new String(databuf);
    } catch ( IOException exception ) {
      System.err.println("IOException: " + exception.getMessage() );
      return null;
    }
  } 
  public int      hashCode() { return 0; }
  public int      indexOf(Object o) { return 0;}

  /** 
   * get an iterator over the current posting list.
   * @return iterator over postings list.
   */
  public Iterator iterator()
  { 
    try {
      return new PostingsListIterator(this.file, 
				      this.address, this.count, this.offsets); 
    } catch ( IOException exception ) { 
      return null; 
    } 
  }
  public int      lastIndexOf(Object o) { return 0;  }

  /** 
   * get an list iterator over the current posting list.
   * @return list iterator over postings.
   */
  public ListIterator    listIterator() 
  {
    try {
      return new PostingsListIterator(this.file, 
				      this.address, this.count, this.offsets); 
    } catch ( IOException exception ) { 
      System.err.println("exception occurred while creating list iterator: " + 
			 exception.getMessage()); 
      return null; 
    } 
  }
  /** 
   * get an list iterator over the current posting list, starting a index.
   * @param index index to start iterating at.
   * @return list iterator over postings.
   */
  public ListIterator    listIterator(int index) { 
    try {
      return new PostingsListIterator(this.file, 
				      this.address, this.count, this.offsets,
				      index); 
    } catch ( IOException exception ) { 
      System.err.println("exception occurred while creating list iterator: " +
			 exception.getMessage()); 
      return null; 
    } 
  }
  public Object	 remove(int index) {
    // not implemented
    return get(index); 
  }
  protected void removeRange(int fromIndex, int toIndex) 
  {
    // not implemented
  }
  public Object	  set(int index, Object element) 
  {
    // not implemented
    return null;
  }
  public List	   subList(int fromIndex, int toIndex) 
  { 
    try {
      return new PostingsList(file, this.offsets[fromIndex], toIndex - fromIndex);
    } catch (IOException exception) {
      System.err.println("exception occurred while creating sub list: " + 
			 exception.getMessage()); 
      return null;
    }
  }

  private class PostingsListIterator implements Iterator, ListIterator
    {
      /** offset in file to beginning of postings list */
      int address;
      /** number of postings in list */
      int count;
      /** current index in postings */
      int index = 0;
      /** absolute offset (addresses) in postings file of postings in this
       * list. */
      int[] offsets;
      /** random access file object for postings file */
      RandomAccessFile file;

      public PostingsListIterator(RandomAccessFile file, 
				  int address, int count, int offsets[])
	throws IOException
      {
	this.count = count;
	this.address = address;
	this.offsets = offsets;
	this.file = file; 
	file.seek(address);
      }
      public PostingsListIterator(RandomAccessFile file, 
				  int address, int count, int offsets[],
				  int index)
	throws IOException
      {
	this.count = count;
	this.address = address;
	this.offsets = offsets;
	this.index = index;
	this.file = file; 
	file.seek(offsets[index]);
      }
      /** non-implementation of interface ListIterator */
      public void add(Object o)
      {
	// not implemented
      }
      
      /** implementation of interface Iterator */
      public boolean hasNext()
      {
	//	System.out.println("PostingsList " + this + " .hasNext() => " +
	// (this.index < this.count) );
	return (this.index < this.count);
      }

      /** implementation of interface ListIterator */
      public boolean hasPrevious()
      {
	return (this.index > 0);
      }

      /** implementation of interface Iterator */
      public Object next()
	throws NoSuchElementException
      {
	if (this.index == this.count)
	  {
	    throw new NoSuchElementException("at end of list.");
	  } 
	this.index++;
	try {
	  int postingsLen = this.file.readInt();  
	  byte[] databuf = new byte[postingsLen]; 
	  this.file.read(databuf); 
	  return new String(databuf); 
	} catch ( IOException exception ) {
	  System.out.println("exception: " + exception);
	  return null;
	}
      }
      /** implementation of interface ListIterator */
      public int nextIndex()
      {
	return this.index + 1;
      }
      /** implementation of interface ListIterator */
      public Object previous()
	throws NoSuchElementException
      {
	if (this.index == 0)
	  {
	    throw new NoSuchElementException("at beginning of list.");
	  } 
	this.index--;
	try {
	  this.file.seek( this.offsets[index] );
	  int postingsLen = this.file.readInt();  
	  byte[] databuf = new byte[postingsLen]; 
	  this.file.read(databuf); 
	  return new String(databuf); 
	} catch ( IOException exception ) {
	  return null;
	}
      }
      /** implementation of interface ListIterator */
      public int previousIndex()
      {
	return this.index - 1;
      }
      /** implementation of interface Iterator */
      public void remove()
      {
	// not implemented
      }
      public void set(Object o)
      {
	// not implemented
      }
    }

}// PostingsList
