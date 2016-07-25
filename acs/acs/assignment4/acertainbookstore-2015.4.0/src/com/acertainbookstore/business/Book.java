/**
 * 
 */
package com.acertainbookstore.business;

/**
 * Book implements the book data-structure that a BookStoreClient works with.
 * 
 */
public interface Book {

	/**
	 * Returns the ISBN of the book.
	 * 
	 * @return
	 */
	public int getISBN();

	/**
	 * Returns the title of the book.
	 * 
	 * @return
	 */
	public String getTitle();

	/**
	 * Returns the author of the book.
	 * 
	 * @return
	 */
	public String getAuthor();

	/**
	 * Returns the price of the book.
	 * 
	 * @return
	 */
	public float getPrice();

}
