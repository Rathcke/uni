package com.acertainbookstore.business;

/**
 * 
 * Book implements the book data-structure that a StockManager works with.
 * 
 */
public interface StockBook extends Book {

	/**
	 * Returns to accumulated rating.
	 * 
	 * @return
	 */
	public long getTotalRating();

	/**
	 * Returns the number of times a book has been rated.
	 * 
	 * @return
	 */
	public long getTimesRated();

	/**
	 * Returns the number of book copies.
	 * 
	 * @return
	 */
	public int getNumCopies();

	/**
	 * Returns the number of times that a client wanted to buy a book when it
	 * was not in stock.
	 * 
	 * @return
	 */
	public long getSaleMisses();

	/**
	 * Returns the rating of the book.
	 * 
	 * @return
	 */
	public float getAverageRating();

	/**
	 * Return True if the book is an editor pick.
	 * 
	 * @return
	 */
	public boolean isEditorPick();
	
}
