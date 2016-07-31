package com.acertainbookstore.interfaces;

import java.util.List;
import java.util.Set;

import com.acertainbookstore.business.BookCopy;
import com.acertainbookstore.business.BookEditorPick;
import com.acertainbookstore.business.StockBook;
import com.acertainbookstore.utils.BookStoreException;

/**
 * These methods needs to be implemented by clients who wants to sell items in
 * the book store.
 * 
 */
public interface StockManager {

	/**
	 * Adds the books in bookSet to the stock.
	 * 
	 * @param bookSet
	 * @return
	 * @throws BookStoreException
	 */
	public void addBooks(Set<StockBook> bookSet) throws BookStoreException;

	/**
	 * Add copies of the existing book to the bookstore.
	 * 
	 * @param bookCopiesSet
	 * @return
	 * @throws BookStoreException
	 */
	public void addCopies(Set<BookCopy> bookCopiesSet)
			throws BookStoreException;

	/**
	 * Returns the list of books in the bookstore
	 * 
	 * @return
	 * @throws BookStoreException
	 */
	public List<StockBook> getBooks() throws BookStoreException;

	/**
	 * Returns the books matching the set of ISBNs given, is different to
	 * getBooks in the BookStore interface because of the return type of the
	 * books
	 * 
	 * @param isbns
	 * @return
	 * @throws BookStoreException
	 */
	public List<StockBook> getBooksByISBN(Set<Integer> isbns)
			throws BookStoreException;

	/**
	 * Returns the list of books which has sale miss
	 * 
	 * @return
	 * @throws BookStoreException
	 */
	public List<StockBook> getBooksInDemand() throws BookStoreException;

	/**
	 * Books are marked/unmarked as an editor pick
	 * 
	 * @return
	 * @throws BookStoreException
	 */
	public void updateEditorPicks(Set<BookEditorPick> editorPicks)
			throws BookStoreException;

	/**
	 * Clean up the bookstore - remove all the books and the associated data
	 * 
	 * @throws BookStoreException
	 */
	public void removeAllBooks() throws BookStoreException;

	/**
	 * Clean up the bookstore selectively for the list of isbns provided
	 * 
	 * @param isbnSet
	 * @throws BookStoreException
	 */
	public void removeBooks(Set<Integer> isbnSet) throws BookStoreException;

}