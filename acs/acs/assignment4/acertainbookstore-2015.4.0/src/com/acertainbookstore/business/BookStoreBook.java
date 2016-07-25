package com.acertainbookstore.business;

import com.acertainbookstore.utils.BookStoreUtility;

/**
 * The implementation of all parts of the book. Only parts of it are available
 * in the BookStoreClient and StockManager, cf. the Book interface and the
 * StockBook interface.
 * 
 */
public class BookStoreBook extends ImmutableBook {
	private int numCopies;
	private long totalRating;
	private long timesRated;
	private long saleMisses;
	private boolean editorPick;

	/**
	 * Constructor to create a book object
	 */
	public BookStoreBook(int ISBN, String title, String author, float price,
			int numCopies) {
		super(ISBN, title, author, price);
		this.setSaleMisses(0);
		this.setTimesRated(0);
		this.setNumCopies(numCopies);
		this.setTotalRating(0);
		this.setEditorPick(false);
	}

	/**
	 * Constructor to create a book store book object from a stock book object
	 * 
	 * @param bookToCopy
	 */
	public BookStoreBook(StockBook bookToCopy) {
		super(bookToCopy.getISBN(), bookToCopy.getTitle(), bookToCopy
				.getAuthor(), bookToCopy.getPrice());
		this.setSaleMisses(bookToCopy.getSaleMisses());
		this.setTimesRated(bookToCopy.getTimesRated());
		this.setNumCopies(bookToCopy.getNumCopies());
		this.setTotalRating(bookToCopy.getTotalRating());
		this.setEditorPick(bookToCopy.isEditorPick());
	}

	public long getTotalRating() {
		return totalRating;
	}

	public long getTimesRated() {
		return timesRated;
	}

	public int getNumCopies() {
		return numCopies;
	}

	public long getSaleMisses() {
		return saleMisses;
	}

	public float getAverageRating() {
		return (float) (timesRated == 0 ? -1.0 : totalRating / timesRated);
	}

	public boolean isEditorPick() {
		return editorPick;
	}

	/**
	 * Sets the total rating of the book.
	 * 
	 * @param totalRating
	 */
	private void setTotalRating(long totalRating) {
		this.totalRating = totalRating;
	}

	/**
	 * Sets the number of times that a book was rated.
	 * 
	 * @param timesRated
	 */
	private void setTimesRated(long timesRated) {
		this.timesRated = timesRated;
	}

	/**
	 * Sets the number of copies of a book, that is in stock.
	 * 
	 * @param numCopies
	 */
	private void setNumCopies(int numCopies) {
		this.numCopies = numCopies;
	}

	/**
	 * Sets the number of times that a client wanted to buy a book when it was
	 * not in stock.
	 * 
	 * @param saleMisses
	 */
	private void setSaleMisses(long saleMisses) {
		this.saleMisses = saleMisses;
	}

	/**
	 * Sets the book to be an editor pick if the boolean is true, otherwise the
	 * book is not an editor pick.
	 * 
	 * @param editorPick
	 */
	public void setEditorPick(boolean editorPick) {
		this.editorPick = editorPick;
	}

	/**
	 * Checks if numCopies of the book are available
	 * 
	 * @param numCopies
	 * @return
	 */
	public boolean areCopiesInStore(int numCopies) {
		return (this.numCopies >= numCopies);
	}

	/**
	 * Reduces the number of copies of the books
	 * 
	 * @param numCopies
	 * @return
	 */
	public boolean buyCopies(int numCopies) {
		if (!BookStoreUtility.isInvalidNoCopies(numCopies))
			if (areCopiesInStore(numCopies)) {
				this.numCopies -= numCopies;
				return true;
			}
		return false;
	}

	/**
	 * Adds newCopies to the total number of copies of the book.
	 */
	public void addCopies(int newCopies) {
		if (!BookStoreUtility.isInvalidNoCopies(newCopies)) {
			this.numCopies += newCopies;
			this.saleMisses = 0;
		}
	}

	/**
	 * Increases the amount of missed sales of the book.
	 */
	public void addSaleMiss() {
		this.saleMisses++;
	}

	/**
	 * Adds the rating to the total rating of the book.
	 * 
	 * @param rating
	 */
	public void addRating(int rating) {
		if (!BookStoreUtility.isInvalidRating(rating)) {
			this.totalRating += rating;
			this.timesRated++;
		}
	}

	/**
	 * Returns True if someone tried to buy the book, while the book was not in
	 * stock.
	 * 
	 * @return
	 */
	public boolean hadSaleMiss() {
		return this.saleMisses > 0;
	}

	/**
	 * Returns a string representation of the book.
	 */
	public String toString() {
		String bookString = "ISBN = " + this.getISBN() + ", Title = "
				+ this.getTitle() + ", Author = " + this.getAuthor()
				+ ", Price = " + this.getPrice();
		return bookString;
	}

	/**
	 * Returns a ImmutableBook copy of the book.
	 * 
	 * @return
	 */
	public ImmutableBook immutableBook() {
		return new ImmutableBook(this.getISBN(), new String(this.getTitle()),
				new String(this.getAuthor()), this.getPrice());
	}

	/**
	 * Returns a ImmutableStockBook copy of the book.
	 * 
	 * @return
	 */
	public StockBook immutableStockBook() {
		return new ImmutableStockBook(this.getISBN(), new String(
				this.getTitle()), new String(this.getAuthor()),
				this.getPrice(), this.numCopies, this.saleMisses,
				this.timesRated, this.totalRating, this.editorPick);
	}

	/**
	 * Returns a copy of the book.
	 * 
	 * @return
	 */
	public BookStoreBook copy() {
		return new BookStoreBook(this.getISBN(), new String(this.getTitle()),
				new String(this.getAuthor()), this.getPrice(), this.numCopies);
	}

}
