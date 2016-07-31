package com.acertainbookstore.business;

/**
 * Simple class for giving book ratings.
 * 
 */
public class BookRating {
	private int ISBN;
	private int rating;

	/**
	 * Creates a BookRating representing the rating of the book with the given
	 * ISBN.
	 * 
	 * @return
	 */
	public BookRating(int ISBN, int rating) {
		this.ISBN = ISBN;
		this.rating = rating;
	}

	/**
	 * Returns the ISBN of the book.
	 * 
	 * @return
	 */
	public int getISBN() {
		return ISBN;
	}

	/**
	 * Sets the ISBN of the book.
	 * 
	 * @return
	 */
	public void setISBN(int iSBN) {
		ISBN = iSBN;
	}

	/**
	 * Returns the rating of the book.
	 * 
	 * @return
	 */
	public int getRating() {
		return rating;
	}

	/**
	 * Sets the rating of the book.
	 * 
	 * @return
	 */
	public void setRating(int rating) {
		this.rating = rating;
	}

	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		if ((this.getISBN() == ((BookRating) obj).getISBN())) {
			return true;
		}
		return false;
	}

	public int hashCode() {
		return getISBN();
	}
}
