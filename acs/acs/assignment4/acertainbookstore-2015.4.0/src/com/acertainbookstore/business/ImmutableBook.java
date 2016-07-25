package com.acertainbookstore.business;

/**
 * The book object that gets sent back to the BookStoreClient. We do not allow
 * the BookStoreClient to make any changes to the data structure. See the Book
 * interface for comments.
 * 
 */
public class ImmutableBook implements Book {
	private final int ISBN;
	private final String title;
	private final String author;
	private final float price;

	/**
	 * Creates an immutable book.
	 * 
	 * @param ISBN
	 * @param title
	 * @param author
	 * @param price
	 */
	public ImmutableBook(int ISBN, String title, String author, float price) {
		this.ISBN = ISBN;
		this.title = title;
		this.author = author;
		this.price = price;
	}

	public int getISBN() {
		return ISBN;
	}

	public String getTitle() {
		return title;
	}

	public String getAuthor() {
		return author;
	}

	public float getPrice() {
		return price;
	}

	public boolean equals(Object obj) {
		if (!(obj instanceof Book))
			return false;
		Book book = (Book) obj;
		return (this.ISBN == book.getISBN()
				&& this.getTitle().equals(book.getTitle())
				&& this.getAuthor().equals(book.getAuthor()) && this.getPrice() == book
				.getPrice());
	}

	public String toString() {
		String bookString = "ISBN = " + this.getISBN() + " Title = "
				+ this.getTitle() + " Author = " + this.getAuthor()
				+ " Price = " + this.getPrice();
		return bookString;
	}

	public int hashCode() {
		return getISBN();
	}

}
