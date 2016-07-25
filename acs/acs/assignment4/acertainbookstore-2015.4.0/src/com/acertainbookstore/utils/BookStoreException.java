package com.acertainbookstore.utils;

/**
 * Exception to signal a book store error
 */
public class BookStoreException extends Exception {
	private static final long serialVersionUID = 1L;

	public BookStoreException() {
		super();
	}

	public BookStoreException(String message) {
		super(message);
	}

	public BookStoreException(String message, Throwable cause) {
		super(message, cause);
	}

	public BookStoreException(Throwable ex) {
		super(ex);
	}
}
