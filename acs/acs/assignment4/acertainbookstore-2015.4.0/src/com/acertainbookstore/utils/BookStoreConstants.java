/**
 * 
 */
package com.acertainbookstore.utils;

/**
 * BookStoreConstants declares the constants used in the CertainBookStore (by
 * both servers and clients)
 * 
 */
public final class BookStoreConstants {

	// Constants used when creating URLs	
	public static final String BOOKISBN_PARAM = "ISBN";
	public static final String BOOK_NUM_PARAM = "number_of_books";
	public static final String XMLSTRINGLEN_PARAM = "len";
	
	// Used as error code when converting numbers to integer
	public static final int INVALID_PARAMS = -1;

	// Constants used when creating exception messages
	// When book has an invalid ISBN
	public static final String INVALID = " is invalid";
	// When book is already in the store
	public static final String DUPLICATED = " is duplicated";
	// When book is not in the store
	public static final String NOT_AVAILABLE = " is not available";
	public static final String BOOK = "The Book: ";
	public static final String ISBN = "The ISBN: ";
	public static final String NUM_COPIES = "The Number of copies: ";
	public static final String RATING = "The rating: ";
	public static final String NULL_INPUT = "null input parameters";

	public static final String PROPERTY_KEY_LOCAL_TEST = "localtest";
	public static final String PROPERTY_KEY_SERVER_PORT = "port";
}
