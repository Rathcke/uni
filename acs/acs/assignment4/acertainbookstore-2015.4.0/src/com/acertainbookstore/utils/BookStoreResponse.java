package com.acertainbookstore.utils;

import java.util.List;

import com.acertainbookstore.business.Book;

/**
 * Data Structure that we use to communicate objects and error messages
 * from the server to the client.
 * 
 */
public class BookStoreResponse {
	private BookStoreException exception;
	private List<?> list;

	public BookStoreException getException() {
		return exception;
	}

	public void setException(BookStoreException exception) {
		this.exception = exception;
	}

	public BookStoreResponse(BookStoreException exception, List<Book> list) {
		this.setException(exception);
		this.setList(list);
	}

	public BookStoreResponse() {
		this.setException(null);
		this.setList(null);
	}

	public List<?> getList() {
		return list;
	}

	public void setList(List<?> list) {
		this.list = list;
	}
}
