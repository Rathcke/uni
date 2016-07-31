/**
 * 
 */
package com.acertainbookstore.client;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;
import java.util.Set;

import org.eclipse.jetty.client.ContentExchange;
import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.io.Buffer;
import org.eclipse.jetty.io.ByteArrayBuffer;
import org.eclipse.jetty.util.thread.QueuedThreadPool;

import com.acertainbookstore.business.Book;
import com.acertainbookstore.business.BookCopy;
import com.acertainbookstore.business.BookRating;
import com.acertainbookstore.interfaces.BookStore;
import com.acertainbookstore.utils.BookStoreConstants;
import com.acertainbookstore.utils.BookStoreException;
import com.acertainbookstore.utils.BookStoreMessageTag;
import com.acertainbookstore.utils.BookStoreUtility;

/**
 * BookStoreHTTPProxy implements the client level synchronous CertainBookStore
 * API declared in the BookStore class
 * 
 */
public class BookStoreHTTPProxy implements BookStore {
	protected HttpClient client;
	protected String serverAddress;

	/**
	 * Initialize the client object
	 */
	public BookStoreHTTPProxy(String serverAddress) throws Exception {
		setServerAddress(serverAddress);
		client = new HttpClient();
		client.setConnectorType(HttpClient.CONNECTOR_SELECT_CHANNEL);
		client.setMaxConnectionsPerAddress(BookStoreClientConstants.CLIENT_MAX_CONNECTION_ADDRESS); // max
																									// concurrent
																									// connections
																									// to
																									// every
																									// address
		client.setThreadPool(new QueuedThreadPool(
				BookStoreClientConstants.CLIENT_MAX_THREADSPOOL_THREADS)); // max
																			// threads
		client.setTimeout(BookStoreClientConstants.CLIENT_MAX_TIMEOUT_MILLISECS); // seconds
																					// timeout;
																					// if
																					// no
																					// server
																					// reply,
																					// the
																					// request
																					// expires
		client.start();
	}

	public String getServerAddress() {
		return serverAddress;
	}

	public void setServerAddress(String serverAddress) {
		this.serverAddress = serverAddress;
	}

	public void buyBooks(Set<BookCopy> isbnSet) throws BookStoreException {
		ContentExchange exchange = new ContentExchange();
		String urlString = serverAddress + "/" + BookStoreMessageTag.BUYBOOKS;

		String listISBNsxmlString = BookStoreUtility
				.serializeObjectToXMLString(isbnSet);
		exchange.setMethod("POST");
		exchange.setURL(urlString);
		Buffer requestContent = new ByteArrayBuffer(listISBNsxmlString);
		exchange.setRequestContent(requestContent);

		BookStoreUtility.SendAndRecv(this.client, exchange);

	}

	@SuppressWarnings("unchecked")
	public List<Book> getBooks(Set<Integer> isbnSet) throws BookStoreException {
		ContentExchange exchange = new ContentExchange();
		String urlString = serverAddress + "/" + BookStoreMessageTag.GETBOOKS;

		String listISBNsxmlString = BookStoreUtility
				.serializeObjectToXMLString(isbnSet);
		exchange.setMethod("POST");
		exchange.setURL(urlString);
		Buffer requestContent = new ByteArrayBuffer(listISBNsxmlString);
		exchange.setRequestContent(requestContent);

		return (List<Book>) BookStoreUtility.SendAndRecv(this.client, exchange);
	}

	@SuppressWarnings("unchecked")
	public List<Book> getEditorPicks(int numBooks) throws BookStoreException {
		ContentExchange exchange = new ContentExchange();
		String urlEncodedNumBooks = null;

		try {
			urlEncodedNumBooks = URLEncoder.encode(Integer.toString(numBooks),
					"UTF-8");
		} catch (UnsupportedEncodingException ex) {
			throw new BookStoreException("unsupported encoding of numbooks", ex);
		}

		String urlString = serverAddress + "/"
				+ BookStoreMessageTag.EDITORPICKS + "?"
				+ BookStoreConstants.BOOK_NUM_PARAM + "=" + urlEncodedNumBooks;

		exchange.setURL(urlString);

		return (List<Book>) BookStoreUtility.SendAndRecv(this.client, exchange);
	}

	public void stop() {
		try {
			client.stop();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void rateBooks(Set<BookRating> bookRating) throws BookStoreException {
		// TODO Auto-generated method stub
		throw new BookStoreException("Not implemented");
		
	}

	@Override
	public List<Book> getTopRatedBooks(int numBooks) throws BookStoreException {
		// TODO Auto-generated method stub
		throw new BookStoreException("Not implemented");
	}

}
