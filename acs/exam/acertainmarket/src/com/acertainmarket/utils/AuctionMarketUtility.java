/**
 *
 */
package com.acertainmarket.utils;

import com.acertainmarket.client.AuctionMarketClientConstants;
import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.StaxDriver;
import org.eclipse.jetty.client.ContentExchange;
import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.client.HttpExchange;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.List;

/**
 * AuctionMarketUtility implements utility methods used by AuctionMarket servers and
 * clients
 *
 */
public final class AuctionMarketUtility {

	public static boolean isInvalidID(int id) {return (id < 0);
	}

	/**
	 * Checks if a string is empty or null
	 *
	 * @param str
	 * @return
	 */
	public static boolean isEmpty(String str) {
		return ((str == null) || str.isEmpty());
	}

	/**
	 * Converts a string to a float if possible else it returns the signal value
	 * for failure passed as parameter
	 *
	 * @param str
	 * @param failureSignal
	 * @return
	 */
	public static float convertStringToFloat(String str, float failureSignal) {
		float returnValue = failureSignal;
		try {
			returnValue = Float.parseFloat(str);

		} catch (NumberFormatException ex) {
			;
		} catch (NullPointerException ex) {
			;
		}
		return returnValue;
	}

	/**
	 * Converts a string to a int if possible else it returns the signal value
	 * for failure passed as parameter
	 *
	 * @param str
	 * @return
	 */
	public static int convertStringToInt(String str) throws AuctionMarketException {
		int returnValue = 0;
		try {
			returnValue = Integer.parseInt(str);
		} catch (Exception ex) {
			throw new AuctionMarketException(ex);
		}
		return returnValue;
	}

	/**
	 * Convert a request URI to the message tags supported in ConcurrentCertainMarket
	 *
	 * @param requestURI
	 * @return
	 */
	public static AuctionMarketMessageTag convertURItoMessageTag(String requestURI) {

		try {
			AuctionMarketMessageTag messageTag = AuctionMarketMessageTag
					.valueOf(requestURI.substring(1).toUpperCase());
			return messageTag;
		} catch (IllegalArgumentException ex) {
			; // Enum type matching failed so non supported message
		} catch (NullPointerException ex) {
			; // RequestURI was empty
		}
		return null;
	}

	/**
	 * Serializes an object to an xml string
	 *
	 * @param object
	 * @return
	 */
	public static String serializeObjectToXMLString(Object object) {
		String xmlString;
		XStream xmlStream = new XStream(new StaxDriver());
		xmlString = xmlStream.toXML(object);
		return xmlString;
	}

	/**
	 * De-serializes an xml string to object
	 *
	 * @param xmlObject
	 * @return
	 */
	public static Object deserializeXMLStringToObject(String xmlObject) {
		Object dataObject = null;
		XStream xmlStream = new XStream(new StaxDriver());
		dataObject = xmlStream.fromXML(xmlObject);
		return dataObject;
	}


	/**
	 * Manages the sending of an exchange through the client, waits for the
	 * response and unpacks the response
	 *
	 * @param client
	 * @param exchange
	 * @return A List<Item> for a get function, otherwise null
	 * @throws AuctionMarketException
	 */
	public static List<?> SendAndRecv(HttpClient client,
									  ContentExchange exchange) throws AuctionMarketException {
		int exchangeState;
		try {
			client.send(exchange);
		} catch (IOException ex) {
			throw new AuctionMarketException(
					AuctionMarketClientConstants.strERR_CLIENT_REQUEST_SENDING, ex);
		}

		try {
			exchangeState = exchange.waitForDone(); // block until the response
			// is available
		} catch (InterruptedException ex) {
			throw new AuctionMarketException(
					AuctionMarketClientConstants.strERR_CLIENT_REQUEST_SENDING, ex);
		}

		if (exchangeState == HttpExchange.STATUS_COMPLETED) {
			try {
				AuctionMarketResponse AuctionMarketResponse = (AuctionMarketResponse) AuctionMarketUtility
						.deserializeXMLStringToObject(exchange
								.getResponseContent().trim());
				AuctionMarketException ex = AuctionMarketResponse.getException();
				if (ex != null) {
					throw ex;
				}
				return AuctionMarketResponse.getList();

			} catch (UnsupportedEncodingException ex) {
				throw new AuctionMarketException(
						AuctionMarketClientConstants.strERR_CLIENT_RESPONSE_DECODING,
						ex);
			}
		} else if (exchangeState == HttpExchange.STATUS_EXCEPTED) {
			throw new AuctionMarketException(
					AuctionMarketClientConstants.strERR_CLIENT_REQUEST_EXCEPTION);
		} else if (exchangeState == HttpExchange.STATUS_EXPIRED) {
			throw new AuctionMarketException(
					AuctionMarketClientConstants.strERR_CLIENT_REQUEST_TIMEOUT);
		} else {
			throw new AuctionMarketException(
					AuctionMarketClientConstants.strERR_CLIENT_UNKNOWN);
		}
	}

	/**
	 * Returns the message of the request as a string
	 *
	 * @param request
	 * @return xml string
	 * @throws IOException
	 */
	public static String extractPOSTDataFromRequest(HttpServletRequest request)
			throws IOException {
		Reader reader = request.getReader();
		int len = request.getContentLength();

		// Request must be read into a char[] first
		char res[] = new char[len];
		reader.read(res);
		reader.close();
		return new String(res);
	}
}
