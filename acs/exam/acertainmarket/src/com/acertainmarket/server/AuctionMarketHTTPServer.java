/**
 * 
 */
package com.acertainmarket.server;

import com.acertainmarket.business.ConcurrentCertainMarket;
import com.acertainmarket.utils.AuctionMarketConstants;

/**
 * Starts the auction market HTTP server that the clients will communicate with.
 */
public class AuctionMarketHTTPServer {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		ConcurrentCertainMarket auctionMarket = new ConcurrentCertainMarket();
		int listen_on_port = 8081;
		AuctionMarketHTTPMessageHandler handler = new AuctionMarketHTTPMessageHandler(
				auctionMarket);
		String server_port_string = System.getProperty(AuctionMarketConstants.PROPERTY_KEY_SERVER_PORT);
		if(server_port_string != null) {
			try {
				listen_on_port = Integer.parseInt(server_port_string);
			} catch(NumberFormatException ex) {
				System.err.println(ex);
			}
		}
		if (AuctionMarketHTTPServerUtility.createServer(listen_on_port, handler)) {
			;
		}
	}

}
