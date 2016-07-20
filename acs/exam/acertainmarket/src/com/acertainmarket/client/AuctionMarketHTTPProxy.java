/**
 * 
 */
package com.acertainmarket.client;

import com.acertainmarket.business.Bid;
import com.acertainmarket.business.Item;
import com.acertainmarket.interfaces.AuctionMarket;
import com.acertainmarket.utils.AuctionMarketException;
import com.acertainmarket.utils.AuctionMarketMessageTag;
import com.acertainmarket.utils.AuctionMarketUtility;
import org.eclipse.jetty.client.ContentExchange;
import org.eclipse.jetty.client.HttpClient;
import org.eclipse.jetty.io.Buffer;
import org.eclipse.jetty.io.ByteArrayBuffer;
import org.eclipse.jetty.util.thread.QueuedThreadPool;

import java.util.List;
import java.util.Set;

/**
 * AuctionMarketHTTPProxy implements the client level synchronous ConcurrentCertainMarket
 * API declared in the AuctionMarket class
 * 
 */
public class AuctionMarketHTTPProxy implements AuctionMarket {
	protected HttpClient client;
	protected String serverAddress;

	/**
	 * Initialize the client object
	 */
	public AuctionMarketHTTPProxy(String serverAddress) throws Exception {
		setServerAddress(serverAddress);
		client = new HttpClient();
		client.setConnectorType(HttpClient.CONNECTOR_SELECT_CHANNEL);
		client.setMaxConnectionsPerAddress(AuctionMarketClientConstants.CLIENT_MAX_CONNECTION_ADDRESS);
		client.setThreadPool(new QueuedThreadPool(
				AuctionMarketClientConstants.CLIENT_MAX_THREADSPOOL_THREADS));
		client.setTimeout(AuctionMarketClientConstants.CLIENT_MAX_TIMEOUT_MILLISECS);
		client.start();
	}

	public String getServerAddress() {
		return serverAddress;
	}

	public void setServerAddress(String serverAddress) {
		this.serverAddress = serverAddress;
	}

	public void addItems(Set<Item> items) throws AuctionMarketException {
		ContentExchange exchange = new ContentExchange();
		String urlString = serverAddress + "/" + AuctionMarketMessageTag.ADDITEMS;

		String listISBNsxmlString = AuctionMarketUtility
				.serializeObjectToXMLString(items);
		exchange.setMethod("POST");
		exchange.setURL(urlString);
		Buffer requestContent = new ByteArrayBuffer(listISBNsxmlString);
		exchange.setRequestContent(requestContent);

		AuctionMarketUtility.SendAndRecv(this.client, exchange);
	}


	@SuppressWarnings("unchecked")
	public List<Item> queryItems() throws AuctionMarketException {
        ContentExchange exchange = new ContentExchange();
        String urlString = serverAddress + "/" + AuctionMarketMessageTag.QUERYITEMS;

        exchange.setURL(urlString);

        return (List<Item>) AuctionMarketUtility.SendAndRecv(this.client, exchange);
    }

	public void bid(Set<Bid> bids) throws AuctionMarketException {
		ContentExchange exchange = new ContentExchange();
		String urlString = serverAddress + "/" + AuctionMarketMessageTag.BID;

		String listISBNsxmlString = AuctionMarketUtility
				.serializeObjectToXMLString(bids);
		exchange.setMethod("POST");
		exchange.setURL(urlString);
		Buffer requestContent = new ByteArrayBuffer(listISBNsxmlString);
		exchange.setRequestContent(requestContent);

		AuctionMarketUtility.SendAndRecv(this.client, exchange);
	}

    public void switchEpoch() throws AuctionMarketException {
        ContentExchange exchange = new ContentExchange();
        String urlString = serverAddress + "/" + AuctionMarketMessageTag.SWITCHEPOCH;

        exchange.setMethod("POST");
        exchange.setURL(urlString);

        AuctionMarketUtility.SendAndRecv(this.client, exchange);
    }

	public void stop() {
		try {
			client.stop();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


}
