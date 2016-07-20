/**
 * 
 */
package com.acertainmarket.server;

import com.acertainmarket.business.Bid;
import com.acertainmarket.business.ConcurrentCertainMarket;
import com.acertainmarket.business.Item;
import com.acertainmarket.utils.AuctionMarketException;
import com.acertainmarket.utils.AuctionMarketMessageTag;
import com.acertainmarket.utils.AuctionMarketResponse;
import com.acertainmarket.utils.AuctionMarketUtility;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Set;

/**
 * AuctionMarketHTTPMessageHandler implements the message handler class which is
 * invoked to handle messages received by the AuctionMarketHTTPServerUtility. It
 * decodes the HTTP message and invokes the ConcurrentCertainMarket server API
 * 
 * 
 */
public class AuctionMarketHTTPMessageHandler extends AbstractHandler {

	private ConcurrentCertainMarket myAuctionMarket = null;
	
	public AuctionMarketHTTPMessageHandler(ConcurrentCertainMarket auctionMarket) {

		myAuctionMarket = auctionMarket;
	}

	@SuppressWarnings("unchecked")
	public void handle(String target, Request baseRequest,
					   HttpServletRequest request, HttpServletResponse response)
			throws IOException, ServletException {
		AuctionMarketMessageTag messageTag;
		String requestURI;
		AuctionMarketResponse auctionMarketResponse = null;

		response.setContentType("text/html;charset=utf-8");
		response.setStatus(HttpServletResponse.SC_OK);
		requestURI = request.getRequestURI();

		// Need to do request multi-plexing
		if (!AuctionMarketUtility.isEmpty(requestURI)
				&& requestURI.toLowerCase().startsWith("/stock")) {
			messageTag = AuctionMarketUtility.convertURItoMessageTag(requestURI
					.substring(6)); // the request is from store
			// manager, more
			// sophisticated security
			// features could be added
			// here
		} else {
			messageTag = AuctionMarketUtility.convertURItoMessageTag(requestURI);
		}
		// the RequestURI before the switch
		if (messageTag == null) {
			System.out.println("Unknown message tag");
		} else {
			switch (messageTag) {
			case ADDITEMS:
				String xml = AuctionMarketUtility.extractPOSTDataFromRequest(request);

				Set<Item> newItems = (Set<Item>) AuctionMarketUtility
						.deserializeXMLStringToObject(xml);

				auctionMarketResponse = new AuctionMarketResponse();
				try {
					myAuctionMarket.addItems(newItems);
				} catch (AuctionMarketException ex) {
					auctionMarketResponse.setException(ex);
				}
				String listItemsxmlString = AuctionMarketUtility
						.serializeObjectToXMLString(auctionMarketResponse);
				response.getWriter().println(listItemsxmlString);
				break;

            case QUERYITEMS:
				auctionMarketResponse = new AuctionMarketResponse();
				auctionMarketResponse.setList(myAuctionMarket.queryItems());
				listItemsxmlString = AuctionMarketUtility
						.serializeObjectToXMLString(auctionMarketResponse);
				response.getWriter().println(listItemsxmlString);
				break;

            case BID:
                xml = AuctionMarketUtility.extractPOSTDataFromRequest(request);

                Set<Bid> newBids = (Set<Bid>) AuctionMarketUtility
                        .deserializeXMLStringToObject(xml);

                auctionMarketResponse = new AuctionMarketResponse();
                try {
                    myAuctionMarket.bid(newBids);
                } catch (AuctionMarketException ex) {
                    auctionMarketResponse.setException(ex);
                }
                listItemsxmlString = AuctionMarketUtility
                        .serializeObjectToXMLString(auctionMarketResponse);
                response.getWriter().println(listItemsxmlString);
                break;

            case SWITCHEPOCH:
                auctionMarketResponse = new AuctionMarketResponse();
                try {
                    myAuctionMarket.switchEpoch();
                } catch (AuctionMarketException ex) {
                    auctionMarketResponse.setException(ex);
                }
                listItemsxmlString = AuctionMarketUtility
                        .serializeObjectToXMLString(auctionMarketResponse);
                response.getWriter().println(listItemsxmlString);
                break;

			default:
				System.out.println("Unhandled message tag");
				break;
			}
		}
		// Mark the request as handled so that the HTTP response can be sent
		baseRequest.setHandled(true);

	}
}
