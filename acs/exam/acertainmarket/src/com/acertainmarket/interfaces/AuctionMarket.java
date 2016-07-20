package com.acertainmarket.interfaces;

import com.acertainmarket.business.Bid;
import com.acertainmarket.business.Item;
import com.acertainmarket.utils.AuctionMarketException;

import java.util.List;
import java.util.Set;

/**
 * The AuctionMarket interface defines the operations of an epoch-based market
 * abstraction. The auction market manages items made available for trading by
 * seller organizations, records or updates bids for items from buyer
 * organizations, and matches bids to items across epochs.
 * 
 * @author vmarcos
 */
public interface AuctionMarket {

	/**
	 * Adds a set of items for trading in the auction market.
	 * 
	 * @param items
	 *            - the set of items to be added.
	 */
	public void addItems(Set<Item> items) throws AuctionMarketException;

	/**
	 * Returns all items currently available for trading in the auction market.
	 * 
	 * @return the items in the market as a list.
	 */
	public List<Item> queryItems() throws AuctionMarketException;

	/**
	 * Adds a set of bids for items to the auction market. Since each buyer
	 * organization can only have one active bid for a given item, each bid in
	 * the set is either recorded, in case no previous bid has been made by the
	 * organization to the item, or the bid replaces the previous bid for the
	 * given organization to the given item.
	 * 
	 * @param bids
	 *            - the set of bids to be recorded or updated.
	 */
	public void bid(Set<Bid> bids) throws AuctionMarketException;

	/**
	 * Advances the epoch of the auction market, performing all necessary
	 * end-of-epoch activities. These activities include calculating the
	 * matching bids for each item, making the matching results durable, and
	 * resetting the market state for the start of a new epoch.
	 */
	public void switchEpoch() throws AuctionMarketException;

}
