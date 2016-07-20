package com.acertainmarket.business;

/**
 * A bid is an offer from a buyer organization to acquire a given item for a
 * given amount. A bid corresponds to a buyer organization identifier, an item
 * identifier, and a bid amount. For example, buyer organization 21 can place a
 * bid for item 42 for an amount of 1680.0. A bid is identified by the
 * combination of buyer organization ID and item ID.
 * 
 * @author vmarcos
 */
public final class Bid {

	/**
	 * The ID of the buyer organization.
	 */
	private final int buyerOrganizationID;

	/**
	 * The ID of the item.
	 */
	private final int itemID;

	/**
	 * The amount placed for the bid.
	 */
	private final float bidAmount;

	/**
	 * Constructs a new bid with the given information.
	 * 
	 * @param buyerOrganizationID
	 * @param itemID
	 * @param bidAmount
	 */
	public Bid(int buyerOrganizationID, int itemID, float bidAmount) {
		this.buyerOrganizationID = buyerOrganizationID;
		this.itemID = itemID;
		this.bidAmount = bidAmount;
	}

	/**
	 * @return the buyerOrganizationID
	 */
	public final int getBuyerOrganizationID() {
		return buyerOrganizationID;
	}

	/**
	 * @return the itemID
	 */
	public final int getItemID() {
		return itemID;
	}

	/**
	 * @return the bidAmount
	 */
	public final float getBidAmount() {
		return bidAmount;
	}

}
