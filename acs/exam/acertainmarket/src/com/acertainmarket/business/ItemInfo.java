package com.acertainmarket.business;

/**
 * Info on an item which has been paired to a buyer during switchepoch
 */
public final class ItemInfo {

    /**
     * The ID of the item.
     */
    private final int itemID;

    /**
     * The description of the item.
     */
    private final String itemDescription;

    /**
     * The ID of the organization selling the item.
     */
    private final int sellerOrganizationID;

    /**
     * The ID of the buyer organization.
     */
    private final int buyerOrganizationID;

    /**
     * The amount placed for the bid.
     */
    private final float bidAmount;

    /**
     * Constructs a new item with the given information.
     */
    public ItemInfo(int itemID, String itemDescription, int sellerOrganizationID,
                int buyerOrganizationID, float bidAmount) {
        this.itemID = itemID;
        this.itemDescription = itemDescription;
        this.sellerOrganizationID = sellerOrganizationID;
        this.buyerOrganizationID = buyerOrganizationID;
        this.bidAmount = bidAmount;
    }

    /**
     * @return the itemID
     */
    public final int getItemID() {
        return itemID;
    }

    /**
     * @return the itemDescription
     */
    public final String getItemDescription() {
        return itemDescription;
    }

    /**
     * @return the sellerOrganizationID
     */
    public final int getSellerOrganizationID() {
        return sellerOrganizationID;
    }

    /**
     * @return the buyerOrganizationID
     */
    public final int getBuyerOrganizationID() {
        return buyerOrganizationID;
    }

    /**
     * @return the bidAmount
     */
    public final float getBidAmount() {
        return bidAmount;
    }

    public boolean bidEquals(Bid bid) {
        return (this.buyerOrganizationID == bid.getBuyerOrganizationID() &&
                this.itemID == bid.getItemID() && this.bidAmount == bid.getBidAmount());
    }

}
