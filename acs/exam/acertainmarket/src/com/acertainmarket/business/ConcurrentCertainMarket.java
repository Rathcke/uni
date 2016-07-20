package com.acertainmarket.business;

import com.acertainmarket.interfaces.AuctionMarket;
import com.acertainmarket.utils.AuctionMarketConstants;
import com.acertainmarket.utils.AuctionMarketException;
import com.acertainmarket.utils.AuctionMarketUtility;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class ConcurrentCertainMarket implements AuctionMarket {
    private Map<Integer, Item> itemMap = null;
    private Map<Integer, List<Bid>> bidMap = null;
    private ReadWriteLock bidLock, itemLock;

    public ConcurrentCertainMarket() {
        itemMap = new ConcurrentHashMap<Integer, Item>();
        bidMap = new ConcurrentHashMap<Integer, List<Bid>>();
        bidLock = new ReentrantReadWriteLock();
        itemLock = new ReentrantReadWriteLock();
    }

    public void addItems(Set<Item> items)
            throws AuctionMarketException {

        if (items == null) {
            throw new AuctionMarketException(AuctionMarketConstants.NULL_INPUT);
        }

        itemLock.writeLock().lock();
        // Check if all items are valid
        for (Item item : items) {

            int id = item.getItemID();
            int sellID = item.getSellerOrganizationID();

            if (id < 0 || sellID < 0) {
                itemLock.writeLock().unlock();
                throw new AuctionMarketException(AuctionMarketConstants.ITEM
                        + item.toString() + AuctionMarketConstants.INVALID);
            } else if (itemMap.containsKey(id)) {
                itemLock.writeLock().unlock();
                throw new AuctionMarketException(AuctionMarketConstants.ID + id
                        + AuctionMarketConstants.DUPLICATED);
            }
        }

        for (Item item : items) {
            int id = item.getItemID();
            String desc = item.getItemDescription();
            int sellID = item.getSellerOrganizationID();
            itemMap.put(id, new Item(id, desc, sellID));
        }
        itemLock.writeLock().unlock();
    }

    public List<Item> queryItems() {

        itemLock.readLock().lock();
        List<Item> listItems = new ArrayList<Item>();
        Collection<Item> itemMapValues = itemMap.values();
        for (Item item : itemMapValues) {
            listItems.add(item);
        }
        itemLock.readLock().unlock();
        return listItems;
    }

    public void bid(Set<Bid> bids) throws AuctionMarketException {

        if (bids == null) {
            throw new AuctionMarketException(AuctionMarketConstants.NULL_INPUT);
        }
        bidLock.writeLock().lock();
        itemLock.readLock().lock();

        // Check if all bids are valid
        for (Bid bid : bids) {

            int id = bid.getItemID();
            float amount = bid.getBidAmount();
            int buyID = bid.getBuyerOrganizationID();

            if (id < 0 || buyID < 0 || amount < 0 || (!itemMap.containsKey(id))) {
                bidLock.writeLock().unlock();
                itemLock.readLock().unlock();
                throw new AuctionMarketException(AuctionMarketConstants.ITEM
                        + bid.toString() + AuctionMarketConstants.INVALID);
            }
        }

        for (Bid bid : bids) {
            boolean didExist = false;
            int id = bid.getItemID();
            int buyID = bid.getBuyerOrganizationID();
            float amount = bid.getBidAmount();

            if (bidMap.containsKey(id)) {
                List<Bid> bidList = bidMap.get(id);
                for (Bid existingBid : bidList) {
                    if (existingBid.getBuyerOrganizationID() == buyID) {
                        didExist = true;
                        Bid replaceBid = new Bid(buyID, id, amount);
                        bidList.remove(existingBid);
                        bidList.add(replaceBid);
                        bidMap.put(id, bidList);
                        break;
                    }
                }
                if (!didExist) {
                    bidList.add(new Bid(buyID, id, amount));
                    bidMap.put(id, bidList);
                }
            } else {
                List<Bid> bidList = new ArrayList<Bid>();
                bidList.add(bid);
                bidMap.put(id, bidList);
            }
        }
        bidLock.writeLock().unlock();
        itemLock.readLock().unlock();
    }

    public void switchEpoch() throws AuctionMarketException {

        bidLock.writeLock().lock();
        itemLock.writeLock().lock();
        List<ItemInfo> itemAndBids = new ArrayList<ItemInfo>();
        Collection<Item> items = itemMap.values();

        for (Item item : items) {
            Bid bestBid = null;
            if (bidMap.containsKey(item.getItemID())) {
                List<Bid> bidList = bidMap.get(item.getItemID());
                for (Bid bid : bidList) {
                    if (bestBid == null || bid.getBidAmount() > bestBid.getBidAmount()) {
                        bestBid = bid;
                    }
                }
                if (!(bestBid == null)) {
                    ItemInfo itemInfo = new ItemInfo(item.getItemID(), item.getItemDescription(),
                            item.getSellerOrganizationID(), bestBid.getBuyerOrganizationID(), bestBid.getBidAmount());
                    itemAndBids.add(itemInfo);
                }
            }
        }

        AuctionMarketUtility aum = new AuctionMarketUtility();
        String output = aum.serializeObjectToXMLString(itemAndBids);

        PrintWriter writer = null;
        try {
            writer = new PrintWriter("out.xml", "UTF-8");
            writer.println(output);
            writer.close();
            bidMap.clear();
            itemMap.clear();
        } catch (FileNotFoundException e) {
            bidLock.writeLock().unlock();
            itemLock.writeLock().unlock();
            throw new AuctionMarketException(e);
        } catch (UnsupportedEncodingException e) {
            bidLock.writeLock().unlock();
            itemLock.writeLock().unlock();
            throw new AuctionMarketException(e);
        }

        bidLock.writeLock().unlock();
        itemLock.writeLock().unlock();
    }

}