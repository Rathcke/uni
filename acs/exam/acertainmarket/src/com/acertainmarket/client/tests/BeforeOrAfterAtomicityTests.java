package com.acertainmarket.client.tests;

import com.acertainmarket.business.ConcurrentCertainMarket;
import com.acertainmarket.business.Item;
import com.acertainmarket.client.AuctionMarketHTTPProxy;
import com.acertainmarket.interfaces.AuctionMarket;
import com.acertainmarket.utils.AuctionMarketConstants;
import com.acertainmarket.utils.AuctionMarketException;
import org.junit.*;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertTrue;

/**
 * Test class to test the AuctionMarket interface
 *
 */
public class BeforeOrAfterAtomicityTests {

    private static final int RUNS = 1000;
    private static boolean localTest = true;
    private static AuctionMarket client;

    @BeforeClass
    public static void setUpBeforeClass() throws AuctionMarketException {
        try {
            String localTestProperty = System
                    .getProperty(AuctionMarketConstants.PROPERTY_KEY_LOCAL_TEST);
            localTest = (localTestProperty != null) ? Boolean
                    .parseBoolean(localTestProperty) : localTest;
            if (localTest) {
                ConcurrentCertainMarket market = new ConcurrentCertainMarket();
                client = market;

            } else {
                client = new AuctionMarketHTTPProxy("http://localhost:8081");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Tests that two clients, one who continuously adds a set of 5 items and another that
     * keeps invoking queryItems(). Every time queryItems() should return a list that has
     * a size of 0 when modulo 5.
     *
     * @throws AuctionMarketException
     */
    @Test
    public void testBeforeOrAfterAddAndQuery() throws AuctionMarketException {

        final int runs = RUNS;

        Thread threadAddItems = new Thread(() -> {
            try {
                for (int i = 0; i < runs; i++) {
                    Set<Item> itemsToAdd = new HashSet<Item>();
                    itemsToAdd.add(new Item(i*5, "validItem", 9));
                    itemsToAdd.add(new Item(i*5+1, "validItem", 9));
                    itemsToAdd.add(new Item(i*5+2, "validItem", 9));
                    itemsToAdd.add(new Item(i*5+3, "validItem", 9));
                    itemsToAdd.add(new Item(i*5+4, "validItem", 9));
                    client.addItems(itemsToAdd);
                }
            } catch (AuctionMarketException e) {
                e.printStackTrace();
            }
        });

        Thread threadQueryItems = new Thread(() -> {
            try {
                for (int i = 0; i < runs; i++) {
                    List<Item> items = client.queryItems();
                    assertTrue((items.size() % 5) == 0);
                }
            } catch (AuctionMarketException e) {
                e.printStackTrace();
            }
        });

        threadAddItems.start();
        threadQueryItems.start();

        try {
            threadAddItems.join();
            threadQueryItems.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        client.switchEpoch();
    }

    @AfterClass
    public static void tearDownAfterClass() throws AuctionMarketException {
        client.switchEpoch();
        if (!localTest) {
            ((AuctionMarketHTTPProxy) client).stop();
        }
    }

}