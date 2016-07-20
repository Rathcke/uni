package com.acertainmarket.client.tests;

import com.acertainmarket.business.Bid;
import com.acertainmarket.business.ConcurrentCertainMarket;
import com.acertainmarket.business.Item;
import com.acertainmarket.client.AuctionMarketHTTPProxy;
import com.acertainmarket.interfaces.AuctionMarket;
import com.acertainmarket.utils.AuctionMarketConstants;
import com.acertainmarket.utils.AuctionMarketException;
import com.acertainmarket.utils.AuctionMarketUtility;
import org.junit.*;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertTrue;

/**
 * Test class to test the AuctionMarket interface
 *
 */
public class AllOrNothingAtomicityTests {

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
     * Helper method to add some items to the market - executed before every test run.
     */
    @Before
    public void initializeItemsAndBids() throws AuctionMarketException {

        Set<Item> itemsToAdd = new HashSet<Item>();
        itemsToAdd.add(new Item(0, "validItem", 9));
        itemsToAdd.add(new Item(1, "validItem1", 8));
        itemsToAdd.add(new Item(2, "validItem2", 7));
        itemsToAdd.add(new Item(3, "validItem3", 6));
        client.addItems(itemsToAdd);

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(8, 0, 10));
        bidsToAdd.add(new Bid(8, 1, 20));
        client.bid(bidsToAdd);
    }

    /**
     * Tests that a client either adds all items or none at all when there is an invalid item ID.
     * The client tries to add an invalid item along with a valid item and invokes queryItems()
     * to test that none were added.
     */
    @Test
    public void testAllOrNothingItemInvalidItemID() throws InterruptedException, AuctionMarketException {

        Set<Item> itemsToAdd = new HashSet<Item>();
        itemsToAdd.add(new Item(4, "validItem4", 9));
        itemsToAdd.add(new Item(-1, "invalidItem", 9));

        try {
            client.addItems(itemsToAdd);
        } catch (AuctionMarketException e) {
        }

        List<Item> itemsInMarket = client.queryItems();
        assertTrue(itemsInMarket.size() == 4);
        client.switchEpoch();
    }

    /**
     * Tests that a client either adds all items or none at all when there is an invalid seller ID.
     * The client tries to add an invalid item along with a valid item and invokes queryItems()
     * to test that none were added.
     */
    @Test
    public void testAllOrNothingItemInvalidSellerID() throws InterruptedException, AuctionMarketException {

        Set<Item> itemsToAdd = new HashSet<Item>();
        itemsToAdd.add(new Item(4, "validItem4", 9));
        itemsToAdd.add(new Item(5, "invalidItem", -1));

        try {
            client.addItems(itemsToAdd);
        } catch (AuctionMarketException e) {
        }

        List<Item> itemsInMarket = client.queryItems();
        assertTrue(itemsInMarket.size() == 4);
        client.switchEpoch();
    }

    /**
     * Tests that a client either adds all items or none at all when there is a duplicate item.
     * The client tries to add an invalid item along with a valid item and invokes queryItems()
     * to test that none were added.
     */
    @Test
    public void testAllOrNothingItemDuplicateItem() throws InterruptedException, AuctionMarketException {

        Set<Item> itemsToAdd = new HashSet<Item>();
        itemsToAdd.add(new Item(4, "validItem4", 9));
        itemsToAdd.add(new Item(2, "invalidItem", 9));

        try {
            client.addItems(itemsToAdd);
        } catch (AuctionMarketException e) {
        }

        List<Item> itemsInMarket = client.queryItems();
        assertTrue(itemsInMarket.size() == 4);
        client.switchEpoch();
    }

    /**
     * Tests that a client either adds all items or none at all when all items are valid.
     * The client tries to add a valid item and invokes queryItems()
     * to test that they were added.
     */
    @Test
    public void testAllOrNothingItemAllItemsAdded() throws InterruptedException, AuctionMarketException {

        Set<Item> itemsToAdd = new HashSet<Item>();
        itemsToAdd.add(new Item(4, "validItem4", 9));
        itemsToAdd.add(new Item(5, "validItem5", 9));
        itemsToAdd.add(new Item(6, "validItem6", 9));

        try {
            client.addItems(itemsToAdd);
        } catch (AuctionMarketException e) {
        }

        List<Item> itemsInMarket = client.queryItems();
        assertTrue(itemsInMarket.size() == 7);
        client.switchEpoch();
    }

    /**
     * Tests that a client either adds all bids or none at all when one bid has an invalid item ID.
     * The client tries to add an invalid bid along with a valid and checks that no bids were added.
     */
    @Test
    public void testAllOrNothingBidInvalidItemID() throws InterruptedException, AuctionMarketException, IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(4, 2, 9));
        bidsToAdd.add(new Bid(4, -1, 9));

        try {
            client.bid(bidsToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<?> bidsInMarket = (List<?>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        assertTrue(bidsInMarket.size() == 2);
    }

    /**
     * Tests that a client either adds all bids or none at all when one bid has an invalid buyer ID.
     * The client tries to add an invalid bid along with a valid and checks that no bids were added.
     */
    @Test
    public void testAllOrNothingBidInvalidBuyerID() throws InterruptedException, AuctionMarketException, IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(4, 2, 9));
        bidsToAdd.add(new Bid(-1, 3, 9));

        try {
            client.bid(bidsToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<?> bidsInMarket = (List<?>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        assertTrue(bidsInMarket.size() == 2);
    }

    /**
     * Tests that a client either adds all bids or none at all when one bid has an invalid amount.
     * The client tries to add an invalid bid along with a valid and checks that no bids were added.
     */
    @Test
    public void testAllOrNothingBidInvalidAmount() throws InterruptedException, AuctionMarketException, IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(4, 2, 9));
        bidsToAdd.add(new Bid(4, 3, -1));

        try {
            client.bid(bidsToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<?> bidsInMarket = (List<?>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        assertTrue(bidsInMarket.size() == 2);
    }

    /**
     * Tests that a client either adds all bids or none at all when one bid has a nonexistant item ID.
     * The client tries to add an invalid bid along with a valid and checks that no bids were added.
     */
    @Test
    public void testAllOrNothingBidNonExistantItemID() throws InterruptedException, AuctionMarketException,
            IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(4, 2, 9));
        bidsToAdd.add(new Bid(4, 9, 9));

        try {
            client.bid(bidsToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<?> bidsInMarket = (List<?>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        assertTrue(bidsInMarket.size() == 2);
    }

    /**
     * Tests that a client either adds all bids or none at all when all bids are valid and new.
     * The client tries to add valid bids and check that all bids were added.
     */
    @Test
    public void testAllOrNothingBidAllBidsAddedWithoutReplace() throws InterruptedException, AuctionMarketException,
            IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(4, 2, 9));
        bidsToAdd.add(new Bid(4, 3, 9));

        try {
            client.bid(bidsToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<?> bidsInMarket = (List<?>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        assertTrue(bidsInMarket.size() == 4);
    }

    /**
     * Tests that a client either adds all bids or none at all when all bids are valid and replaces one.
     * The client tries to add valid bids and check that all bids were added.
     */
    @Test
    public void testAllOrNothingBidAllBidsAddedWithReplace() throws InterruptedException, AuctionMarketException,
            IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(4, 2, 9));
        bidsToAdd.add(new Bid(8, 1, 11));

        try {
            client.bid(bidsToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<?> bidsInMarket = (List<?>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        assertTrue(bidsInMarket.size() == 3);
    }

    /**
     * Helper function to get ItemInfo when switchEpoch() is invoked.
     */
    static String readFile(String path, Charset encoding)
            throws IOException
    {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, encoding);
    }


    @AfterClass
    public static void tearDownAfterClass() throws AuctionMarketException {
        client.switchEpoch();
        if (!localTest) {
            ((AuctionMarketHTTPProxy) client).stop();
        }
    }

}