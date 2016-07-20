package com.acertainmarket.client.tests;

import com.acertainmarket.business.Bid;
import com.acertainmarket.business.ConcurrentCertainMarket;
import com.acertainmarket.business.Item;
import com.acertainmarket.business.ItemInfo;
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
public class CorrectnessTests {

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
        client.addItems(itemsToAdd);

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(8, 0, 10));
        client.bid(bidsToAdd);
    }

    /**
     * Tests that a bid on an item from a buyer who has already made a bid overwrites the old bid.
     */
    @Test
    public void testBidOverwrites() throws InterruptedException, AuctionMarketException, IOException {

        Set<Bid> bidToAdd = new HashSet<Bid>();
        bidToAdd.add(new Bid(8, 0, 11));
        Bid oldBid = new Bid(8, 0, 10);

        try {
            client.bid(bidToAdd);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<ItemInfo> bidsInMarket = (List<ItemInfo>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        for (ItemInfo info : bidsInMarket) {
            assertTrue(!info.bidEquals(oldBid)); // check it's not the same bid
        }
        assertTrue(bidsInMarket.size() == 1); // check there is still only one match.
    }

    /**
     * Tests that the correct bidder wins when a buyer underbids his previous bid.
     */
    @Test
    public void testUnderBidLoses() throws InterruptedException, AuctionMarketException, IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(7, 0, 10));
        bidsToAdd.add(new Bid(8, 0, 11));

        Set<Bid> underBid = new HashSet<Bid>();
        underBid.add(new Bid(8, 0, 9));

        try {
            client.bid(bidsToAdd);
            client.bid(underBid);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<ItemInfo> bidsInMarket = (List<ItemInfo>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        for (ItemInfo info : bidsInMarket) {
            assertTrue(info.getBuyerOrganizationID() == 7 && info.getBidAmount() == 10);
        }
        assertTrue(bidsInMarket.size() == 1); // check there is still only one match
    }

    /**
     * Tests that the correct bidder wins when a buyer overbids his previous bid.
     */
    @Test
    public void testOverBidWins() throws InterruptedException, AuctionMarketException, IOException {

        Set<Bid> bidsToAdd = new HashSet<Bid>();
        bidsToAdd.add(new Bid(7, 0, 10));
        bidsToAdd.add(new Bid(8, 0, 9));

        Set<Bid> overBid = new HashSet<Bid>();
        overBid.add(new Bid(8, 0, 11));

        try {
            client.bid(bidsToAdd);
            client.bid(overBid);
        } catch (AuctionMarketException e) {
        }

        client.switchEpoch();
        String in = readFile("out.xml", StandardCharsets.UTF_8);
        List<ItemInfo> bidsInMarket = (List<ItemInfo>) AuctionMarketUtility.deserializeXMLStringToObject(in);
        for (ItemInfo info : bidsInMarket) {
            assertTrue(info.getBuyerOrganizationID() == 8 && info.getBidAmount() == 11);
        }
        assertTrue(bidsInMarket.size() == 1); // check there is still only one match
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