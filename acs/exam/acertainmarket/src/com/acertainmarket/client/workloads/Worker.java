/**
 * 
 */
package com.acertainmarket.client.workloads;

import com.acertainmarket.business.Bid;
import com.acertainmarket.business.Item;
import com.acertainmarket.interfaces.AuctionMarket;
import com.acertainmarket.utils.AuctionMarketException;

import java.util.*;
import java.util.concurrent.Callable;

/**
 * 
 * Worker represents the workload runner which runs the workloads with
 * parameters using WorkloadConfiguration and then reports the results
 * 
 */
public class Worker implements Callable<WorkerRunResult> {
	private WorkloadConfiguration configuration = null;
	private int numSuccessfulBidInteraction = 0;
	private int numTotalBidInteraction = 0;
    private int itemIDCounter = 0;
    private int bidCounter = 0;

	public Worker(WorkloadConfiguration config) {
		configuration = config;
	}

	/**
	 * Run the appropriate interaction while trying to maintain the configured
	 * distributions
	 * 
	 * Updates the counts of total runs and successful runs for customer
	 * interaction
	 */
	private boolean runInteraction(float chooseInteraction) {
		try {
			if (chooseInteraction < configuration
					.getPercentRareInteraction()) {
				runAddInteraction();
			} else if (chooseInteraction < configuration
					.getPercentFrequentInteraction()) {
				runQueryInteraction();
			} else {
				numTotalBidInteraction++;
				runBidInteraction();
				numSuccessfulBidInteraction++;
			}
		} catch (AuctionMarketException ex) {
			return false;
		}
		return true;
	}

	/**
	 * Run the workloads trying to respect the distributions of the interactions
	 * and return result in the end
	 */
	public WorkerRunResult call() throws Exception {
		int count = 1;
		long startTimeInNanoSecs = 0;
		long endTimeInNanoSecs = 0;
		int successfulInteractions = 0;
		long timeForRunsInNanoSecs = 0;

		Random rand = new Random();
		float chooseInteraction;

		// Perform the warmup runs
		while (count++ <= configuration.getWarmUpRuns()) {
			chooseInteraction = rand.nextFloat() * 100f;
			runInteraction(chooseInteraction);
		}

		count = 1;
		numTotalBidInteraction = 0;
		numSuccessfulBidInteraction = 0;

		// Perform the actual runs
		startTimeInNanoSecs = System.nanoTime();
		while (count++ <= configuration.getNumActualRuns()) {
			chooseInteraction = rand.nextFloat() * 100f;
            if (runInteraction(chooseInteraction)) {
				successfulInteractions++;
			}
		}
		endTimeInNanoSecs = System.nanoTime();
		timeForRunsInNanoSecs += (endTimeInNanoSecs - startTimeInNanoSecs);
		return new WorkerRunResult(successfulInteractions,
				timeForRunsInNanoSecs, configuration.getNumActualRuns(),
				numSuccessfulBidInteraction,
				numTotalBidInteraction);
	}

	/**
	 * Runs queryItems()
	 * 
	 * @throws AuctionMarketException
	 */
	private void runQueryInteraction() throws AuctionMarketException {

		AuctionMarket auctionMarket = configuration.getAuctionMarket();
        auctionMarket.queryItems();
	}

	/**
	 * Runs addItems() with a random set of items.
	 * 
	 * @throws AuctionMarketException
	 */
	private void runAddInteraction() throws AuctionMarketException {

		int numItems = configuration.getNumItemsToAdd();
        AuctionMarket auctionMarket = configuration.getAuctionMarket();

        Set<Item> itemsToAdd = new HashSet<>();

        for (int i = 0; i < numItems; i++){
            itemsToAdd.add(new Item(itemIDCounter, Integer.toString(itemIDCounter), 1));
            ++itemIDCounter;
        }

        auctionMarket.addItems(itemsToAdd);
    }

	/**
	 * Runs bid() with a random set of bids.
	 * 
	 * @throws AuctionMarketException
	 */
	private void runBidInteraction() throws AuctionMarketException {

        int numBids = configuration.getNumItemsToBid();
        AuctionMarket auctionMarket = configuration.getAuctionMarket();

        Set<Bid> bidsToAdd = new HashSet<>();

        for (int i = 0; i < numBids; i++){
            bidsToAdd.add(new Bid(1, bidCounter, 10));
            ++bidCounter;
        }

        auctionMarket.bid(bidsToAdd);
    }


}
