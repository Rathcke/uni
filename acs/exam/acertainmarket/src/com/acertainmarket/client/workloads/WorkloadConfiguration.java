package com.acertainmarket.client.workloads;


import com.acertainmarket.interfaces.AuctionMarket;

/**
 * 
 * WorkloadConfiguration represents the configuration parameters to be used by
 * Workers class for running the workloads
 * 
 */
public class WorkloadConfiguration {
	private int numItemsToAdd = 3;
	private int numItemsToBid = 3;
    private int numSellerThreads = 5;
	private int warmUpRuns = 100;
	private int numActualRuns = 500;
	private float percentRareInteraction = 20f;
	private float percentFrequentInteraction = 80f;
	private AuctionMarket auctionMarket = null;

	public WorkloadConfiguration(AuctionMarket auctionMarket) throws Exception {
		this.auctionMarket = auctionMarket;
	}

	public int getNumItemsToAdd() {
		return numItemsToAdd;
	}

	public void setNumItemsToAdd(int numItemsToAdd) {this.numItemsToAdd = numItemsToAdd; }

	public int getNumItemsToBid() {
		return numItemsToBid;
	}

	public void setNumItemsToBid(int numItemsToBid) {
		this.numItemsToBid = numItemsToBid;
	}

    public int getNumSellerThreads() {
        return numSellerThreads;
    }

    public void setNumSellerThreads(int sellers) {
        this.numItemsToBid = sellers;
    }

	public AuctionMarket getAuctionMarket() {
		return auctionMarket;
	}

	public void setAuctionMarket(AuctionMarket auctionMarket) {
		this.auctionMarket = auctionMarket;
	}

	public float getPercentRareInteraction() {
		return percentRareInteraction;
	}

	public void setPercentRareInteraction(float percentRareInteraction) {
		this.percentRareInteraction = percentRareInteraction;
	}

	public float getPercentFrequentInteraction() {
		return percentFrequentInteraction;
	}

	public void setPercentFrequentInteraction(float percentFrequentInteraction) {
		this.percentFrequentInteraction = percentFrequentInteraction;
	}
	public int getWarmUpRuns() {
		return warmUpRuns;
	}

	public void setWarmUpRuns(int warmUpRuns) {
		this.warmUpRuns = warmUpRuns;
	}

	public int getNumActualRuns() {
		return numActualRuns;
	}

	public void setNumActualRuns(int numActualRuns) {
		this.numActualRuns = numActualRuns;
	}

}
