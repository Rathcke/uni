package com.acertainmarket.client.workloads;

/**
 * 
 * WorkerRunResult class represents the result returned by a worker class after
 * running the workload interactions
 * 
 */
public class WorkerRunResult {
	private int successfulInteractions;
	private int totalRuns;
	private long elapsedTimeInNanoSecs;
	private int successfulFrequentAuctionMarketInteractionRuns;
	private int totalFrequentAuctionMarketInteractionRuns;

	public WorkerRunResult(int successfulInteractions, long elapsedTimeInNanoSecs,
			int totalRuns, int successfulFrequentAuctionMarketInteractionRuns,
			int totalFrequentAuctionMarketInteractionRuns) {
		this.setSuccessfulInteractions(successfulInteractions);
		this.setElapsedTimeInNanoSecs(elapsedTimeInNanoSecs);
		this.setTotalRuns(totalRuns);
		this.setSuccessfulFrequentAuctionMarketInteractionRuns(successfulFrequentAuctionMarketInteractionRuns);
		this.setTotalFrequentAuctionMarketInteractionRuns(totalFrequentAuctionMarketInteractionRuns);
	}

	public int getTotalRuns() {
		return totalRuns;
	}

	public void setTotalRuns(int totalRuns) {
		this.totalRuns = totalRuns;
	}

	public int getSuccessfulInteractions() {
		return successfulInteractions;
	}

	public void setSuccessfulInteractions(int successfulInteractions) {
		this.successfulInteractions = successfulInteractions;
	}

	public long getElapsedTimeInNanoSecs() {
		return elapsedTimeInNanoSecs;
	}

	public void setElapsedTimeInNanoSecs(long elapsedTimeInNanoSecs) {
		this.elapsedTimeInNanoSecs = elapsedTimeInNanoSecs;
	}

	public int getSuccessfulFrequentAuctionMarketInteractionRuns() {
		return successfulFrequentAuctionMarketInteractionRuns;
	}

	public void setSuccessfulFrequentAuctionMarketInteractionRuns(
			int successfulFrequentAuctionMarketInteractionRuns) {
		this.successfulFrequentAuctionMarketInteractionRuns = successfulFrequentAuctionMarketInteractionRuns;
	}

	public int getTotalFrequentAuctionMarketInteractionRuns() {
		return totalFrequentAuctionMarketInteractionRuns;
	}

	public void setTotalFrequentAuctionMarketInteractionRuns(
			int totalFrequentAuctionMarketInteractionRuns) {
		this.totalFrequentAuctionMarketInteractionRuns = totalFrequentAuctionMarketInteractionRuns;
	}

}
