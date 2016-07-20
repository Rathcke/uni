/**
 * 
 */
package com.acertainmarket.client.workloads;

import com.acertainmarket.business.ConcurrentCertainMarket;
import com.acertainmarket.client.AuctionMarketHTTPProxy;
import com.acertainmarket.interfaces.AuctionMarket;
import com.acertainmarket.utils.AuctionMarketConstants;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

/**
 * 
 * CertainWorkload class runs the workloads by different workers concurrently.
 * It configures the environment for the workers using WorkloadConfiguration
 * objects and reports the metrics
 * 
 */
public class CertainWorkload {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {

		String serverAddress = "http://localhost:8081";
		boolean localTest = true;
		List<WorkerRunResult> workerRunResults = new ArrayList<WorkerRunResult>();
		List<Future<WorkerRunResult>> runResults = new ArrayList<Future<WorkerRunResult>>();

		// Initialize the RPC interfaces if its not a localTest, the variable is
		// overridden if the property is set
		String localTestProperty = System
				.getProperty(AuctionMarketConstants.PROPERTY_KEY_LOCAL_TEST);
		localTest = (localTestProperty != null) ? Boolean
				.parseBoolean(localTestProperty) : localTest;

		AuctionMarket auctionMarket;

		if (localTest) {
			ConcurrentCertainMarket store = new ConcurrentCertainMarket();
			auctionMarket = store;
		} else {
			auctionMarket = new AuctionMarketHTTPProxy(serverAddress);
		}

        List<Integer> numThreads = new ArrayList<>(Arrays.asList(2, 4, 6, 8, 10, 12, 14, 16, 18, 20));
        for (Integer j : numThreads) {
            int numConcurrentWorkloadThreads = j;
            ExecutorService exec = Executors
                    .newFixedThreadPool(numConcurrentWorkloadThreads);

            for (int i = 0; i < numConcurrentWorkloadThreads; i++) {
                WorkloadConfiguration config = new WorkloadConfiguration(auctionMarket);
                Worker workerTask = new Worker(config);
                // Keep the futures to wait for the result from the thread
                runResults.add(exec.submit(workerTask));
            }

            // Get the results from the threads using the futures returned
            for (Future<WorkerRunResult> futureRunResult : runResults) {
                WorkerRunResult runResult = futureRunResult.get(); // blocking call
                workerRunResults.add(runResult);
            }

            exec.shutdownNow(); // shutdown the executor

            reportMetric(workerRunResults, j);
            runResults.clear();
            workerRunResults.clear();
            auctionMarket.switchEpoch();

        }

        // Finished initialization, stop the clients if not localTest
        if (!localTest) {
            ((AuctionMarketHTTPProxy) auctionMarket).stop();
        }

	}

	/**
	 * Computes the metrics and prints them
	 * 
	 * @param workerRunResults
	 */
	public static void reportMetric(List<WorkerRunResult> workerRunResults, int threads) {

        float aggTotalInteraction = 0;
        float aggElapsedTime = 0;
        float aggThroughput = 0;

        for (WorkerRunResult runRes : workerRunResults) {
            double buyerInteractionFreq = runRes.getTotalFrequentAuctionMarketInteractionRuns()/runRes.getTotalRuns();
            double succFreq = (double) runRes.getSuccessfulInteractions()/runRes.getTotalRuns();
            if (succFreq > 0.5 && ((buyerInteractionFreq < 0.95) || (buyerInteractionFreq > 0.6))){
                aggTotalInteraction += runRes.getTotalRuns();
                aggThroughput += runRes.getSuccessfulInteractions()/(runRes.getElapsedTimeInNanoSecs()*Math.pow(10,
                        -9));
            }

        }
        //System.out.println("Measured throughput with " + threads + " threads:");
        //System.out.println("Agg. Throughput: " + aggThroughput);
        System.out.println(aggThroughput);
    }

}
