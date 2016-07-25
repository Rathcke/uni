/**
 * 
 */
package com.acertainbookstore.client.workloads;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.IntStream;

import com.acertainbookstore.business.CertainBookStore;
import com.acertainbookstore.business.ImmutableStockBook;
import com.acertainbookstore.business.StockBook;
import com.acertainbookstore.client.BookStoreHTTPProxy;
import com.acertainbookstore.client.StockManagerHTTPProxy;
import com.acertainbookstore.interfaces.BookStore;
import com.acertainbookstore.interfaces.StockManager;
import com.acertainbookstore.utils.BookStoreConstants;
import com.acertainbookstore.utils.BookStoreException;

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
		// overriden if the property is set
		String localTestProperty = System
				.getProperty(BookStoreConstants.PROPERTY_KEY_LOCAL_TEST);
		localTest = (localTestProperty != null) ? Boolean
				.parseBoolean(localTestProperty) : localTest;

		BookStore bookStore;
		StockManager stockManager;
		if (localTest) {
			CertainBookStore store = new CertainBookStore();
			bookStore = store;
			stockManager = store;
		} else {
			stockManager = new StockManagerHTTPProxy(serverAddress + "/stock");
			bookStore = new BookStoreHTTPProxy(serverAddress);
		}

		// Generate data in the bookstore before running the workload
		initializeBookStoreData(bookStore, stockManager);

        List<Integer> numThreads = new ArrayList<>(Arrays.asList(2, 4, 6, 8, 10));
        for (Integer j : numThreads) {
            int numConcurrentWorkloadThreads = j;
            ExecutorService exec = Executors
                    .newFixedThreadPool(numConcurrentWorkloadThreads);

            for (int i = 0; i < numConcurrentWorkloadThreads; i++) {
                WorkloadConfiguration config = new WorkloadConfiguration(bookStore,
                        stockManager);
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

            // Finished initialization, stop the clients if not localTest
            if (!localTest) {
                ((BookStoreHTTPProxy) bookStore).stop();
                ((StockManagerHTTPProxy) stockManager).stop();
            }

            reportMetric(workerRunResults);
        }
	}

	/**
	 * Computes the metrics and prints them
	 * 
	 * @param workerRunResults
	 */
	public static void reportMetric(List<WorkerRunResult> workerRunResults) {
        float aggTotalInteraction = 0;
        float aggElapsedTime = 0;
        float aggThroughput = 0;
        double avgLatency = 0;

        for (WorkerRunResult runRes : workerRunResults){
            double customerInteractionFreq = runRes.getTotalFrequentBookStoreInteractionRuns()/runRes.getTotalRuns();
            double succFreq = (double) runRes.getSuccessfulInteractions()/runRes.getTotalRuns();
            if (succFreq > 0.99 && ((customerInteractionFreq < 0.70) || (customerInteractionFreq > 0.50))){
                aggTotalInteraction += runRes.getTotalRuns();
                aggThroughput += runRes.getSuccessfulInteractions()/(runRes.getElapsedTimeInNanoSecs()*Math.pow(10,
                        -9));
                aggElapsedTime += runRes.getElapsedTimeInNanoSecs();
            }

        }
        avgLatency = aggElapsedTime/aggTotalInteraction;
        System.out.println(aggThroughput);
        System.out.println(avgLatency);
    }

	/**
	 * Generate the data in bookstore before the workload interactions are run
	 * 
	 * Ignores the serverAddress if its a localTest
	 * 
	 */
	public static void initializeBookStoreData(BookStore bookStore,
			StockManager stockManager) throws BookStoreException {

		Set<StockBook> initBooks = new HashSet<>();
        Random rand = new Random();

        for (int i = 0; i < 500; i++) {
            int isbn = rand.nextInt(1000)+1;
			int copiesInStock = 50 + rand.nextInt(100);

			initBooks.add(new ImmutableStockBook(isbn, Integer.toString(isbn), Integer.toString(isbn), 99,
                    copiesInStock, 0, 0, 0, rand.nextBoolean()));
        }

        stockManager.addBooks(initBooks);
	}
}
