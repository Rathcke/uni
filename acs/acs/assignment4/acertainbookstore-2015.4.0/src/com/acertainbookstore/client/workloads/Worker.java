/**
 * 
 */
package com.acertainbookstore.client.workloads;

import java.util.*;

import java.util.Random;
import java.util.concurrent.Callable;

import com.acertainbookstore.business.Book;
import com.acertainbookstore.business.BookCopy;
import com.acertainbookstore.business.StockBook;
import com.acertainbookstore.interfaces.BookStore;
import com.acertainbookstore.interfaces.StockManager;
import com.acertainbookstore.utils.BookStoreException;

/**
 * 
 * Worker represents the workload runner which runs the workloads with
 * parameters using WorkloadConfiguration and then reports the results
 * 
 */
public class Worker implements Callable<WorkerRunResult> {
	private WorkloadConfiguration configuration = null;
	private int numSuccessfulFrequentBookStoreInteraction = 0;
	private int numTotalFrequentBookStoreInteraction = 0;

	public Worker(WorkloadConfiguration config) {
		configuration = config;
	}

	/**
	 * Run the appropriate interaction while trying to maintain the configured
	 * distributions
	 * 
	 * Updates the counts of total runs and successful runs for customer
	 * interaction
	 * 
	 * @param chooseInteraction
	 * @return
	 */
	private boolean runInteraction(float chooseInteraction) {
		try {
			if (chooseInteraction < configuration
					.getPercentRareStockManagerInteraction()) {
				runRareStockManagerInteraction();
			} else if (chooseInteraction < configuration
					.getPercentFrequentStockManagerInteraction()) {
				runFrequentStockManagerInteraction();
			} else {
				numTotalFrequentBookStoreInteraction++;
				runFrequentBookStoreInteraction();
				numSuccessfulFrequentBookStoreInteraction++;
			}
		} catch (BookStoreException ex) {
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
		numTotalFrequentBookStoreInteraction = 0;
		numSuccessfulFrequentBookStoreInteraction = 0;

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
				numSuccessfulFrequentBookStoreInteraction,
				numTotalFrequentBookStoreInteraction);
	}

	/**
	 * Runs the new stock acquisition interaction
	 * 
	 * @throws BookStoreException
	 */
	private void runRareStockManagerInteraction() throws BookStoreException {
        int numBooks = configuration.getNumBooksToAdd();
        StockManager stockManager = configuration.getStockManager();
        BookSetGenerator bookSetGenerator = configuration.getBookSetGenerator();

        List<StockBook> books = stockManager.getBooks();
        Set<StockBook> genBooks = bookSetGenerator.nextSetOfStockBooks(numBooks);
        Set<StockBook> booksToAdd = new HashSet<>();

        for (StockBook book : genBooks){
            if (!containsISBN(books, book.getISBN()) ){
                booksToAdd.add(book);
            }
        }

        stockManager.addBooks(booksToAdd);
	}

	/**
	 * Runs the stock replenishment interaction
	 * 
	 * @throws BookStoreException
	 */
	private void runFrequentStockManagerInteraction() throws BookStoreException {
        int numCopies = configuration.getNumAddCopies();
        int numBooksWithLeastCopies = configuration.getNumBooksWithLeastCopies();
        StockManager stockManager = configuration.getStockManager();

        List<StockBook> books = stockManager.getBooks();
        Set<BookCopy> copiesToAdd = new HashSet<>();

        Collections.sort(books, new Comparator<StockBook>() {
            @Override
            public int compare(StockBook stockBook, StockBook t1) {
                float stock1 = stockBook.getNumCopies();
                float stock2 = t1.getNumCopies();
                if (stock1 > stock2) return -1;
                if (stock1 < stock2) return 1;
                return 0;
            }
        });

        for (int i = 0; i < numBooksWithLeastCopies; i++){
            copiesToAdd.add(new BookCopy(books.get(i).getISBN(), numCopies));
        }

        stockManager.addCopies(copiesToAdd);
    }

	/**
	 * Runs the customer interaction
	 * 
	 * @throws BookStoreException
	 */
	private void runFrequentBookStoreInteraction() throws BookStoreException {
        int numBooksToBuy = configuration.getNumBooksToBuy();
        int numEditorPicks = configuration.getNumEditorPicksToGet();
        int numBookCopiesToBuy = configuration.getNumBookCopiesToBuy();
        BookStore bookStore = configuration.getBookStore();
        BookSetGenerator bookSetGenerator = configuration.getBookSetGenerator();

        Set<BookCopy> booksToBuy = new HashSet<>();
        Set<Integer> ISBNsToBuy = new HashSet<>();
        Set<Integer> ISBNList = new HashSet<>();
        List<Book> editorPicks = bookStore.getEditorPicks(numEditorPicks);


        for (Book editorPick : editorPicks){
            ISBNList.add(editorPick.getISBN());
        }

        ISBNsToBuy = bookSetGenerator.sampleFromSetOfISBNs(ISBNList, numBooksToBuy);
        //System.out.println(ISBNsToBuy);

        for (Integer isbn : ISBNsToBuy){
            booksToBuy.add(new BookCopy(isbn, numBookCopiesToBuy));
        }

        bookStore.buyBooks(booksToBuy);
    }

    /**
     * Helper function that checks if an ISBN is in a list of StockBook
     * @param list
     * @param isbn
     * @return
     */
    public static boolean containsISBN(List<StockBook> list, long isbn) {
        for (StockBook book : list) {
            if (book.getISBN() == isbn) {
                return true;
            }
        }
        return false;
    }

}
