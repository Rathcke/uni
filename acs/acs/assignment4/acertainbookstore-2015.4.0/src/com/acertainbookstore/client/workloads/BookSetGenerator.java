package com.acertainbookstore.client.workloads;

import java.util.*;

import com.acertainbookstore.business.ImmutableStockBook;
import com.acertainbookstore.business.StockBook;

/**
 * Helper class to generate stockbooks and isbns modelled similar to Random
 * class
 */
public class BookSetGenerator {

	public BookSetGenerator() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * Returns num randomly selected isbns from the input set
	 * 
	 * @param num
	 * @return
	 */
	public Set<Integer> sampleFromSetOfISBNs(Set<Integer> isbns, int num) {
        ArrayList<Integer> ISBNlist = new ArrayList<Integer>();
        for (int i = 0; i < isbns.size(); i++) {
            ISBNlist.add(new Integer(i));
        }

        Collections.shuffle(ISBNlist);
        Set<Integer> sampleSet = new HashSet<>();
        ArrayList<Integer> ISBNinds = new ArrayList<>();
        ISBNinds.addAll(isbns);

        for (int i = 0; i < num; i++) {
            sampleSet.add(ISBNinds.get(ISBNlist.get(i)));
        }

        return sampleSet;
	}

	/**
	 * Return num stock books. For now return an ImmutableStockBook
	 * 
	 * @param num
	 * @return
	 */
	public Set<StockBook> nextSetOfStockBooks(int num) {
        StockBook stockBook;
        Set<StockBook> nextSet = new HashSet<>();
        Random rand = new Random();

        for (int i = 1; i <= num; i++){
			int isbn = rand.nextInt(1000) + 1;
			int copiesInStock = 50 + rand.nextInt(100);

            stockBook = new ImmutableStockBook(isbn, Integer.toString(isbn), Integer.toString(isbn), 99,
					copiesInStock, 0, 0, 0, rand.nextBoolean());
            nextSet.add(stockBook);
        }

		return nextSet;
	}
}
