package com.acertainmarket.utils;

import com.acertainmarket.business.Item;

import java.util.List;

/**
 * Data Structure that we use to communicate objects and error messages
 * from the server to the client.
 * 
 */
public class AuctionMarketResponse {
	private AuctionMarketException exception;
	private List<?> list;

	public AuctionMarketException getException() {
		return exception;
	}

	public void setException(AuctionMarketException exception) {
		this.exception = exception;
	}

	public AuctionMarketResponse(AuctionMarketException exception, List<Item> list) {
		this.setException(exception);
		this.setList(list);
	}

	public AuctionMarketResponse() {
		this.setException(null);
		this.setList(null);
	}

	public List<?> getList() {
		return list;
	}

	public void setList(List<?> list) {
		this.list = list;
	}
}
