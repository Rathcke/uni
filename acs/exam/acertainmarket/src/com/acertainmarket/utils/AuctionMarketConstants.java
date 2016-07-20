/**
 * 
 */
package com.acertainmarket.utils;

/**
 * AuctionMarketConstants declares the constants used in the CertainMarket (by
 * both servers and clients)
 * 
 */
public final class AuctionMarketConstants {

	// Constants used when creating URLs	
	public static final String ITEMID_PARAM = "ID";
	public static final String XMLSTRINGLEN_PARAM = "len";
	
	// Used as error code when converting numbers to integer
	public static final int INVALID_PARAMS = -1;

	// Constants used when creating exception messages
	// When item has an invalid ISBN
	public static final String INVALID = " is invalid";
	// When item is already in the store
	public static final String DUPLICATED = " is duplicated";
	// When item is not in the store
	public static final String NOT_AVAILABLE = " is not available";
	public static final String ITEM = "The Item: ";
	public static final String ID = "The ID: ";
	public static final String NULL_INPUT = "null input parameters";

	public static final String PROPERTY_KEY_LOCAL_TEST = "localtest";
	public static final String PROPERTY_KEY_SERVER_PORT = "port";
}
