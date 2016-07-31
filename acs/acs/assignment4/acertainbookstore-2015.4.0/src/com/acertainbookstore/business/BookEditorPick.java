package com.acertainbookstore.business;

/**
 * Structure to represent the editor's rating for a book
 * 
 */
public class BookEditorPick {
	private int ISBN;
	private boolean editorPick;

	public BookEditorPick(int ISBN, boolean editorPick) {
		this.setISBN(ISBN);
		this.setEditorPick(editorPick);
	}

	public int getISBN() {
		return ISBN;
	}

	public void setISBN(int iSBN) {
		ISBN = iSBN;
	}

	public boolean isEditorPick() {
		return editorPick;
	}

	public void setEditorPick(boolean editorPick) {
		this.editorPick = editorPick;
	}

	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		if ((this.getISBN() == ((BookEditorPick) obj).getISBN())) {
			return true;
		}
		return false;
	}

	public int hashCode() {
		return getISBN();
	}
}
