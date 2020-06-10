# kindle-highlights

This scans through the kindle highlights file and filters by books.

## Usage
- `kindle-highlights -f example.txt -l` or `kindle-highlights -f example.txt --list`: This lists all the books found in the file. It also adds a filter column that can be used when filtering. 
- `kindle-highlights -f example.txt --filter id/name_of_book`: This
  takes in a book id (provided by the list command) or the name of a
  book to filter by. It outputs all the highlights found from the file.

Features to add:
- support different types of highlights e.g. notes, bookmarks
