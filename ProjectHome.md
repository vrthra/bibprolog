Converter from bibtex to prolog

To run, check out the source, and run make in the directory. It would compile using GNU-Prolog and create bibdb in the same directory. Use the bibbd with the .db.bib in the same directory (concatenate your bib files to .bib.db in the directory from where bibdb is run).
## Screen Shot ##
The format for querying is "key : value" for matching, "key ! value" for anything but value, "expr ; expr" for boolean or and "expr ,expr " for boolean and. Nesting using parenthesis is also supported.

![http://bibprolog.googlecode.com/hg/screen.png](http://bibprolog.googlecode.com/hg/screen.png)

String (case insensitive) matches are possible with ~ and boolean expressions >, <, >=, <= are supported.

![http://bibprolog.googlecode.com/hg/screen2.png](http://bibprolog.googlecode.com/hg/screen2.png)

(The finite domain solver for >, < etc are on range 1 .. 10000. Change the fd\_domain value if you need higher numbers).
You can also search for specific type of resources e.g

All articles that have probability in the title
```
*article , title ~ probability
```
Just print your db
```
*
```
Look for a particular string
```
*string, key ~ march
```
```
*string, val ~ mystring
```
## More ##

![http://bibprolog.googlecode.com/hg/screen3.png](http://bibprolog.googlecode.com/hg/screen3.png)

![http://bibprolog.googlecode.com/hg/screen4.png](http://bibprolog.googlecode.com/hg/screen4.png)

![http://bibprolog.googlecode.com/hg/screen5.png](http://bibprolog.googlecode.com/hg/screen5.png)


## Important Information ##
The downloads are usually out of date. Either you can checkout the source, run make on the directory, and it creates the bibdb executable. Or since it is a single prolog file bibdb.pl, just download it, import your bibtex entries to .db.bib file in the same directory, and run it with gnu-prolog. Use the command "help" at bibdb> prompt for most uptodate help.