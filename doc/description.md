
# Idea

The basic notion is that of entry. What an entry is depends on its *kind*. Each
kind corresponds to a specific way to store the entry. Each entry has a name, a
short text that can be used to link to the entry. Furthermore, each entry has
an optional *position* (either a point, a line/gpx or a polygon), an optional
time range and arbitrary metadata in the form of keys associated to JSON
values.

The wiki is backed by a directory versionned by git-annex. It requires a
postgresql database to function correctly, but this database does not store
data not present in the files. Syncing different instances then means syncing
the git repos, and then syncing the state of the file tree with the database.

The state actually stored in the database will be dependent on the version of
the wiki engine, but the stored files should always look the same.

One particular kind is that of *notes*. Notes are markdown files, and serve as
the main structuring element of the wiki. Anything that connects multiple
entries is a note, that contains in its metadata block a list of entries in
contains.

# Kinds

notes

: A note is a pandoc markdown file, flatly stored in a directory. It is used as
the basis for the wiki. Metadata for files is stored at the top of each file in
yaml.

links

: Links are stored in a csv file, storing the name, link destination and date
of adding of each link.

files

: Files are stored in a directory, in a `year/month/day` manner depending on
the date either of the document or if the document has a builtin date using
this one.

mails

: Figure out a way to store mails.

headers

: A specific sub-header in a note. Metadata is stored in a specific code block.

# Notes

Notes are stored as pandoc markdown files. The web rendering engine for
markdown files will support many additional features, like the ability to embed
arbitrary haskell code that will be run on rendering to add interactive
features to the pages. Ideally some notebook features will be implemented, and
some excel-like features to tables.
